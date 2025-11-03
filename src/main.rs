use anyhow::{Context, Result};
use clap::Parser;
use full_moon::{ast, visitors::Visitor};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser)]
#[command(author, version, about)]
struct Cli {
    #[arg(short, long)]
    banner: Option<PathBuf>,
    #[arg(short, long)]
    input: PathBuf,
    #[arg(short, long)]
    output: PathBuf,
    #[arg(short, long, default_value = ".")]
    root: PathBuf,
}

const KEYWORDS: &[&str] = &[
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local",
    "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
];

struct ReqVisitor {
    requires: Vec<String>,
}

impl ReqVisitor {
    fn new() -> Self {
        Self {
            requires: Vec::new(),
        }
    }
}

impl Visitor for ReqVisitor {
    fn visit_function_call(&mut self, call: &ast::FunctionCall) {
        if let ast::Prefix::Name(name) = call.prefix() {
            if name.token().to_string() == "require" {
                for suffix in call.suffixes() {
                    if let ast::Suffix::Call(ast::Call::AnonymousCall(
                        ast::FunctionArgs::Parentheses { arguments, .. },
                    )) = suffix
                    {
                        if let Some(ast::Expression::String(token)) = arguments.iter().next() {
                            let module = token.token().to_string();
                            self.requires
                                .push(module.trim_matches(|c| c == '"' || c == '\'').to_string());
                        }
                    }
                }
            }
        }
    }
}

struct Collector {
    locals: HashSet<String>,
}

impl Collector {
    fn new() -> Self {
        Self {
            locals: HashSet::new(),
        }
    }

    fn collect(&mut self, content: &str) -> Result<()> {
        let ast = full_moon::parse(content).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
        self.visit_ast(&ast);
        Ok(())
    }
}

impl Visitor for Collector {
    fn visit_local_assignment(&mut self, assignment: &ast::LocalAssignment) {
        for name in assignment.names() {
            self.locals.insert(name.to_string().trim().to_string());
        }
    }

    fn visit_local_function(&mut self, func: &ast::LocalFunction) {
        self.locals
            .insert(func.name().to_string().trim().to_string());
    }

    fn visit_function_body(&mut self, body: &ast::FunctionBody) {
        for param in body.parameters() {
            self.locals.insert(param.to_string().trim().to_string());
        }
    }

    fn visit_generic_for(&mut self, generic_for: &ast::GenericFor) {
        for name in generic_for.names() {
            self.locals.insert(name.to_string().trim().to_string());
        }
    }

    fn visit_numeric_for(&mut self, numeric_for: &ast::NumericFor) {
        self.locals
            .insert(numeric_for.index_variable().to_string().trim().to_string());
    }
}

struct Bundler {
    root: PathBuf,
    visited: HashSet<PathBuf>,
    modules: HashMap<String, String>,
    order: Vec<String>,
}

impl Bundler {
    fn new(root: PathBuf) -> Self {
        Self {
            root,
            visited: HashSet::new(),
            modules: HashMap::new(),
            order: Vec::new(),
        }
    }

    fn find_module(&self, name: &str, current: Option<&Path>) -> Result<PathBuf> {
        let filename = if name.ends_with(".lua") {
            name.to_string()
        } else {
            format!("{}.lua", name)
        };

        let path = self.root.join(&filename);
        if path.exists() {
            return path.canonicalize().context("Failed to canonicalize");
        }

        if let Some(current_path) = current {
            if let Some(parent) = current_path.parent() {
                let path = parent.join(&filename);
                if path.exists() {
                    return path.canonicalize().context("Failed to canonicalize");
                }
            }
        }

        anyhow::bail!("Module not found: {}", name)
    }

    fn normalize_path(&self, path: &Path) -> Result<String> {
        let root_canonical = self.root.canonicalize()?;
        let relative = path.strip_prefix(&root_canonical).unwrap_or(path);
        Ok(relative.to_string_lossy().replace('\\', "/"))
    }

    fn process_module(&mut self, path: &Path) -> Result<String> {
        let canonical = path.canonicalize()?;
        let key = self.normalize_path(&canonical)?;

        if self.visited.contains(&canonical) {
            return Ok(key);
        }
        self.visited.insert(canonical.clone());

        let content = fs::read_to_string(&canonical)?;
        let ast =
            full_moon::parse(&content).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

        let mut visitor = ReqVisitor::new();
        visitor.visit_ast(&ast);

        for require_name in &visitor.requires {
            let require_path = self.find_module(require_name, Some(&canonical))?;
            self.process_module(&require_path)?;
        }

        if !self.modules.contains_key(&key) {
            self.modules.insert(key.clone(), content);
            self.order.push(key.clone());
        }

        Ok(key)
    }

    fn bundle(&mut self, entry: &Path) -> Result<String> {
        self.process_module(entry)
    }
}

fn num_to_alpha(mut n: usize) -> String {
    let mut result = String::new();
    loop {
        result.push((b'a' + (n % 26) as u8) as char);
        n /= 26;
        if n == 0 {
            break;
        }
        n -= 1;
    }
    result.chars().rev().collect()
}

fn generate_name_map(locals: &HashSet<String>, prefix: &str) -> HashMap<String, String> {
    let reserved: HashSet<_> = KEYWORDS.iter().copied().collect();
    let mut mapping = HashMap::new();
    let mut counter = 0;

    for local in locals {
        if reserved.contains(local.as_str()) {
            continue;
        }
        loop {
            let name = format!("{}{}", prefix, num_to_alpha(counter));
            counter += 1;
            if !reserved.contains(name.as_str()) {
                mapping.insert(local.clone(), name);
                break;
            }
        }
    }

    mapping
}

fn is_word_boundary(text: &str, start: usize, end: usize) -> bool {
    let before = start == 0 || {
        let ch = text[..start].chars().last().unwrap_or('\0');
        !ch.is_alphanumeric() && ch != '_' && ch != '.'
    };
    let after = end == text.len() || {
        let ch = text[end..].chars().next().unwrap_or('\0');
        !ch.is_alphanumeric() && ch != '_'
    };
    before && after
}

fn find_string_ranges(content: &str) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    let mut chars = content.chars().enumerate();

    while let Some((idx, ch)) = chars.next() {
        if ch == '"' || ch == '\'' {
            let delimiter = ch;
            let start = idx;
            let mut escaped = false;

            for (pos, current) in chars.by_ref() {
                if escaped {
                    escaped = false;
                    continue;
                }
                if current == '\\' {
                    escaped = true;
                    continue;
                }
                if current == delimiter {
                    ranges.push((start, pos + 1));
                    break;
                }
            }
        }
    }
    ranges
}

fn rename_locals(content: &str, mapping: &HashMap<String, String>) -> String {
    let string_ranges = find_string_ranges(content);
    let mut sorted: Vec<_> = mapping.iter().collect();
    sorted.sort_by_key(|(old, _)| std::cmp::Reverse(old.len()));

    let mut replacements = Vec::new();
    let content_bytes = content.as_bytes();

    for (old, new) in sorted {
        let old_bytes = old.as_bytes();
        let mut start = 0;

        while start + old_bytes.len() <= content_bytes.len() {
            if let Some(pos) = content_bytes[start..]
                .windows(old_bytes.len())
                .position(|w| w == old_bytes)
            {
                let abs_pos = start + pos;
                let end_pos = abs_pos + old.len();

                let in_string = string_ranges
                    .iter()
                    .any(|(s, e)| abs_pos >= *s && abs_pos < *e);
                let is_word = is_word_boundary(content, abs_pos, end_pos);
                let overlaps = replacements.iter().any(|(s, e, _)| {
                    (abs_pos >= *s && abs_pos < *e) || (end_pos > *s && end_pos <= *e)
                });

                if !in_string && is_word && !overlaps {
                    replacements.push((abs_pos, end_pos, new.clone()));
                }
                start = end_pos;
            } else {
                break;
            }
        }
    }

    replacements.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));

    let mut output = content.to_string();
    for (start, end, new_str) in replacements {
        output.replace_range(start..end, &new_str);
    }
    output
}

fn minify(content: &str) -> String {
    let mut result = String::with_capacity(content.len());
    let mut chars = content.chars().peekable();
    let mut in_string = false;
    let mut delimiter = '\0';
    let mut escaped = false;

    while let Some(ch) = chars.next() {
        if in_string {
            result.push(ch);
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == delimiter {
                in_string = false;
            }
            continue;
        }

        if ch == '"' || ch == '\'' {
            in_string = true;
            delimiter = ch;
            result.push(ch);
            continue;
        }

        if ch == '-' && chars.peek() == Some(&'-') {
            chars.next();

            if chars.peek() == Some(&'[') {
                chars.next();
                if chars.peek() == Some(&'[') {
                    chars.next();
                    let mut prev = '\0';
                    while let Some(next_ch) = chars.next() {
                        if prev == ']' && next_ch == ']' {
                            break;
                        }
                        prev = next_ch;
                    }
                    result.push(' ');
                    continue;
                }
            }

            while chars.next_if(|&c| c != '\n').is_some() {}
            result.push(' ');
            continue;
        }

        if ch.is_whitespace() {
            if let Some(last) = result.chars().last() {
                if let Some(&next) = chars.peek() {
                    if (last.is_alphanumeric() || last == '_')
                        && (next.is_alphanumeric() || next == '_')
                        && last != ' '
                    {
                        result.push(' ');
                    }
                }
            }
            continue;
        }

        result.push(ch);
    }
    result
}

fn replace_requires(content: &str, bundler: &Bundler, current_path: &Path) -> Result<String> {
    let ast = full_moon::parse(content).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
    let mut visitor = ReqVisitor::new();
    visitor.visit_ast(&ast);

    let mut output = content.to_string();
    let mut positions = Vec::new();

    for require_name in &visitor.requires {
        let require_path = bundler.find_module(require_name, Some(current_path))?;
        let key = bundler.normalize_path(&require_path)?;

        for quote in ['"', '\''] {
            let pattern = format!("require({}{}{})", quote, require_name, quote);
            let mut start = 0;
            while let Some(pos) = output[start..].find(&pattern) {
                let abs_pos = start + pos;
                positions.push((abs_pos, abs_pos + pattern.len(), key.clone()));
                start = abs_pos + pattern.len();
            }
        }
    }

    positions.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));

    for (start, end, module) in positions {
        output.replace_range(start..end, &format!("__R(\"{}\")", module));
    }

    Ok(output)
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut bundler = Bundler::new(cli.root.clone());
    let entry_key = bundler.bundle(&cli.input)?;
    let mut processed = HashMap::new();

    for (idx, (key, content)) in bundler.modules.iter().enumerate() {
        let prefix = format!("{}_", num_to_alpha(idx));

        let mut collector = Collector::new();
        collector.collect(content)?;

        let mapping = generate_name_map(&collector.locals, &prefix);
        let renamed = rename_locals(content, &mapping);
        let minified = minify(&renamed);

        processed.insert(key.clone(), minified);
    }

    let mut output = String::new();
    if let Some(banner) = cli.banner {
        let banner_content = fs::read_to_string(&banner)?;
        output.push_str(&banner_content);
        if !output.ends_with('\n') {
            output.push('\n');
        }
    } else {
        output.push_str("-- File generated automatically by luapack\n");
    }

    output.push_str("local __M={};local function __R(m)if type(__M[m])=='function'then local r=__M[m]()__M[m]=r~=nil and r or true return r end return __M[m]~=true and __M[m]or nil end;");

    for key in &bundler.order {
        if key == &entry_key {
            continue;
        }
        if let Some(content) = processed.get(key) {
            let path = cli.root.join(key);
            let with_requires = replace_requires(content, &bundler, &path)?;
            output.push_str(&format!("__M[\"{}\"]=function(){}end;", key, with_requires));
        }
    }

    if let Some(content) = processed.get(&entry_key) {
        let with_requires = replace_requires(content, &bundler, &cli.input)?;
        output.push_str(&with_requires);
    }

    let original_size: usize = bundler.modules.values().map(|s| s.len()).sum();
    let minified_size = output.len();
    fs::write(&cli.output, &output)?;

    eprintln!(
        "\n{} -> {} ({} modules, {:.1}% reduction)",
        cli.input.display(),
        cli.output.display(),
        bundler.modules.len(),
        100.0 - (minified_size as f64 / original_size as f64 * 100.0)
    );

    Ok(())
}
