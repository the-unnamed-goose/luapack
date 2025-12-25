use anyhow::Result;
use regex::Regex;
use std::collections::{HashMap, HashSet, VecDeque, hash_map::DefaultHasher};
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

pub struct Bundler {
    path_ids: HashMap<PathBuf, String>,
    processed: HashSet<PathBuf>,
    pub output: String,
}

impl Bundler {
    pub fn bundle(entry: PathBuf) -> Result<String> {
        let mut bundler = Self {
            path_ids: HashMap::new(),
            processed: HashSet::new(),
            output: String::new(),
        };

        // The packer has an issue with the module name in this function declaration ... for some reason.
        // I'm just going to replace it with c and call it day. That's because of another bug in the way variables are declared.
        // The compression improvements aren't big enough for me to fix them so I won't.
        bundler.output.push_str("local function require(c)\n");
        #[cfg(feature = "cache")]
        {
            bundler
                .output
                .push_str("if type(__M[c]) == 'function' then\n");
            bundler.output.push_str("local result=__M[c]()\n");
            bundler.output.push_str("__M[c] = result\n");
            bundler.output.push_str("return result\n");
            bundler.output.push_str("end\n");
            bundler.output.push_str("return __M[c]\n");
        }
        #[cfg(not(feature = "cache"))]
        bundler.output.push_str("return __M[c]()\n");
        bundler.output.push_str("end\n");
        bundler.output.push_str("__M={");

        let entry_canonical = entry.canonicalize()?;
        let mut queue = VecDeque::new();
        let mut first = true;
        queue.push_back(entry_canonical.clone());

        while let Some(path) = queue.pop_front() {
            if bundler.processed.contains(&path) {
                continue;
            }

            let content = fs::read_to_string(&path)?;
            let module_id = Self::generate_id(&path, &content);
            bundler.path_ids.insert(path.clone(), module_id.clone());
            bundler.processed.insert(path.clone());

            if !first {
                bundler.output.push(',');
            }
            first = false;

            bundler
                .output
                .push_str(&format!("[\"{}\"]=function()", module_id));

            let module_dir = path.parent().unwrap_or(Path::new(""));
            bundler.process_content(&content, module_dir, &mut queue);

            bundler.output.push_str("end");
        }

        bundler.output.push_str("};");

        let entry_id = bundler.path_ids.get(&entry_canonical).unwrap();
        // A hacky implementation of an entry point. It works I guess.
        bundler
            .output
            .push_str(&format!("return require(\"{}\")", entry_id));

        Ok(bundler.output.clone())
    }

    fn generate_id(path: &Path, content: &str) -> String {
        let mut hasher = DefaultHasher::new();
        content
            .lines()
            .take(20)
            .collect::<Vec<_>>()
            .join("\n")
            .hash(&mut hasher);
        path.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .hash(&mut hasher);

        // Take only first 4 hex chars (16 bits = 65,536 combinations)
        format!("m{:04x}", hasher.finish() & 0xFFFF)
    }

    fn process_content(&mut self, content: &str, module_dir: &Path, queue: &mut VecDeque<PathBuf>) {
        let re = Regex::new(r#"require\s*\(?\s*["']([^"']+)["']\)?"#).unwrap();

        for line in content.lines() {
            if let Some(caps) = re.captures(line) {
                let path = &caps[1];

                if path.starts_with('.') {
                    let resolved = module_dir.join(path);
                    let resolved_with_ext = if resolved.extension().is_none() {
                        resolved.with_extension("lua")
                    } else {
                        resolved
                    };

                    if let Ok(canonical) = resolved_with_ext.canonicalize() {
                        if !self.path_ids.contains_key(&canonical) {
                            let dep_content = fs::read_to_string(&canonical).unwrap_or_default();
                            let module_id = Self::generate_id(&canonical, &dep_content);
                            self.path_ids.insert(canonical.clone(), module_id);
                        }

                        queue.push_back(canonical.clone());

                        let module_id = self.path_ids.get(&canonical).unwrap();

                        let replaced = re.replace(line, format!("require(\"{}\")", module_id));
                        self.output.push_str(&replaced);
                        self.output.push('\n');
                        continue;
                    }
                }
            }

            self.output.push_str(line);
            self.output.push('\n');
        }
    }
}
