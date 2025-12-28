Lua Pack
===============
A basic rust application for efficiently bundling Lua scripts into monolithic releases. This will probably not be maintained unless it breaks.

It supports every version of Lua from 5.1 to 5.4 and maybe soon 5.5 if full-moon introduces support for it. As for LuaU and CXLua, you will have to add them to the full-moon feature list if you wish to compile in support for them.

Dynamic requires are not supported due to the packing overhead that would cause. If anyone cares I might add it behind a `dynamic` feature as all that is needed for this to work would be changing the function overload to an assignment so it won't get picked up by the packer and using canonicalized file paths as module identifiers, maybe trimming them to the lowest common folder for efficiency.

You can make some small improvements to the compression and output structuring but they would at most improve the packing by 8Kb so it's not really worth it if your environment provides lz4 utilities.
