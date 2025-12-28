Lua Pack
===============
A basic rust application for efficiently bundling Lua scripts into monolithic releases. This will probably not be maintained unless it breaks. 

You can make some small improvements to the compression and output structuring but they would at most improve the packing by 8Kb so it's not really worth it for me as the lz4 feature already allows you to halve your output size in most cases.
