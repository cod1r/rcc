# rcc - C17 compiler written in Rust

status: VAPORWARE

## Progress Report:
- Preprocessor works to a certain extent.
    - Things like actually good diagnostic messages are non-existent.
    - The preprocessing process isn't very specific in the C spec so behavior will differ if compared to gcc.
    - `#error`, `#pragma`, `#line`, etc. aren't implemented yet.
- Can parse like 90 percent of C17 syntax. (I think)
- Currently working on Semantic Analysis (language constraints etc), AST.

## TODO:
- Apply semantic analysis (constraints etc)
- Write library headers (stdio, math, etc)

# References

[C17 Spec](https://open-std.org/JTC1/SC22/WG14/www/docs/n2310.pdf)
