# rcc - C17 compiler written in Rust

status: VAPORWARE

## Progress Report:
- Preprocessor works to a certain extent.
    - Things like actually good diagnostic messages are non-existent.
    - `#error`, `#pragma`, etc. aren't implemented yet.
- Currently working on parsing C declarations.

## TODO:
- Finish parsing statements
- Finish parsing declarations
- Apply semantic analysis (constraints etc)

# References

[C17 Spec](https://open-std.org/JTC1/SC22/WG14/www/docs/n2310.pdf)
