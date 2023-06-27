# rcc - C17 compiler written in Rust

status: VAPORWARE

## Progress Report:
- Preprocessor works to a certain extent.
    - Things like actually good diagnostic messages are non-existent.
    - `#error`, `#pragma`, etc. aren't implemented yet.
- Currently working on parsing C declarations.

# References

[C17 Spec](https://web.archive.org/web/20181230041359/http://www.open-std.org/jtc1/sc22/wg14/www/abq/c17_updated_proposed_fdis.pdf)
