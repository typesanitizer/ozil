# ozil - A man with intelligence and vision

`ozil` assists you with viewing man/help pages. It is intended as a
porcelain for `man`/`--help` + `less`/`more`/`most`.

## Features

- [X] A top, top-qualitee name.
- [ ] Transparently works with man pages and help pages (typically accessed with
  `--help` or `-h`). [TODO: Insert GIF]
- [ ] Follow links inside man/help pages with hints.
  - TODO: insert GIF
- [ ] Autodetection of subcommand help pages.
  - TODO: insert GIF
- [ ] Simple but flexible configuration with hot-reloading by default.
  - TODO: insert GIF
- [ ] Sensible keyboard shortcuts by default.
- [ ] Option to highlight information for easier use later.
  - TODO: insert GIF

## Platforms

Currently, only Linux is supported. Support for other platforms depends
on user contributions.

## Installing

Protip: When your colleagues ask you what software you're using, you can say
"It's my man ozil!".

### Download a binary

### Building from source

## Relation to mandb

Currently `ozil` depends on `man` for path handling; it doesn't duplicate
databases. Also, on many (most?) modern distributions, `man-db` provides
the `man` command plus a few others (`whatis`, `apropos`, `mandb` etc).
While `ozil` is not intended as a direct substitute for `man-db`, it does
provide substitutes for common use cases -

* `whatis` becomes `ozil wat`.
* `apropos` becomes `ozil waaat`.
* `mandb` becomes `ozil sync`.

## Contributing

## Thanks
