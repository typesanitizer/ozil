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

### Download a binary

### Building from source

Get [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
first. Then run

```
git clone https://github.com/theindigamer/ozil.git
cd ozil && stack install
```

This will install `ozil` to `~/.local/bin` which is (hopefully?) on your `$PATH`.

## Relation to mandb

- Currently `ozil` depends on `man` for path handling; it doesn't duplicate
  databases or go hunting for files by itself. This may change in the future.
- Also, on many (most?) modern distributions, `man-db` provides
  the `man` command plus a few others (`whatis`, `apropos`, `mandb` etc).
  While `ozil` is not intended as a direct substitute for `man-db`, it does
  provide 1 substitute -

  * `whatis` becomes `ozil wat`.

## Contributing

## Thanks
