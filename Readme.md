# ozil - A man with intelligence and vision [![Build Status](https://travis-ci.com/theindigamer/ozil.svg?token=atg5zCeDiWzbYpJit3Kx&branch=master)](https://travis-ci.com/theindigamer/ozil)

`ozil` assists you with viewing man/help pages. It is intended as a
porcelain for `man`/`--help` + `less`/`more`/`most`.

## Features

- [X] A top, top qualitee name.
- [ ] Transparently works with man pages and help pages (typically accessed with
  `--help` or `-h`).
  - [TODO: Insert GIF]
- [ ] Follow links inside man/help pages with hints.
  - TODO: insert GIF
- [ ] Automatic detection of subcommand help pages.
  - TODO: insert GIF
- [ ] Simple but flexible configuration with hot-reloading by default.
  - TODO: insert GIF
- [ ] Option to highlight information for later use.
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

## Usage

If you want to see the help for `foo`, run `ozil foo`. That's it.
`ozil` will guide you as needed. Of course, you can try using `ozil ozil`
as well :smile:.

## Contributing

## Thanks

## Miscellaneous

### Relation to mandb

- Currently `ozil` depends on `man` for path handling; it doesn't duplicate
  databases or go hunting for files by itself. This may change in the future.

### Non-Goals

- Be a 1-to-1 replacement for `man` or `less`.
