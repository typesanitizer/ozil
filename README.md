# ozil - A man with intelligence and vision

`ozil` assists you with viewing man/help pages. It is intended as a
replacement to `man`/`--help` + `less`/`more`/`most`.\*

Currently, only Linux is supported. Support for other platforms depends
on user contributions.

\* On many (most?) modern distributions, `man-db` provides
the `man` command plus a few others (`whatis`, `apropos`, `mandb` etc).
While `ozil` is not intended as a direct substitute for `man-db`, it does
provide substitutes for common use cases -

* `whatis` becomes `wat` (alias for `ozil whatis`).
* `apropos` becomes `wat --query` (or `wat -q`).
* `mandb` becomes `ozil sync`.

## Features

* Transparently works with man pages and help pages (typically accessed with
  `--help` or `-h`):
  - TODO: insert GIF here
* Follow links inside man/help pages with hints:
  - TODO: insert GIF here
* Autodetection of subcommand help pages:
  - TODO: insert GIF here
* Option to highlight information for easier use later.
  - TODO: insert GIF here
* Intelligent binary lookup in case it isn't found (can be turned off):
  - TODO: insert GIF here
* Simple but flexible configuration with hot-reloading by default.
  - TODO: insert GIF here
* Sensible keyboard shortcuts by default.
* A world-class name.

## Installing

Protip: When your colleagues ask you what software you're using, you can say
"It's my man ozil!".

### Download a binary

### Building from source

## Contributing

## Thanks
