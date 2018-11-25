# ozil - A man with intelligence and vision [![Build Status](https://travis-ci.com/theindigamer/ozil.svg?token=atg5zCeDiWzbYpJit3Kx&branch=master)](https://travis-ci.com/theindigamer/ozil)

`ozil` assists you with viewing man/help pages. It is intended as a
porcelain for `man`/`--help` + `less`/`more`/`most`.

[Note: Support for man pages is not yet ready. If you try to read a man page,
`ozil` might crash or display something weird.]

## Table of contents

- [Features](#features)
  * [Planned](#planned)
- [Platforms](#platforms)
- [Installing](#installing)
  * [Download a binary](#download-a-binary)
  * [Building from source](#building-from-source)
- [Usage](#usage)
  * [Configuration](#configuration)
- [Contributing](#contributing)
- [Thanks](#thanks)
- [Miscellaneous](#miscellaneous)
  * [Alternatives](#alternatives)
  * [Relation to mandb](#relation-to-mandb)
  * [Non-goals](#non-goals)

## Features

- [X] A top, top qualitee name.
- [ ] Transparently works with man pages and help pages (typically
    accessed with `--help` or `-h`).
  + [ ] [TODO: Insert GIF]
  + [X] Help pages
  + [ ] Man pages
- [ ] Follow links inside man/help pages with hints.
  + [X] Help pages
  + [ ] Man page
- [ ] Automatic detection of subcommand help pages.
  + [X] Help pages
  + [ ] Man pages
- [X] Configurable keybindings with hot-reloading.
- [X] Automatic search for local, project-specific executables
      based on your build system (contributions to support more build systems
      are welcome!).

![Demo with subcommand detection and links](https://i.imgur.com/vz4pPug.gif)

### Planned

- Proper man page support (fill remain check-boxes in Feature list). Given the
  variety of macros present in man pages, this is a big task, so please have
  some patience. :sweat_smile:
- Basic options to customize appearance (with hot-reloading).
- Search text (see [issue 10](https://github.com/theindigamer/ozil/issues/10)).

## Platforms

Currently, only Linux is supported. Support for other platforms depends
on user contributions. (Note: Windows support is not possible before
changes are made to the Vty package which is a dependency.)

## Installing

### Download a binary

A binary is available for 64-bit Linux.
See [Releases](https://github.com/theindigamer/ozil/releases/tag/v1.0-alpha).
I've only tested it on an Ubuntu VM, so if you face any problems in using it
(either on Ubuntu or on some other Linux distro), please open an issue.

### Building from source

Get [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
first. Then run

    git clone https://github.com/theindigamer/ozil.git
    cd ozil && stack install

This will install `ozil` to `~/.local/bin` which is (hopefully?) on your
`$PATH`.

If you wish to use `cabal` instead of `stack`, use `cabal v2-install`
instead of `stack install`. This will install `ozil` to `~/.cabal/bin`.

## Usage

If you want to see the help for `foo`, run `ozil foo`. If you want to
see the help for `foo bar` (where `bar` is a subcommand of `foo`) run
`ozil 'foo bar'`.

If you're want a man page from a specific section (e.g. `man(1)`), you
can use `ozil man.1` (Note: This might still crash as documented at the
beginning of the Readme).

That's mostly it. `ozil` will guide you as needed. In case of
ambiguities, it will present you with an option to make a choice and
possibly save that choice.

### Configuration

Currently, `ozil` will place a configuration file at
`$HOME/.config/ozil/ozil.yaml` (it will prompt you every time you run it
if that file doesn't exist). You can save your preferred keybindings
there. The default keybindings are already present, so you can tweak
those to your liking. (Note: due to the quirkiness of YAML, `n` needs to
be quoted.)

If that path doesn't work for your distro (or on Mac OS) - please open
an issue and I'll try to figure something out.

TODO: Document keybinding configuration more thoroughly.

## Contributing

See [Contributing.md](Contributing.md).

## Thanks

Special thanks to the Brick maintainer @jtdaugherty for making such as
easy to use and well-documented TUI library. Writing `ozil` wouldn't
have been possible without it :smile:. Also, thanks to Mark Karpov for
Megaparsec.

## Miscellaneous

### Alternatives

Let's say you don't want to install `ozil` for some reason but still want
some of the features. How can you get that?

* For automatic launching, you can have a shell function. For example, I have
  the following function which I used before writing `ozil` -
  ```
  function vh() { # vh == view help
      if [ "$1" = "stack" ]; then
          "$@" --help | less
      else
          man "$@" || ("$@" --help | less -R) || ("$@ -h | less -R)
      fi
  }
  ```
  which is fairly crude, doesn't handle all the cases, but still kinda' works.
* For following links inside man pages, you can use
  [`man2html(1)`](https://linux.die.net/man/1/man2html) to generate HTML files
  for all the man pages you typically consult. It will try to guess links to
  other man pages and insert them as hyperlinks. You can then use a text-based
  browser like [`lynx(1)`](https://linux.die.net/man/1/lynx) to view those pages
  in your terminal.

### Relation to mandb

- Currently `ozil` depends on `man` for path handling; it doesn't
  duplicate databases or go hunting for files by itself. This may
  change in the future.

### Non-Goals

- Be a 1-to-1 replacement for `man` or `less`.
