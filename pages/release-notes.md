---
layout: page
title: Release notes
---

# Yi 0.6.3 Release Notes

## What's new?

* New vte UI. This is a terminal UI inside a GUI, much like gvim. It depends
  on Gtk2Hs for the GUI, and then launches the vty UI inside the terminal.
* Compatibility with the latest Haskell Platform release
* Start yi-contrib package. We intend to move more stuff here, to clean up the
  core yi package.

## Yi

Yi is a text editor written in Haskell and extensible in Haskell. The
long-term goal of the Yi project is to provide the editor of
choice for Haskell programmers.

Yi now works relatively well in the terminal, using the vty package, and also
has Gtk frontends using vte (which interfaces with the terminal interface) and
a Pango frontend. There is also a Cocoa frontend under (slow) development.

## Installation

Using cabal install:

    $ cabal update
    $ cabal install yi

The default UI depends on the vty package, which will only compile with the
ncurses development headers available. On Ubuntu, you need to install the
`libncurses5-dev` package.

On Windows, you'll need to disable the default vty terminal UI, and use a Gtk
UI instead (the vte UI requires vty, so you can't install that either):

    $ cabal install yi -f-vty -fpango

(Windows support is not well-tested, though.)

Optionally also install the contrib package:

    $ cabal install yi-contrib

## Features

* A purely functional editor core
* Key-bindings written as parsers of the input
* Emacs, Vim and (partial) Cua emulations provided by default
* Console front-end (Gtk2Hs and Cocoa front-ends in development)
* Static configuration (XMonad style) for fast load
* Haskell support:
  * Lexical highlighting and (unicode-based) beautification.
  * Layout-aware parenthesis-matching
  * Auto-indentation
  * cabal-build within the editor
* Syntax highlighting for a number of other languages (latex, python, perl, ...)

## More Info

Read the [README][] on GitHub for more information. The [source code][] is
also hosted there.

## Credits

This release is brought to you by:

* Alexey Levan
* Gwern Branwen
* Issac Trotts
* Jean-Philippe Bernardy
* Jeff Wheeler
* Jeremy Wall
* Maciej Piechotka
* Malte Sommerkorn

and all the contributors to the previous versions.

Also, Yi would not exist without all the work put into the Haskell platform.

[README]: https://github.com/yi-editor/yi/blob/master/README.md
[source code]: https://github.com/yi-editor/yi
