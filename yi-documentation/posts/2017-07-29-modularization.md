---
title: Modularization
author: Dmitry Ivanov
---

Following [previous
post](https://yi-editor.github.io/posts/2017-07-25-release-0.14/) it is
a good moment to talk about ongoing modularization of Yi.

Rewind to a moment I started contributing to Yi: 2012. After two years
of using vim I found a bug in it. It wasn’t anything too serious like a
crash or corruption of user text, just some undocumented inconsistency
in behavior. I thought, well, it’s open source why not try to fix it?
This was the first time I looked at vim’s source code and was completely
overwhelmed. Hundreds of thousands of lines of C. It’s not
unprecedented, of course, but it’s A LOT. At that point I’ve only seen a
codebase comparable in size one time at work, but that was, while
larger, much more modular.

After some hours of trying to find a relevant place in vim, I was
sufficiently lost to arrive at a question “Is there a vim-like editor
that is written simpler?”. I remember looking at
[Kate](https://kate-editor.org/),
[Yzis](https://github.com/chrizel/Yzis) and Yi. Was I going through a
list of vi emulations in reverse lexicographical order? Probably. Was I
using KDE at the time? Definitely. Anyway, Yi seemed interesting because
it had about 20 thousands lines of code and had multiple frontends
(Terminal, Gtk and Cocoa) and multiple keymaps (vim and emacs,
naturally).

This is how Yi was split into packages, or rather into a library part
and an executable part within one cabal project at the time:

    > cloc yi/src/library
         158 text files.
         158 unique files.
          29 files ignored.

    github.com/AlDanial/cloc v 1.72  T=0.49 s (262.7 files/s, 57554.5 lines/s)
    -------------------------------------------------------------------------------
    Language                     files          blank        comment           code
    -------------------------------------------------------------------------------
    Haskell                        129           4514           4647          19103
    -------------------------------------------------------------------------------

    > cloc yi/src/executable
           1 text file.
           1 unique file.
           0 files ignored.

    github.com/AlDanial/cloc v 1.72  T=0.01 s (95.5 files/s, 1241.3 lines/s)
    -------------------------------------------------------------------------------
    Language                     files          blank        comment           code
    -------------------------------------------------------------------------------
    Haskell                          1              5              3              5
    -------------------------------------------------------------------------------

So basically one package. Interestingly, Yi had a custom prelude and
that gathered lots of complaints over the following years until Mateusz
finally removed it in 2014. And yet these days custom preludes seem
[to](https://hackage.haskell.org/package/rebase)
[be](https://hackage.haskell.org/package/basic-prelude)
[all](https://hackage.haskell.org/package/classy-prelude)
[the](https://hackage.haskell.org/package/foundation)
[rage](https://hackage.haskell.org/package/protolude)? By contrast, top
result in google for “custom prelude” from 2012 is [this answer by Don
Stewart](https://stackoverflow.com/questions/13649415/custom-prelude-module-bad-idea)
saying not to do it.

Of course, I was not the first person to come up with the idea to split
emacs emulation from vim one and terminal interface from GUI. In fact,
here is the quote from 2012 README:

    We also want to simplify the core Yi package to make it more accessible, splitting some parts into several packages.

Back then it was significantly harder to do just because of the tooling.
Not only there was no
[stack](https://docs.haskellstack.org/en/stable/README/) at the time,
cabal sandboxes were not a thing [until late
2013](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html). I fondly
remember the character-building days of nuking your global ghc and cabal
directories.

But thanks to cabal and later also stack folks things were steadily
improving since.

The tooling situation was not the only difficulty, Yi had several
circular references between modules. One by one, we untangled these and
split some libraries potentially useful outside of yi.

[yi-rope](http://packdeps.haskellers.com/reverse/yi-rope) is actually
used by [our friendly competitor
rasa](https://github.com/ChrisPenner/rasa) and
[haskell-lsp](https://github.com/alanz/haskell-lsp).

[oo-prototypes](https://hackage.haskell.org/package/oo-prototypes-0.1.0.0/docs/Data-Prototype.html)
still blows my mind five years later. Yi was my intro to Haskell after a
HelloWorld and I was basically greeted by a module saying “here we
implement OOP inheritance in 7 lines out of thin air”.

[yi-language](https://github.com/yi-editor/yi/tree/master/yi-language)
I’m not too happy about, because we split it not because it’s a
self-contained thing, but just to isolate
[alex](https://www.haskell.org/alex/)-related stuff that was killing
incremental compilation. I’m hoping to reshuffle this part, so that
pieces of yi-language like Yi.Buffer.Basic and Yi.Region end up in
yi-core and everything alex-related lives in yi-alex-utils-or-something
and becomes entirely optional, that is you will be able assemble an
editor without a dependency on alex.

Some time later stack came about and made it easy to work with
multiproject repos and we finally split all the frontends and keymaps
into separate projects.

So this is how project structure looks now:

    Project                     Lines of haskell code
    yi-core                     10335
    yi-dynamic-configuration    81
    yi-frontend-pango           1566
    yi-frontend-vty             407
    yi-fuzzy-open               214
    yi-intero                   140
    yi-ireader                  124
    yi-keymap-cua               147
    yi-keymap-emacs             643
    yi-keymap-vim               4669
    yi-language                 803
    yi-misc-modes               449
    yi-mode-haskell             1246
    yi-mode-javascript          601
    yi-snippet                  375

So in my mind the next thing in modularization of Yi is moving
alex-powered highlighting into a plugin while making yi-core expose some
general interface. It is already possible to make syntax highlighting
without alex, e.g. I have [rainbow parens
mode](https://github.com/ethercrow/yi-config/blob/master/modules/RainbowMode.hs)
in my config where actual parsing is done by
[regex-applicative](https://hackage.haskell.org/package/regex-applicative),
but it doesn’t feel like a first class citizen.

Finally, if this story was interesting to you, you’re very welcome to
join the development!

Do you care about how pretty does editor look? Make a new shiny
frontend!

Have an idea about a new crazy ergonomic control scheme? Try it out as a
new keymap for Yi.

Maybe you’re interested in optimizing haskell code? Yi has plenty of
that.

In any case, don’t hesitate to file an issue, make a PR or chat.
