---
layout: page
title: Installing Yi
---


Yi requires GHC 7.6.3 at minimum and is mainly developed with GHC 7.8.3.

With the Haskell Platform installed, yi should be installed with cabal-install:

    $ cabal update
    $ cabal install yi

On Linux systems, you'll probably need ncurses development headers for
the Vty frontend. On Ubuntu, you'll need to install the
`libncurses5-dev` package.

You can specify frontends to compile, also:

    $ cabal install yi -fvty -fpango

Options are `-fvty` and `-fpango`.

You can also install the `yi-contrib` package, which contains some
extra contributed things (like user configs):

    $ cabal install yi-contrib

If you're in the source repository, you can install yi from source:

    $ cabal update # Still update to get updated dependencies
    $ cd yi && cabal install

And the contrib package:

    $ cd yi-contrib && cabal install

Please note that if you are looking to get absolutely latest sources,
you should set up the supporting repositories from
[the GitHub project page][ghproject] first.

### Installing inside a Cabal sandbox

Many people want to install Yi inside a cabal sandbox (cabal-install
1.18 feature). This is especially important if you plan on hacking on
Yi itself or on libraries for Yi.

As Yi compiles your config file once you start it, the config needs to
know where to look for any of its dependencies, such as Yi itself! If
these are inside of the sandbox, it doesn't know where to look and
you'll get config compilation errors due to missing modules.

To sandbox, navigate to your source yi directory. For me it's
`~/programming/yi/yi`.

We then setup a cabal sandbox following instructions from the
[cabal userguide](http://www.haskell.org/cabal/users-guide/installing-packages.html#sandboxes-basic-usage):

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

From cabal-install 1.20, Yi can be launched in an environment using the
sandbox's package DB using `cabal exec ./dist/build/yi/yi`. It may be useful
to create an alias or small script for this, along the lines of:

```
#!/usr/bin/env bash
YI_DIR=$HOME/programming/yi/yi
env CABAL_SANDBOX_CONFIG=$YI_DIR/cabal.sandbox.config cabal exec $YI_DIR/dist/build/yi/yi "$@"
```

The `"$@"` part means that all the
arguments we pass to this script are passed on to the Yi binary which
means we can still use all the regular flags, such as `runyi
--as=emacs`. Of course, you'll need to adjust the paths used to match
your sandbox and package directories.

There's one more thing to mention in this section and that is config
dependencies. One of the great things about Yi is that we have access
to the wealth of existing Haskell libraries and we can take advantage
of this in our config file. There are two scenarios:

If the package your config depends on is on Hackage and you want to
use that, just use `cabal install` in the sandboxed Yi directory. So
if your config depends on `semigroups`, you'd run `cabal install
semigroups`. After doing this, `semigroups` should now be visible when
your config is getting compiled.

If the package your config depends on is local, for example when
you're developing the library that you want to use or if you need a
patched version, you'll have to use `cabal sandbox add-source`
command. As an example, I'm developing a `yi-haskell-utils` package
and my config depends on it. To accommodate for this, I ran `cabal
sandbox add-source ~/programming/yi-haskell-utils`.
You can then `cabal install yi-haskell-utils` to add the package to
the sandbox. You should call `cabal build` in the sandbox directory
after you modify a local package so that the sandbox has an up-to-date
version of the package.

I suspect that it'd be perfectly possible to make your config file
into a cabal project and manage the dependencies that way but I have
not yet investigated this approach.

Still doesn't work? Try the older instructions such as those removed
[here](https://github.com/yi-editor/yi/commit/05b4d89b5e6a2ecd17a23a04659c7b5d828786d3)
or
[here](https://github.com/yi-editor/yi/commit/63cefe048e4f3f50d364150085b617424477e333).
Make sure to let us know!

## Getting Source

Yi source repository is available on [GitHub](https://github.com/yi-editor/yi).

To get the git version,

    $ git clone git://github.com/yi-editor/yi.git

If you plan to do more serious hacking, you probably want the
supporting repositories from the
[GitHub project page][ghproject]. You should
cross-reference with the cabal file to see what you might need.

[ghproject]: https://github.com/yi-editor
