---
title: Installing/hacking Yi
---

# Basics

Yi requires GHC 7.8 at minimum and is mainly developed with GHC 7.10.1.

With the Haskell Platform installed, yi can be installed with cabal-install:

~~~ bash
$ cabal update
$ cabal install yi
~~~

On Linux systems, you will need ncurses development headers for
the Vty frontend. For example, Ubuntu requires `libncurses5-dev`
for ncurses support  and `libicu-dev` for unicode support.

The frontends for yi can be manually specified as follows:

~~~ bash
$ cabal install yi -fvty -fpango
~~~

Options are `-fvty` and `-fpango`.

Note: If you get a `frontend not found` warning, install yi with the frontend
manually specified by -fvty or -fpango while doing cabal install.

Note: If you're having weird problems such as your changes not seeming
to take effect, you might have some stale stuff in the cache
directory which you should empty: on my system it's `~/.cache/yi`.


# Installing Yi from source

You can install Yi from source as follows:

~~~ bash
$ git clone https://github.com/yi-editor/yi
$ cd yi
$ cabal update
$ cabal install
~~~

Yi is split into multiple smaller supporting packages (such as yi-rope
and yi-language). If you are looking to get absolutely latest sources,
make sure to install the supporting packages before installing Yi.
Usually these packages are rarely updated and the version on Hackage
is up-to-date. You can set up the supporting repositories from
[the GitHub project page][ghproject].

# Installing inside a Cabal sandbox

Cabal-install 1.18 and higher support sandboxing which isolates
the Yi installation from the rest of the cabal environment. This
is especially important if you plan on hacking on Yi itself, or on
libraries for Yi.

If your cabal version is lower than 1.20, you can install a newer
version of cabal-install using cabal itself. This is recommended
since newer versions of Cabal have several useful utilities for
sandboxed environments.

~~~ bash
$ cabal update
$ cabal install cabal-install
$ cabal --version
~~~

As Yi compiles your config file once you start it, the config needs to
know where to look for any of its dependencies, such as Yi itself! If
these are inside of the sandbox, it doesn't know where to look and
you'll get config compilation errors due to missing modules.

To sandbox, navigate to your source yi directory. For me it's
`~/programming/yi/`.

We then setup a cabal sandbox:

~~~ bash
$ cd ~/programming/yi
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal install
~~~

From cabal-install 1.20, Yi can be launched in an environment using the
sandbox's package DB using `cabal exec ./dist/build/yi/yi`. It may be useful
to create an alias or small script for this, along the lines of:

~~~ bash
#!/usr/bin/env bash
YI_DIR=$HOME/programming/yi/yi
env CABAL_SANDBOX_CONFIG=$YI_DIR/cabal.sandbox.config cabal exec $YI_DIR/dist/build/yi/yi -- "$@"
~~~

The `"$@"` part means that all the arguments we pass to this script
are passed on to the Yi binary which means we can still use all the
regular flags, such as `yi --as=emacs`. Of course, you'll need to
adjust the paths used to match your sandbox and package directories.

There's one more thing to mention in this section, and that is config
dependencies. One of the great things about Yi is that we have access
to the wealth of existing Haskell libraries and we can take advantage
of this in our config file. There are two scenarios:

* If the package your config depends on is on Hackage and you want to
  use that, just use `cabal install` in the sandboxed Yi directory. So
  if your config depends on `semigroups`, you'd run `cabal install
  semigroups`. After doing this, `semigroups` should now be visible when
  your config is getting compiled.

* If the package your config depends on is local, for example when
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

# Hacking Yi with nix

Hacking Yi with nix is pretty easy. Forget about cabal sanboxes and
other nastiness. You will first need a way to override the Haskell
package set. I will not go into this here, see NixOS wiki and other
associated resources for help.

Once you have done this, you'll need project files for each repository
Yi depends on, as seen in its cabal file and from the repos
[here][ghproject].

I'll first discuss setting up the supporting repos and then move onto
the main repository. The supporting repos don't need anything unusual,
make project files and stick them in your overrides as always.

## Setting up supporting repositiories

For each repository, you'll need `default.nix` and most likely
`shell.nix` too if you want to do hacking in it. Consider `yi-rope`
repository as an example.

Check out `yi-rope` from git into somewhere. Generate `default.nix`
with `cabal2nix` and stash it somewhere accessible. Add `shell.nix`
in the same directory, with the trivial contents:

~~~ haskell
let pkgs = import <nixpkgs> {};
    myHaskellPackages = pkgs.myHaskellPackages;
    haskellPackages = myHaskellPackages.override {
      extension = self: super: {
        yiRope = myHaskellPackages.callPackage ./. {};
      };
    };
in pkgs.lib.overrideDerivation haskellPackages.yiRope (attrs: {
  noHaddock = true;
  buildInputs = [ ] ++ attrs.buildInputs;
})

~~~

`myHaskellPackages` is my overriden package set. `noHaddock = true` so
that hacking is not slow inside nix-shell. You can add extra tools you
like to use to the `buildInputs` list.

Symlink your `default.nix` and `shell.nix` from within your `yi-rope`
checkout and ignore them in git (locally, not in .gitignore)

~~~ bash
[shana@lenalee:~/programming/yi]$ l ~/programming/yi-rope
total 168K
drwxr-xr-x   6 shana shana   4.0K Sep 29 06:59 .
drwxr-xr-x 199 shana shana   4.0K Oct  5 18:36 ..
lrwxrwxrwx   1 shana shana     64 Sep  6 06:59 default.nix -> /home/shana/programming/nix-project-defaults/yi-rope/default.nix
lrwxrwxrwx   1 shana shana     62 Sep  6 06:58 shell.nix -> /home/shana/programming/nix-project-defaults/yi-rope/shell.nix
…
~~~

Then stick it into your Haskell overrides. Currently my overrides for
Yi repos look as follows:

~~~ haskell
wordTrie          = normalPackageS se "word-trie";
ooPrototypes      = normalPackageS se "oo-prototypes";
yiLanguage        = normalPackageS se "yi-language";
yiRope            = normalPackageS se "yi-rope";
dynamicState      = normalPackageS se "dynamic-state";
~~~

where `normalPackageS` is just a handy alias for `callPackage`. See
[my setup](https://github.com/Fuuzetsu/nix-project-defaults/blob/master/nixpkgs-config/config.nix)
for details. Hack on these as you would with any normal project.

## Setting up main repository

Now for the main repository. The tricky bit is that Yi needs to be
able to find its packages and its dependencies at runtime so that
running the GHCi evaluator (`M-x`) works. First generate `default.nix`
and use a stock `shell.nix` as usual. You might want to manually add
`vty` to dependency list and set

~~~ haskell
  configureFlags = [ "-fpango" "-fvty" ];

  # https://ghc.haskell.org/trac/ghc/ticket/9170
  noHaddock = self.ghc.version == "7.6.3";
~~~

Symlink the project files as usual. The generated `default.nix` should
have all the supporting repositories listed so `nix-shell --pure`
should just work: remember to add the repos to your Haskell package
set.

Once you're in the shell, the first thing you want to do is `eval
"$configurePhase" && eval "$buildPhase"`: this will fully build Yi
once which has the side effect of generating all the CPP macros and
cabal-generated files. It also has the effect of setting
`GHC_PACKAGE_PATH` which is useful later.

You should now be able to simply use `ghci` on any file in the
repository: `.ghci` is already set up to pick everything up.

Once you're done making changes, you probably want to run Yi and try
them out. Run `eval "$buildPhase"` to build Yi. You can run the tests
with `eval "$checkPhase"`.

Now to run it, use something like

~~~ bash
[nix-shell:~/programming/yi/yi]$ GHC_PACKAGE_PATH="$(pwd)/dist/package.conf.inplace:$GHC_PACKAGE_PATH" TERM=xterm yi_datadir=. dist/build/yi/yi -y /tmp -fpango --as=emacs /tmp/T.hs
~~~

Here's what happens: we set `GHC_PACKAGE_PATH` to the generated Yi
package config inside dist (this allows us to find Yi modules) and the
rest of packages (this allows to find all the dependencies). The
reason for `$(pwd)/…` rather than a relative path is to keep this
working after we `M-x cd` inside the editor. We set `TERM` which you
might need depending on your terminal due to `nix-shell` messing with
some env vars to ensure purity. We also set `yi_datadir` to the
current directory so that Yi can finds its icon and such.

Once all those are set, we run the compiled Yi itself
(`dist/build/yi/yi`), point it somewhere *away* from the user config directory
(`-y /tmp`), choose the frontend (`-fpango`), choose the keymap
(`--as=emacs`) and choose the file to open (`/tmp/T.hs`). This should
succesfully start up Yi and ensure `M-x` and all such dynamic commands
work. All in all, this gives you an environment allowing you to hack
Yi with `nix-shell` as always even though it's spread across multiple
repositories and has an unusual setup.

Actually installing Yi with nix along with user config is a bit out of
scope of this section but it works similarly, wrapping the binary into
a script which sets `GHC_PACKAGE_PATH`.

# Automatic version tracking

Here's an extra bit: manually tracking changing versions of the
subrepositories can get pretty boring, pretty quickly. You can however
use nix to automatically read the version from the cabal file and
create the derivation based on the result. It ends up a bit ugly but
it works. For `yi-rope` my `default.nix` is therefore

~~~ haskell
{ cabal, binary, deepseq, fingertree, hspec, QuickCheck
, quickcheckInstances, text, criterion
}:


let pkgs = import <nixpkgs> {};
    lib = pkgs.lib;
    sr  = "/home/shana/programming/yi-rope";
    file = builtins.readFile (sr + "/yi-rope.cabal");
    strs = lib.strings.splitString "\n" file;
    vstr = builtins.head (builtins.filter (s: lib.strings.hasPrefix "version:" s) strs);
    vrsn = lib.strings.removePrefix "version:" (lib.strings.replaceChars [" "] [""] vstr);

in
cabal.mkDerivation (self: {
  pname = "yi-rope";
  version = vrsn;
  src = sr;
  buildDepends = [ binary deepseq fingertree text criterion ];
  testDepends = [ hspec QuickCheck quickcheckInstances text ];
  meta = {
    description = "A rope data structure used by Yi";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
~~~

The downside is if you ever run `cabal2nix` over it, this scheme will
get clobbered so it's mostly only useful for the repositories which
will most likely not get many extra dependencies but are likely to
change versions often.


[ghproject]: https://github.com/yi-editor

