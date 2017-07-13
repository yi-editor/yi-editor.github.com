Yi
==

This repository contains the documentation for the Yi text editor. The documentation is available on [http://yi-editor.github.io/](http://yi-editor.github.io/).

# Building

The site is built with [Hakyll](https://github.com/jaspervdj/hakyll). If you're familiar with Hakyll, you should be able to just `cd` into `yi-documentation` and proceed normally.

The source folder for the documentation is `yi-documentation`. Files in the top level directory are most likely Hakyll's output.

If you're not a frequent Hakyll user, you can get into a Hakyll environment either using stack, or using Nix. Both the necessary cabal and nix files are attached. The makefile will automatically build the site for you, and preview it on localhost:8000.

The development workflow would look like the following:

~~~ bash
$ git clone https://github.com/yi-editor/yi-editor.github.com
$ cd yi-editor.github.com
$ cd yi-documentation
$ stack exec --package hakyll -- make
~~~

If you prefer Nix instead, just drop into a nix-shell once you're inside `yi-documentation` and run make.

# Deploying

The output of the site is present it `yi-documentation/_site`. Copy the files in the output directory into the root of the github project.
