---
layout: page
title: About M-x/:yi
---

We sometimes get questions about M-x, the most common being

* why can't I see the function I just defined in my config?
* why is it so slow?

Below I insert almost verbatim a reply to such a question (‘do
user-config functions still have to be bound to keys first?’). The
thing I forgot to say in the e-mail is that the look-up table approach
can be employed immediately by the user by effectively wrapping around
M-x and adding own items from such table, even though it's not exactly
easy to do in this scenario. Then one would just override the M-x
binding to their custom M-x which *does* contain config-defined
functions.

%%%

Hi,

I saw you asking about M-x in Yi but I dislike reddit so I do not post
the reply there. I would still like to answer your question.

You're correct that putting ‘helloWorld’ function in your config is
not enough. but I think it's important to understand *why* it doesn't
work and why binding it to a key does work.

Firstly why binding things to a key works: your user config is in fact
the Main module of Yi: you can even put ‘module Main where’ up top.

There is a default Main (the stock, config-less one) and a user Main
(yi.hs). When you start Yi, it first goes and looks for yi.hs. If it
sees that, it tries to compile it and then start itself with the
freshly-compiled binary. This is also how ‘reload’ works: compile the
config, shut down (preserving some state) and start the newly compiled
binary (reading back some state). As you state in the ‘main’ function
of yi.hs what config gets used, you are able to use your keybindings,
including your newly bound function. It is in essence no different
than adding some functionality in ‘main’ of any Haskell program and
restarting it. We use the ‘dyre’ to perform this recompilation and
switch. You can see the ‘real’ binary which is the result of config
compilation in a cache directory: for me it's under `~/.config/yi`.

The way M-x works is through the use of ‘hint’ package. It starts what
is more or less GHCi, at least as far as I'm aware. You can see it in
Yi.Eval.hs . As part of its setup we do the following:

```haskell
LHI.setImportsQ [("Yi", Nothing), ("Yi.Keymap",Just "Yi.Keymap")]
```

So as you can see, there is no magic going on here, it simply imports
the ‘Yi’ and ‘Yi.Keymap’ modules just as you could do so yourself with
GHCi. When you press ‘M-x’, it calls ‘getAllNamesInScope’ and shows
the results as hints in the minibuffer. As the functions defined in
your config are neither in ‘Yi’ nor in ‘Yi.Keymap’, it can not
possibly know about them. Another thing that's result of this is that
it just shows everything those libraries export rather than some sane
set that the user can actually use. See [1][] for relevant issue.

By the way, both ‘dyre’ and ‘hint’ are the reasons why you have to
jump through some hoops setting up `GHC_PACKAGE_PATH` (or equivalent)
if sandboxing Yi: they both need to know where to find stuff needed
for config compilation/loading the libraries for M-x.

There are a couple of things that can be done to achieve a slightly
more expected M-x result:

* We could explicitly list every function we want to be usable with
M-x in a large lookup table. The advantages to this are are clear: we
don't need ‘hint’, M-x becomes fast, no `GHC_PACKAGE_PATH` issues for
M-x, we can control exactly what things user should have access to in
order to solve [1][]. There is a big disadvantage however: a person
has to maintain such a table. It is error-prone and a pain to
maintain. Even if we require extensions to export such a lookup table,
we now have problems of one table overriding the other resulting in
scoping problems. Of course a workaround is to prefix names with
package name. It sounds bad and like a hack but I think it might be
useful to do regardless, it certainly helps in emacs. There is an
issue relating to this at [2][].

* We can allow the user to specify extra modules for ‘hint’ to load.
This approach means that all we have to do is specify a list of
modules we want to have available and it should do the right thing.
The downside is that M-x would be slower and slower to start the first
time and I don't know how it deals with function name clashes. This
also means that functions ran through M-x keep being slow.

[1]: https://github.com/yi-editor/yi/issues/292
[2]: https://github.com/yi-editor/yi/issues/523
