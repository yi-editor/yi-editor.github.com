---
layout: page
title: Problems with Yi
---

This page is taken from [Richard Goulter's blog](http://rgoulter.com/blog/yi.html).

### Editing More Than One File

:ls will show a list of currently open buffers, and :buffer # will go to that buffer number.

But there isn’t a :bnext, :bprev which my muscle memory uses.
(One could add this, right, but I wouldn’t know how [See below], as of 2014-07-02).

I’ve not used [arglist](http://vimdoc.sourceforge.net/htmldoc/editing.html#arglist) much, but as a mutable subset of buffers list, I’ve heard it’s an improvement over aforementioned commands. (That, and some more).
There’s a [VimCast](http://vimcasts.org/episodes/meet-the-arglist/) on it.
Not that I can complain (since I haven’t used it in Vim), but also not in the keymap.

### Line Numbers

By default Vim doesn’t have line numbers on, and to get line numbers on Emacs is kindof hackish, I hear.
Maybe it’s because of the Emacs crowd, but it’s not clear to me how to get line numbers on Yi. (as of 2014-07-02).

I haven’t seen any screenshot of Yi with line numbers.

### Visual Mode

One of the cool things about Visual mode is the insertion/appending on multiple lines.
See the [vim docs](http://vimdoc.sourceforge.net/htmldoc/visual.html#v_b_I). The [vim wiki](http://vim.wikia.com/wiki/Inserting_text_in_multiple_lines) has some examples.
Yi now does this better than Vim (as far as I can tell); where Vim only has this multi-line insert for block mode, Yi supports the feature for linewise visual mode as well. And, Yi’s multi-cursor insert writes things “live”, whereas Vim’s is delayed. [Brag](https://github.com/yi-editor/yi/commit/caa06cadbdbd140dc837e042c89a972323386671). (The caveat is that Yi’s block mode is slightly different. In theory it ought to work for standard visual mode, I suppose; examine the diff of that commit to see how, but like, visual mode seems to be for other things).

That Yi’s Vim keymap supports multiple cursors like this implies a multiple cursors feature are supported out-of-the-box (which doesn’t appear to be the case with a grok at the source), or that it should be reasonably straightforward to add a plugin like terryma/vim-multiple-cursors (in the style of Sublime Text).

I took a screenshot of Vim and of Yi for comparison about what their Visual Lines modes look like:

**Vim**

![vim](http://i.imgur.com/POtRl7m.png)

**Yi**

![yi](http://i.imgur.com/0h2erqm.png)

Using [this Yi.hs](https://github.com/rgoulter/dotfiles/blob/28b9712fc66c84121eed82113ab61c66b7d699f3/yi.hs) with Vty frontend (terminal), in a terminal with Solarized colors theme.

### Command-line Window

This really shoots at “obscure Vim things”, but I’m making a list of differences I’ve noticed. This is the q:, q/, q? window which shows up. [Vimdoc:cmdline-window](http://vimdoc.sourceforge.net/htmldoc/cmdline.html#cmdline-window).

(But I’m not sure how commonly this feature is used by Vim folk).

### Quickfix Window

Quickfix is not an obscure feature of Vim. (Though I don’t use it as often as I should). I’m not sure what Yi’s support for Quickfix, or equivalent, is; but I haven’t looked. (as of 2014-07-02). [Vimdoc:quickfix](http://vimdoc.sourceforge.net/htmldoc/quickfix.html).

### Tags

Yi appears to have some support for tags, which I need to look into. (as of 2014-07-02).

### Searching

Yi does have Incremental Searching, and it does this the way Emacs does, which is better than Vim.
That is, while typing the search, all things which match are highlighted, rather than just one entry.

![searching](http://i.imgur.com/QE5fNXu.png)

### Jumplist

Yi does appear to have C-I, C-O for navigating between cursor jumps.
The :ju Ex command to show the jumplist isn’t there.
The changelist (g;, g,) also doesn’t appear to be implemented. [Vimdoc:jump-motions](http://vimdoc.sourceforge.net/htmldoc/motion.html#jump-motions)

I’ve also seen '. used for “jump to last edit”. Yi doesn’t have this.
[Vimdoc:’.](http://vimdoc.sourceforge.net/htmldoc/motion.html#'.)

### Questions About Yi

* How to write in some extra function and bind it with a keymap?

    The sample user configs have an example of this. e.g. [yi-contrib’s Amy.hs](https://github.com/yi-editor/yi/blob/master/yi-contrib/src/Yi/Config/Users/Amy.hs),

~~~ haskell

myConfig = defaultCuaConfig {
    -- Keymap Configuration
    defaultKm = extendedCuaKeymapSet,
    ....
}

-- Add M-x (which is probably Alt-x on your system) to the default
-- keyset, and have it launch our custom macro.
extendedCuaKeymapSet = customizedCuaKeymapSet $
    choice [
        metaCh 'x' ?>>! helloWorld
    ]

-- A custom macro
helloWorld :: YiM ()
helloWorld = withBuffer $ insertN "Hello, world!"

~~~

Unfortunately, this is specifically for the CUA bindings. (Emacs, Vim don’t get the equivalent of customizedCuaKeymapSet).
Fortunately, there are multiple ways to customize bindings for Vim. See some notes on this.

* How to add in some extra function and bind it to an Ex expression? (e.g. :helloWorld).

    For this example, :yi helloWorld will execute helloWorld :: YiM(). See Yi Ex command. Well. This worked for me when using a Simple config, and when publishAction is used. (publishAction "helloWorld" helloWorld was enough).
    For non-Simple configs, it’s not clear how to ‘publish’ an Action, and trying the same doesn’t work. What happens instead is Yi complains about helloWorld not being in scope. Yi.Eval mentions $HOME/.config/yi/local/Env.hs. Trying to put my helloWorld in here didn’t work for me.

    It appears that this only works for functions of type Yim(), as opposed to Int -> Yim() or so. (Or, if possible, it’s not as trivial as I’d hope). (as of 2014-07-08).

If you really want to have an Ex command :helloWorld, then you need to implement an Ex command parser, and your code would look like:

        defaultKM = mkKeymapSet $ defVimConfig `override` \ super self -> super
            { vimExCommandParsers = myExCmdParsers ++ vimExCommandParsers super }
where myExCmdParsers is of type [String -> Maybe ExCommand], something like [helloWorldEC.parser, ...]. While none of the samples have a custom Ex command, here’s a simple Hello World example.

* How can I modularise code I write to customize Yi? e.g. like “source extra-stuff.vim”

Put your haskell files in ```.config/yi/lib``` and import them in yi.hs as you would normally import modules in the same directory. For more information on this, see the section on Modularizing the config.

* How to show a list of keybindings, or something else which can show me what Yi can do?

Haven’t figured that out yet. (as of 2014-07-02).
yi-editor/yi#504 touches upon this issue, with the suggestion of documenting the keybindings (since these don’t change at a pace as to make this too awful).
Those with the programming bug, though, would rather have this done ‘automatically’.

### Troubles with Yi

* Not particularly a bug, but: since Yi is Emacs-like, <Esc> is treated as the Meta key, and when (using a Vim keymap) in Insert mode, one presses Escape quickly followed by another letter (e.g. Escape, then j), it gets handled as <M-j> rather than <Esc>j.
I imagine some people might take advantage of this to allow for different insert-mode inputs. (Probably using <Alt> as meta).
I also imagine it’s possible to configure this behaviour in a yi.hs.
