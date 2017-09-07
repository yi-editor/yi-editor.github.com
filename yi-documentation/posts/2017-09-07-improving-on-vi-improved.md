---
title: Improving on Vi Improved
author: Dmitry Ivanov
---

Vim emulation in Yi is far from perfect. It would be interesting to have a test suite
to quantify what portion of Vim one or the other editor implements and have a
leaderboard and a healthy competition. The most useful metric for the quality
of Vim emulation that I have is saying to a person new to Yi to try and use it
like Vim and counting the seconds until something doesn't work as expected.

Nevertheless, Yi has some little bits and pieces in its emulation that are
improvements over the real Vim. Just today I finally realized that I often go
from insert mode to normal mode just to go back a character or two. But there
already is a very popular shortcut for going back a character: `<C-b>`! It works
in a shell, in many commandline programs, in text fields in macOS, in emacs of course.

What does it do in Vim? Inserts `^B`, which is probably less useful thing. So as
of today `<C-f>` and `<C-b>` move the cursor instead of doing nothing in insert mode.
Going through other [standard readline bindings](https://en.wikipedia.org/wiki/GNU_Readline)
and porting some to insert mode in Yi would be a good beginner project.

Of course, there are other ways in Vim (and Yi) to move the cursor one character
left, like `<C-o>h` and the left arrow (no judging). I just find `<C-b>` to be the
easiest.

Another tiny bit where Yi improves on Vim is blockwise visual insertion. While
in Vim you have to type the whole thing, press Escape and then observe the
result, Yi updates all rows live. This one is best described with a screen
recording:

<video controls><source type="video/mp4" src="/images/visualblock.mp4"><p>This should have been a video, oh well.</p></video>

For some more (probably more obscure) points where Yi is intentionally
incompatible with Vim, see [yi-keymap-vim readme](https://github.com/yi-editor/yi/blob/master/yi-keymap-vim/README.rst)