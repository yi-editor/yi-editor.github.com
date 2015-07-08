---
layout: page
title: Vim configuration
---

The simplest Vim configuration you can start with is as follows:

~~~ haskell
import Yi

main = yi $ defaultVimConfig
~~~

Basically, importing ```Yi``` imports all the modules required to get started. ```defaultVimConfig``` sets the default Vim config.

Let's first refactor our config.

~~~ haskell
import Yi

main = yi $ myConfig

myConfig = defaultVimConfig
~~~

Now, the nice part about being extensible in Haskell, is the ease of configuration. ```defaultVimConfiguration``` uses Haskell's record syntax. Don't worry if you don't understand what that means. Basically, you can express the configuration more verbosely as follows:

~~~ haskell
import Yi

main = yi $ myConfig

myConfig = defaultVimConfig
    { defaultKm = defaultKm defaultVimConfig
    , configUI = configUI defaultVimConfig
    }
~~~

We haven't really changed anything, but now we're being more explicit about the fields in the config file. We've only listed two fields, ```defaultKm``` which sets the keymap, and ```configUI``` which sets the UI. There are actually many more fields to defaultVimConfig, but right now we only care about these two fields.

Let's make a small change. We want our UI to look more like Vim's, so we edit the configUI field:

~~~ haskell
import Yi

main = yi $ myConfig

myConfig = defaultVimConfig
    { defaultKm = defaultKm defaultVimConfig
    , configUI = (configUI defaultVimConfig) { configWindowFill = '~' }
    }
~~~

The parenthesis there was needed to make sure the value of configUI is modified. Again, we're able to edit ```configWindowFill``` thanks to Haskell's record syntax.

Now we have ~ signs on every line except the first. Feels much more like vim. But we can do better.

~~~ haskell
import Yi

import Yi.Style (Color(Default))

import Data.Monoid ((<>))

main = yi $ myConfig

myConfig = defaultVimConfig
  { defaultKm = defaultKm defaultVimConfig
  , configUI  = (configUI defaultVimConfig) { configWindowFill = '~'
                                            , configTheme      = myTheme
                                            }
  }

defaultColor :: Yi.Style.Color
defaultColor = Yi.Style.Default

myTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes   = emptyAttributes { foreground = black,   background = darkcyan }
  , tabBarAttributes     = emptyAttributes { foreground = white,   background = defaultColor }
  , baseAttributes       = emptyAttributes { foreground = defaultColor, background = defaultColor, bold = True }
  , commentStyle         = withFg darkred <> withBd False <> withItlc True
  , selectedStyle        = withReverse True
  , errorStyle           = withBg red   <> withFg white
  , operatorStyle        = withFg brown <> withBd False
  , hintStyle            = withBg brown <> withFg black
  , importStyle          = withFg blue
  , dataConstructorStyle = withFg blue
  , typeStyle            = withFg blue
  , keywordStyle         = withFg yellow
  , builtinStyle         = withFg brown
  , strongHintStyle      = withBg brown <> withUnderline True
  , stringStyle          = withFg brown <> withBd False
  , preprocessorStyle    = withFg blue
  }
~~~

That should add a nice looking theme. You can of course customize the theme as you like.

Now, finally, the most useful part. Custom keybindings. Vim users typically use nmap and imap to define custom keybindings. So let's deine nmap and imap ourselves, call those definitions part of boilerplate, and concentrate on what's important.

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}

import Yi

import qualified Yi.Keymap.Vim        as V2
import qualified Yi.Keymap.Vim.Common as V2
import qualified Yi.Keymap.Vim.Utils  as V2

import           Data.Monoid ((<>), mappend)

main = yi $ myConfig

myConfig = defaultVimConfig
  { defaultKm = myKeymap
  , configUI  = (configUI defaultVimConfig) { configWindowFill = '~' }
  }

myKeymap = v2KeymapSet $ myBindings

myBindings :: (V2.EventString -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
  [ nmap  "<C-h>" previousTabE
  , nmap  "<C-l>" nextTabE

  -- Press space to clear incremental search highlight
  , nmap  " " (eval ":nohlsearch<CR>")

  -- for times when you don't press shift hard enough
  , nmap  ";" (eval ":")

  -- Vim's behavior of Y, for historical reasons.
  , nmap "Y" (eval "yy")

  , nmap  "<F3>" (withCurrentBuffer deleteTrailingSpaceB)
  , nmap  "<F4>" (withCurrentBuffer moveToSol)

  , imap  "<Home>" (withCurrentBuffer moveToSol)
  , imap  "<End>"  (withCurrentBuffer moveToEol)
  ]

-- Boilerplate begins here
v2KeymapSet :: ((V2.EventString -> EditorM ()) -> [V2.VimBinding]) -> KeymapSet
v2KeymapSet myBindings = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          V2.vimBindings = myBindings eval <> V2.vimBindings super
        }

nmap  x y = V2.mkStringBindingE V2.Normal V2.Drop (x, y, id)
imap  x y = V2.VimBindingE (\evs state -> case V2.vsMode state of
                            V2.Insert _ ->
                                fmap (const (y >> return V2.Continue))
                                     (evs `V2.matchesString` x)
                            _ -> V2.NoMatch)
nmap'  x y = V2.mkStringBindingY V2.Normal (x, y, id)

leader :: V2.EventString -> V2.EventString
leader = mappend "\\"
--Boilerplate ends here

~~~


And that's it. The nmap's and imap's look exactly like Vim's config! You don't need to bother about how nmap and imap are defined. With a little bit of boilerplate, you can start configuring Yi just like Vim.

The above config (including the theme) is a simplified version of [Michal's config](https://github.com/yi-editor/yi/blob/master/yi-contrib/src/Yi/Config/Users/Michal.hs)

### Extending Vim numbers

Just to show you how easy it is to configure Yi, consider the following case.

You want to modify the behavior of ```<C-a>``` in Vim by making the cursor stay at the same location, while still incrementing the number. You went ahead and looked at the source code for ```<C-a>``` which gave you ```getCountE >>= withCurrentBuffer . incrementNextNumberByB```. Using this, you can simple use function composition, to add ```savingPointB``` which saves the cursor location and makes sure the cursor moves back to that location after the function is executed. In our case, the function ```incrementNextNumberByB``` is what gets executed, and that's the function responsible for moving the cursor.

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}

import Yi

import qualified Yi.Keymap.Vim        as V2
import qualified Yi.Keymap.Vim.Common as V2
import qualified Yi.Keymap.Vim.Utils  as V2

import           Yi.Keymap.Vim.StateUtils (getCountE)

import           Data.Monoid ((<>), mappend)

main = yi $ myConfig

myConfig = defaultVimConfig
  { defaultKm = myKeymap
  , configUI  = (configUI defaultVimConfig) { configWindowFill = '~' }
  }

myKeymap = v2KeymapSet $ myBindings

myBindings :: (V2.EventString -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
  [ nmap  (leader "<C-a>") (getCountE >>= withCurrentBuffer . savingPointB . incrementNextNumberByB)
  , nmap  (leader "<C-x>") (getCountE >>= withCurrentBuffer . savingPointB . incrementNextNumberByB . negate)
  ]

-- Boilerplate begins here
v2KeymapSet :: ((V2.EventString -> EditorM ()) -> [V2.VimBinding]) -> KeymapSet
v2KeymapSet myBindings = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          V2.vimBindings = myBindings eval <> V2.vimBindings super
        }

nmap  x y = V2.mkStringBindingE V2.Normal V2.Drop (x, y, id)
imap  x y = V2.VimBindingE (\evs state -> case V2.vsMode state of
                            V2.Insert _ ->
                                fmap (const (y >> return V2.Continue))
                                     (evs `V2.matchesString` x)
                            _ -> V2.NoMatch)
nmap'  x y = V2.mkStringBindingY V2.Normal (x, y, id)

leader :: V2.EventString -> V2.EventString
leader = mappend "\\"
--Boilerplate ends here

~~~

That's how easy configuring Yi is. Simple function composition (of course, you should not mind looking at the source code first).

### Removing Boilerplate

See the section, Modularizing the config, for details on moving the boilerplate outside the yi.hs config file. In short, files that need to be imported can be placed in the ```.config/yi/lib``` directory.
