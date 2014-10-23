---
layout: page
title: Modularizing the config
---

Let's first show you an extension for Yi numbers with boilerplate in the yi.hs file, then modularize the code and factor out some components

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

main = yi $ myConfig

myConfig = defaultVimConfig { defaultKm = myKeymap }

myKeymap = v2KeymapSet $ myBindings

myBindings :: (V2.EventString -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
  [ nmap  "\\<C-a>" ((getCountE >>= withCurrentBuffer . savingPointB . incrementNextNumberByB) :: EditorM ())
  , nmap  "\\<C-x>" ((getCountE >>= withCurrentBuffer . savingPointB . incrementNextNumberByB . negate) :: EditorM ())
  ]

-- Boilerplate begins here

v2KeymapSet :: ((V2.EventString -> EditorM ()) -> [V2.VimBinding]) -> KeymapSet
v2KeymapSet myBindings = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          V2.vimBindings = myBindings eval ++ V2.vimBindings super
        }

nmap  x y = V2.mkStringBindingE V2.Normal V2.Drop (x, y, id)

-- Boilerplate ends here
~~~

### Removing Boilerplate

The boilerplate code mentioned above looks kinda ugly in the Yi config, and won't be useful unless you understand the eDSL thoroughly. So let's try and eliminate it.

Basically, we want to modularize our config by moving the boilerplate to a different file. You can do this by simply adding your file to the ```.config/yi/lib``` directory.

Include the following in ```.config/yi/lib/Yi/Custom/Helper.hs```.

~~~ haskell
-- File Helper.hs
{-# LANGUAGE OverloadedStrings #-}
module Yi.Custom.Helper
       ( v2KeymapSet
       , nmap
       , imap
       , nmap'
       , leader
       )
       where

import Data.Monoid (mappend)

import           Yi
import qualified Yi.Keymap.Vim        as V2
import qualified Yi.Keymap.Vim.Common as V2
import qualified Yi.Keymap.Vim.Utils  as V2

import qualified Data.Text            as T

v2KeymapSet :: ((V2.EventString -> EditorM ()) -> [V2.VimBinding]) -> KeymapSet
v2KeymapSet myBindings = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          V2.vimBindings = myBindings eval ++ V2.vimBindings super
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
~~~

Then, you can simply use the following Yi config:

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}

import           Yi
import qualified Yi.Keymap.Vim        as V2
import qualified Yi.Keymap.Vim.Common as V2
import qualified Yi.Keymap.Vim.Utils  as V2

import           Yi.Custom.Helper (v2KeymapSet, nmap, imap, nmap')

import           Yi.Keymap.Vim.StateUtils (getCountE)

main = yi $ myConfig

myConfig = defaultVimConfig { defaultKm = myKeymap }

myKeymap = v2KeymapSet $ myBindings

myBindings :: (V2.EventString -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
  [ nmap  (leader "<C-a>") ((getCountE >>= withCurrentBuffer . savingPointB . incrementNextNumberByB) :: EditorM ())
  , nmap  (leader "<C-x>") ((getCountE >>= withCurrentBuffer . savingPointB . incrementNextNumberByB . negate) :: EditorM ())
  ]
~~~

And that's it. It looks much cleaner now.
