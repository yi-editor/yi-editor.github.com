---
layout: page
title: Vimgolf client
---

You might have heard that Yi being extensible in Haskell offers the ability to use existing Haskell libraries. In this page, I'll demonstrate the extensibility of Yi by writing a Vimgolf client. The client is very rudimentary, being able to get the required output, and the starting text. From there, the user can modify the starting text, and if the user succeeds, the buffer text is changed to `success`, else, the buffer text is changed to `failure`.

First, we need to get JSON data from vimgolf. This typically has the format `"http://vimgolf.com/challenges/" ++ challenge-id ++ ".json"`. We can use http-conduit to fetch this using http. Let's start with just this much. Create a file `~/.config/yi/lib/Vimgolf.hs` and put the following in the file:

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}
module Vimgolf where

import qualified Data.ByteString.Lazy.Char8 as BS (ByteString(..))
import           Network.HTTP.Conduit             (simpleHttp)

getJSON :: String -> IO BS.ByteString
getJSON challenge = simpleHttp ("http://vimgolf.com/challenges/" ++ challenge ++ ".json")
~~~

Now that we have a way of getting the json data, let's convert that to haskell's record syntax so that it would become useful. You can have a look at vimgolf's json data to make sense out of this. An example is [here](http://vimgolf.com/challenges/540629666a1e4000020d9e5a.json). You would need to add the following to convert to Haskell's record syntax. This should be familiar to you if you are used to the Aeson library for JSON parsing.

~~~ haskell
import           Control.Applicative              ((<$>), (<*>))
import           Data.Aeson                       ((.:), FromJSON(..), eitherDecode, Value(..))

data VGTopLevel = VGTopLevel { getInput  :: VGData
                             , getOutput :: VGData
                             , vimRC     :: String
                             , client    :: String
                             } deriving (Show)

data VGData = VGData { getData :: String
                     , getType :: String
                     } deriving (Show)

instance FromJSON VGTopLevel where
    parseJSON (Object v) = VGTopLevel <$> v .: "in"
                                      <*> v .: "out"
                                      <*> v .: "vimrc"
                                      <*> v .: "client"

instance FromJSON VGData where
    parseJSON (Object v) = VGData <$> v .: "data"
                                  <*> v .: "type"

getVimgolfJSON :: String -> IO VGTopLevel
getVimgolfJSON str = do
    d <- (eitherDecode <$> getJSON str) :: IO (Either String VGTopLevel)
    case d of
        Left err -> error "FAIL"
        Right vg -> return vg
~~~

And lastly, you would need make this accessible in Yi. This can be done by adding the following:

~~~ haskell
import           Yi
import qualified Yi.Rope as R (toString, fromString)
import           Yi.Utils     (io)

import           Control.Lens                     (assign)
import           Data.Text                        (Text(..))
import qualified Data.ByteString.Lazy.Char8 as BS (ByteString(..))

checkChallenge :: String -> YiM ()
checkChallenge challenge = do
    vg <- io $ getVimgolfJSON challenge
    withBuffer $ do buf <- readRegionB =<< regionOfB Document
                    if ((R.toString buf) == ((getData . getOutput) vg))
                    then replaceBufferContent "SUCCESS"
                    else replaceBufferContent "FAILURE"

getChallenge :: String -> YiM ()
getChallenge challenge = do
    vg <- io $ getVimgolfJSON challenge
    strFun "OUTPUT"
    withBuffer $ insertN (R.fromString ((getData . getOutput) vg))
    withEditor splitE
    bufRef <- withEditor newTempBufferE
    strFun "INPUT"
    withBuffer $ insertN (R.fromString ((getData . getInput) vg))
    where strFun = withBuffer . assign identA . MemBuffer
~~~

Putting all of this together, you would get this final file:

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}

module Vimgolf where

import           Yi
import qualified Yi.Rope as R (toString, fromString)
import           Yi.Utils     (io)

import           Control.Applicative              ((<$>), (<*>))
import           Control.Lens                     (assign)
import           Data.Aeson                       ((.:), FromJSON(..), eitherDecode, Value(..))
import           Data.Text                        (Text(..))
import qualified Data.ByteString.Lazy.Char8 as BS (ByteString(..))
import           Network.HTTP.Conduit             (simpleHttp)

getJSON :: String -> IO BS.ByteString
getJSON challenge = simpleHttp ("http://vimgolf.com/challenges/" ++ challenge ++ ".json")

data VGTopLevel = VGTopLevel { getInput  :: VGData
                             , getOutput :: VGData
                             , vimRC     :: String
                             , client    :: String
                             } deriving (Show)

data VGData = VGData { getData :: String
                     , getType :: String
                     } deriving (Show)

instance FromJSON VGTopLevel where
    parseJSON (Object v) = VGTopLevel <$> v .: "in"
                                      <*> v .: "out"
                                      <*> v .: "vimrc"
                                      <*> v .: "client"

instance FromJSON VGData where
    parseJSON (Object v) = VGData <$> v .: "data"
                                  <*> v .: "type"

getVimgolfJSON :: String -> IO VGTopLevel
getVimgolfJSON str = do
    d <- (eitherDecode <$> getJSON str) :: IO (Either String VGTopLevel)
    case d of
        Left err -> error "FAIL"
        Right vg -> return vg

checkChallenge :: String -> YiM ()
checkChallenge challenge = do
    vg <- io $ getVimgolfJSON challenge
    withBuffer $ do buf <- readRegionB =<< regionOfB Document
                    if ((R.toString buf) == ((getData . getOutput) vg))
                    then replaceBufferContent "SUCCESS"
                    else replaceBufferContent "FAILURE"

getChallenge :: String -> YiM ()
getChallenge challenge = do
    vg <- io $ getVimgolfJSON challenge
    renameBuffer "OUTPUT"
    withBuffer $ insertN (R.fromString ((getData . getOutput) vg))
    withEditor splitE
    bufRef <- withEditor newTempBufferE
    renameBuffer "INPUT"
    withBuffer $ insertN (R.fromString ((getData . getInput) vg))
    where renameBuffer = withBuffer . assign identA . MemBuffer
~~~

And finally, we need a way to call this from Yi. This can be done with a vim config as follows:

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Main (main) where

import           Yi hiding
import qualified Yi.Keymap.Vim        as V2
import qualified Yi.Keymap.Vim.Common as V2
import qualified Yi.Keymap.Vim.Utils  as V2

import           Data.Monoid (mappend)
import           Vimgolf (getChallenge, checkChallenge)

main = yi $ myConfig

myConfig :: Config
myConfig = defaultVimConfig { defaultKm = v2KeymapSet $ myBindings }

myBindings :: (V2.EventString -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
       [ nmap' (leader "s") (getChallenge vgChallenge)
       , nmap' (leader "e") (checkChallenge vgChallenge)
       ]

vgChallenge :: String
vgChallenge = "540629666a1e4000020d9e5a"

v2KeymapSet :: ((V2.EventString -> EditorM ()) -> [V2.VimBinding]) -> KeymapSet
v2KeymapSet myBindings = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          V2.vimBindings = myBindings eval ++ V2.vimBindings super
        }

nmap'  x y = V2.mkStringBindingY V2.Normal (x, y, id)

leader :: V2.EventString -> V2.EventString
leader = mappend "\\"
~~~

The challenge id is set in the config file. You can always edit the config file and use the yi function `reload` to restart as `:yi reload` in vim.

Now, you can open up yi, and with an empty buffer, press `\s` to get the vimgolf challenge, edit the bottom buffer to match the top buffer, and press `\e` to check your result. We could easily extend this example to use the minibuffer, have a retry option, etc.
