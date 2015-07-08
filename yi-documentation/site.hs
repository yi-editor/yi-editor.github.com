{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "pages/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html"    defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*" >>= recentFirst
      pages <- loadAll "pages/*"
      let indexCtx = listField "posts" postCtx        (return posts)
                  <> listField "pages" defaultContext (return pages)
                  <> constField "title" "Home"
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- loadAll "posts/*" >>= recentFirst
        pages <- loadAll "pages/*"
        let archiveCtx = listField "posts" postCtx        (return posts)
                      <> listField "pages" defaultContext (return pages)
                      <> constField "title" "Archive"
                      <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

