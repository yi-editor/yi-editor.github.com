{-# LANGUAGE OverloadedStrings #-}

import Control.Monad   (mapM)
import Data.Monoid     ((<>))
import System.FilePath (takeDirectory, takeBaseName, (</>))

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
    route $ setExtension "html" `composeRoutes` niceRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html"    defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
      >>= removeIndexHtml

  match "posts/*" $ do
    route $ setExtension "html" `composeRoutes` niceRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
      >>= removeIndexHtml
      
  create ["atom.xml"] $ do
  route idRoute
  compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
      renderAtom feedConfiguration feedCtx posts

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*" >>= recentFirst
      pages <- loadAll "pages/*" >>= mapM removeIndexHtml
      let indexCtx = listField "posts" postCtx        (return posts)
                  <> listField "pages" defaultContext (return pages)
                  <> constField "title" "Home"
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
        >>= removeIndexHtml

    create ["archive.html"] $ do
      route $ niceRoute
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
          >>= removeIndexHtml

  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext


-----------------------------------------------------------
-- This is a hack. We should be able to do better than this.

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar/ in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                             where p=toFilePath ident
 
-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr str@(x:xs) | str == "/index.html" = "/"
                              | otherwise = x:removeIndexStr xs
    removeIndexStr [] = []
-----------------------------------------------------------------

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Yi blog"
    , feedDescription = "Yi blog"
    , feedAuthorName  = "Yi developers"
    , feedAuthorEmail = "yi-devel@googlegroups.com"
    , feedRoot        = "https://yi-editor.github.io/"
    }