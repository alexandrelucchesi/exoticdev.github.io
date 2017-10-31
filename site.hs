--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
import           Hakyll

-- Custom imports
import           Data.List (isSuffixOf)
import           System.FilePath.Posix (takeBaseName, takeDirectory, (</>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md", "404.md"]) $ do
        --route   $ setExtension "html"
        route cleanPageRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    match "posts/*" $ do
        --route $ setExtension "html"
        route cleanPostRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        --route idRoute
        route cleanPageRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
cleanPageRoute :: Routes
cleanPageRoute = customRoute createIndexRoute
  where
    createIndexRoute identifier =
        let p = toFilePath identifier
         in takeDirectory p
              </> takeBaseName p
              </> "index.html"

cleanPostRoute :: Routes
cleanPostRoute = customRoute createIndexRoute
  where
    createIndexRoute identifier =
        let p = toFilePath identifier
         in drop 5 (takeDirectory p) -- Drops "posts"
              </> drop 11 (takeBaseName p) -- Drops "YYYY-MM-DD-"
              </> "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
  where
    idx = "index.html"

    clean url
      | idx `isSuffixOf` url = take (length url - length idx) url
      | otherwise            = url

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

