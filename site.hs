--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend, (<>))
import           Hakyll

-- Custom imports
import           Data.List (isSuffixOf)
import           System.FilePath.Posix (takeBaseName, takeDirectory, (</>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "assets/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*" $ do
        route cleanPageRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" cleanCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route cleanPostRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route cleanPageRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog" `mappend`
                    constField "isBlog" "True" `mappend`
                    cleanCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "isHomePage" "True" `mappend`
                    cleanCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            pages <- loadAll "pages/*"
            let allPosts = return (pages ++ posts)
                sitemapCtx = mconcat
                    [ listField "entries" cleanCtx allPosts
                    , constField "siteUrl" siteUrl
                    , defaultContext ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= relativizeUrls
                -- >>= cleanIndexHtmls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
cleanPageRoute :: Routes
cleanPageRoute = customRoute createIndexRoute
  where
    createIndexRoute identifier =
        let p = toFilePath identifier
         in drop 5 (takeDirectory p) -- Drops "pages" prefix
              </> takeBaseName p
              </> "index.html"

cleanPostRoute :: Routes
cleanPostRoute = customRoute createIndexRoute
  where
    createIndexRoute identifier =
        let p = toFilePath identifier
         in drop 5 (takeDirectory p) -- Drops "posts" prefix
              </> drop 11 (takeBaseName p) -- Drops "YYYY-MM-DD-"
              </> "index.html"

cleanUrlField :: String -> Context String
cleanUrlField key = field key $
    fmap (maybe mempty clean) . getRoute . itemIdentifier
  where
      clean = dropIndex . toUrl

      dropIndex url
        | not (isExternal url) && "/index.html" `isSuffixOf` url =
            take (length url - 11) url
        | otherwise = url

-- IMPORTANT: Without trailing slash!
siteUrl :: String
siteUrl = "https://exoticdev.com"

-- TODO: Create proper development/production settings!
siteIsProduction :: Bool
siteIsProduction = False

cleanCtx :: Context String
cleanCtx = cleanUrlField "url" -- Overrides the default "url".
    `mappend` constField "siteUrl" siteUrl
    `mappend` boolField "siteIsProduction" (const siteIsProduction)
    `mappend` defaultContext

postCtx :: Context String
postCtx = dateField "date" "%b %e, %Y"
    `mappend` constField "author" "Alexandre Lucchesi"
    `mappend` constField "twitter" "alexandrelucch"
    `mappend` constField "isBlog" "True"
    `mappend` cleanCtx
