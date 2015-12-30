--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
--------------------------------------------------------------------------------
import           Data.Monoid (mconcat,(<>))
import           Control.Monad  (liftM)

--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match ("images/*" .||. "css/*/*" .||. "files/*" .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ liftM (fmap demoteHeaders) pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
--------------------------------------------------------------------------------
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do posts <- recentFirst =<< loadAll pattern
                     let ctx = constField "title" title <>
                               listField "posts" (postCtx tags) (return posts) <>
                                 defaultContext
                     makePost ctx

        -- Create RSS feed as well
        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx

---------------------------------------------------------------------------INDEX

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/post.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

--------------------------------------------------------------------------------
    match "templates/*" $ compile templateCompiler
---------------------------------------------------------------------------POSTS

    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ liftM (fmap demoteHeaders) (pandocCompiler >>= saveSnapshot "content")
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do posts <- recentFirst =<< loadAll "posts/*"
                     let ctx = constField "title" "Posts" <>
                               listField "posts" (postCtx tags) (return posts) <>
                               defaultContext
                     makePost ctx

--------------------------------------------------------------------------------

makePost :: Context String -> Compiler (Item String)
makePost ctx = makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags = mconcat $
    [ modificationTimeField "mtime" "%U"
    , tagsField "tags" tags
    , dateField "date" "%B %e, %Y"
    ] ++
    [ dateField "Month" "%B"
    , dateField "day"   "%e"
    , dateField "year"  "%Y"
    , dateField "mon" "%b"
    , mapContext (drop 3) $ dateField "rest" "%B"
    ] ++
    [defaultContext]

rezeptCtx :: Tags -> Context String
rezeptCtx tags = tagsField "tags" tags <> defaultContext

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "bricolage - " ++ title
    , feedDescription = "Personal blog of epsilonhalbe"
    , feedAuthorName  = "Martin Heuschober"
    , feedAuthorEmail = "epsilonhalbe@gmail.com"
    , feedRoot        = "//epsilonhalbe.github.io"
    }

config :: Configuration
config = defaultConfiguration
          { deployCommand = "rsync -avz ./_site/ /home/epsilonhalbe/epsilonhalbe.github.io"
          , previewHost = "0.0.0.0"
          }
