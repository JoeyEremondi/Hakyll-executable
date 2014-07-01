--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md", "publications.md", "research.md", "software.md"]) $ do
        route   $ setExtension "html"
        compile $ do
            posts <- fakePosts
            let sidebarCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate sidebarCtx
                >>= loadAndApplyTemplate "templates/default.html" sidebarCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            posts <- fakePosts
            let sidebarCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home" `mappend`
                    postCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" sidebarCtx
                >>= loadAndApplyTemplate "templates/default.html" sidebarCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- getPostsList
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fakePosts
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
    
fakePosts :: Compiler [Item String]
fakePosts = do
        identifiers <- getMatches "posts/*"
        return [Item identifier "" | identifier <- identifiers]
    
getPostsList = recentFirst =<< loadAll "posts/*"
