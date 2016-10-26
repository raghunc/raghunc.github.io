--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc.Options

--------------------------------------------------------------------------------
siteTitle = "Raghu's Home"

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "slides/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/fonts/do" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "files/*.tex" $ do
        route $setExtension "pdf"
        compile pandocCompiler

    match "index.rst" $do
        route $ setExtension "html"
        compile $ do
            let indexCtx = mconcat
                    [ field "toc" $ \item ->
                            loadBody ((itemIdentifier item) {identifierVersion = Just "toc"} )
                    , defaultContext
                    ]

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/homepage.html" indexCtx
                >>= relativizeUrls

                -- from http://julien.jhome.fr/posts/2013-05-14-adding-toc-to-posts.html
        version "toc" $
            compile $ pandocCompilerWith defaultHakyllReaderOptions
                                         defaultHakyllWriterOptions {
                                             writerTableOfContents = True
                                           , writerTemplate = "$toc$"
                                           , writerStandalone = True
                                         }

                    

    match (fromList ["about.rst", "contact.markdown", "*.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
{-|
    create ["archive.html"] $ do
        route idRoute
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
-}

 {-|   match "index.html" $ do
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
-}

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
