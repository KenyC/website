--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad
import           Hakyll
import           System.FilePath.Posix


--------------------------------------------------------------------------------
main :: IO ()
main = website


website :: IO ()
website = hakyll $ do

    match "src/contents/*" $ do

        route $ gsubRoute "src/contents/" $ const ""
        compile $ do
            contents  <- getResourceBody
            name_page <- (takeBaseName . toFilePath) <$> getUnderlying
            full      <- loadAndApplyTemplate 
                             "src/templates/template_black.html" 
                             (context_page name_page) 
                             contents
            -- >>= loadAndApplys  Template "template.html" defaultContext
            relativizeUrls full



    let folders_simple_compile = ["fonts", "resources/docs", "resources/files", "img", "js"]
        to_glob_pattern folder = fromGlob $ "src/" ++ folder ++ "/*"

    forM_ folders_simple_compile $ \folder -> do
        match (to_glob_pattern folder) $ do
            route   removeSrc
            compile copyFileCompiler

    match "src/css/*.css" $ do 
        route   removeSrc
        compile compressCssCompiler

    match "src/templates/*" $ compile templateBodyCompiler


-----------------------------------------------------------------------------------

removeSrc :: Routes
removeSrc = gsubRoute "src/" $ const ""



context_page :: FilePath -> Context String
context_page name_page = mconcat $ [
      field name_page (const $ return "") 
    , defaultContext ]

