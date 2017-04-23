{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Hakyll.Favicon (faviconsRules, faviconsField)

main :: IO ()
main = do

  let config = defaultConfiguration
                  { destinationDirectory = "example/site"
                  , providerDirectory = "example"
                  , storeDirectory = "example/_cache"
                  , inMemoryCache = True
                  }

  hakyllWith config $ do
    faviconsRules "images/favicon.svg"

    match "index.html" $ do
      route idRoute
      compile $ do
        let ctx = faviconsField `mappend` defaultContext
        getResourceBody >>= applyAsTemplate ctx

