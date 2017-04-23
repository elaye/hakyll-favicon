{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Hakyll.Favicon (faviconsRules)

main :: IO ()
main = do

  let config = defaultConfiguration
                  { destinationDirectory = "test/out"
                  , providerDirectory = "test"
                  , storeDirectory = "test/out/_cache"
                  , inMemoryCache = True
                  }

  hakyllWith config $ do
    faviconsRules "favicon.svg"

  
