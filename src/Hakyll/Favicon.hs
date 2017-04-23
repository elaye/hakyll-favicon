module Hakyll.Favicon
( faviconsField
, faviconsRules
) where

import Data.Monoid ((<>))
import Data.List (intersperse)

import Hakyll
import System.FilePath (takeExtension, (</>))

import Debug.Trace (traceShow)

newtype IconSize = IconSize Int

instance Show IconSize where
  show (IconSize s) = show s

data IconType = Basic IconSize | Ico [IconSize] | IOS IconSize

data Favicon = Favicon IconType

favicons :: [Favicon]
favicons =
  [ Favicon (Ico [IconSize 32, IconSize 64])
  -- basic favicon
  , Favicon (Basic (IconSize 32))
  -- third-generation iPad with high-resolution Retina display
  , Favicon (IOS (IconSize 144))
  -- iPhone with high-resolution Retina display
  , Favicon (IOS (IconSize 114))
  -- first- and second-generation iPad
  , Favicon (IOS (IconSize 72))
  -- non-Retina iPhone, iPod Touch, and Android 2.1+ devices
  , Favicon (IOS (IconSize 57))
  ]

iosTemplate :: Compiler (Item String)
iosTemplate = makeItem "<link rel=\"apple-touch-icon-precomposed\" sizes=\"$size$x$size$\" href=\"$src$\">"

icoTemplate :: Compiler (Item String)
icoTemplate = makeItem "<link rel=\"shortcut icon\" href=\"favicon.ico\" type=\"image/x-icon\">"

basicTemplate :: Compiler (Item String)
basicTemplate = makeItem "<link rel=\"shortcut icon\" href=\"$src$\">"

faviconsField :: Context String
faviconsField = field "favicons" $ \_ -> do
  itemBody <$> faviconsCompiler favicons

faviconsCompiler :: [Favicon] -> Compiler (Item String)
faviconsCompiler favicons = do
  htmls <- mapM faviconCompiler favicons :: (Compiler [Item String])
  makeItem $ concatMap itemBody htmls

faviconCompiler :: Favicon -> Compiler (Item String)
faviconCompiler favicon@(Favicon faviconType) = case faviconType of
  Ico _ -> icoTemplate >>= applyAsTemplate ctx
  IOS size -> iosTemplate >>= applyAsTemplate (ctx <> constField "size" (show size))
  Basic _ -> basicTemplate >>= applyAsTemplate ctx
  where ctx = constField "src" (toUrl (faviconPath favicon))

faviconName :: Favicon -> String
faviconName (Favicon (Ico _)) = "favicon.ico"
faviconName (Favicon (IOS size)) = "favicon" ++ show size ++ ".png"
faviconName (Favicon (Basic size)) = "favicon" ++ show size ++ ".png"

faviconPath :: Favicon -> FilePath
faviconPath favicon@(Favicon (Ico _)) = faviconName favicon
faviconPath favicon = "images" </> "favicons" </> faviconName favicon
faviconPath favicon = "images" </> "favicons" </> faviconName favicon

faviconsRules :: Pattern -> Rules ()
faviconsRules ptn = match ptn $ mapM_ processFavicon favicons

processFavicon :: Favicon -> Rules ()
processFavicon favicon@(Favicon (Ico sizes)) = processIco favicon sizes
processFavicon favicon@(Favicon (IOS size)) = processPng favicon size
processFavicon favicon@(Favicon (Basic size)) = processPng favicon size

processIco :: Favicon -> [IconSize] -> Rules ()
processIco favicon sizes = version ("ico-" ++ concat (intersperse "-" (show <$> sizes))) $ do
  route $ customRoute $ \_ -> faviconPath favicon
  let
    cmd = "convert"
    args =
      [ "-background"
      , "none"
      , "svg:-"
      , "-define"
      , concat ["icon:auto-resize=", concat (intersperse "," (show <$> sizes))]
      , "+repage"
      , "ico:-"
      ]
  compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)

processPng :: Favicon -> IconSize -> Rules ()
processPng favicon (IconSize size) = version ("png" ++ show size) $ do
  route $ customRoute $ \_ -> faviconPath favicon
  let
    cmd = "convert"
    args =
      [ "-background"
      , "none"
      , "svg:-"
      , "-resize"
      , concat [show size, "x", show size, "!"]
      , "+repage"
      , "png:-"
      ]
  compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)
