module Util.Template where

import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.String (IsString)
import Data.Text as T
import Data.Text.Lazy as TL
import System.IO (IOMode(ReadMode), hGetContents, openFile)
import System.IO.Error (tryIOError)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Ginger
       (formatParserError, makeContextHtml, parseGingerFile, runGinger,
        toGVal)
import Text.Ginger.GVal (GVal, ToGVal, asText, fromFunction)
import Text.Ginger.Html (htmlSource, unsafeRawHtml)
import Text.Markdown

-- Load a template from file and render it using HashMap context
-- It will return Right Text if OK or Left String in case of error
loadAndRender fn contextMap = do
  eTemplate <- parseGingerFile mLoadFile fn
  return $
    case eTemplate of
      Right template -> Right $ render template contextMap
      Left err -> Left . formatParserError Nothing $ err

-- Given a Template and a HashMap of context, render the template to Text
render template contextMap = htmlSource $ runGinger (makeContextHtml $ contextLookup contextMap) template

contextLookup contextMap key =
  case lookup key customFilters of
    (Just filter) -> filter
    Nothing -> scopeLookup key contextMap

-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup :: (Hashable k, Eq k, Data.String.IsString k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

mLoadFile :: FilePath -> IO (Maybe String)
mLoadFile fn =
  tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    loadFile :: FilePath -> IO String
    loadFile fn' = openFile fn' ReadMode >>= hGetContents

customFilters :: (Eq a, Data.String.IsString a, ToGVal m a, Monad m) => [(a, GVal m)]
customFilters = [("markdown", fromFunction gfnMarkdown)]

-- Markdown filter
gfnMarkdown :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnMarkdown ((_, input):_) =
  return $ toGVal . unsafeRawHtml . TL.toStrict . renderHtml . markdown def . TL.fromStrict . asText $ input
gfnMarkdown _ = return def
