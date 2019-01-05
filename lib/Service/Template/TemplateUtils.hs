module Service.Template.TemplateUtils where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import System.IO (IOMode(ReadMode), hGetContents, openFile)
import System.IO.Error (tryIOError)
import Text.Ginger (makeContextHtml, runGinger, toGVal)
import Text.Ginger.GVal (GVal, ToGVal)
import Text.Ginger.Html (htmlSource)

-- Given a Template and a HashMap of context, render the template to Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template

-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
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
