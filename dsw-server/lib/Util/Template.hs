module Util.Template where

import Data.Char (chr, ord)
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.IO
       (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, utf8)
import System.IO.Error (tryIOError)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Ginger
       (formatParserError, makeContextHtml, parseGingerFile, runGinger,
        toGVal)
import Text.Ginger.GVal
       (GVal, ToGVal, asList, asText, fromFunction)
import Text.Ginger.Html (htmlSource, unsafeRawHtml)
import Text.Markdown
import Text.Read (readMaybe)

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
    loadFile fn' = do
      h <- openFile fn' ReadMode
      hSetEncoding h utf8
      hGetContents h

customFilters :: (Eq a, Data.String.IsString a, ToGVal m a, Monad m) => [(a, GVal m)]
customFilters =
  [ ("endswith", fromFunction gfnEndsWith)
  , ("indexOf", fromFunction gfnIndexOf)
  , ("intercalate", fromFunction gfnJoin)
  , ("join", fromFunction gfnJoin)
  , ("markdown", fromFunction gfnMarkdown)
  , ("ofAlphabet", fromFunction gfnOfAlphabet)
  , ("range", fromFunction gfnRange)
  , ("reverse", fromFunction gfnReverse)
  , ("roman", fromFunction gfnRoman)
  , ("toCharArray", fromFunction gfnToCharArray)
  ]

gfnMarkdown :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnMarkdown ((_, input):_) =
  return . toGVal . unsafeRawHtml . TL.toStrict . renderHtml . markdown def . TL.fromStrict . asText $ input
gfnMarkdown _ = return def

gfnRange :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnRange ((_, from):(_, to):(_, step):_) =
  return . toGVal $ makeRange (T.unpack . asText $ from) (T.unpack . asText $ to) (T.unpack . asText $ step)
gfnRange ((_, from):(_, to):_) = return . toGVal $ makeRange (T.unpack . asText $ from) (T.unpack . asText $ to) "1"
gfnRange ((_, to):_) = return . toGVal $ makeRange "0" (T.unpack . asText $ to) "1"
gfnRange _ = return def

makeRange :: String -> String -> String -> [Integer]
makeRange from to step =
  case (readMaybe from :: Maybe Integer, readMaybe to :: Maybe Integer, readMaybe step :: Maybe Integer) of
    (Just f, Just t, Just s) -> enumFromThenTo f (f + s) t
    _ -> []

gfnEndsWith :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnEndsWith ((_, txt):(_, suffix):_) = return . toGVal $ endswith (T.unpack . asText $ txt) (T.unpack . asText $ suffix)
gfnEndsWith _ = return def

endswith :: String -> String -> Bool
endswith t1 t2
  | length t1 < length t2 = False
  | otherwise = drop (length t1 - length t2) t1 == t2

gfnIndexOf :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnIndexOf ((_, lst):(_, txt):_) =
  return . toGVal $ elemIndex (T.unpack . asText $ txt) (map (T.unpack . asText) . fromMaybe [] . asList $ lst)
gfnIndexOf _ = return def

gfnRoman :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnRoman ((_, n):_) = return . toGVal . toRoman . makeInt $ n
gfnRoman _ = return def

toRoman :: Int -> String
toRoman x
  | x <= 0 = ""
  | x >= 1000 = 'M' : toRoman (x - 1000)
  | x >= 100 =
    let (q, r) = x `divMod` 100
    in digit 'C' 'D' 'M' q ++ toRoman r
  | x >= 10 =
    let (q, r) = x `divMod` 10
    in digit 'X' 'L' 'C' q ++ toRoman r
  | otherwise = digit 'I' 'V' 'X' x
  where
    digit :: Char -> Char -> Char -> Int -> String
    digit x y z k = [[x], [x, x], [x, x, x], [x, y], [y], [y, x], [y, x, x], [y, x, x, x], [x, z]] !! (k - 1)

gfnOfAlphabet :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnOfAlphabet ((_, n):_) = return . toGVal . ofAlphabet . makeInt $ n
gfnOfAlphabet _ = return def

ofAlphabet n = chr (n + ord ('a') - 1)

makeInt = fromMaybe 0 . readMaybe . T.unpack . asText

gfnReverse :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnReverse ((_, xs):_) = return . toGVal . reverse . fromMaybe [] . asList $ xs
gfnReverse _ = return def

gfnJoin :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnJoin ((_, txts):(_, glue):_) = return . toGVal $ T.intercalate (asText glue) (map asText . fromMaybe [] $ asList txts)
gfnJoin _ = return def

gfnToCharArray :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnToCharArray ((_, txt):_) = return . toGVal . T.unpack . asText $ txt
gfnToCharArray _ = return def
