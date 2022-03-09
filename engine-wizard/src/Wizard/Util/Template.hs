module Wizard.Util.Template where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Writer.Lazy (Writer)
import Data.Char (chr, ord)
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List (elemIndex)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Ginger (IncludeResolver, SourcePos, makeContextHtml, parseGinger, runGinger, toGVal)
import Text.Ginger.AST (Template, VarName)
import Text.Ginger.GVal (GVal, ToGVal, asList, asText, fromFunction)
import Text.Ginger.Html (Html, htmlSource, unsafeRawHtml)
import Text.Ginger.Run.Type (Run)
import Text.Markdown
import Text.Read (readMaybe)

-- Given a Template and a HashMap of context, render the template to Text
render ::
     (ToGVal (Run p (Writer Html) Html) p, ToGVal (Run p (Writer Html) Html) b)
  => Template p
  -> HashMap.HashMap VarName b
  -> T.Text
render template contextMap = htmlSource $ runGinger (makeContextHtml $ contextLookup contextMap) template

renderEither ::
     (ToGVal (Run p (Writer Html) Html) p, ToGVal (Run p (Writer Html) Html) b)
  => Template p
  -> HashMap.HashMap VarName b
  -> Either String T.Text
renderEither template contextMap =
  let result = render template contextMap
   in if T.null result || _ERROR_TEMPLATE_PLACEHOLDER `L.isSubsequenceOf` T.unpack result
        then Left "There is a missing parameter(s) in the context"
        else Right result

renderEither' templateString contextMap = do
  template <- createTemplate templateString
  renderEither template contextMap

contextLookup contextMap key =
  case lookup key customFilters of
    (Just filter) -> filter
    Nothing -> scopeLookup key contextMap

-- Wrapper around HashMap.lookup that applies toGVal to the value found.
-- Any value referenced in a template, returned from within a template, or used
-- in a template context, will be a GVal
scopeLookup :: (Hashable k, Show k, Eq k, Data.String.IsString k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context =
  case HashMap.lookup key context of
    Just key -> toGVal key
    Nothing -> toGVal _ERROR_TEMPLATE_PLACEHOLDER

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
  return . toGVal $ elemIndex (T.unpack . asText $ txt) ((maybe [] (map (T.unpack . asText)) . asList) lst)
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

ofAlphabet n = chr (n + ord 'a' - 1)

makeInt = fromMaybe 0 . readMaybe . T.unpack . asText

gfnReverse :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnReverse ((_, xs):_) = return . toGVal . reverse . fromMaybe [] . asList $ xs
gfnReverse _ = return def

gfnJoin :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnJoin ((_, txts):(_, glue):_) = return . toGVal $ T.intercalate (asText glue) (maybe [] (map asText) $ asList txts)
gfnJoin _ = return def

gfnToCharArray :: Monad m => [(Maybe T.Text, GVal m)] -> m (GVal m)
gfnToCharArray ((_, txt):_) = return . toGVal . T.unpack . asText $ txt
gfnToCharArray _ = return def

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
_ERROR_TEMPLATE_PLACEHOLDER = "_____ERROR_____"

-- | We don't need to support includes, so we'll create an include resolver
-- that always fails. If you need to use includes, you'll want to use an actual
-- resolver here (see the next section for an example implementation), and
-- use a suitable monad for the parsing step (e.g. 'IO').
nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

-- | This is our template. Because 'parseGinger' wants a monad (as loading
-- includes would normally go through some sort of monadic API like 'IO'), we
-- use 'Identity' here.
createTemplate :: String -> Either String (Template SourcePos)
createTemplate template =
  case runIdentity $ parseGinger nullResolver Nothing template of
    Right result -> return result
    Left error -> Left . show $ error
