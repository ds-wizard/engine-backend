module Shared.Util.String (
  lowerFirst,
  replace,
  splitOn,
  stripSuffix,
  stripSuffixIfExists,
  isSuffixOf,
  trim,
  f',
  toLower,
  toSnake,
  takeLastOf,
  printTuples,
  fromHumps,
) where

import qualified Data.Char as CH
import qualified Data.List as L
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Text.Regex (mkRegex, subRegex)

trim :: String -> String
trim = unwords . words

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [CH.toLower c]
lowerFirst (s : str) = CH.toLower s : str

toLower :: String -> String
toLower = fmap CH.toLower

toSnake :: String -> String
toSnake = downcase . go
  where
    downcase = map CH.toLower
    go s =
      case subRegex (mkRegex "[A-Z]") s "_\\0" of
        ('_' : xs) -> xs
        x -> x

replace :: String -> String -> String -> String
replace name value string = T.unpack $ T.replace (T.pack name) (T.pack value) (T.pack string)

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs

takeLastOf :: String -> String -> Maybe String
takeLastOf separator string =
  case splitOn separator string of
    [] -> Nothing
    xs -> Just . last $ xs

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix string = T.unpack <$> T.stripSuffix (T.pack suffix) (T.pack string)

stripSuffixIfExists :: String -> String -> String
stripSuffixIfExists suffix string = fromMaybe string (stripSuffix suffix string)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix name = T.isSuffixOf (T.pack suffix) (T.pack name)

f' :: String -> [String] -> String
f' str terms =
  case str of
    '%' : 's' : rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ f' rest (drop 1 terms)
    '%' : '%' : 's' : rest -> '%' : 's' : f' rest terms
    a : rest -> a : f' rest terms
    [] -> []

printTuples :: [(String, String)] -> String
printTuples = L.intercalate ", " . fmap printTuple
  where
    printTuple (key, value) = key ++ ": " ++ value

fromHumps :: String -> [String]
fromHumps = go
  where
    go "" = [""]
    go [x] = [[x]]
    go xxs@(x : xs)
      | CH.isUpper x =
          let lhs = takeWhile CH.isUpper xxs
              rhs = dropWhile CH.isUpper xxs
           in if null rhs
                then [lhs]
                else
                  let curLen = length lhs - 1
                      cur = take curLen lhs
                      rec = go rhs
                      nxt = drop curLen lhs ++ concat (take 1 rec)
                      rem = drop 1 rec
                      curL = ([cur | not (null cur)])
                      nxtL = ([nxt | not (null nxt)])
                   in curL ++ nxtL ++ rem
      | otherwise =
          let cur = takeWhile (not . CH.isUpper) xxs
              rem = dropWhile (not . CH.isUpper) xxs
           in if null rem
                then [cur]
                else cur : go rem
