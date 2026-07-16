module Wizard.Util.Gettext where

import Data.Char (isSpace, toLower)
import qualified Data.List as L

import Shared.Common.Util.String (trim)

data PotEntry = PotEntry
  { references :: [String]
  , msgid :: String
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------------------------------------------------
-- POT SERIALIZATION
-- ---------------------------------------------------------------------------------------------------------------------
serializePot :: [(String, String)] -> [PotEntry] -> String
serializePot headers entries = L.intercalate "\n\n" (headerBlock : fmap entryBlock entries) ++ "\n"
  where
    headerBlock =
      L.intercalate "\n" $
        ["msgid \"\"", "msgstr \"\""]
          ++ fmap (\(k, v) -> "\"" ++ escapePoString (k ++ ": " ++ v) ++ "\\n\"") headers
    entryBlock entry =
      L.intercalate "\n" $
        fmap ("#: " ++) entry.references
          ++ ["msgid \"" ++ escapePoString entry.msgid ++ "\"", "msgstr \"\""]

escapePoString :: String -> String
escapePoString = concatMap escape
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape c = [c]

unescapePoString :: String -> String
unescapePoString ('\\' : c : rest) =
  case c of
    'n' -> '\n' : unescapePoString rest
    't' -> '\t' : unescapePoString rest
    'r' -> '\r' : unescapePoString rest
    '"' -> '"' : unescapePoString rest
    '\\' -> '\\' : unescapePoString rest
    _ -> c : unescapePoString rest
unescapePoString (c : rest) = c : unescapePoString rest
unescapePoString [] = []

-- ---------------------------------------------------------------------------------------------------------------------
-- PO HEADER PARSING
-- ---------------------------------------------------------------------------------------------------------------------
parsePoHeaderFields :: String -> Either String [(String, String)]
parsePoHeaderFields content =
  case findHeaderText . groupStatements . fmap classifyLine . lines . stripBom $ content of
    Just headerText -> Right . parseHeaderFields $ headerText
    Nothing -> Left "Missing PO header entry (msgid \"\" with msgstr)"

getPoHeaderField :: String -> [(String, String)] -> Maybe String
getPoHeaderField name fields =
  case lookup name fields of
    Just value
      | not (null value) -> Just value
    _ -> Nothing

-- ---------------------------------------------------------------------------------------------------------------------
-- PLURAL-FORMS
-- ---------------------------------------------------------------------------------------------------------------------
-- Gettext Plural-Forms header value for a language code (e.g. "en", "cs", "pt-BR").
pluralForms :: String -> String
pluralForms code =
  case primarySubtag code of
    "ja" -> onePlural
    "ko" -> onePlural
    "zh" -> onePlural
    "vi" -> onePlural
    "th" -> onePlural
    "id" -> onePlural
    "tr" -> onePlural
    "fr" -> "nplurals=2; plural=(n > 1);"
    "cs" -> "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;"
    "sk" -> "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;"
    "pl" -> "nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);"
    "ru" -> slavicThreePlurals
    "uk" -> slavicThreePlurals
    "sr" -> slavicThreePlurals
    "hr" -> slavicThreePlurals
    "lt" -> "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2);"
    "lv" -> "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n != 0 ? 1 : 2);"
    "ro" -> "nplurals=3; plural=(n==1 ? 0 : (n==0 || (n%100 > 0 && n%100 < 20)) ? 1 : 2);"
    "sl" -> "nplurals=4; plural=(n%100==1 ? 0 : n%100==2 ? 1 : n%100==3 || n%100==4 ? 2 : 3);"
    "ar" -> "nplurals=6; plural=(n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 ? 4 : 5);"
    _ -> twoPlurals
  where
    onePlural = "nplurals=1; plural=0;"
    twoPlurals = "nplurals=2; plural=(n != 1);"
    slavicThreePlurals = "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);"

primarySubtag :: String -> String
primarySubtag = fmap toLower . takeWhile (\c -> c /= '-' && c /= '_')

-- ---------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ---------------------------------------------------------------------------------------------------------------------
data PoLine
  = PoKeywordLine String String
  | PoContinuationLine String
  | PoIgnoredLine
  deriving (Show, Eq)

stripBom :: String -> String
stripBom ('\xfeff' : rest) = rest
stripBom content = content

classifyLine :: String -> PoLine
classifyLine rawLine =
  case trim rawLine of
    [] -> PoIgnoredLine
    line@(firstChar : _)
      | firstChar == '#' -> PoIgnoredLine
      | firstChar == '"' -> PoContinuationLine (unquote line)
      | otherwise ->
          let keyword = takeWhile (\c -> not (isSpace c) && c /= '"') line
           in PoKeywordLine keyword (unquote . drop (length keyword) $ line)
  where
    unquote line =
      case dropWhile (/= '"') line of
        '"' : rest -> unescapePoString . takeQuoted $ rest
        _ -> ""
    takeQuoted ('\\' : c : rest) = '\\' : c : takeQuoted rest
    takeQuoted ('"' : _) = []
    takeQuoted (c : rest) = c : takeQuoted rest
    takeQuoted [] = []

groupStatements :: [PoLine] -> [(String, String)]
groupStatements = reverse . L.foldl' step []
  where
    step statements (PoKeywordLine keyword value) = (keyword, value) : statements
    step ((keyword, previousValue) : statements) (PoContinuationLine value) = (keyword, previousValue ++ value) : statements
    step [] (PoContinuationLine _) = []
    step statements PoIgnoredLine = statements

findHeaderText :: [(String, String)] -> Maybe String
findHeaderText (("msgid", "") : ("msgstr", headerText) : _) = Just headerText
findHeaderText (_ : rest) = findHeaderText rest
findHeaderText [] = Nothing

parseHeaderFields :: String -> [(String, String)]
parseHeaderFields = concatMap parseField . lines
  where
    parseField headerLine =
      case break (== ':') headerLine of
        (name, ':' : value) -> [(trim name, trim value)]
        _ -> []
