module Shared.Localization.Locale where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)

import Shared.Model.Localization.LocaleRecord

locale :: M.Map String String -> LocaleRecord -> String
locale ls (LocaleRecord key defaultValue variables) =
  case M.lookup key ls of
    Just value -> format value $ variables
    Nothing -> format defaultValue $ variables

format :: String -> [String] -> String
format str terms =
  case str of
    '%':'s':rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ (format rest (drop 1 terms))
    '%':'%':'s':rest -> '%' : 's' : format rest terms
    a:rest -> a : format rest terms
    [] -> []
