module Localization.Locale where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)

import Model.Context.BaseContext
import Model.Localization.LocaleRecord

locale :: LocaleRecord -> BaseContextM String
locale (LocaleRecord key defaultValue variables) = do
  ls <- asks _baseContextLocalization
  case M.lookup key ls of
    Just value -> return . format value $ variables
    Nothing -> return . format defaultValue $ variables

format :: String -> [String] -> String
format str terms =
  case str of
    '%':'s':rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ (format rest (drop 1 terms))
    '%':'%':'s':rest -> '%' : 's' : format rest terms
    a:rest -> a : format rest terms
    [] -> []
