module Util.Interpolation where

import Data.Map.Strict as M
import Text.Replace

interpolateString :: M.Map String String -> String -> String
interpolateString variables text =
  let replaceMapFn (key, value) = (string'fromString $ "${" ++ key ++ "}", value)
      replaceMap = M.fromList . fmap replaceMapFn . M.toList $ variables
  in replaceWithMap replaceMap text

interpolateMapValues :: M.Map String String -> M.Map String String -> M.Map String String
interpolateMapValues variables = fmap (interpolateString variables)
