module Wizard.Util.Interpolation where

import Data.Map.Strict as M
import Data.Text as TS
import Data.Text.Lazy as TL
import Text.Replace

interpolateString :: M.Map String String -> String -> String
interpolateString variables text =
  let replaceMapFn (key, value) = (text'fromString $ "${" ++ key ++ "}", TS.pack value)
      replaceMap = M.fromList . fmap replaceMapFn . M.toList $ variables
   in TL.unpack $ replaceWithMap replaceMap (TL.pack text)

interpolateMapValues :: M.Map String String -> M.Map String String -> M.Map String String
interpolateMapValues variables = fmap (interpolateString variables)
