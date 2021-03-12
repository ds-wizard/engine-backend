module Wizard.Service.Report.Evaluator.Common where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Wizard.Model.Questionnaire.QuestionnaireReply

getReply :: [ReplyTuple] -> String -> Maybe ReplyTuple
getReply replies p = L.find (\(path, _) -> path == p) replies

isRequiredNow :: Maybe Int -> Int -> Int -> Int
isRequiredNow mQLevel qtnLevel currentValue =
  if qtnLevel == 9999
    then currentValue
    else let qLevel = fromMaybe 9999 mQLevel
          in if qLevel <= qtnLevel
               then currentValue
               else 0

composePath :: String -> String -> String
composePath path element = path ++ "." ++ element

composePathUuid :: String -> U.UUID -> String
composePathUuid path uuid = composePath path (U.toString uuid)
