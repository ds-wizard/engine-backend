module Wizard.Service.Report.Evaluator.Common where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Wizard.Model.Questionnaire.QuestionnaireReply

getReply :: [ReplyTuple] -> String -> Maybe ReplyTuple
getReply replies p = L.find (\(path, _) -> path == p) replies

isRequiredNow :: [U.UUID] -> Maybe U.UUID -> U.UUID -> Int -> Int
isRequiredNow phaseUuids mQPhase qtnPhase currentValue
  | qtnPhase == U.nil = currentValue
  | qPhaseIndex <= qtnPhaseIndex = currentValue
  | otherwise = 0
  where
    qtnPhaseIndex = fromMaybe 9999 (qtnPhase `L.elemIndex` phaseUuids)
    qPhaseIndex =
      case mQPhase of
        Just qPhase -> fromMaybe 9999 (qPhase `L.elemIndex` phaseUuids)
        Nothing -> 9999

composePath :: String -> String -> String
composePath path element = path ++ "." ++ element

composePathUuid :: String -> U.UUID -> String
composePathUuid path uuid = composePath path (U.toString uuid)
