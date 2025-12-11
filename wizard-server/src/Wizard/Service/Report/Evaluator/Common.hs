module Wizard.Service.Report.Evaluator.Common where

import qualified Data.List as L
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.UUID as U

import Wizard.Model.Project.ProjectReply

getReply :: [ReplyTuple] -> String -> Maybe ReplyTuple
getReply replies p = L.find (\(path, _) -> path == p) replies

isRequiredNow :: [U.UUID] -> Maybe U.UUID -> Maybe U.UUID -> Int -> Int
isRequiredNow phaseUuids mQPhase mProjectPhase currentValue
  | isNothing mProjectPhase = currentValue
  | qPhaseIndex <= projectPhaseIndex = currentValue
  | otherwise = 0
  where
    projectPhaseIndex =
      case mProjectPhase of
        Just projectPhase -> fromMaybe 9999 (projectPhase `L.elemIndex` phaseUuids)
        Nothing -> 9999
    qPhaseIndex =
      case mQPhase of
        Just qPhase -> fromMaybe 9999 (qPhase `L.elemIndex` phaseUuids)
        Nothing -> 9999

composePath :: String -> String -> String
composePath path element = path ++ "." ++ element

composePathUuid :: String -> U.UUID -> String
composePathUuid path uuid = composePath path (U.toString uuid)
