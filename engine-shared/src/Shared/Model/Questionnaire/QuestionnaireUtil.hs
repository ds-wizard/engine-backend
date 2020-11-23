module Shared.Model.Questionnaire.QuestionnaireUtil where

import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Shared.Util.String (splitOn)

createReplyKey :: [U.UUID] -> String
createReplyKey = L.intercalate "." . fmap U.toString

readReplyKey :: String -> [U.UUID]
readReplyKey = fmap (fromJust . U.fromString) . splitOn "."

replyKeyContains :: String -> U.UUID -> Bool
replyKeyContains replyKey uuid = uuid `elem` readReplyKey replyKey
