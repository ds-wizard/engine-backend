module Shared.Model.Questionnaire.QuestionnaireUtil where

import qualified Data.List as L

createReplyKey :: [String] -> String
createReplyKey = L.intercalate "."
