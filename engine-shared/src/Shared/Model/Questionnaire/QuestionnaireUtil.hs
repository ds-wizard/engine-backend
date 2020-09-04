module Shared.Model.Questionnaire.QuestionnaireUtil where

import qualified Data.List as L
import qualified Data.UUID as U

createReplyKey :: [U.UUID] -> String
createReplyKey = L.intercalate "." . fmap U.toString
