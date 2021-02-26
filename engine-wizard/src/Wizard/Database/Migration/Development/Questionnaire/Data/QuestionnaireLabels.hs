module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Util.Uuid
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies

fLabel1 = u' "3268ae3b-8c1a-44ea-ba69-ad759b3ef2ae"

fLabels :: M.Map String [U.UUID]
fLabels = M.fromList [(fst rQ1, [fLabel1])]

fLabelsEdited :: M.Map String [U.UUID]
fLabelsEdited = M.fromList [(fst rQ1, [fLabel1]), (fst rQ2, [fLabel1])]
