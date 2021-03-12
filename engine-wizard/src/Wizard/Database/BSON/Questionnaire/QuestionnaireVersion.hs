module Wizard.Database.BSON.Questionnaire.QuestionnaireVersion where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Questionnaire.QuestionnaireVersion

instance ToBSON QuestionnaireVersion

instance FromBSON QuestionnaireVersion
