module Wizard.Database.BSON.Questionnaire.Questionnaire where

import Data.Bson.Generic
import Data.Maybe ()

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireReply ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.Questionnaire

instance ToBSON Questionnaire

instance FromBSON Questionnaire
