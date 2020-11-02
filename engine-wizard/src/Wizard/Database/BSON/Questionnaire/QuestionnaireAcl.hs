module Wizard.Database.BSON.Questionnaire.QuestionnaireAcl where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Acl.Acl ()
import Wizard.Model.Questionnaire.QuestionnaireAcl

instance ToBSON QuestionnairePermRecord

instance FromBSON QuestionnairePermRecord
