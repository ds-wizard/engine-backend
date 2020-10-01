module Wizard.Database.BSON.Questionnaire.QuestionnaireSharing where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireSharing where
  val = genericVal
  cast' = genericCast'
