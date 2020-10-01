module Wizard.Database.BSON.Questionnaire.QuestionnaireVisibility where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireVisibility where
  val = genericVal
  cast' = genericCast'
