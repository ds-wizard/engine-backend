module Wizard.Database.BSON.Questionnaire.QuestionnaireVisibility where

import qualified Data.Bson as BSON

import Wizard.Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireVisibility where
  val PublicQuestionnaire = BSON.String "PublicQuestionnaire"
  val PrivateQuestionnaire = BSON.String "PrivateQuestionnaire"
  val PublicReadOnlyQuestionnaire = BSON.String "PublicReadOnlyQuestionnaire"
  cast' (BSON.String "PublicQuestionnaire") = Just PublicQuestionnaire
  cast' (BSON.String "PrivateQuestionnaire") = Just PrivateQuestionnaire
  cast' (BSON.String "PublicReadOnlyQuestionnaire") = Just PublicReadOnlyQuestionnaire
  cast' _ = Nothing
