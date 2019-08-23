module Database.BSON.Questionnaire.QuestionnaireAccessibility where

import qualified Data.Bson as BSON

import Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireAccessibility where
  val PublicQuestionnaire = BSON.String "PublicQuestionnaire"
  val PrivateQuestionnaire = BSON.String "PrivateQuestionnaire"
  val PublicReadOnlyQuestionnaire = BSON.String "PublicReadOnlyQuestionnaire"
  cast' (BSON.String "PublicQuestionnaire") = Just PublicQuestionnaire
  cast' (BSON.String "PrivateQuestionnaire") = Just PrivateQuestionnaire
  cast' (BSON.String "PublicReadOnlyQuestionnaire") = Just PublicReadOnlyQuestionnaire
  cast' _ = Nothing
