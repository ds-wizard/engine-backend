module Wizard.Database.BSON.Questionnaire.QuestionnaireVisibility where

import qualified Data.Bson as BSON

import Wizard.Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireVisibility where
  val VisibleEditQuestionnaire = BSON.String "VisibleEditQuestionnaire"
  val PrivateQuestionnaire = BSON.String "PrivateQuestionnaire"
  val VisibleViewQuestionnaire = BSON.String "VisibleViewQuestionnaire"
  cast' (BSON.String "VisibleEditQuestionnaire") = Just VisibleEditQuestionnaire
  cast' (BSON.String "PrivateQuestionnaire") = Just PrivateQuestionnaire
  cast' (BSON.String "VisibleViewQuestionnaire") = Just VisibleViewQuestionnaire
  cast' _ = Nothing
