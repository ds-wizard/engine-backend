module Wizard.Database.BSON.Questionnaire.QuestionnaireSharing where

import qualified Data.Bson as BSON

import Wizard.Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireSharing where
  val RestrictedQuestionnaire = BSON.String "RestrictedQuestionnaire"
  val AnyoneWithLinkViewQuestionnaire = BSON.String "AnyoneWithLinkViewQuestionnaire"
  val AnyoneWithLinkEditQuestionnaire = BSON.String "AnyoneWithLinkEditQuestionnaire"
  cast' (BSON.String "RestrictedQuestionnaire") = Just RestrictedQuestionnaire
  cast' (BSON.String "AnyoneWithLinkViewQuestionnaire") = Just AnyoneWithLinkViewQuestionnaire
  cast' (BSON.String "AnyoneWithLinkEditQuestionnaire") = Just AnyoneWithLinkEditQuestionnaire
  cast' _ = Nothing
