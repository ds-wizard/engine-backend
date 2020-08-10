module Wizard.Database.BSON.Questionnaire.QuestionnaireSharing where

import qualified Data.Bson as BSON

import Wizard.Model.Questionnaire.Questionnaire

instance BSON.Val QuestionnaireSharing where
  val RestrictedQuestionnaire = BSON.String "RestrictedQuestionnaire"
  val AnyoneWithLinkQuestionnaire = BSON.String "AnyoneWithLinkQuestionnaire"
  cast' (BSON.String "RestrictedQuestionnaire") = Just RestrictedQuestionnaire
  cast' (BSON.String "AnyoneWithLinkQuestionnaire") = Just AnyoneWithLinkQuestionnaire
  cast' _ = Nothing
