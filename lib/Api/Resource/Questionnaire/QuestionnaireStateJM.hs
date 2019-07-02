module Api.Resource.Questionnaire.QuestionnaireStateJM where

import Control.Monad
import Data.Aeson

import Model.Questionnaire.QuestionnaireState

instance FromJSON QuestionnaireState where
  parseJSON (String "Default") = return QSDefault
  parseJSON (String "Migrating") = return QSMigrating
  parseJSON (String "Outdated") = return QSOutdated
  parseJSON _ = mzero

instance ToJSON QuestionnaireState where
  toJSON QSDefault = "Default"
  toJSON QSMigrating = "Migrating"
  toJSON QSOutdated = "Outdated"
