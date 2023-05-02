module Wizard.Database.Mapping.Questionnaire.QuestionnaireState where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField

import Wizard.Model.Questionnaire.QuestionnaireState

instance FromField QuestionnaireState where
  fromField f dat =
    case fmap BS.unpack dat of
      Just "Default" -> return QSDefault
      Just "Migrating" -> return QSMigrating
      Just "Outdated" -> return QSOutdated
      _ -> returnError ConversionFailed f "Unsupported type"
