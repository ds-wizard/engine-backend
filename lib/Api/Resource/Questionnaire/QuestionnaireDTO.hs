module Api.Resource.Questionnaire.QuestionnaireDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Api.Resource.Package.PackageDTO
import Common.Types
import Common.Uuid

data QuestionnaireDTO = QuestionnaireDTO
  { _questionnaireDTOUuid :: UUID
  , _questionnaireDTOName :: String
  , _questionnaireDTOPackage :: PackageDTO
  } deriving (Show, Eq)

instance FromJSON QuestionnaireDTO where
  parseJSON (Object o) = do
    _questionnaireDTOUuid <- o .: "uuid"
    _questionnaireDTOName <- o .: "name"
    _questionnaireDTOPackage <- o .: "package"
    return QuestionnaireDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireDTO where
  toJSON QuestionnaireDTO {..} =
    object ["uuid" .= _questionnaireDTOUuid, "name" .= _questionnaireDTOName, "package" .= _questionnaireDTOPackage]
