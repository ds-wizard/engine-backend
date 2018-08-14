module Api.Resource.Questionnaire.QuestionnaireDTO where

import Control.Monad
import Data.Aeson
import Data.Time
import Data.UUID

import Api.Resource.Package.PackageDTO

data QuestionnaireDTO = QuestionnaireDTO
  { _questionnaireDTOUuid :: UUID
  , _questionnaireDTOName :: String
  , _questionnaireDTOLevel :: Int
  , _questionnaireDTOPackage :: PackageDTO
  , _questionnaireDTOCreatedAt :: UTCTime
  , _questionnaireDTOUpdatedAt :: UTCTime
  } deriving (Show, Eq)

instance FromJSON QuestionnaireDTO where
  parseJSON (Object o) = do
    _questionnaireDTOUuid <- o .: "uuid"
    _questionnaireDTOName <- o .: "name"
    _questionnaireDTOLevel <- o .: "level"
    _questionnaireDTOPackage <- o .: "package"
    _questionnaireDTOCreatedAt <- o .: "createdAt"
    _questionnaireDTOUpdatedAt <- o .: "updatedAt"
    return QuestionnaireDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireDTO where
  toJSON QuestionnaireDTO {..} =
    object
      [ "uuid" .= _questionnaireDTOUuid
      , "name" .= _questionnaireDTOName
      , "level" .= _questionnaireDTOLevel
      , "package" .= _questionnaireDTOPackage
      , "createdAt" .= _questionnaireDTOCreatedAt
      , "updatedAt" .= _questionnaireDTOUpdatedAt
      ]
