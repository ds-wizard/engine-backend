module Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { _questionnaireCreateDTOName :: String
  , _questionnaireCreateDTOPackageId :: String
  , _questionnaireCreateDTOPrivate :: Bool
  , _questionnaireCreateDTOTagUuids :: [U.UUID]
  }

instance FromJSON QuestionnaireCreateDTO where
  parseJSON (Object o) = do
    _questionnaireCreateDTOName <- o .: "name"
    _questionnaireCreateDTOPackageId <- o .: "packageId"
    _questionnaireCreateDTOPrivate <- o .: "private"
    _questionnaireCreateDTOTagUuids <- o .: "tagUuids"
    return QuestionnaireCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireCreateDTO where
  toJSON QuestionnaireCreateDTO {..} =
    object
      [ "name" .= _questionnaireCreateDTOName
      , "packageId" .= _questionnaireCreateDTOPackageId
      , "private" .= _questionnaireCreateDTOPrivate
      , "tagUuids" .= _questionnaireCreateDTOTagUuids
      ]
