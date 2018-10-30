module Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import Control.Monad
import Data.Aeson

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { _questionnaireCreateDTOName :: String
  , _questionnaireCreateDTOPackageId :: String
  , _questionnaireCreateDTOPrivate :: Bool
  }

instance FromJSON QuestionnaireCreateDTO where
  parseJSON (Object o) = do
    _questionnaireCreateDTOName <- o .: "name"
    _questionnaireCreateDTOPackageId <- o .: "packageId"
    _questionnaireCreateDTOPrivate <- o .: "private"
    return QuestionnaireCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireCreateDTO where
  toJSON QuestionnaireCreateDTO {..} =
    object
      [ "name" .= _questionnaireCreateDTOName
      , "packageId" .= _questionnaireCreateDTOPackageId
      , "private" .= _questionnaireCreateDTOPrivate
      ]
