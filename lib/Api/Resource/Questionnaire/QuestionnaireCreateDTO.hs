module Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import Control.Monad
import Data.Aeson

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { _questionnaireCreateDTOName :: String
  , _questionnaireCreateDTOLevel :: Int
  , _questionnaireCreateDTOPackageId :: String
  }

instance FromJSON QuestionnaireCreateDTO where
  parseJSON (Object o) = do
    _questionnaireCreateDTOName <- o .: "name"
    _questionnaireCreateDTOLevel <- o .: "level"
    _questionnaireCreateDTOPackageId <- o .: "packageId"
    return QuestionnaireCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireCreateDTO where
  toJSON QuestionnaireCreateDTO {..} =
    object
      [ "name" .= _questionnaireCreateDTOName
      , "level" .= _questionnaireCreateDTOLevel
      , "packageId" .= _questionnaireCreateDTOPackageId
      ]
