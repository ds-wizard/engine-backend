module Api.Resource.Questionnaire.QuestionnaireDTO where

import Data.Time
import qualified Data.UUID as U

import Api.Resource.Package.PackageDTO
import Model.Questionnaire.Questionnaire

data QuestionnaireDTO = QuestionnaireDTO
  { _questionnaireDTOUuid :: U.UUID
  , _questionnaireDTOName :: String
  , _questionnaireDTOLevel :: Int
  , _questionnaireDTOAccessibility :: QuestionnaireAccessibility
  , _questionnaireDTOPackage :: PackageDTO
  , _questionnaireDTOOwnerUuid :: Maybe U.UUID
  , _questionnaireDTOCreatedAt :: UTCTime
  , _questionnaireDTOUpdatedAt :: UTCTime
  } deriving (Show)

instance Eq QuestionnaireDTO where
  a == b =
    _questionnaireDTOUuid a == _questionnaireDTOUuid b &&
    _questionnaireDTOName a == _questionnaireDTOName b &&
    _questionnaireDTOLevel a == _questionnaireDTOLevel b &&
    _questionnaireDTOAccessibility a == _questionnaireDTOAccessibility b &&
    _questionnaireDTOPackage a == _questionnaireDTOPackage b &&
    _questionnaireDTOOwnerUuid a == _questionnaireDTOOwnerUuid b
