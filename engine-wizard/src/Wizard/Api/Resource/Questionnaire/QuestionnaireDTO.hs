module Wizard.Api.Resource.Questionnaire.QuestionnaireDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState

data QuestionnaireDTO =
  QuestionnaireDTO
    { _questionnaireDTOUuid :: U.UUID
    , _questionnaireDTOName :: String
    , _questionnaireDTOLevel :: Int
    , _questionnaireDTOVisibility :: QuestionnaireVisibility
    , _questionnaireDTOState :: QuestionnaireState
    , _questionnaireDTOPackage :: PackageSimpleDTO
    , _questionnaireDTOOwner :: Maybe UserDTO
    , _questionnaireDTOReport :: QuestionnaireReportDTO
    , _questionnaireDTOCreatedAt :: UTCTime
    , _questionnaireDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireDTO where
  a == b =
    _questionnaireDTOUuid a == _questionnaireDTOUuid b &&
    _questionnaireDTOName a == _questionnaireDTOName b &&
    _questionnaireDTOLevel a == _questionnaireDTOLevel b &&
    _questionnaireDTOVisibility a == _questionnaireDTOVisibility b &&
    _questionnaireDTOState a == _questionnaireDTOState b &&
    _questionnaireDTOPackage a == _questionnaireDTOPackage b &&
    _questionnaireDTOOwner a == _questionnaireDTOOwner b && _questionnaireDTOReport a == _questionnaireDTOReport b
