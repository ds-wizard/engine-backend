module Wizard.Model.Questionnaire.QuestionnaireDetail where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackageSimple
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireState

data QuestionnaireDetail =
  QuestionnaireDetail
    { _questionnaireDetailUuid :: U.UUID
    , _questionnaireDetailName :: String
    , _questionnaireDetailVisibility :: QuestionnaireVisibility
    , _questionnaireDetailSharing :: QuestionnaireSharing
    , _questionnaireDetailSelectedTagUuids :: [U.UUID]
    , _questionnaireDetailEvents :: [QuestionnaireEvent]
    , _questionnaireDetailState :: QuestionnaireState
    , _questionnaireDetailPackageId :: String
    , _questionnaireDetailPackage :: PackageSimple
    , _questionnaireDetailPermissions :: [QuestionnairePermRecordDTO]
    , _questionnaireDetailCreatedAt :: UTCTime
    , _questionnaireDetailUpdatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq QuestionnaireDetail where
  a == b =
    _questionnaireDetailUuid a == _questionnaireDetailUuid b &&
    _questionnaireDetailName a == _questionnaireDetailName b &&
    _questionnaireDetailVisibility a == _questionnaireDetailVisibility b &&
    _questionnaireDetailSharing a == _questionnaireDetailSharing b &&
    _questionnaireDetailSelectedTagUuids a == _questionnaireDetailSelectedTagUuids b &&
    _questionnaireDetailEvents a == _questionnaireDetailEvents b &&
    _questionnaireDetailState a == _questionnaireDetailState b &&
    _questionnaireDetailPackageId a == _questionnaireDetailPackageId b &&
    _questionnaireDetailPackage a == _questionnaireDetailPackage b &&
    _questionnaireDetailPermissions a == _questionnaireDetailPermissions b
