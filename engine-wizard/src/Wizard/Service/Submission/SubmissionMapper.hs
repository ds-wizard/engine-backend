module Wizard.Service.Submission.SubmissionMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Submission.Submission
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toSubmissionServiceSimpleDTO :: AppConfigSubmissionService -> SubmissionServiceSimpleDTO
toSubmissionServiceSimpleDTO config =
  SubmissionServiceSimpleDTO
    { _submissionServiceSimpleDTOId = config ^. sId
    , _submissionServiceSimpleDTOName = config ^. name
    , _submissionServiceSimpleDTODescription = config ^. description
    }

toDTO :: Submission -> Maybe String -> User -> SubmissionDTO
toDTO sub mServiceName user =
  SubmissionDTO
    { _submissionDTOUuid = sub ^. uuid
    , _submissionDTOState = sub ^. state
    , _submissionDTOLocation = sub ^. location
    , _submissionDTOReturnedData = sub ^. returnedData
    , _submissionDTOServiceId = sub ^. serviceId
    , _submissionDTOServiceName = mServiceName
    , _submissionDTODocumentUuid = sub ^. documentUuid
    , _submissionDTOCreatedBy = U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion $ user
    , _submissionDTOCreatedAt = sub ^. createdAt
    , _submissionDTOUpdatedAt = sub ^. updatedAt
    }

fromCreate :: U.UUID -> String -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> Submission
fromCreate uuid serviceId documentUuid appUuid createdBy now =
  Submission
    { _submissionUuid = uuid
    , _submissionState = InProgressSubmissionState
    , _submissionLocation = Nothing
    , _submissionReturnedData = Nothing
    , _submissionServiceId = serviceId
    , _submissionDocumentUuid = documentUuid
    , _submissionAppUuid = appUuid
    , _submissionCreatedBy = createdBy
    , _submissionCreatedAt = now
    , _submissionUpdatedAt = now
    }
