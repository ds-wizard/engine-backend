module Wizard.Database.Migration.Development.Submission.Data.Submissions where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Submission.Submission
import Wizard.Service.Submission.SubmissionMapper

submissionCreate :: SubmissionCreateDTO
submissionCreate = SubmissionCreateDTO {_submissionCreateDTOServiceId = defaultSubmissionService ^. sId}

submission1 :: Submission
submission1 =
  Submission
    { _submissionUuid = u' "f9e14cfb-4435-45a5-8a10-2ea5dbcb92b0"
    , _submissionState = DoneSubmissionState
    , _submissionLocation = Nothing
    , _submissionReturnedData = Nothing
    , _submissionServiceId = defaultSubmissionService ^. sId
    , _submissionDocumentUuid = doc1 ^. uuid
    , _submissionCreatedBy = userAlbert ^. uuid
    , _submissionCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _submissionUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission1Dto :: SubmissionDTO
submission1Dto = toDTO submission1 (Just $ defaultSubmissionService ^. name) userAlbert

submission2 :: Submission
submission2 =
  Submission
    { _submissionUuid = u' "bca23893-1d44-4980-a3ba-b50c6b8df342"
    , _submissionState = DoneSubmissionState
    , _submissionLocation = Nothing
    , _submissionReturnedData = Nothing
    , _submissionServiceId = defaultSubmissionService ^. sId
    , _submissionDocumentUuid = doc1 ^. uuid
    , _submissionCreatedBy = userAlbert ^. uuid
    , _submissionCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _submissionUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission2Dto :: SubmissionDTO
submission2Dto = toDTO submission1 (Just $ defaultSubmissionService ^. name) userAlbert
