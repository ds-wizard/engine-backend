module Wizard.Database.Migration.Development.Submission.Data.Submissions where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Util.Uuid
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Document.Document
import Wizard.Model.Submission.Submission
import Wizard.Model.User.User
import Wizard.Service.Submission.SubmissionMapper

submissionCreate :: SubmissionCreateDTO
submissionCreate = SubmissionCreateDTO {serviceId = defaultSubmissionService.sId}

submission1 :: Submission
submission1 =
  Submission
    { uuid = u' "f9e14cfb-4435-45a5-8a10-2ea5dbcb92b0"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = doc1.uuid
    , appUuid = defaultApp.uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission1Dto :: SubmissionDTO
submission1Dto = toDTO submission1 (Just defaultSubmissionService.name) userAlbert

submission2 :: Submission
submission2 =
  Submission
    { uuid = u' "bca23893-1d44-4980-a3ba-b50c6b8df342"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = doc1.uuid
    , appUuid = defaultApp.uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission2Dto :: SubmissionDTO
submission2Dto = toDTO submission1 (Just defaultSubmissionService.name) userAlbert

differentSubmission :: Submission
differentSubmission =
  Submission
    { uuid = u' "de51c280-2a6f-49d0-b3de-405401ffba74"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = differentDoc.uuid
    , appUuid = differentApp.uuid
    , createdBy = userCharles.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
