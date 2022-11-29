module Wizard.Service.Submission.SubmissionMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Submission.Submission
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper

toSubmissionServiceSimpleDTO :: AppConfigSubmissionService -> SubmissionServiceSimpleDTO
toSubmissionServiceSimpleDTO config =
  SubmissionServiceSimpleDTO
    { sId = config.sId
    , name = config.name
    , description = config.description
    }

toDTO :: Submission -> Maybe String -> User -> SubmissionDTO
toDTO sub mServiceName user =
  SubmissionDTO
    { uuid = sub.uuid
    , state = sub.state
    , location = sub.location
    , returnedData = sub.returnedData
    , serviceId = sub.serviceId
    , serviceName = mServiceName
    , documentUuid = sub.documentUuid
    , createdBy = U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion $ user
    , createdAt = sub.createdAt
    , updatedAt = sub.updatedAt
    }

fromCreate :: U.UUID -> String -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> Submission
fromCreate uuid serviceId documentUuid appUuid createdBy now =
  Submission
    { uuid = uuid
    , state = InProgressSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = serviceId
    , documentUuid = documentUuid
    , appUuid = appUuid
    , createdBy = createdBy
    , createdAt = now
    , updatedAt = now
    }
