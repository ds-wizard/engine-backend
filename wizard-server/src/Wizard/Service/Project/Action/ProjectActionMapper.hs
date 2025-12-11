module Wizard.Service.Project.Action.ProjectActionMapper where

import Data.Time

import Wizard.Api.Resource.Project.Action.ProjectActionChangeDTO
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Model.Project.Action.ProjectAction

toDTO :: ProjectAction -> ProjectActionDTO
toDTO action =
  ProjectActionDTO
    { paId = action.paId
    , name = action.name
    , description = action.description
    , url = action.url
    , enabled = action.enabled
    , createdAt = action.createdAt
    , updatedAt = action.updatedAt
    }

toChangeDTO :: ProjectAction -> ProjectActionChangeDTO
toChangeDTO action = ProjectActionChangeDTO {enabled = action.enabled}

fromChangeDTO :: ProjectAction -> ProjectActionChangeDTO -> UTCTime -> ProjectAction
fromChangeDTO action reqDto now =
  action {enabled = reqDto.enabled, updatedAt = now}
