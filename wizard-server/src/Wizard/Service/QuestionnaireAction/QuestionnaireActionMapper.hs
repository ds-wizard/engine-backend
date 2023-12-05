module Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper where

import Data.Time

import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Model.QuestionnaireAction.QuestionnaireAction

toDTO :: QuestionnaireAction -> QuestionnaireActionDTO
toDTO action =
  QuestionnaireActionDTO
    { qaId = action.qaId
    , name = action.name
    , description = action.description
    , url = action.url
    , enabled = action.enabled
    , createdAt = action.createdAt
    , updatedAt = action.updatedAt
    }

toChangeDTO :: QuestionnaireAction -> QuestionnaireActionChangeDTO
toChangeDTO action = QuestionnaireActionChangeDTO {enabled = action.enabled}

fromChangeDTO :: QuestionnaireAction -> QuestionnaireActionChangeDTO -> UTCTime -> QuestionnaireAction
fromChangeDTO action reqDto now =
  action {enabled = reqDto.enabled, updatedAt = now}
