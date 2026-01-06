module Wizard.Service.Project.Migration.ProjectMigrationMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Migration.ProjectMigration

toDTO :: ProjectDetailQuestionnaireDTO -> ProjectDetailQuestionnaireDTO -> [U.UUID] -> U.UUID -> ProjectMigrationDTO
toDTO oldProject newProject projectUuids tenantUuid =
  ProjectMigrationDTO
    { oldProject = oldProject
    , newProject = newProject
    , resolvedQuestionUuids = projectUuids
    , tenantUuid = tenantUuid
    }

fromCreateDTO :: U.UUID -> U.UUID -> U.UUID -> ProjectMigration
fromCreateDTO oldProjectUuid newProjectUuid tenantUuid =
  ProjectMigration
    { oldProjectUuid = oldProjectUuid
    , newProjectUuid = newProjectUuid
    , resolvedQuestionUuids = []
    , tenantUuid = tenantUuid
    }

fromChangeDTO :: ProjectMigrationChangeDTO -> ProjectMigrationDTO -> ProjectMigration
fromChangeDTO changeDto ms =
  ProjectMigration
    { oldProjectUuid = ms.oldProject.uuid
    , newProjectUuid = ms.newProject.uuid
    , resolvedQuestionUuids = changeDto.resolvedQuestionUuids
    , tenantUuid = ms.tenantUuid
    }

toProjectPhaseEvent :: U.UUID -> Maybe U.UUID -> U.UUID -> U.UUID -> Maybe UserDTO -> UTCTime -> ProjectEvent
toProjectPhaseEvent phaseEventUuid kmPhaseUuid projectUuid tenantUuid mCurrentUserUuid now =
  SetPhaseEvent' $
    SetPhaseEvent
      { uuid = phaseEventUuid
      , phaseUuid = kmPhaseUuid
      , projectUuid = projectUuid
      , tenantUuid = tenantUuid
      , createdBy = fmap (.uuid) mCurrentUserUuid
      , createdAt = now
      }
