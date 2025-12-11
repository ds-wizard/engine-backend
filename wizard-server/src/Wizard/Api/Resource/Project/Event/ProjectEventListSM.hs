module Wizard.Api.Resource.Project.Event.ProjectEventListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Event.ProjectEventListJM ()
import Wizard.Api.Resource.Project.ProjectReplySM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.Project
import Wizard.Service.Project.Event.ProjectEventMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema ProjectEventList where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventList where
  declareNamedSchema = toSwagger (toSetReplyEventList (sre_rQ1 project1.uuid) (Just userAlbert))

instance ToSchema ClearReplyEventList where
  declareNamedSchema = toSwagger (toClearReplyEventList (cre_rQ1 project1.uuid) (Just userAlbert))

instance ToSchema SetPhaseEventList where
  declareNamedSchema = toSwagger (toSetPhaseEventList (sphse_1 project1.uuid) (Just userAlbert))

instance ToSchema SetLabelsEventList where
  declareNamedSchema = toSwagger (toSetLabelsEventList (slble_rQ2 project1.uuid) (Just userAlbert))
