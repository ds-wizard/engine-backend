module Wizard.Api.Resource.Project.Event.ProjectEventChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Wizard.Api.Resource.Project.Event.ProjectEventJM ()
import Wizard.Api.Resource.Project.ProjectReplySM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Project
import Wizard.Service.Project.Event.ProjectEventMapper
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema ProjectEventChangeDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetReplyEventChangeDTO (sre_rQ1 project1.uuid))

instance ToSchema ClearReplyEventChangeDTO where
  declareNamedSchema =
    toSwagger (toClearReplyEventChangeDTO (cre_rQ1 project1.uuid))

instance ToSchema SetPhaseEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetPhaseEventChangeDTO (sphse_1 project1.uuid))

instance ToSchema SetLabelsEventChangeDTO where
  declareNamedSchema =
    toSwagger (toSetLabelsEventChangeDTO (slble_rQ2 project1.uuid))

instance ToSchema ResolveCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger rtche_rQ1_t1

instance ToSchema ReopenCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger otche_rQ1_t1

instance ToSchema AssignCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger asche_rQ1_t1

instance ToSchema DeleteCommentThreadEventChangeDTO where
  declareNamedSchema = toSwagger dtche_rQ1_t1

instance ToSchema AddCommentEventChangeDTO where
  declareNamedSchema = toSwagger acche_rQ2_t1_1

instance ToSchema EditCommentEventChangeDTO where
  declareNamedSchema = toSwagger ecche_rQ1_t1_1

instance ToSchema DeleteCommentEventChangeDTO where
  declareNamedSchema = toSwagger dcche_rQ1_t1_1
