module Wizard.Api.Resource.Branch.Event.BranchEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Api.Resource.Branch.Event.BranchEventJM ()
import Wizard.Database.Migration.Development.Branch.Data.BranchEvents
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema BranchEventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema AddBranchEventDTO where
  declareNamedSchema = toSwagger branchEvent1'
