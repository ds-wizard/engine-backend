module Wizard.Api.Resource.Branch.Event.BranchEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Api.Resource.Branch.Event.BranchEventJM ()
import Wizard.Database.Migration.Development.Branch.Data.BranchEvents

instance ToSchema BranchEventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema AddBranchEventDTO where
  declareNamedSchema = simpleToSchema' "_addBranchEventDTO" branchEvent1'
