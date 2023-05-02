module WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

instance ToSchema KnowledgeModelChangeDTO where
  declareNamedSchema = toSwagger kmChange

kmChange :: KnowledgeModelChangeDTO
kmChange =
  KnowledgeModelChangeDTO
    { packageId = Just $ germanyPackage.pId
    , events = []
    , tagUuids = []
    }
