module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Swagger

instance ToSchema KnowledgeModelChangeDTO where
  declareNamedSchema = toSwagger kmChange

kmChange :: KnowledgeModelChangeDTO
kmChange =
  KnowledgeModelChangeDTO
    { packageId = Just $ germanyPackage.pId
    , events = []
    , tagUuids = []
    }
