module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToSchema KnowledgeModelChangeDTO where
  declareNamedSchema = toSwagger kmChange

kmChange :: KnowledgeModelChangeDTO
kmChange =
  KnowledgeModelChangeDTO
    { knowledgeModelPackageId = Just $ germanyKmPackage.pId
    , events = []
    , tagUuids = []
    }
