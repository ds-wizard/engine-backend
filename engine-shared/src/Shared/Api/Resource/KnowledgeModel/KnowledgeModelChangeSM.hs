module Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Api.Resource.Event.EventSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Util.Swagger

instance ToSchema KnowledgeModelChangeDTO where
  declareNamedSchema = simpleToSchema kmChange

kmChange :: KnowledgeModelChangeDTO
kmChange =
  KnowledgeModelChangeDTO
    { _knowledgeModelChangeDTOPackageId = Just $ germanyPackage ^. pId
    , _knowledgeModelChangeDTOEvents = []
    , _knowledgeModelChangeDTOTagUuids = []
    }
