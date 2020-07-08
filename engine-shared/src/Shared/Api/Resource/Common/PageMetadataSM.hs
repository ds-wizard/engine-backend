module Shared.Api.Resource.Common.PageMetadataSM where

import Data.Swagger
import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Model.Common.PageMetadata
import Shared.Util.Swagger

instance ToSchema PageMetadata where
  declareNamedSchema = simpleToSchema' "_pageMetadata" pageMetadata
