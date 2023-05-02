module Shared.Common.Api.Resource.Common.PageMetadataSM where

import Data.Swagger
import Shared.Common.Api.Resource.Common.PageMetadataJM ()
import Shared.Common.Database.Migration.Development.Common.Data.Pages
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Util.Swagger

instance ToSchema PageMetadata where
  declareNamedSchema = toSwagger pageMetadata
