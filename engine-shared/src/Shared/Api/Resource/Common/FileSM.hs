module Shared.Api.Resource.Common.FileSM where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Shared.Api.Resource.Common.FileDTO

instance HasSwagger api => HasSwagger (MultipartForm Mem FileDTO :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) & addParam param
    where
      param =
        mempty
          & name .~ "file"
          & required ?~ True
          & description ?~ "File to upload"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ (mempty & type_ ?~ SwaggerFile))
