module Shared.Api.Resource.Multipart.MultiPartSM where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

instance HasSwagger api =>
         HasSwagger (MultipartForm a b
                     :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) & addParam param
    where
      param =
        mempty & name .~ "file" & required ?~ True & description ?~ "File to upload" &
        schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ (mempty & type_ ?~ SwaggerFile))
