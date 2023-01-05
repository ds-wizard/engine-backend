module Wizard.Api.Resource.PackageBundle.PackageBundleFileSM where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.PackageBundle.PackageBundleFileDTO hiding (name)

instance HasSwagger api => HasSwagger (MultipartForm Mem PackageBundleFileDTO :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) & addParam param
    where
      param =
        mempty
          & name .~ "file"
          & required ?~ True
          & description ?~ "File to upload"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ (mempty & type_ ?~ SwaggerFile))
