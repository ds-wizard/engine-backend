module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateSM where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO

instance HasSwagger api => HasSwagger (MultipartForm Mem DocumentTemplateAssetCreateDTO :> api) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      & addParam fileNameField
      & addParam fileField
    where
      fileNameField =
        mempty
          & name .~ "fileName"
          & required ?~ True
          & description ?~ "FileName"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      fileField =
        mempty
          & name .~ "file"
          & required ?~ True
          & description ?~ "File to upload"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ (mempty & type_ ?~ SwaggerFile))
