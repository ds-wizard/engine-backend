module Wizard.Api.Resource.PackageBundle.PackageBundleFileSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.PackageBundle.PackageBundleFileDTO hiding (name)

instance HasSwagger api => HasSwagger (MultipartForm Mem PackageBundleFileDTO :> api) where
  toSwagger _ = addParam param (toSwagger (Proxy :: Proxy api))
    where
      param =
        Param
          { _paramName = "file"
          , _paramDescription = Just "File to upload"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerFile}
                    }
                )
          }
