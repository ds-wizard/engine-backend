module Shared.Common.Api.Resource.Common.FileSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger.Internal

import Shared.Common.Api.Resource.Common.FileDTO

instance HasSwagger api => HasSwagger (MultipartForm Mem FileDTO :> api) where
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
