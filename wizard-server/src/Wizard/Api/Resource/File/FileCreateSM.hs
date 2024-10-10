module Wizard.Api.Resource.File.FileCreateSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.File.FileCreateDTO

instance HasSwagger api => HasSwagger (MultipartForm Mem FileCreateDTO :> api) where
  toSwagger _ = addParam fileNameField . addParam fileField $ toSwagger (Proxy :: Proxy api)
    where
      fileNameField =
        Param
          { _paramName = "fileName"
          , _paramDescription = Just "FileName"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      fileField =
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
