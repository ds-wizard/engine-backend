module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO

instance HasSwagger api => HasSwagger (MultipartForm Mem DocumentTemplateAssetCreateDTO :> api) where
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
