module Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO (KnowledgeModelLocaleCreateDTO)

instance HasSwagger api => HasSwagger (MultipartForm Mem KnowledgeModelLocaleCreateDTO :> api) where
  toSwagger _ =
    addParam nameField
      . addParam poContentField
      . addParam jsonContentField
      $ toSwagger (Proxy :: Proxy api)
    where
      nameField =
        Param
          { _paramName = "name"
          , _paramDescription = Just "Name"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      poContentField =
        Param
          { _paramName = "poContent"
          , _paramDescription = Just "PO translation file for the knowledge model"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerFile}
                    }
                )
          }
      jsonContentField =
        Param
          { _paramName = "jsonContent"
          , _paramDescription = Just "JSON translation file for the knowledge model"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    , _paramOtherSchemaParamSchema = mempty {_paramSchemaType = Just SwaggerFile}
                    }
                )
          }
