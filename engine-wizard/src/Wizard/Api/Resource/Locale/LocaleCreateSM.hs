module Wizard.Api.Resource.Locale.LocaleCreateSM where

import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.Locale.LocaleCreateDTO (LocaleCreateDTO)

instance HasSwagger api => HasSwagger (MultipartForm Mem LocaleCreateDTO :> api) where
  toSwagger _ =
    addParam nameField
      . addParam descriptionField
      . addParam codeField
      . addParam localeIdField
      . addParam versionField
      . addParam licenseField
      . addParam readmeField
      . addParam recommendedAppVersionField
      . addParam fileField
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
      descriptionField =
        Param
          { _paramName = "description"
          , _paramDescription = Just "Description"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      codeField =
        Param
          { _paramName = "code"
          , _paramDescription = Just "Code"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      localeIdField =
        Param
          { _paramName = "localeId"
          , _paramDescription = Just "Locale ID"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      versionField =
        Param
          { _paramName = "version"
          , _paramDescription = Just "Version"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      licenseField =
        Param
          { _paramName = "license"
          , _paramDescription = Just "License"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      readmeField =
        Param
          { _paramName = "readme"
          , _paramDescription = Just "Readme"
          , _paramRequired = Just True
          , _paramSchema =
              ParamOther
                ( mempty
                    { _paramOtherSchemaIn = ParamFormData
                    }
                )
          }
      recommendedAppVersionField =
        Param
          { _paramName = "recommendedAppVersion"
          , _paramDescription = Just "Recommended App Version"
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
