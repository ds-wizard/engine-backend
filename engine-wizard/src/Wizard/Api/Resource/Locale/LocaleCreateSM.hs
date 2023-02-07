module Wizard.Api.Resource.Locale.LocaleCreateSM where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Multipart
import Servant.Swagger
import Servant.Swagger.Internal

import Wizard.Api.Resource.Locale.LocaleCreateDTO (LocaleCreateDTO)

instance HasSwagger api => HasSwagger (MultipartForm Mem LocaleCreateDTO :> api) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      & addParam fileField
      & addParam recommendedAppVersionField
      & addParam readmeField
      & addParam licenseField
      & addParam versionField
      & addParam localeIdField
      & addParam codeField
      & addParam descriptionField
      & addParam nameField
    where
      nameField =
        mempty
          & name .~ "name"
          & required ?~ True
          & description ?~ "Name"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      descriptionField =
        mempty
          & name .~ "description"
          & required ?~ True
          & description ?~ "Description"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      codeField =
        mempty
          & name .~ "code"
          & required ?~ True
          & description ?~ "Code"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      localeIdField =
        mempty
          & name .~ "localeId"
          & required ?~ True
          & description ?~ "Locale ID"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      versionField =
        mempty
          & name .~ "version"
          & required ?~ True
          & description ?~ "Version"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      licenseField =
        mempty
          & name .~ "license"
          & required ?~ True
          & description ?~ "License"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      readmeField =
        mempty
          & name .~ "readme"
          & required ?~ True
          & description ?~ "Readme"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      recommendedAppVersionField =
        mempty
          & name .~ "recommendedAppVersion"
          & required ?~ True
          & description ?~ "Recommended App Version"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ mempty)
      fileField =
        mempty
          & name .~ "file"
          & required ?~ True
          & description ?~ "File to upload"
          & schema .~ ParamOther (mempty & in_ .~ ParamFormData & paramSchema .~ (mempty & type_ ?~ SwaggerFile))
