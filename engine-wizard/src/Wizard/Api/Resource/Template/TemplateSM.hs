module Wizard.Api.Resource.Template.TemplateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Template.Template

instance ToSchema Template where
  declareNamedSchema = simpleToSchema' "_template" commonWizardTemplate

instance ToSchema TemplateAllowedPackage where
  declareNamedSchema = simpleToSchema' "_templateAllowedPackage" templateAllowedPackage

instance ToSchema TemplateFormat where
  declareNamedSchema = simpleToSchema' "_templateFormat" templateFormatJson

instance ToSchema TemplateDTO where
  declareNamedSchema = simpleToSchema commonWizardTemplate
