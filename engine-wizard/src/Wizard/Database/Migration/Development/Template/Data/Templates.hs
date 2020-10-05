module Wizard.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((^.))

import LensesConfig
import qualified Registry.Database.Migration.Development.Template.Data.Templates as R_Templates
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Service.Template.TemplateMapper

templateFileDefaultHtmlEditedChangeDto :: TemplateFileChangeDTO
templateFileDefaultHtmlEditedChangeDto =
  TemplateFileChangeDTO
    { _templateFileChangeDTOFileName = templateFileDefaultHtmlEdited ^. fileName
    , _templateFileChangeDTOContent = templateFileDefaultHtmlEdited ^. content
    }

templateFileNewFileChangeDto :: TemplateFileChangeDTO
templateFileNewFileChangeDto =
  TemplateFileChangeDTO
    { _templateFileChangeDTOFileName = templateFileNewFile ^. fileName
    , _templateFileChangeDTOContent = templateFileNewFile ^. content
    }

commonWizardTemplateSimpleDTO :: TemplateSimpleDTO
commonWizardTemplateSimpleDTO =
  toSimpleDTO'
    [R_Templates.commonWizardTemplateSimpleDTO]
    [orgGlobalSimple]
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]
    commonWizardTemplate

commonWizardTemplateDetailDTO :: TemplateDetailDTO
commonWizardTemplateDetailDTO =
  toDetailDTO
    commonWizardTemplate
    [R_Templates.commonWizardTemplateSimpleDTO]
    [orgGlobalSimple]
    ["1.0.0"]
    "https://registry-test.ds-wizard.org/templates/global:questionnaire-report:1.0.0"
    [SPM.toPackage globalPackage, SPM.toPackage netherlandsPackageV2]

commonWizardTemplateEditedChangeDto :: TemplateChangeDTO
commonWizardTemplateEditedChangeDto = toChangeDTO commonWizardTemplateEdited
