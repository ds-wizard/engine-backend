module Wizard.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import qualified Shared.Service.Package.PackageMapper as PM_Mapper
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDTO
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

commonWizardTemplateDTO :: TemplateDTO
commonWizardTemplateDTO = toDTO [PM_Mapper.toPackage globalPackageEmpty] commonWizardTemplate

commonWizardTemplateEditedChangeDto :: TemplateChangeDTO
commonWizardTemplateEditedChangeDto = toChangeDTO commonWizardTemplateEdited
