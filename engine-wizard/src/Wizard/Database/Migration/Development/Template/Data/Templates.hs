module Wizard.Database.Migration.Development.Template.Data.Templates where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import qualified Shared.Service.Package.PackageMapper as PM_Mapper
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.Template.TemplateMapper
import Wizard.Service.Template.TemplateUtil

getCommonWizardTemplate :: AppContextM Template
getCommonWizardTemplate = do
  folder <- getTemplateFolder
  html <- liftIO $ BSL.readFile (folder ++ "/default.html.j2")
  let updatedTemplateFileDefaultHtml = templateFileDefaultHtml & content .~ BSL.unpack html
  css <- liftIO $ BSL.readFile (folder ++ "/default.css")
  let updatedTemplateFileDefaultCss = templateFileDefaultCss & content .~ BSL.unpack css
  return $ commonWizardTemplate & files .~ [updatedTemplateFileDefaultHtml, updatedTemplateFileDefaultCss]

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
