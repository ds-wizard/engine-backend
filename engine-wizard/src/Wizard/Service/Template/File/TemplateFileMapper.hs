module Wizard.Service.Template.File.TemplateFileMapper where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Template.Template
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO

fromChangeDTO :: TemplateFileChangeDTO -> U.UUID -> TemplateFile
fromChangeDTO dto uuid =
  TemplateFile
    {_templateFileUuid = uuid, _templateFileFileName = dto ^. fileName, _templateFileContent = dto ^. content}
