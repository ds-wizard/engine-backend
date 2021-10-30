module Wizard.Service.Template.File.TemplateFileMapper where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Template.Template
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO

fromChangeDTO :: TemplateFileChangeDTO -> String -> U.UUID -> U.UUID -> TemplateFile
fromChangeDTO dto tmlId uuid appUuid =
  TemplateFile
    { _templateFileTemplateId = tmlId
    , _templateFileUuid = uuid
    , _templateFileFileName = dto ^. fileName
    , _templateFileContent = dto ^. content
    , _templateFileAppUuid = appUuid
    }
