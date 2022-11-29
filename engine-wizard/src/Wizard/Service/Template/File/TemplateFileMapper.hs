module Wizard.Service.Template.File.TemplateFileMapper where

import qualified Data.UUID as U

import Shared.Model.Template.Template
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO

fromChangeDTO :: TemplateFileChangeDTO -> String -> U.UUID -> U.UUID -> TemplateFile
fromChangeDTO dto tmlId uuid appUuid =
  TemplateFile
    { templateId = tmlId
    , uuid = uuid
    , fileName = dto.fileName
    , content = dto.content
    , appUuid = appUuid
    }
