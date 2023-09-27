module Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

toList :: DocumentTemplateFile -> DocumentTemplateFileList
toList file =
  DocumentTemplateFileList
    { uuid = file.uuid
    , fileName = file.fileName
    , createdAt = file.createdAt
    , updatedAt = file.updatedAt
    }

fromChangeDTO :: DocumentTemplateFileChangeDTO -> String -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFile
fromChangeDTO dto tmlId uuid tenantUuid createdAt updatedAt =
  DocumentTemplateFile
    { documentTemplateId = tmlId
    , uuid = uuid
    , fileName = dto.fileName
    , content = dto.content
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromContentChangeDTO :: DocumentTemplateFile -> String -> UTCTime -> DocumentTemplateFile
fromContentChangeDTO file content now =
  DocumentTemplateFile
    { documentTemplateId = file.documentTemplateId
    , uuid = file.uuid
    , fileName = file.fileName
    , content = content
    , tenantUuid = file.tenantUuid
    , createdAt = file.createdAt
    , updatedAt = now
    }

fromDuplicateDTO :: DocumentTemplateFile -> String -> U.UUID -> UTCTime -> DocumentTemplateFile
fromDuplicateDTO file documentTemplateId uuid now =
  DocumentTemplateFile
    { documentTemplateId = documentTemplateId
    , uuid = uuid
    , fileName = file.fileName
    , content = file.content
    , tenantUuid = file.tenantUuid
    , createdAt = now
    , updatedAt = now
    }
