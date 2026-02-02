module Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO

toList :: DocumentTemplateFile -> DocumentTemplateFileList
toList file =
  DocumentTemplateFileList
    { uuid = file.uuid
    , fileName = file.fileName
    , createdAt = file.createdAt
    , updatedAt = file.updatedAt
    }

fromChangeDTO :: DocumentTemplateFileChangeDTO -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFile
fromChangeDTO dto documentTemplateUuid uuid tenantUuid createdAt updatedAt =
  DocumentTemplateFile
    { documentTemplateUuid = documentTemplateUuid
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
    { documentTemplateUuid = file.documentTemplateUuid
    , uuid = file.uuid
    , fileName = file.fileName
    , content = content
    , tenantUuid = file.tenantUuid
    , createdAt = file.createdAt
    , updatedAt = now
    }

fromDuplicateDTO :: DocumentTemplateFile -> U.UUID -> U.UUID -> UTCTime -> DocumentTemplateFile
fromDuplicateDTO file documentTemplateUuid uuid now =
  DocumentTemplateFile
    { documentTemplateUuid = documentTemplateUuid
    , uuid = uuid
    , fileName = file.fileName
    , content = file.content
    , tenantUuid = file.tenantUuid
    , createdAt = now
    , updatedAt = now
    }
