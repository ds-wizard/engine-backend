module Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO

toList :: DocumentTemplateFile -> DocumentTemplateFileList
toList file =
  DocumentTemplateFileList
    { uuid = file.uuid
    , fileName = file.fileName
    , createdAt = file.createdAt
    , updatedAt = file.updatedAt
    }

fromChangeDTO :: DocumentTemplateFileChangeDTO -> String -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> DocumentTemplateFile
fromChangeDTO dto tmlId uuid appUuid createdAt updatedAt =
  DocumentTemplateFile
    { documentTemplateId = tmlId
    , uuid = uuid
    , fileName = dto.fileName
    , content = dto.content
    , appUuid = appUuid
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
    , appUuid = file.appUuid
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
    , appUuid = file.appUuid
    , createdAt = now
    , updatedAt = now
    }
