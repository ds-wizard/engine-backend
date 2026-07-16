module Wizard.S3.KnowledgeModel.KnowledgeModelLocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "knowledge-model-locales"

translationPoFileName = "translation.po"

translationJsonFileName = "translation.json"

retrieveKnowledgeModelLocale :: U.UUID -> String -> AppContextM BS.ByteString
retrieveKnowledgeModelLocale localeUuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName])

putKnowledgeModelLocale :: U.UUID -> String -> BS.ByteString -> AppContextM String
putKnowledgeModelLocale localeUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]) Nothing Nothing

removeKnowledgeModelLocales :: AppContextM ()
removeKnowledgeModelLocales = createRemoveObjectFn folderName

removeKnowledgeModelLocale :: U.UUID -> AppContextM ()
removeKnowledgeModelLocale localeUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString localeUuid])
