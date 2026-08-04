module Wizard.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper

czechGlobalKmLocale :: KnowledgeModelLocale
czechGlobalKmLocale =
  KnowledgeModelLocale
    { uuid = u' "9c7ea219-dd28-49fc-9c9a-c7c00bb83cd9"
    , name = "Czech"
    , code = "cs"
    , knowledgeModelPackageUuid = globalKmPackage.uuid
    , tenantUuid = globalKmPackage.tenantUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

czechGlobalKmLocaleList :: KnowledgeModelLocaleList
czechGlobalKmLocaleList = toList czechGlobalKmLocale

czechNetherlandsKmLocale :: KnowledgeModelLocale
czechNetherlandsKmLocale =
  KnowledgeModelLocale
    { uuid = u' "e2c30514-fd8e-4feb-baf5-b57ac1a37a3c"
    , name = "Czech"
    , code = "cs"
    , knowledgeModelPackageUuid = netherlandsKmPackage.uuid
    , tenantUuid = netherlandsKmPackage.tenantUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

czechNetherlandsKmLocaleList :: KnowledgeModelLocaleList
czechNetherlandsKmLocaleList = toList czechNetherlandsKmLocale

czechGlobalKmLocaleCreate :: KnowledgeModelLocaleCreateDTO
czechGlobalKmLocaleCreate =
  KnowledgeModelLocaleCreateDTO
    { name = czechGlobalKmLocale.name
    , poContent = czechPoContent
    , jsonContent = czechJsonContent
    }

czechPoContent :: BS.ByteString
czechPoContent =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Language: cs\\n\""
    , "\"Plural-Forms: nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;\\n\""
    , ""
    , "msgid \"Chapter 1\""
    , "msgstr \"Kapitola 1\""
    ]

czechJsonContent :: BS.ByteString
czechJsonContent = BS.pack "{\"Chapter 1\": \"Kapitola 1\"}"

poContentWithoutLanguage :: BS.ByteString
poContentWithoutLanguage =
  BS.pack . unlines $
    [ "msgid \"\""
    , "msgstr \"\""
    , "\"Content-Type: text/plain; charset=UTF-8\\n\""
    ]
