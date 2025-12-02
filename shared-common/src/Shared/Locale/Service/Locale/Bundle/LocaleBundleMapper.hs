module Shared.Locale.Service.Locale.Bundle.LocaleBundleMapper where

import Codec.Archive.Zip
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.String
import Shared.Locale.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Locale.Api.Resource.LocaleBundle.LocaleBundleJM ()
import Shared.Locale.Localization.Messages.Public
import Shared.Locale.Model.Locale.Locale

toLocaleArchive :: Locale -> BS.ByteString -> BS.ByteString -> BSL.ByteString
toLocaleArchive lb wizardTranslation mailTranslation = fromArchive $ toLocaleZip lb wizardTranslation mailTranslation

toLocaleZip :: Locale -> BS.ByteString -> BS.ByteString -> Archive
toLocaleZip lb wizardTranslation mailTranslation =
  foldr
    addEntryToArchive
    emptyArchive
    [ toLocaleEntry . toLocaleBundle $ lb
    , toTranslationEntry "wizard.json" wizardTranslation
    , toTranslationEntry "mail.po" mailTranslation
    ]

toLocaleEntry :: LocaleBundleDTO -> Entry
toLocaleEntry lb =
  let localeJson = encode lb
   in toEntry "locale/locale.json" 0 localeJson

toTranslationEntry :: String -> BS.ByteString -> Entry
toTranslationEntry filename content = toEntry (f' "locale/%s" [filename]) 0 (BSL.fromStrict content)

toLocaleBundle :: Locale -> LocaleBundleDTO
toLocaleBundle locale =
  LocaleBundleDTO
    { lId = locale.lId
    , name = locale.name
    , description = locale.description
    , code = locale.code
    , organizationId = locale.organizationId
    , localeId = locale.localeId
    , version = locale.version
    , readme = locale.readme
    , license = locale.license
    , recommendedAppVersion = locale.recommendedAppVersion
    , createdAt = locale.createdAt
    }

fromLocaleArchive :: BSL.ByteString -> Either AppError (LocaleBundleDTO, BS.ByteString, BS.ByteString)
fromLocaleArchive = fromLocaleZip . toArchive

fromLocaleZip :: Archive -> Either AppError (LocaleBundleDTO, BS.ByteString, BS.ByteString)
fromLocaleZip archive = do
  lb <- fromLocaleEntry archive
  wizard <- fromTranslationEntry "wizard.json" lb archive
  mail <- fromTranslationEntry "mail.po" lb archive
  Right (lb, wizard, mail)

fromLocaleEntry :: Archive -> Either AppError LocaleBundleDTO
fromLocaleEntry archive =
  case findEntryByPath "locale/locale.json" archive of
    Just localeEntry ->
      case eitherDecode . fromEntry $ localeEntry of
        Right lb -> Right lb
        Left error -> Left $ UserError (_ERROR_SERVICE_LB__UNABLE_TO_DECODE_LOCALE_JSON error)
    Nothing -> Left $ UserError _ERROR_SERVICE_LB__MISSING_LOCALE_JSON

fromTranslationEntry :: String -> LocaleBundleDTO -> Archive -> Either AppError BS.ByteString
fromTranslationEntry filename lb archive =
  case findEntryByPath (f' "locale/%s" [filename]) archive of
    Just translationEntry -> Right . BSL.toStrict . fromEntry $ translationEntry
    Nothing -> Left $ UserError (_ERROR_SERVICE_LB__MISSING_FILE filename)

fromLocaleBundle :: LocaleBundleDTO -> U.UUID -> Locale
fromLocaleBundle lb tenantUuid =
  Locale
    { lId = lb.lId
    , name = lb.name
    , description = lb.description
    , code = lb.code
    , organizationId = lb.organizationId
    , localeId = lb.localeId
    , version = lb.version
    , defaultLocale = False
    , readme = lb.readme
    , license = lb.license
    , recommendedAppVersion = lb.recommendedAppVersion
    , enabled = False
    , tenantUuid = tenantUuid
    , createdAt = lb.createdAt
    , updatedAt = lb.createdAt
    }
