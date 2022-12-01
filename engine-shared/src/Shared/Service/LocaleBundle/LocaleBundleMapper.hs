module Shared.Service.LocaleBundle.LocaleBundleMapper where

import Codec.Archive.Zip
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Api.Resource.LocaleBundle.LocaleBundleJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale

toLocaleArchive :: Locale -> BS.ByteString -> BSL.ByteString
toLocaleArchive lb = fromArchive . toLocaleZip lb

toLocaleZip :: Locale -> BS.ByteString -> Archive
toLocaleZip lb content =
  foldr addEntryToArchive emptyArchive [toLocaleEntry . toLocaleBundle $ lb, toTranslationEntry content]

toLocaleEntry :: LocaleBundleDTO -> Entry
toLocaleEntry lb =
  let localeJson = encode lb
   in toEntry "locale/locale.json" 0 localeJson

toTranslationEntry :: BS.ByteString -> Entry
toTranslationEntry content = toEntry "locale/translation.json" 0 (BSL.fromStrict content)

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

fromLocaleArchive :: BSL.ByteString -> Either AppError (LocaleBundleDTO, BS.ByteString)
fromLocaleArchive = fromLocaleZip . toArchive

fromLocaleZip :: Archive -> Either AppError (LocaleBundleDTO, BS.ByteString)
fromLocaleZip archive = do
  lb <- fromLocaleEntry archive
  content <- fromTranslationEntry lb archive
  Right (lb, content)

fromLocaleEntry :: Archive -> Either AppError LocaleBundleDTO
fromLocaleEntry archive =
  case findEntryByPath "locale/locale.json" archive of
    Just localeEntry ->
      case eitherDecode . fromEntry $ localeEntry of
        Right lb -> Right lb
        Left error -> Left $ UserError (_ERROR_SERVICE_LB__UNABLE_TO_DECODE_LOCALE_JSON error)
    Nothing -> Left $ UserError _ERROR_SERVICE_LB__MISSING_LOCALE_JSON

fromTranslationEntry :: LocaleBundleDTO -> Archive -> Either AppError BS.ByteString
fromTranslationEntry lb archive =
  case findEntryByPath "locale/translation.json" archive of
    Just translationEntry -> Right . BSL.toStrict . fromEntry $ translationEntry
    Nothing -> Left $ UserError (_ERROR_SERVICE_LB__MISSING_FILE "translation.json")

fromLocaleBundle :: LocaleBundleDTO -> U.UUID -> Locale
fromLocaleBundle lb appUuid =
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
    , appUuid = appUuid
    , createdAt = lb.createdAt
    , updatedAt = lb.createdAt
    }
