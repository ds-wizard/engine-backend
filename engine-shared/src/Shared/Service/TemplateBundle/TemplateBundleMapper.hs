module Shared.Service.TemplateBundle.TemplateBundleMapper where

import Codec.Archive.Zip
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Api.Resource.TemplateBundle.TemplateBundleDTO
import Shared.Api.Resource.TemplateBundle.TemplateBundleJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Shared.Service.Template.TemplateMapper

toTemplateArchive :: TemplateBundleDTO -> [(TemplateAsset, BS.ByteString)] -> BSL.ByteString
toTemplateArchive tb = fromArchive . toTemplateZip tb

toTemplateZip :: TemplateBundleDTO -> [(TemplateAsset, BS.ByteString)] -> Archive
toTemplateZip tb assets =
  let templateEntry = toTemplateEntry tb
      assetEntries = fmap toAssetEntry assets
   in foldr addEntryToArchive emptyArchive (templateEntry : assetEntries)

toTemplateEntry :: TemplateBundleDTO -> Entry
toTemplateEntry tb =
  let templateJson = encode tb
   in toEntry "template/template.json" 0 templateJson

toAssetEntry :: (TemplateAsset, BS.ByteString) -> Entry
toAssetEntry (asset, content) = toEntry ("template/assets/" ++ asset.fileName) 0 (BSL.fromStrict content)

toTemplateBundle :: Template -> [TemplateFile] -> [TemplateAsset] -> TemplateBundleDTO
toTemplateBundle template files assets =
  TemplateBundleDTO
    { tId = template.tId
    , name = template.name
    , organizationId = template.organizationId
    , templateId = template.templateId
    , version = template.version
    , metamodelVersion = template.metamodelVersion
    , description = template.description
    , readme = template.readme
    , license = template.license
    , allowedPackages = template.allowedPackages
    , recommendedPackageId = template.recommendedPackageId
    , formats = template.formats
    , files = fmap toFileDTO files
    , assets = fmap toAssetDTO assets
    , createdAt = template.createdAt
    }

fromTemplateArchive :: BSL.ByteString -> Either AppError (TemplateBundleDTO, [(TemplateAssetDTO, BS.ByteString)])
fromTemplateArchive = fromTemplateZip . toArchive

fromTemplateZip :: Archive -> Either AppError (TemplateBundleDTO, [(TemplateAssetDTO, BS.ByteString)])
fromTemplateZip archive = do
  tb <- fromTemplateEntry archive
  assets <- traverse (fromAssetEntry tb archive) tb.assets
  Right (tb, assets)

fromTemplateEntry :: Archive -> Either AppError TemplateBundleDTO
fromTemplateEntry archive =
  case findEntryByPath "template/template.json" archive of
    Just templateEntry ->
      case eitherDecode . fromEntry $ templateEntry of
        Right tb -> Right tb
        Left error -> Left $ UserError (_ERROR_SERVICE_TB__UNABLE_TO_DECODE_TEMPLATE_JSON error)
    Nothing -> Left $ UserError _ERROR_SERVICE_TB__MISSING_TEMPLATE_JSON

fromAssetEntry :: TemplateBundleDTO -> Archive -> TemplateAssetDTO -> Either AppError (TemplateAssetDTO, BS.ByteString)
fromAssetEntry tb archive asset =
  case findEntryByPath ("template/assets/" ++ asset.fileName) archive of
    Just assetEntry -> Right (asset, BSL.toStrict . fromEntry $ assetEntry)
    Nothing -> Left $ UserError (_ERROR_SERVICE_TB__MISSING_ASSET asset.fileName)

fromTemplateBundle :: TemplateBundleDTO -> U.UUID -> Template
fromTemplateBundle tb appUuid =
  Template
    { tId = tb.tId
    , name = tb.name
    , organizationId = tb.organizationId
    , templateId = tb.templateId
    , version = tb.version
    , metamodelVersion = tb.metamodelVersion
    , description = tb.description
    , readme = tb.readme
    , license = tb.license
    , allowedPackages = tb.allowedPackages
    , recommendedPackageId = tb.recommendedPackageId
    , formats = tb.formats
    , appUuid = appUuid
    , createdAt = tb.createdAt
    }
