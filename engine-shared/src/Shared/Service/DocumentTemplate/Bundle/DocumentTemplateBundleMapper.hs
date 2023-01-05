module Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper where

import Codec.Archive.Zip
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM ()
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.DocumentTemplateMapper

toDocumentTemplateArchive :: DocumentTemplateBundleDTO -> [(DocumentTemplateAsset, BS.ByteString)] -> BSL.ByteString
toDocumentTemplateArchive tb = fromArchive . toDocumentTemplateZip tb

toDocumentTemplateZip :: DocumentTemplateBundleDTO -> [(DocumentTemplateAsset, BS.ByteString)] -> Archive
toDocumentTemplateZip tb assets =
  let templateEntry = toDocumentTemplateEntry tb
      assetEntries = fmap toAssetEntry assets
   in foldr addEntryToArchive emptyArchive (templateEntry : assetEntries)

toDocumentTemplateEntry :: DocumentTemplateBundleDTO -> Entry
toDocumentTemplateEntry tb =
  let templateJson = encode tb
   in toEntry "template/template.json" 0 templateJson

toAssetEntry :: (DocumentTemplateAsset, BS.ByteString) -> Entry
toAssetEntry (asset, content) = toEntry ("template/assets/" ++ asset.fileName) 0 (BSL.fromStrict content)

toBundle :: DocumentTemplate -> [DocumentTemplateFile] -> [DocumentTemplateAsset] -> DocumentTemplateBundleDTO
toBundle tml files assets =
  DocumentTemplateBundleDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , allowedPackages = tml.allowedPackages
    , formats = tml.formats
    , files = fmap toFileDTO files
    , assets = fmap toAssetDTO assets
    , createdAt = tml.createdAt
    }

fromDocumentTemplateArchive :: BSL.ByteString -> Either AppError (DocumentTemplateBundleDTO, [(DocumentTemplateAssetDTO, BS.ByteString)])
fromDocumentTemplateArchive = fromDocumentTemplateZip . toArchive

fromDocumentTemplateZip :: Archive -> Either AppError (DocumentTemplateBundleDTO, [(DocumentTemplateAssetDTO, BS.ByteString)])
fromDocumentTemplateZip archive = do
  tb <- fromDocumentTemplateEntry archive
  assets <- traverse (fromAssetEntry tb archive) tb.assets
  Right (tb, assets)

fromDocumentTemplateEntry :: Archive -> Either AppError DocumentTemplateBundleDTO
fromDocumentTemplateEntry archive =
  case findEntryByPath "template/template.json" archive of
    Just templateEntry ->
      case eitherDecode . fromEntry $ templateEntry of
        Right tb -> Right tb
        Left error -> Left $ UserError (_ERROR_SERVICE_TB__UNABLE_TO_DECODE_TEMPLATE_JSON error)
    Nothing -> Left $ UserError _ERROR_SERVICE_TB__MISSING_TEMPLATE_JSON

fromAssetEntry :: DocumentTemplateBundleDTO -> Archive -> DocumentTemplateAssetDTO -> Either AppError (DocumentTemplateAssetDTO, BS.ByteString)
fromAssetEntry tb archive asset =
  case findEntryByPath ("template/assets/" ++ asset.fileName) archive of
    Just assetEntry -> Right (asset, BSL.toStrict . fromEntry $ assetEntry)
    Nothing -> Left $ UserError (_ERROR_SERVICE_TB__MISSING_ASSET asset.fileName)

fromBundle :: DocumentTemplateBundleDTO -> U.UUID -> DocumentTemplate
fromBundle tb appUuid =
  DocumentTemplate
    { tId = tb.tId
    , name = tb.name
    , organizationId = tb.organizationId
    , templateId = tb.templateId
    , version = tb.version
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = tb.metamodelVersion
    , description = tb.description
    , readme = tb.readme
    , license = tb.license
    , allowedPackages = tb.allowedPackages
    , formats = tb.formats
    , appUuid = appUuid
    , createdAt = tb.createdAt
    , updatedAt = tb.createdAt
    }
