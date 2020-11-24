module Shared.Service.TemplateBundle.TemplateBundleMapper where

import Codec.Archive.Zip
import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()

toTemplateArchive :: Template -> [(TemplateAsset, BS.ByteString)] -> BSL.ByteString
toTemplateArchive template = fromArchive . toTemplateZip template

toTemplateZip :: Template -> [(TemplateAsset, BS.ByteString)] -> Archive
toTemplateZip template assets =
  let templateEntry = toTemplateEntry template
      assetEntries = fmap toAssetEntry assets
   in foldr addEntryToArchive emptyArchive (templateEntry : assetEntries)

toTemplateEntry :: Template -> Entry
toTemplateEntry template =
  let templateJson = encode template
   in toEntry "template/template.json" 0 templateJson

toAssetEntry :: (TemplateAsset, BS.ByteString) -> Entry
toAssetEntry (asset, content) = toEntry ("template/assets/" ++ asset ^. fileName) 0 (BSL.fromStrict content)

fromTemplateArchive :: BSL.ByteString -> Either AppError (Template, [(TemplateAsset, BS.ByteString)])
fromTemplateArchive = fromTemplateZip . toArchive

fromTemplateZip :: Archive -> Either AppError (Template, [(TemplateAsset, BS.ByteString)])
fromTemplateZip archive = do
  template <- fromTemplateEntry archive
  assets <- traverse (fromAssetEntry archive) (template ^. assets)
  Right (template, assets)

fromTemplateEntry :: Archive -> Either AppError Template
fromTemplateEntry archive =
  case findEntryByPath "template/template.json" archive of
    Just templateEntry ->
      case eitherDecode . fromEntry $ templateEntry of
        Right template -> Right template
        Left error -> Left $ UserError (_ERROR_SERVICE_TB__UNABLE_TO_DECODE_TEMPLATE_JSON error)
    Nothing -> Left $ UserError _ERROR_SERVICE_TB__MISSING_TEMPLATE_JSON

fromAssetEntry :: Archive -> TemplateAsset -> Either AppError (TemplateAsset, BS.ByteString)
fromAssetEntry archive asset =
  case findEntryByPath ("template/assets/" ++ asset ^. fileName) archive of
    Just assetEntry -> Right (asset, BSL.toStrict . fromEntry $ assetEntry)
    Nothing -> Left $ UserError (_ERROR_SERVICE_TB__MISSING_ASSET (asset ^. fileName))
