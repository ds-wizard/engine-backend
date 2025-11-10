module Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper where

import Codec.Archive.Zip
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Registry.Api.Resource.DocumentTemplate.Bundle.DocumentTemplateBundleJM ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

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
