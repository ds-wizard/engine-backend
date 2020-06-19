module Registry.Service.Template.TemplateMapper where

import Codec.Archive.Zip
import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import LensesConfig
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
   in toEntry "template.json" 0 templateJson

toAssetEntry :: (TemplateAsset, BS.ByteString) -> Entry
toAssetEntry (asset, content) = toEntry ("assets/" ++ asset ^. fileName) 0 (BSL.fromStrict content)
