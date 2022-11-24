module Registry.Service.TemplateBundle.TemplateBundleService where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Template.TemplateS3
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Model.Template.Template
import Shared.Service.Template.TemplateUtil
import Shared.Service.TemplateBundle.TemplateBundleMapper

exportTemplateBundle :: String -> AppContextM BSL.ByteString
exportTemplateBundle tmlId = do
  resolvedId <- resolveTemplateId tmlId
  template <- findTemplateById resolvedId
  files <- findTemplateFilesByTemplateId resolvedId
  assets <- findTemplateAssetsByTemplateId resolvedId
  assetContents <- traverse (findAsset template.tId) assets
  return $ toTemplateArchive (toTemplateBundle template files assets) assetContents

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: String -> TemplateAsset -> AppContextM (TemplateAsset, BS.ByteString)
findAsset templateId asset = do
  content <- getAsset templateId (U.toString asset.uuid)
  return (asset, content)
