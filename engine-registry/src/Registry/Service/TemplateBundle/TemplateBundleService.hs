module Registry.Service.TemplateBundle.TemplateBundleService where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Template.TemplateS3
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Model.Template.Template
import Shared.Service.TemplateBundle.TemplateBundleMapper

exportTemplateBundle :: String -> AppContextM BSL.ByteString
exportTemplateBundle tmlId = do
  template <- findTemplateById tmlId
  files <- findTemplateFilesByTemplateId tmlId
  assets <- findTemplateAssetsByTemplateId tmlId
  assetContents <- traverse (findAsset (template ^. tId)) assets
  return $ toTemplateArchive (toTemplateBundle template files assets) assetContents

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: String -> TemplateAsset -> AppContextM (TemplateAsset, BS.ByteString)
findAsset templateId asset = do
  content <- getAsset templateId (U.toString $ asset ^. uuid)
  return (asset, content)
