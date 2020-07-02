module Registry.Service.TemplateBundle.TemplateBundleService where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Model.Template.Template
import Shared.Service.TemplateBundle.TemplateBundleMapper

exportTemplateBundle :: String -> AppContextM BSL.ByteString
exportTemplateBundle tmlId = do
  template <- findTemplateById tmlId
  assets <- traverse findAsset (template ^. assets)
  return $ toTemplateArchive template assets

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: TemplateAsset -> AppContextM (TemplateAsset, BS.ByteString)
findAsset asset = do
  content <- findTemplateAssetContent (U.toString $ asset ^. uuid)
  return (asset, content)
