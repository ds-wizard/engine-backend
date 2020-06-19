module Registry.Service.Template.TemplateService where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import LensesConfig
import Registry.Database.DAO.Template.TemplateDAO
import Registry.Model.Context.AppContext
import Registry.Service.Template.TemplateMapper
import Shared.Model.Template.Template

getTemplates :: AppContextM [Template]
getTemplates = findTemplates

getTemplateById :: String -> AppContextM Template
getTemplateById = findTemplateById

getTemplateBundle :: String -> AppContextM BSL.ByteString
getTemplateBundle tmlId = do
  template <- findTemplateById tmlId
  assets <- traverse findAsset (template ^. assets)
  return $ toTemplateArchive template assets

findAsset :: TemplateAsset -> AppContextM (TemplateAsset, BS.ByteString)
findAsset asset = do
  content <- findTemplateAssetContent (U.toString $ asset ^. uuid)
  return (asset, content)
