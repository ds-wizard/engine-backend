module Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (toDocumentTemplateArchive)
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (toBundle)
import Shared.Service.DocumentTemplate.DocumentTemplateUtil

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle tmlId = do
  resolvedId <- resolveDocumentTemplateId tmlId
  tml <- findDocumentTemplateById resolvedId
  files <- findFilesByDocumentTemplateId resolvedId
  assets <- findAssetsByDocumentTemplateId resolvedId
  assetContents <- traverse (findAsset tml.tId) assets
  return $ toDocumentTemplateArchive (toBundle tml files assets) assetContents

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: String -> DocumentTemplateAsset -> AppContextM (DocumentTemplateAsset, BS.ByteString)
findAsset templateId asset = do
  content <- retrieveAsset templateId (U.toString asset.uuid)
  return (asset, content)
