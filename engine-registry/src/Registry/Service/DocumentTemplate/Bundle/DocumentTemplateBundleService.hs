module Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAcl
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (toDocumentTemplateArchive)
import Registry.Service.DocumentTemplate.DocumentTemplateService
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (fromBundle, fromDocumentTemplateArchive, toBundle)
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Service.DocumentTemplate.DocumentTemplateUtil

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle tmlId = do
  resolvedId <- resolveDocumentTemplateId tmlId
  tml <- findDocumentTemplateById resolvedId
  files <- findFilesByDocumentTemplateId resolvedId
  assets <- findAssetsByDocumentTemplateId resolvedId
  assetContents <- traverse (findAsset tml.tId) assets
  return $ toDocumentTemplateArchive (toBundle tml files assets) assetContents

importBundle :: BSL.ByteString -> AppContextM DocumentTemplateDetailDTO
importBundle contentS = do
  checkWritePermission
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      let appUuid = U.nil
      let tml = fromBundle bundle appUuid
      traverse_ (\(a, content) -> putAsset tml.tId a.uuid a.contentType content) assetContents
      insertDocumentTemplate tml
      traverse_ (insertFile . fromFileDTO tml.tId appUuid tml.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO tml.tId (fromIntegral . BS.length $ content) appUuid tml.createdAt assetDto
        )
        assetContents
      getDocumentTemplateById tml.tId
    Left error -> throwError error

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: String -> DocumentTemplateAsset -> AppContextM (DocumentTemplateAsset, BS.ByteString)
findAsset templateId asset = do
  content <- retrieveAsset templateId asset.uuid
  return (asset, content)
