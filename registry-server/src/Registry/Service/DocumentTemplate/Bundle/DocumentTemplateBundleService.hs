module Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Registry.Service.Audit.AuditService
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAcl
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (toDocumentTemplateArchive)
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.DocumentTemplate.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (fromBundle, fromDocumentTemplateArchive, toBundle)
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil

exportBundle :: Coordinate -> AppContextM BSL.ByteString
exportBundle documentTemplateId = do
  _ <- auditGetDocumentTemplateBundle documentTemplateId
  dt <- resolveDocumentTemplateCoordinate documentTemplateId
  formats <- findDocumentTemplateFormats dt.uuid
  files <- findFilesByDocumentTemplateUuid dt.uuid
  assets <- findAssetsByDocumentTemplateUuid dt.uuid
  assetContents <- traverse (findAsset dt.uuid) assets
  return $ toDocumentTemplateArchive (toBundle dt formats files assets) assetContents

importBundle :: BSL.ByteString -> AppContextM DocumentTemplateSimple
importBundle contentS = do
  checkWritePermission
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      uuid <- liftIO generateUuid
      let tenantUuid = U.nil
      let dt = fromBundle bundle uuid tenantUuid
      traverse_ (\(a, content) -> putAsset dt.uuid a.uuid a.contentType content) assetContents
      insertDocumentTemplate dt
      traverse_ (insertDocumentTemplateFormat . fromFormatDTO dt.uuid tenantUuid dt.createdAt dt.updatedAt) bundle.formats
      traverse_ (insertFile . fromFileDTO dt.uuid tenantUuid dt.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO dt.uuid (fromIntegral . BS.length $ content) tenantUuid dt.createdAt assetDto
        )
        assetContents
      return . toSimple $ dt
    Left error -> throwError error

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: U.UUID -> DocumentTemplateAsset -> AppContextM (DocumentTemplateAsset, BS.ByteString)
findAsset documentTemplateUuid asset = do
  content <- retrieveAsset documentTemplateUuid asset.uuid
  return (asset, content)
