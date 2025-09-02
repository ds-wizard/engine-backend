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
import Registry.Service.Audit.AuditService
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAcl
import Registry.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (toDocumentTemplateArchive)
import Registry.Service.DocumentTemplate.DocumentTemplateService
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (fromBundle, fromDocumentTemplateArchive, toBundle)
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle tmlId = do
  _ <- auditGetDocumentTemplateBundle tmlId
  resolvedId <- resolveDocumentTemplateId tmlId
  tml <- findDocumentTemplateById resolvedId
  formats <- findDocumentTemplateFormats resolvedId
  files <- findFilesByDocumentTemplateId resolvedId
  assets <- findAssetsByDocumentTemplateId resolvedId
  assetContents <- traverse (findAsset tml.tId) assets
  return $ toDocumentTemplateArchive (toBundle tml formats files assets) assetContents

importBundle :: BSL.ByteString -> AppContextM DocumentTemplateDetailDTO
importBundle contentS = do
  checkWritePermission
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      let tenantUuid = U.nil
      let tml = fromBundle bundle tenantUuid
      traverse_ (\(a, content) -> putAsset tml.tId a.uuid a.contentType content) assetContents
      insertDocumentTemplate tml
      traverse_ (insertDocumentTemplateFormat . fromFormatDTO tml.tId tenantUuid tml.createdAt tml.updatedAt) bundle.formats
      traverse_ (insertFile . fromFileDTO tml.tId tenantUuid tml.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO tml.tId (fromIntegral . BS.length $ content) tenantUuid tml.createdAt assetDto
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
