module Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String
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
import Shared.DocumentTemplate.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithBundle :: U.UUID -> AppContextM TemporaryFileDTO
getTemporaryFileWithBundle dtUuid =
  runInTransaction $ do
    (coordinate, bundle) <- exportBundle dtUuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.zip" [show coordinate]) "application/octet-stream" mCurrentUserUuid bundle
    return $ TemporaryFileMapper.toDTO url "application/zip"

exportBundle :: U.UUID -> AppContextM (Coordinate, BSL.ByteString)
exportBundle dtUuid =
  runInTransaction $ do
    dt <- findDocumentTemplateByUuid dtUuid
    when
      dt.nonEditable
      (throwError . UserError $ _ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML)
    formats <- findDocumentTemplateFormats dtUuid
    files <- findFilesByDocumentTemplateUuid dtUuid
    assets <- findAssetsByDocumentTemplateUuid dtUuid
    assetContents <- traverse (findAsset dt.uuid) assets
    auditBundleExport (createCoordinate dt)
    return (createCoordinate dt, toDocumentTemplateArchive (toBundle dt formats files assets) assetContents)

pullBundleFromRegistry :: Coordinate -> AppContextM DocumentTemplateSimple
pullBundleFromRegistry coordinate =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkDocumentTemplateLimit
    tb <- catchError (retrieveDocumentTemplateBundleByCoordinate coordinate) handleError
    importAndConvertBundle tb True
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_TB__PULL_NON_EXISTING_TML (show coordinate)
        else throwError error

importAndConvertBundle :: BSL.ByteString -> Bool -> AppContextM DocumentTemplateSimple
importAndConvertBundle contentS fromRegistry =
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      checkPermission _DOC_TML_WRITE_PERM
      checkDocumentTemplateLimit
      let assetSize = foldl (\acc (_, content) -> acc + (fromIntegral . BS.length $ content)) 0 assetContents
      checkStorageSize assetSize
      uuid <- liftIO generateUuid
      tenantUuid <- asks currentTenantUuid
      let dt = fromBundle bundle uuid tenantUuid
      validateNewDocumentTemplate dt True
      deleteOldDocumentTemplateIfPresent bundle
      traverse_ (\(a, content) -> putAsset dt.uuid a.uuid a.contentType content) assetContents
      insertDocumentTemplate dt
      traverse_ (insertDocumentTemplateFormat . fromFormatDTO dt.uuid tenantUuid dt.createdAt dt.updatedAt) bundle.formats
      traverse_ (insertFile . fromFileDTO dt.uuid tenantUuid dt.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO dt.uuid (fromIntegral . BS.length $ content) tenantUuid dt.createdAt assetDto
        )
        assetContents
      if fromRegistry
        then auditBundlePullFromRegistry (createCoordinate dt)
        else auditBundleImportFromFile (createCoordinate dt)
      return . toSimple $ dt
    Left error -> throwError error

deleteOldDocumentTemplateIfPresent :: DocumentTemplateBundleDTO -> AppContextM ()
deleteOldDocumentTemplateIfPresent bundle =
  runInTransaction $ do
    let coordinate = createCoordinate bundle
    mOldDt <- findDocumentTemplateByCoordinate' coordinate
    case mOldDt of
      Just oldDt -> do
        oldAssets <- findAssetsByDocumentTemplateUuid oldDt.uuid
        traverse_ (\a -> removeAsset oldDt.uuid a.uuid) oldAssets
        void $ deleteDocumentTemplateByUuid oldDt.uuid
      Nothing -> return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: U.UUID -> DocumentTemplateAsset -> AppContextM (DocumentTemplateAsset, BS.ByteString)
findAsset dtUuid asset = do
  content <- retrieveAsset dtUuid asset.uuid
  return (asset, content)
