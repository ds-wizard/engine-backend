module Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String
import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Service.Acl.AclService
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.Limit.AppLimitService
import qualified Wizard.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Wizard.Service.TemporaryFile.TemporaryFileService
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper

getTemporaryFileWithBundle :: String -> AppContextM TemporaryFileDTO
getTemporaryFileWithBundle tmlId =
  runInTransaction $ do
    bundle <- exportBundle tmlId
    url <- createTemporaryFile (f' "%s.zip" [tmlId]) "application/octet-stream" bundle
    return $ TemporaryFileMapper.toDTO url "application/zip"

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle tmlId =
  runInTransaction $ do
    tml <- findDocumentTemplateById tmlId
    files <- findFilesByDocumentTemplateId tmlId
    assets <- findAssetsByDocumentTemplateId tmlId
    assetContents <- traverse (findAsset tml.tId) assets
    auditBundleExport tmlId
    return $ toDocumentTemplateArchive (toBundle tml files assets) assetContents

pullBundleFromRegistry :: String -> AppContextM ()
pullBundleFromRegistry tmlId =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    checkDocumentTemplateLimit
    tb <- catchError (retrieveTemplateBundleById tmlId) handleError
    _ <- importAndConvertBundle tb True
    return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_TB__PULL_NON_EXISTING_TML tmlId
        else throwError error

importAndConvertBundle :: BSL.ByteString -> Bool -> AppContextM DocumentTemplateBundleDTO
importAndConvertBundle contentS fromRegistry =
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      checkDocumentTemplateLimit
      let assetSize = foldl (\acc (_, content) -> acc + (fromIntegral . BS.length $ content)) 0 assetContents
      checkStorageSize assetSize
      appUuid <- asks currentAppUuid
      let tml = fromBundle bundle appUuid
      validateNewDocumentTemplate tml True
      deleteOldDocumentTemplateIfPresent bundle
      traverse_ (\(a, content) -> putAsset tml.tId a.uuid a.contentType content) assetContents
      insertDocumentTemplate tml
      traverse_ (insertFile . fromFileDTO tml.tId appUuid tml.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO tml.tId (fromIntegral . BS.length $ content) appUuid tml.createdAt assetDto
        )
        assetContents
      if fromRegistry
        then auditBundlePullFromRegistry tml.tId
        else auditBundleImportFromFile tml.tId
      return bundle
    Left error -> throwError error

deleteOldDocumentTemplateIfPresent :: DocumentTemplateBundleDTO -> AppContextM ()
deleteOldDocumentTemplateIfPresent bundle =
  runInTransaction $ do
    mOldTemplate <- findDocumentTemplateById' bundle.tId
    mOldAssets <- findAssetsByDocumentTemplateId bundle.tId
    case mOldTemplate of
      Just oldTemplate -> do
        traverse_ (\a -> removeAsset oldTemplate.tId a.uuid) mOldAssets
        deleteDocumentTemplateById oldTemplate.tId
        return ()
      Nothing -> return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: String -> DocumentTemplateAsset -> AppContextM (DocumentTemplateAsset, BS.ByteString)
findAsset tmlId asset = do
  content <- retrieveAsset tmlId asset.uuid
  return (asset, content)
