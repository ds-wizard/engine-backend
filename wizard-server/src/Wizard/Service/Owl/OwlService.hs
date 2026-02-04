module Wizard.Service.Owl.OwlService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Compiler.Compiler
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Service.Owl.Convertor.OwlConvertor
import Wizard.Service.Owl.Diff.Differ
import Wizard.Service.Owl.OwlMapper

importOwl :: KnowledgeModelBundleFile -> AppContextM KnowledgeModelPackageSimpleDTO
importOwl reqDto = do
  logInfoI _CMP_SERVICE "Importing OWL..."
  case (reqDto.rootElement, reqDto.name, reqDto.organizationId, reqDto.kmId, reqDto.version) of
    (Just rootElement, Just name, Just organizationId, Just kmId, Just version) -> do
      tenantUuid <- asks currentTenantUuid
      uuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      previousPackageUuid <-
        case reqDto.previousPackageId of
          Just previousPackageId -> do
            pkg <- findPackageByCoordinate previousPackageId
            return . Just $ pkg.uuid
          Nothing -> return Nothing
      events <- importEvents previousPackageUuid rootElement (TE.decodeUtf8 . BSL.toStrict $ reqDto.content)
      let pkg = fromOwl uuid name organizationId kmId version previousPackageUuid tenantUuid now
      let pkgEvents = fmap (toPackageEvent pkg.uuid tenantUuid) events
      insertPackage pkg
      traverse_ insertPackageEvent pkgEvents
      return $ toSimpleDTO pkg
    _ -> throwError . UserError $ _ERROR_VALIDATION__FIELDS_ABSENCE

importEvents :: Maybe U.UUID -> T.Text -> T.Text -> AppContextM [KnowledgeModelEvent]
importEvents mPreviousPackageUuid rootElement content = do
  pkgEvents <- convertOwlToEvents rootElement content
  case mPreviousPackageUuid of
    Just previousPackageUuid -> do
      previousPackageEvents <- findPackageEvents previousPackageUuid
      let (Right km1) = compile Nothing pkgEvents
      let (Right km2) = compile Nothing (fmap toEvent previousPackageEvents)
      diffKnowledgeModel (km1, km2)
    Nothing -> return pkgEvents

modifyOwlFeature :: Bool -> AppContextM ()
modifyOwlFeature owlEnabled =
  runInTransaction $ do
    tcOwl <- findTenantConfigOwl
    let tcOwlUpdated = tcOwl {enabled = owlEnabled} :: TenantConfigOwl
    updateTenantConfigOwl tcOwlUpdated
    return ()

setOwlProperties :: String -> String -> String -> String -> Maybe String -> String -> AppContextM ()
setOwlProperties name organizationId kmId version previousKnowledgeModelPackageId rootElement =
  runInTransaction $ do
    tcOwl <- findTenantConfigOwl
    let tcOwlUpdated =
          tcOwl
            { name = name
            , organizationId = organizationId
            , kmId = kmId
            , version = version
            , previousKnowledgeModelPackageId = previousKnowledgeModelPackageId
            , rootElement = rootElement
            }
          :: TenantConfigOwl
    updateTenantConfigOwl tcOwlUpdated
    return ()
