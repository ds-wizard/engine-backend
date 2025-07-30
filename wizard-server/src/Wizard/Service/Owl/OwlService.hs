module Wizard.Service.Owl.OwlService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.PackageBundle.PackageBundleFileDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Compiler.Compiler
import Wizard.Service.Owl.Convertor.OwlConvertor
import Wizard.Service.Owl.Diff.Differ
import Wizard.Service.Owl.OwlMapper
import Wizard.Service.Package.PackageMapper
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Service.Package.PackageMapper

importOwl :: PackageBundleFileDTO -> AppContextM [PackageSimpleDTO]
importOwl reqDto = do
  logInfoI _CMP_SERVICE "Importing OWL..."
  case (reqDto.rootElement, reqDto.name, reqDto.organizationId, reqDto.kmId, reqDto.version) of
    (Just rootElement, Just name, Just organizationId, Just kmId, Just version) -> do
      tenantUuid <- asks currentTenantUuid
      now <- liftIO getCurrentTime
      events <- importEvents reqDto.previousPackageId rootElement (TE.decodeUtf8 . BSL.toStrict $ reqDto.content)
      let pkg = fromOwl name organizationId kmId version reqDto.previousPackageId events tenantUuid now
      insertPackage pkg
      return [toSimpleDTO . toPackage $ pkg]
    _ -> throwError . UserError $ _ERROR_VALIDATION__FIELDS_ABSENCE

importEvents :: Maybe String -> T.Text -> T.Text -> AppContextM [Event]
importEvents mPreviousPackageId rootElement content = do
  pkgEvents <- convertOwlToEvents rootElement content
  case mPreviousPackageId of
    Just previousPackageId -> do
      previousPackage <- findPackageWithEventsById previousPackageId
      let (Right km1) = compile Nothing pkgEvents
      let (Right km2) = compile Nothing previousPackage.events
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
setOwlProperties name organizationId kmId version previousPackageId rootElement =
  runInTransaction $ do
    tcOwl <- findTenantConfigOwl
    let tcOwlUpdated =
          tcOwl
            { name = name
            , organizationId = organizationId
            , kmId = kmId
            , version = version
            , previousPackageId = previousPackageId
            , rootElement = rootElement
            }
          :: TenantConfigOwl
    updateTenantConfigOwl tcOwlUpdated
    return ()
