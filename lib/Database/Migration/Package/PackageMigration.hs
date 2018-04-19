module Database.Migration.Package.PackageMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Database.DAO.Package.PackageDAO
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Package.Data.Package
import LensesConfig
import Model.Context.AppContext
import Model.Event.Event
import Service.Package.PackageMapper
import Service.Package.PackageService

runMigration appContext = do
  $(logInfo) "MIGRATION (Package/Package): started"
  let context = appContext ^. oldContext
  liftIO $ deletePackages context
  liftIO $ insertPackage context baseElixir0PackageDto
  liftIO $ insertPackage context baseElixirPackageDto
  liftIO $ insertPackage context elixirNlPackageDto
  liftIO $ insertPackage context elixirNlPackage2Dto
  $(logInfo) "MIGRATION (Package/Package): ended"
