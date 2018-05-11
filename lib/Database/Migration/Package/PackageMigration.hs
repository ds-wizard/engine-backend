module Database.Migration.Package.PackageMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)

import Database.DAO.Package.PackageDAO
import Database.Migration.Package.Data.Package
import LensesConfig

runMigration appContext = do
  $(logInfo) "MIGRATION (Package/Package): started"
  let context = appContext ^. oldContext
  liftIO $ deletePackages context
  liftIO $ insertPackage context baseElixir0PackageDto
  liftIO $ insertPackage context baseElixirPackageDto
  liftIO $ insertPackage context elixirNlPackageDto
  liftIO $ insertPackage context elixirNlPackage2Dto
  $(logInfo) "MIGRATION (Package/Package): ended"
