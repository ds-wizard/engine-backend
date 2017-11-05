module Service.Package.PackageService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Api.Resources.Package.PackageDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.Package.PackageDAO
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.Package.Package
import Service.KnowledgeModelContainer.KnowledgeModelContainerService
import Service.Package.PackageMapper

getAllPackages :: Context -> IO [PackageDTO]
getAllPackages context = do
  kms <- findPackages context
  return . fmap toDTO $ kms

getPackagesForName :: Context -> String -> IO [PackageDTO]
getPackagesForName context name = do
  kms <- findPackagesByName context name
  return . fmap toDTO $ kms

createPackage :: Context
              -> String
              -> String
              -> String
              -> Maybe PackageDTO
              -> IO PackageDTO
createPackage context name shortName version maybeParentPackageDto = do
  let package = buildPackage name shortName version maybeParentPackageDto
  insertPackage context package
  return $ toDTO package

createPackageFromKMC :: Context -> String -> String -> IO (Maybe PackageDTO)
createPackageFromKMC context kmcUuid version = do
  maybeKmcDto <- getKnowledgeModelContainerById context kmcUuid
  case maybeKmcDto of
    Just kmcDto -> do
      let name = kmcDto ^. kmcdtoName
      let shortName = kmcDto ^. kmcdtoShortName
      let ppName = kmcDto ^. kmcdtoParentPackageName
      let ppVersion = kmcDto ^. kmcdtoParentPackageVersion
      maybePackage <- getPackageByNameAndVersion context ppName ppVersion
      createdPackage <-
        createPackage context name shortName version maybePackage
      return . Just $ createdPackage
    Nothing -> return Nothing

getPackageByNameAndVersion :: Context
                           -> String
                           -> String
                           -> IO (Maybe PackageDTO)
getPackageByNameAndVersion context name version = do
  maybeKM <- findPackageByNameAndVersion context name version
  case maybeKM of
    Just km -> return . Just $ toDTO km
    Nothing -> return Nothing

deleteAllPackagesByName :: Context -> String -> IO ()
deleteAllPackagesByName context shortName = do
  deletePackagesByName context shortName

deletePackage :: Context -> String -> String -> IO Bool
deletePackage context shortName version = do
  maybePackage <- findPackageByNameAndVersion context shortName version
  case maybePackage of
    Just package -> do
      deletePackageByNameAndVersion context shortName version
      return True
    Nothing -> return False
