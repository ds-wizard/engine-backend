module Service.Package.PackageService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.Package.PackageDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.Package.PackageDAO
import Model.Package.Package
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

getPackageByNameAndVersion :: Context
                           -> String
                           -> String
                           -> IO (Maybe PackageDTO)
getPackageByNameAndVersion context name version = do
  maybeKM <- findPackageByNameAndVersion context name version
  case maybeKM of
    Just km -> return . Just $ toDTO km
    Nothing -> return Nothing

--modifyPackage :: Context -> String -> PackageDTO -> IO (Maybe PackageDTO)
--modifyPackage context kmcUuid kmcDto = do
--  maybeKmc <- findPackageById context kmcUuid
--  case maybeKmc of
--    Just kmc -> do
--      let kmc = fromDTO kmcDto
--      updatePackageById context kmc
--      return . Just $ kmcDto
--    Nothing -> return Nothing
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
