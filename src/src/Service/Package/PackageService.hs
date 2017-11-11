module Service.Package.PackageService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.UUID as U

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Api.Resources.Package.PackageDTO
import Api.Resources.Package.PackageSimpleDTO
import Api.Resources.Package.PackageWithEventsDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Database.DAO.Package.PackageDAO
import Model.Event.Event
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.Package.Package
import Service.Event.EventMapper
import Service.KnowledgeModelContainer.KnowledgeModelContainerService
import Service.Package.PackageMapper

getAllPackages :: Context -> IO [PackageDTO]
getAllPackages context = do
  packages <- findPackages context
  return . fmap packageToDTO $ packages

getAllSimplePackages :: Context -> IO [PackageSimpleDTO]
getAllSimplePackages context = do
  packages <- findPackages context
  let uniquePackages = makePackagesUnique packages
  return . fmap packageToSimpleDTO $ uniquePackages
  where
    makePackagesUnique :: [Package] -> [Package]
    makePackagesUnique = foldl addIfUnique []
    addIfUnique :: [Package] -> Package -> [Package]
    addIfUnique packages newPackage =
      case isAlreadyInArray packages newPackage of
        (Just _) -> packages
        Nothing -> packages ++ [newPackage]
    isAlreadyInArray :: [Package] -> Package -> Maybe Package
    isAlreadyInArray packages newPackage =
      find (equalSameShortName (newPackage ^. pkgShortName)) packages
    hasSameShortName :: Package -> Package -> Bool
    hasSameShortName pkg1 pkg2 = pkg1 ^. pkgShortName == pkg2 ^. pkgShortName
    equalSameShortName :: String -> Package -> Bool
    equalSameShortName shortName pkg = shortName == pkg ^. pkgShortName

getPackagesForName :: Context -> String -> IO [PackageDTO]
getPackagesForName context name = do
  packages <- findPackagesByName context name
  return . fmap packageToDTO $ packages

getPackageByNameAndVersion :: Context
                           -> String
                           -> String
                           -> IO (Maybe PackageDTO)
getPackageByNameAndVersion context name version = do
  maybeKM <- findPackageByNameAndVersion context name version
  case maybeKM of
    Just km -> return . Just $ packageToDTO km
    Nothing -> return Nothing

getPackageWithEventsByNameAndVersion :: Context
                                     -> String
                                     -> String
                                     -> IO (Maybe PackageWithEventsDTO)
getPackageWithEventsByNameAndVersion context name version = do
  maybeKM <- findPackageWithEventsByNameAndVersion context name version
  case maybeKM of
    Just km -> return . Just $ packageWithEventsToDTOWithEvents km
    Nothing -> return Nothing

createPackage
  :: Context
  -> String
  -> String
  -> String
  -> String
  -> Maybe PackageWithEvents
  -> [Event]
  -> IO PackageDTO
createPackage context name shortName version description maybeParentPackageDto events = do
  let package =
        buildPackage
          name
          shortName
          version
          description
          maybeParentPackageDto
          events
  insertPackage context package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: Context
                     -> String
                     -> String
                     -> String
                     -> IO (Maybe PackageDTO)
createPackageFromKMC context kmcUuid version description = do
  maybeKmc <- findKnowledgeModelContainerWithEventsById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      let name = kmc ^. kmcweName
      let shortName = kmc ^. kmcweShortName
      let ppName = kmc ^. kmcweParentPackageName
      let ppVersion = kmc ^. kmcweParentPackageVersion
      let events = kmc ^. kmcweEvents
      maybePackage <-
        findPackageWithEventsByNameAndVersion context ppName ppVersion
      createdPackage <-
        createPackage
          context
          name
          shortName
          version
          description
          maybePackage
          events
      return . Just $ createdPackage
    Nothing -> return Nothing

importPackage :: Context -> BS.ByteString -> IO (Maybe PackageDTO)
importPackage context fileContent = do
  let maybeDeserializedFile = eitherDecode fileContent
  case maybeDeserializedFile of
    Right deserializedFile -> do
      let packageWithEvents = fromDTOWithEvents deserializedFile
      let pName = packageWithEvents ^. pkgweName
      let pShortName = packageWithEvents ^. pkgweShortName
      let pVersion = packageWithEvents ^. pkgweVersion
      let pDescription = packageWithEvents ^. pkgweDescription
      let pParentPackage = packageWithEvents ^. pkgweParentPackage
      let pEvents = packageWithEvents ^. pkgweEvents
      createdPkg <-
        createPackage
          context
          pName
          pShortName
          pVersion
          pDescription
          pParentPackage
          pEvents
      return . Just $ createdPkg
    Left e -> do
      return Nothing

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
