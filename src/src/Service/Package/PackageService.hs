module Service.Package.PackageService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Text (Text)
import Data.UUID as U

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Api.Resources.Organization.OrganizationDTO
import Api.Resources.Package.PackageDTO
import Api.Resources.Package.PackageSimpleDTO
import Api.Resources.Package.PackageWithEventsDTO
import Common.Types
import Common.Error
import Common.Uuid
import Context
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Database.DAO.Package.PackageDAO
import Model.Event.Event
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.Package.Package
import Service.Event.EventMapper
import Service.KnowledgeModelContainer.KnowledgeModelContainerService
import Service.Organization.OrganizationService
import Service.Package.PackageMapper

getAllPackages :: Context -> IO [PackageDTO]
getAllPackages context = do
  packages <- findPackages context
  return . fmap packageToDTO $ packages

getSimplePackagesFiltered :: Context -> [(Text, Text)] -> IO [PackageSimpleDTO]
getSimplePackagesFiltered context queryParams = do
  packages <- findPackagesFiltered context queryParams
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
      find (equalSameArtefactId (newPackage ^. pkgArtefactId)) packages
    hasSameArtefactId :: Package -> Package -> Bool
    hasSameArtefactId pkg1 pkg2 = pkg1 ^. pkgArtefactId == pkg2 ^. pkgArtefactId
    equalSameArtefactId :: String -> Package -> Bool
    equalSameArtefactId artefactId pkg = artefactId == pkg ^. pkgArtefactId

getPackagesForName :: Context -> String -> IO [PackageDTO]
getPackagesForName context name = do
  packages <- findPackagesByArtefactId context name
  return . fmap packageToDTO $ packages

getPackageById :: Context -> String -> IO (Maybe PackageDTO)
getPackageById context pkgId = do
  maybeKM <- findPackageById context pkgId
  case maybeKM of
    Just km -> return . Just $ packageToDTO km
    Nothing -> return Nothing

getPackageWithEventsById :: Context -> String -> IO (Maybe PackageWithEventsDTO)
getPackageWithEventsById context pkgId = do
  maybeKM <- findPackageWithEventsById context pkgId
  case maybeKM of
    Just km -> return . Just $ packageWithEventsToDTOWithEvents km
    Nothing -> return Nothing

createPackage
  :: Context
  -> String
  -> String
  -> String
  -> String
  -> String
  -> Maybe PackageWithEvents
  -> [Event]
  -> IO PackageDTO
createPackage context name groupId artefactId version description maybeParentPackage events = do
  let package =
        buildPackage
          name
          groupId
          artefactId
          version
          description
          maybeParentPackage
          events
  insertPackage context package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: Context
                     -> String
                     -> String
                     -> String
                     -> IO (Either AppError PackageDTO)
createPackageFromKMC context kmcUuid version description = do
  eitherKmc <- findKnowledgeModelContainerWithEventsById context kmcUuid
  case eitherKmc of
    Right kmc -> do
      eitherOrganization <- getOrganization context
      case eitherOrganization of
        Right organization -> do
          let name = kmc ^. kmcweName
          let groupId = organization ^. orgdtoGroupId
          let artefactId = kmc ^. kmcweArtefactId
          let events = kmc ^. kmcweEvents
          let mPpId = kmc ^. kmcweParentPackageId
          case mPpId of
            Just ppId -> do
              maybePackage <- findPackageWithEventsById context ppId
              createdPackage <-
                createPackage
                  context
                  name
                  groupId
                  artefactId
                  version
                  description
                  maybePackage
                  events
              return . Right $ createdPackage
            Nothing -> do
              createdPackage <-
                createPackage
                  context
                  name
                  groupId
                  artefactId
                  version
                  description
                  Nothing
                  events
              return . Right $ createdPackage
        Left error -> return . Left $ error
    Left error -> return . Left $ error

importPackage :: Context -> BS.ByteString -> IO (Maybe PackageDTO)
importPackage context fileContent = do
  let maybeDeserializedFile = eitherDecode fileContent
  case maybeDeserializedFile of
    Right deserializedFile -> do
      let packageWithEvents = fromDTOWithEvents deserializedFile
      let pName = packageWithEvents ^. pkgweName
      let pGroupId = packageWithEvents ^. pkgweGroupId
      let pArtefactId = packageWithEvents ^. pkgweArtefactId
      let pVersion = packageWithEvents ^. pkgweVersion
      let pDescription = packageWithEvents ^. pkgweDescription
      let pParentPackage = packageWithEvents ^. pkgweParentPackage
      let pEvents = packageWithEvents ^. pkgweEvents
      createdPkg <-
        createPackage
          context
          pName
          pGroupId
          pArtefactId
          pVersion
          pDescription
          pParentPackage
          pEvents
      return . Just $ createdPkg
    Left e -> do
      return Nothing

deleteAllPackagesByName :: Context -> String -> IO ()
deleteAllPackagesByName context artefactId = do
  deletePackagesByArtefactId context artefactId

deletePackage :: Context -> String -> IO Bool
deletePackage context pkgId = do
  maybePackage <- findPackageById context pkgId
  case maybePackage of
    Just package -> do
      deletePackageById context pkgId
      return True
    Nothing -> return False
