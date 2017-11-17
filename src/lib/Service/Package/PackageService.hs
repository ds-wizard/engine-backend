module Service.Package.PackageService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID as U
import Text.Regex

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Api.Resources.Organization.OrganizationDTO
import Api.Resources.Package.PackageDTO
import Api.Resources.Package.PackageSimpleDTO
import Api.Resources.Package.PackageWithEventsDTO
import Common.Error
import Common.Types
import Common.Uuid
import Common.Context
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Database.DAO.Package.PackageDAO
import Model.Event.Event
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.Package.Package
import Service.Event.EventMapper
import Service.KnowledgeModelContainer.KnowledgeModelContainerService
import Service.Organization.OrganizationService
import Service.Package.PackageMapper

getPackagesFiltered :: Context
                    -> [(Text, Text)]
                    -> IO (Either AppError [PackageDTO])
getPackagesFiltered context queryParams = do
  eitherPackages <- findPackagesFiltered context queryParams
  case eitherPackages of
    Right packages -> return . Right . fmap packageToDTO $ packages
    Left error -> return . Left $ error

getSimplePackagesFiltered :: Context
                          -> [(Text, Text)]
                          -> IO (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered context queryParams = do
  eitherPackages <- findPackagesFiltered context queryParams
  case eitherPackages of
    Right packages -> do
      let uniquePackages = makePackagesUnique packages
      return . Right . fmap packageToSimpleDTO $ uniquePackages
      where makePackagesUnique :: [Package] -> [Package]
            makePackagesUnique = foldl addIfUnique []
            addIfUnique :: [Package] -> Package -> [Package]
            addIfUnique packages newPackage =
              case isAlreadyInArray packages newPackage of
                (Just _) -> packages
                Nothing -> packages ++ [newPackage]
            isAlreadyInArray :: [Package] -> Package -> Maybe Package
            isAlreadyInArray packages newPackage =
              find (equalSameArtifactId (newPackage ^. pkgArtifactId)) packages
            hasSameArtifactId :: Package -> Package -> Bool
            hasSameArtifactId pkg1 pkg2 =
              pkg1 ^. pkgArtifactId == pkg2 ^. pkgArtifactId
            equalSameArtifactId :: String -> Package -> Bool
            equalSameArtifactId artifactId pkg =
              artifactId == pkg ^. pkgArtifactId
    Left error -> return . Left $ error

getPackageById :: Context -> String -> IO (Either AppError PackageDTO)
getPackageById context pkgId = do
  eitherPackage <- findPackageById context pkgId
  case eitherPackage of
    Right package -> return . Right . packageToDTO $ package
    Left error -> return . Left $ error

getPackageWithEventsById :: Context
                         -> String
                         -> IO (Either AppError PackageWithEventsDTO)
getPackageWithEventsById context pkgId = do
  eitherPackage <- findPackageWithEventsById context pkgId
  case eitherPackage of
    Right package -> return . Right . packageWithEventsToDTOWithEvents $ package
    Left error -> return . Left $ error

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
createPackage context name groupId artifactId version description maybeParentPackage events = do
  let package =
        buildPackage
          name
          groupId
          artifactId
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
createPackageFromKMC context kmcUuid version description =
  case isVersionInValidFormat version of
    Nothing -> do
      eitherKmc <- findKnowledgeModelContainerWithEventsById context kmcUuid
      case eitherKmc of
        Right kmc -> do
          eitherOrganization <- getOrganization context
          case eitherOrganization of
            Right organization -> do
              let groupId = organization ^. orgdtoGroupId
              let artifactId = kmc ^. kmcweArtifactId
              eitherMaybePackage <-
                getTheNewestPackageByGroupIdAndArtifactId
                  context
                  groupId
                  artifactId
              case eitherMaybePackage of
                Right (Just package) ->
                  case isVersionHigher version (package ^. pkgVersion) of
                    Nothing -> doCreatePackage kmc organization
                    Just error -> return . Left $ error
                Right Nothing -> doCreatePackage kmc organization
                Left error -> return . Left $ error
            Left error -> return . Left $ error
        Left error -> return . Left $ error
    Just error -> return . Left $ error
  where
    doCreatePackage kmc organization = do
      let name = kmc ^. kmcweName
      let groupId = organization ^. orgdtoGroupId
      let artifactId = kmc ^. kmcweArtifactId
      let events = kmc ^. kmcweEvents
      let mPpId = kmc ^. kmcweParentPackageId
      case mPpId of
        Just ppId -> do
          eitherPackage <- findPackageWithEventsById context ppId
          case eitherPackage of
            Right package -> do
              createdPackage <-
                createPackage
                  context
                  name
                  groupId
                  artifactId
                  version
                  description
                  (Just package)
                  events
              return . Right $ createdPackage
            Left error -> return . Left $ error
        Nothing -> do
          createdPackage <-
            createPackage
              context
              name
              groupId
              artifactId
              version
              description
              Nothing
              events
          return . Right $ createdPackage

importPackage :: Context -> BS.ByteString -> IO (Either AppError PackageDTO)
importPackage context fileContent = do
  let eitherDeserializedFile = eitherDecode fileContent
  case eitherDeserializedFile of
    Right deserializedFile -> do
      let packageWithEvents = fromDTOWithEvents deserializedFile
      let pName = packageWithEvents ^. pkgweName
      let pGroupId = packageWithEvents ^. pkgweGroupId
      let pArtifactId = packageWithEvents ^. pkgweArtifactId
      let pVersion = packageWithEvents ^. pkgweVersion
      let pDescription = packageWithEvents ^. pkgweDescription
      let pParentPackage = packageWithEvents ^. pkgweParentPackage
      let pEvents = packageWithEvents ^. pkgweEvents
      createdPkg <-
        createPackage
          context
          pName
          pGroupId
          pArtifactId
          pVersion
          pDescription
          pParentPackage
          pEvents
      return . Right $ createdPkg
    Left error -> return . Left . createErrorWithErrorMessage $ error

deletePackagesByQueryParams :: Context -> [(Text, Text)] -> IO ()
deletePackagesByQueryParams context queryParams = do
  deletePackagesFiltered context queryParams

deletePackage :: Context -> String -> IO (Maybe AppError)
deletePackage context pkgId = do
  eitherPackage <- findPackageById context pkgId
  case eitherPackage of
    Right package -> do
      deletePackageById context pkgId
      return Nothing
    Left error -> return . Just $ error

getTheNewestPackageByGroupIdAndArtifactId :: Context
                                          -> String
                                          -> String
                                          -> IO (Either AppError (Maybe Package))
getTheNewestPackageByGroupIdAndArtifactId context groupId artifactId = do
  eitherPackages <- findPackageByGroupIdAndArtifactId context groupId artifactId
  case eitherPackages of
    Right packages ->
      if length packages == 0
        then return . Right $ Nothing
        else do
          let sorted = sortPackages packages
          return . Right . Just . head $ sorted
    Left error -> return . Left $ error
  where
    sortPackages packages =
      sortBy
        (\p1 p2 -> compareVersionNeg (p1 ^. pkgVersion) (p2 ^. pkgVersion))
        packages

isVersionInValidFormat :: String -> Maybe AppError
isVersionInValidFormat version =
  if isJust $ matchRegex validationRegex version
    then Nothing
    else Just . createErrorWithErrorMessage $ "Version is not in valid format"
  where
    validationRegex = mkRegex "^[0-9].[0-9].[0-9]$"

isVersionHigher :: String -> String -> Maybe AppError
isVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . createErrorWithErrorMessage $
         "New version has to be higher than the previous one"

compareVersionNeg :: String -> String -> Ordering
compareVersionNeg verA verB = compareVersion verB verA

compareVersion :: String -> String -> Ordering
compareVersion versionA versionB =
  case compare versionAMajor versionBMajor of
    LT -> LT
    GT -> GT
    EQ ->
      case compare versionAMinor versionBMinor of
        LT -> LT
        GT -> GT
        EQ ->
          case compare versionAPatch versionBPatch of
            LT -> LT
            GT -> GT
            EQ -> EQ
  where
    versionASplitted = T.splitOn "." (T.pack versionA)
    versionBSplitted = T.splitOn "." (T.pack versionB)
    versionAMajor = read . T.unpack $ (versionASplitted !! 0) :: Int
    versionAMinor = read . T.unpack $ (versionASplitted !! 1) :: Int
    versionAPatch = read . T.unpack $ (versionASplitted !! 2) :: Int
    versionBMajor = read . T.unpack $ (versionBSplitted !! 0) :: Int
    versionBMinor = read . T.unpack $ (versionBSplitted !! 1) :: Int
    versionBPatch = read . T.unpack $ (versionBSplitted !! 2) :: Int
