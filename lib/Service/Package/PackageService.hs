module Service.Package.PackageService where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID as U
import Text.Regex

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageWithEventsDTO
import Common.Context
import Common.Error
import Common.Localization
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Model.Branch.Branch
import Model.Event.Event
import Model.Migrator.MigratorState
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Organization.OrganizationService
import Service.Package.PackageMapper

getPackagesFiltered :: Context -> [(Text, Text)] -> IO (Either AppError [PackageDTO])
getPackagesFiltered context queryParams = do
  eitherPackages <- findPackagesFiltered context queryParams
  case eitherPackages of
    Right packages -> return . Right . fmap packageToDTO $ packages
    Left error -> return . Left $ error

getSimplePackagesFiltered :: Context -> [(Text, Text)] -> IO (Either AppError [PackageSimpleDTO])
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
              find
                (\pkg ->
                   equalSameArtifactId (newPackage ^. pkgArtifactId) pkg &&
                   equalSameGroupId (newPackage ^. pkgGroupId) pkg)
                packages
            hasSameArtifactId :: Package -> Package -> Bool
            hasSameArtifactId pkg1 pkg2 = pkg1 ^. pkgArtifactId == pkg2 ^. pkgArtifactId
            equalSameArtifactId :: String -> Package -> Bool
            equalSameArtifactId artifactId pkg = artifactId == pkg ^. pkgArtifactId
            equalSameGroupId :: String -> Package -> Bool
            equalSameGroupId groupId pkg = groupId == pkg ^. pkgGroupId
    Left error -> return . Left $ error

getPackageById :: Context -> String -> IO (Either AppError PackageDTO)
getPackageById context pkgId = do
  eitherPackage <- findPackageById context pkgId
  case eitherPackage of
    Right package -> return . Right . packageToDTO $ package
    Left error -> return . Left $ error

getPackageWithEventsById :: Context -> String -> IO (Either AppError PackageWithEventsDTO)
getPackageWithEventsById context pkgId = do
  eitherPackage <- findPackageWithEventsById context pkgId
  case eitherPackage of
    Right package -> return . Right . packageWithEventsToDTOWithEvents $ package
    Left error -> return . Left $ error

createPackage :: Context -> String -> String -> String -> String -> String -> Maybe String -> [Event] -> IO PackageDTO
createPackage context name groupId artifactId version description maybeParentPackageId events = do
  let package = buildPackage name groupId artifactId version description maybeParentPackageId events
  insertPackage context package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: Context -> String -> String -> String -> IO (Either AppError PackageDTO)
createPackageFromKMC context branchUuid version description =
  validateVersionFormat version $
  getBranch branchUuid $ \branch ->
    getCurrentOrganization $ \organization ->
      validateVersion version branch organization $
      getEventsForPackage context branch $ \events -> do
        let name = branch ^. bweName
        let groupId = organization ^. organizationId
        let artifactId = branch ^. bweArtifactId
        let mPpId = branch ^. bweParentPackageId
        createdPackage <- createPackage context name groupId artifactId version description mPpId events
        deleteEventsAtBranch context branchUuid
        updateBranchWithParentPackageId context branchUuid (createdPackage ^. pkgdtoId)
        updateBranchIfMigrationIsCompleted context branchUuid
        deleteMigratorStateByBranchUuid context branchUuid
        recompileKnowledgeModel context branch $ return . Right $ createdPackage
  where
    validateVersionFormat version callback =
      case isVersionInValidFormat version of
        Nothing -> callback
        Just error -> return . Left $ error
    getBranch branchUuid callback = do
      eitherBranch <- findBranchWithEventsById context branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error
    getCurrentOrganization callback = do
      eitherOrganization <- getOrganization context
      case eitherOrganization of
        Right organization -> callback organization
        Left error -> return . Left $ error
    validateVersion version branch organization callback = do
      let groupId = organization ^. organizationId
      let artifactId = branch ^. bweArtifactId
      eitherMaybePackage <- getTheNewestPackageByGroupIdAndArtifactId context groupId artifactId
      case eitherMaybePackage of
        Right (Just package) ->
          case isVersionHigher version (package ^. pkgVersion) of
            Nothing -> callback
            Just error -> return . Left $ error
        Right Nothing -> callback
        Left error -> return . Left $ error
    updateBranchIfMigrationIsCompleted context branchUuid = do
      eitherMigrationState <- findMigratorStateByBranchUuid context branchUuid
      case eitherMigrationState of
        Right migrationState -> do
          let branchParentId = migrationState ^. msBranchParentId
          let targetPackageId = migrationState ^. msTargetPackageId
          updateBranchWithMigrationInfo context branchUuid targetPackageId branchParentId
        Left _ -> return ()
    getEventsForPackage context branch callback = do
      let branchUuid = U.toString $ branch ^. bweUuid
      eitherMigrationState <- findMigratorStateByBranchUuid context branchUuid
      case eitherMigrationState of
        Right migrationState -> callback $ migrationState ^. msResultEvents
        Left (NotExistsError _) -> callback $ branch ^. bweEvents
        Left error -> return . Left $ error
    recompileKnowledgeModel context branch callback = do
      let branchUuid = U.toString $ branch ^. bweUuid
      eitherEventsForUuid <- getEventsForBranchUuid context branchUuid
      case eitherEventsForUuid of
        Right eventsForBranchUuid -> do
          recompileKnowledgeModelWithEvents context branchUuid eventsForBranchUuid
          callback
        Left error -> return . Left $ error

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
      let pParentPackageId = packageWithEvents ^. pkgweParentPackageId
      let pEvents = packageWithEvents ^. pkgweEvents
      let pId = buildPackageId pGroupId pArtifactId pVersion
      validatePackageId pId $
        validateParentPackageId pId pParentPackageId $ do
          createdPkg <- createPackage context pName pGroupId pArtifactId pVersion pDescription pParentPackageId pEvents
          return . Right $ createdPkg
    Left error -> return . Left . createErrorWithErrorMessage $ error
  where
    validatePackageId pkgId callback = do
      eitherPackage <- findPackageById context pkgId
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Left . createErrorWithErrorMessage $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId
        Left error -> return . Left $ error
    validateParentPackageId pkgId maybeParentPkgId callback =
      case maybeParentPkgId of
        Just parentPkgId -> do
          eitherPackage <- findPackageById context parentPkgId
          case eitherPackage of
            Right _ -> callback
            Left (NotExistsError _) ->
              return . Left . createErrorWithErrorMessage $
              _ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId
            Left error -> return . Left $ error
        Nothing -> callback

deletePackagesByQueryParams :: Context -> [(Text, Text)] -> IO (Maybe AppError)
deletePackagesByQueryParams context queryParams = do
  eitherPackages <- findPackagesFiltered context queryParams
  case eitherPackages of
    Right packages -> do
      maybeError <- validatePackagesDeletation context (_pkgId <$> packages)
      if isJust maybeError
        then return maybeError
        else do
          deletePackagesFiltered context queryParams
          return Nothing
    Left error -> return . Just $ error

deletePackage :: Context -> String -> IO (Maybe AppError)
deletePackage context pkgId = do
  eitherPackage <- findPackageById context pkgId
  case eitherPackage of
    Right package -> do
      maybeError <- validatePackageDeletation context pkgId
      if isJust maybeError
        then return maybeError
        else do
          deletePackageById context pkgId
          return Nothing
    Left error -> return . Just $ error

getTheNewestPackageByGroupIdAndArtifactId :: Context -> String -> String -> IO (Either AppError (Maybe Package))
getTheNewestPackageByGroupIdAndArtifactId context groupId artifactId = do
  eitherPackages <- findPackageByGroupIdAndArtifactId context groupId artifactId
  case eitherPackages of
    Right packages ->
      if length packages == 0
        then return . Right $ Nothing
        else do
          let sorted = sortPackagesByVersion packages
          return . Right . Just . head $ sorted
    Left error -> return . Left $ error

getAllPreviousEventsSincePackageId :: Context -> String -> IO (Either AppError [Event])
getAllPreviousEventsSincePackageId context pkgId = do
  eitherPackage <- findPackageWithEventsById context pkgId
  case eitherPackage of
    Right package ->
      case package ^. pkgweParentPackageId of
        Just parentPackageId -> do
          eitherEvents <- getAllPreviousEventsSincePackageId context parentPackageId
          case eitherEvents of
            Right events -> return . Right $ events ++ (package ^. pkgweEvents)
            Left error -> return . Left $ error
        Nothing -> return . Right $ package ^. pkgweEvents
    Left error -> return . Left $ error

getAllPreviousEventsSincePackageIdAndUntilPackageId :: Context -> String -> String -> IO (Either AppError [Event])
getAllPreviousEventsSincePackageIdAndUntilPackageId context sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return . Right $ []
        else do
          eitherPackage <- findPackageWithEventsById context pkgId
          case eitherPackage of
            Right package ->
              case package ^. pkgweParentPackageId of
                Just parentPackageId -> do
                  eitherEvents <- go parentPackageId
                  case eitherEvents of
                    Right events -> return . Right $ events ++ (package ^. pkgweEvents)
                    Left error -> return . Left $ error
                Nothing -> return . Right $ package ^. pkgweEvents
            Left error -> return . Left $ error

getEventsForBranchUuid :: Context -> String -> IO (Either AppError [Event])
getEventsForBranchUuid context branchUuid =
  getBranch $ \branch ->
    case branch ^. bweParentPackageId of
      Just ppId -> do
        eitherEventsFromPackage <- getAllPreviousEventsSincePackageId context ppId
        case eitherEventsFromPackage of
          Right eventsFromPackage -> do
            let eventsFromKM = branch ^. bweEvents
            let events = eventsFromPackage ++ eventsFromKM
            return . Right $ events
          Left error -> return . Left $ error
      Nothing -> do
        let events = branch ^. bweEvents
        return . Right $ events
  where
    getBranch callback = do
      eitherBranch <- findBranchWithEventsById context branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error

getNewerPackages :: Context -> String -> IO (Either AppError [Package])
getNewerPackages context currentPkgId =
  getPackages $ \packages -> do
    let packagesWithHigherVersion = filter (\pkg -> isNothing $ isVersionHigher (pkg ^. pkgVersion) version) packages
    return . Right . sortPackagesByVersion $ packagesWithHigherVersion
  where
    getPackages callback = do
      eitherPackages <- findPackageByGroupIdAndArtifactId context groupId artifactId
      case eitherPackages of
        Right packages -> callback packages
        Left error -> return . Left $ error
    groupId = T.unpack $ splitPackageId currentPkgId !! 0
    artifactId = T.unpack $ splitPackageId currentPkgId !! 1
    version = T.unpack $ splitPackageId currentPkgId !! 2

isVersionInValidFormat :: String -> Maybe AppError
isVersionInValidFormat version =
  if isJust $ matchRegex validationRegex version
    then Nothing
    else Just . createErrorWithErrorMessage $ _ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

isVersionHigher :: String -> String -> Maybe AppError
isVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . createErrorWithErrorMessage $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

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
    versionASplitted = splitVersion versionA
    versionBSplitted = splitVersion versionB
    versionAMajor = read . T.unpack $ (versionASplitted !! 0) :: Int
    versionAMinor = read . T.unpack $ (versionASplitted !! 1) :: Int
    versionAPatch = read . T.unpack $ (versionASplitted !! 2) :: Int
    versionBMajor = read . T.unpack $ (versionBSplitted !! 0) :: Int
    versionBMinor = read . T.unpack $ (versionBSplitted !! 1) :: Int
    versionBPatch = read . T.unpack $ (versionBSplitted !! 2) :: Int

sortPackagesByVersion :: [Package] -> [Package]
sortPackagesByVersion = sortBy (\p1 p2 -> compareVersionNeg (p1 ^. pkgVersion) (p2 ^. pkgVersion))

splitPackageId :: String -> [Text]
splitPackageId packageId = T.splitOn ":" (T.pack packageId)

splitVersion :: String -> [Text]
splitVersion version = T.splitOn "." (T.pack version)

validatePackagesDeletation :: Context -> [String] -> IO (Maybe AppError)
validatePackagesDeletation context pkgIdsToDelete =
  foldl foldOne (return Nothing) (validateOnePackage <$> pkgIdsToDelete)
  where
    foldOne :: IO (Maybe AppError) -> IO (Maybe AppError) -> IO (Maybe AppError)
    foldOne accIO resultIO = do
      acc <- accIO
      if isJust acc
        then accIO
        else resultIO
    validateOnePackage :: String -> IO (Maybe AppError)
    validateOnePackage pkgId = do
      eitherBranches <-
        findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId context pkgId
      case eitherBranches of
        Right [] -> do
          eitherPkgs <- findPackagesByParentPackageId context pkgId
          case eitherPkgs of
            Right [] -> return Nothing
            Right pkgs -> do
              if length (filter (filFun) pkgs) > 0
                then return . Just . createErrorWithErrorMessage $
                     _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "package"
                else return Nothing
            Left error -> return . Just $ error
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "branch"
        Left error -> return . Just $ error
    filFun :: Package -> Bool
    filFun p = not ((p ^. pkgId) `elem` pkgIdsToDelete)

validatePackageDeletation :: Context -> String -> IO (Maybe AppError)
validatePackageDeletation context pkgId = do
  eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId context pkgId
  case eitherBranches of
    Right [] -> do
      eitherPkgs <- findPackagesByParentPackageId context pkgId
      case eitherPkgs of
        Right [] -> return Nothing
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "package"
        Left error -> return . Just $ error
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId "branch"
    Left error -> return . Just $ error
