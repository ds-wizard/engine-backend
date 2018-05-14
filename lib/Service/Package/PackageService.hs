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
                   equalSameKmId (newPackage ^. kmId) pkg && equalSameOrganizationId (newPackage ^. organizationId) pkg)
                packages
            hasSameKmId :: Package -> Package -> Bool
            hasSameKmId pkg1 pkg2 = pkg1 ^. kmId == pkg2 ^. kmId
            equalSameKmId :: String -> Package -> Bool
            equalSameKmId pkgKmId pkg = pkgKmId == pkg ^. kmId
            equalSameOrganizationId :: String -> Package -> Bool
            equalSameOrganizationId pkgOrganizationId pkg = pkgOrganizationId == pkg ^. organizationId
    Left error -> return . Left $ error

getPackageById :: Context -> String -> IO (Either AppError PackageDTO)
getPackageById context pkgPId = do
  eitherPackage <- findPackageById context pkgPId
  case eitherPackage of
    Right package -> return . Right . packageToDTO $ package
    Left error -> return . Left $ error

getPackageWithEventsById :: Context -> String -> IO (Either AppError PackageWithEventsDTO)
getPackageWithEventsById context pkgPId = do
  eitherPackage <- findPackageWithEventsById context pkgPId
  case eitherPackage of
    Right package -> return . Right . packageWithEventsToDTOWithEvents $ package
    Left error -> return . Left $ error

createPackage :: Context -> String -> String -> String -> String -> String -> Maybe String -> [Event] -> IO PackageDTO
createPackage context name organizationId kmId version description maybeParentPackageId events = do
  let package = buildPackage name organizationId kmId version description maybeParentPackageId events
  insertPackage context package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: Context -> String -> String -> String -> IO (Either AppError PackageDTO)
createPackageFromKMC context branchUuid pkgVersion pkgDescription =
  validateVersionFormat pkgVersion $
  getBranch branchUuid $ \branch ->
    getCurrentOrganization $ \organization ->
      validateVersion pkgVersion branch organization $
      getEventsForPackage context branch $ \events -> do
        let pkgName = branch ^. bweName
        let pkgOrganizationId = organization ^. organizationId
        let pkgKmId = branch ^. bweKmId
        let mPpId = branch ^. bweParentPackageId
        createdPackage <- createPackage context pkgName pkgOrganizationId pkgKmId pkgVersion pkgDescription mPpId events
        deleteEventsAtBranch context branchUuid
        updateBranchWithParentPackageId context branchUuid (createdPackage ^. pId)
        updateBranchIfMigrationIsCompleted context branchUuid
        deleteMigratorStateByBranchUuid context branchUuid
        recompileKnowledgeModel context branch $ return . Right $ createdPackage
  where
    validateVersionFormat pkgVersion callback =
      case isVersionInValidFormat pkgVersion of
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
    validateVersion pkgVersion branch organization callback = do
      let pkgOrganizationId = organization ^. organizationId
      let pkgKmId = branch ^. bweKmId
      eitherMaybePackage <- getTheNewestPackageByOrganizationIdAndKmId context pkgOrganizationId pkgKmId
      case eitherMaybePackage of
        Right (Just package) ->
          case isVersionHigher pkgVersion (package ^. version) of
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
      let pName = packageWithEvents ^. name
      let pOrganizationId = packageWithEvents ^. organizationId
      let pKmId = packageWithEvents ^. kmId
      let pVersion = packageWithEvents ^. version
      let pDescription = packageWithEvents ^. description
      let pParentPackageId = packageWithEvents ^. parentPackageId
      let pEvents = packageWithEvents ^. events
      let pId = buildPackageId pOrganizationId pKmId pVersion
      validatePackageId pId $
        validateParentPackageId pId pParentPackageId $ do
          createdPkg <- createPackage context pName pOrganizationId pKmId pVersion pDescription pParentPackageId pEvents
          return . Right $ createdPkg
    Left error -> return . Left . createErrorWithErrorMessage $ error
  where
    validatePackageId pkgPId callback = do
      eitherPackage <- findPackageById context pkgPId
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Left . createErrorWithErrorMessage $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgPId
        Left error -> return . Left $ error
    validateParentPackageId pkgPId maybeParentPkgId callback =
      case maybeParentPkgId of
        Just parentPkgId -> do
          eitherPackage <- findPackageById context parentPkgId
          case eitherPackage of
            Right _ -> callback
            Left (NotExistsError _) ->
              return . Left . createErrorWithErrorMessage $
              _ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgPId
            Left error -> return . Left $ error
        Nothing -> callback

deletePackagesByQueryParams :: Context -> [(Text, Text)] -> IO (Maybe AppError)
deletePackagesByQueryParams context queryParams = do
  eitherPackages <- findPackagesFiltered context queryParams
  case eitherPackages of
    Right packages -> do
      maybeError <- validatePackagesDeletation context (_packagePId <$> packages)
      if isJust maybeError
        then return maybeError
        else do
          deletePackagesFiltered context queryParams
          return Nothing
    Left error -> return . Just $ error

deletePackage :: Context -> String -> IO (Maybe AppError)
deletePackage context pkgPId = do
  eitherPackage <- findPackageById context pkgPId
  case eitherPackage of
    Right package -> do
      maybeError <- validatePackageDeletation context pkgPId
      if isJust maybeError
        then return maybeError
        else do
          deletePackageById context pkgPId
          return Nothing
    Left error -> return . Just $ error

getTheNewestPackageByOrganizationIdAndKmId :: Context -> String -> String -> IO (Either AppError (Maybe Package))
getTheNewestPackageByOrganizationIdAndKmId context organizationId kmId = do
  eitherPackages <- findPackageByOrganizationIdAndKmId context organizationId kmId
  case eitherPackages of
    Right packages ->
      if length packages == 0
        then return . Right $ Nothing
        else do
          let sorted = sortPackagesByVersion packages
          return . Right . Just . head $ sorted
    Left error -> return . Left $ error

getAllPreviousEventsSincePackageId :: Context -> String -> IO (Either AppError [Event])
getAllPreviousEventsSincePackageId context pkgPId = do
  eitherPackage <- findPackageWithEventsById context pkgPId
  case eitherPackage of
    Right package ->
      case package ^. parentPackageId of
        Just parentPackageId -> do
          eitherPkgEvents <- getAllPreviousEventsSincePackageId context parentPackageId
          case eitherPkgEvents of
            Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
            Left error -> return . Left $ error
        Nothing -> return . Right $ package ^. events
    Left error -> return . Left $ error

getAllPreviousEventsSincePackageIdAndUntilPackageId :: Context -> String -> String -> IO (Either AppError [Event])
getAllPreviousEventsSincePackageIdAndUntilPackageId context sincePkgId untilPkgId = go sincePkgId
  where
    go pkgPId =
      if pkgPId == untilPkgId
        then return . Right $ []
        else do
          eitherPackage <- findPackageWithEventsById context pkgPId
          case eitherPackage of
            Right package ->
              case package ^. parentPackageId of
                Just parentPackageId -> do
                  eitherPkgEvents <- go parentPackageId
                  case eitherPkgEvents of
                    Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
                    Left error -> return . Left $ error
                Nothing -> return . Right $ package ^. events
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
            let pkgEvents = eventsFromPackage ++ eventsFromKM
            return . Right $ pkgEvents
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
    let packagesWithHigherVersion = filter (\pkg -> isNothing $ isVersionHigher (pkg ^. version) pkgVersion) packages
    return . Right . sortPackagesByVersion $ packagesWithHigherVersion
  where
    getPackages callback = do
      eitherPackages <- findPackageByOrganizationIdAndKmId context pkgOrganizationId pkgKmId
      case eitherPackages of
        Right packages -> callback packages
        Left error -> return . Left $ error
    pkgOrganizationId = T.unpack $ splitPackageId currentPkgId !! 0
    pkgKmId = T.unpack $ splitPackageId currentPkgId !! 1
    pkgVersion = T.unpack $ splitPackageId currentPkgId !! 2

isVersionInValidFormat :: String -> Maybe AppError
isVersionInValidFormat pkgVersion =
  if isJust $ matchRegex validationRegex pkgVersion
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
sortPackagesByVersion = sortBy (\p1 p2 -> compareVersionNeg (p1 ^. version) (p2 ^. version))

splitPackageId :: String -> [Text]
splitPackageId packageId = T.splitOn ":" (T.pack packageId)

splitVersion :: String -> [Text]
splitVersion pkgVersion = T.splitOn "." (T.pack pkgVersion)

validatePackagesDeletation :: Context -> [String] -> IO (Maybe AppError)
validatePackagesDeletation context pkgPIdsToDelete =
  foldl foldOne (return Nothing) (validateOnePackage <$> pkgPIdsToDelete)
  where
    foldOne :: IO (Maybe AppError) -> IO (Maybe AppError) -> IO (Maybe AppError)
    foldOne accIO resultIO = do
      acc <- accIO
      if isJust acc
        then accIO
        else resultIO
    validateOnePackage :: String -> IO (Maybe AppError)
    validateOnePackage pkgPId = do
      eitherBranches <-
        findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId context pkgPId
      case eitherBranches of
        Right [] -> do
          eitherPkgs <- findPackagesByParentPackageId context pkgPId
          case eitherPkgs of
            Right [] -> return Nothing
            Right pkgs -> do
              if length (filter (filFun) pkgs) > 0
                then return . Just . createErrorWithErrorMessage $
                     _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgPId "package"
                else return Nothing
            Left error -> return . Just $ error
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgPId "branch"
        Left error -> return . Just $ error
    filFun :: Package -> Bool
    filFun p = not ((p ^. pId) `elem` pkgPIdsToDelete)

validatePackageDeletation :: Context -> String -> IO (Maybe AppError)
validatePackageDeletation context pkgPId = do
  eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId context pkgPId
  case eitherBranches of
    Right [] -> do
      eitherPkgs <- findPackagesByParentPackageId context pkgPId
      case eitherPkgs of
        Right [] -> return Nothing
        Right _ ->
          return . Just . createErrorWithErrorMessage $
          _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgPId "package"
        Left error -> return . Just $ error
    Right _ ->
      return . Just . createErrorWithErrorMessage $
      _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgPId "branch"
    Left error -> return . Just $ error
