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
import Api.Resource.Version.VersionDTO
import Common.Error
import Common.Localization
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Model.Context.AppContext
import Model.Event.Event
import Model.Migrator.MigratorState
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Organization.OrganizationService
import Service.Package.PackageMapper

getPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageDTO])
getPackagesFiltered queryParams = do
  eitherPackages <- findPackagesFiltered queryParams
  case eitherPackages of
    Right packages -> return . Right . fmap packageToDTO $ packages
    Left error -> return . Left $ error

getSimplePackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams = do
  eitherPackages <- findPackagesFiltered queryParams
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

getPackageById :: String -> AppContextM (Either AppError PackageDTO)
getPackageById pkgPId = do
  eitherPackage <- findPackageById pkgPId
  case eitherPackage of
    Right package -> return . Right . packageToDTO $ package
    Left error -> return . Left $ error

getPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEventsDTO)
getPackageWithEventsById pkgPId = do
  eitherPackage <- findPackageWithEventsById pkgPId
  case eitherPackage of
    Right package -> return . Right . packageWithEventsToDTOWithEvents $ package
    Left error -> return . Left $ error

createPackage :: String -> String -> String -> String -> String -> Maybe String -> [Event] -> AppContextM PackageDTO
createPackage name organizationId kmId version description maybeParentPackageId events = do
  let package = buildPackage name organizationId kmId version description maybeParentPackageId events
  insertPackage package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: String -> String -> VersionDTO -> AppContextM (Either AppError PackageDTO)
createPackageFromKMC branchUuid pkgVersion versionDto =
  validateVersionFormat pkgVersion $
  getBranch branchUuid $ \branch ->
    getCurrentOrganization $ \organization ->
      validateVersion pkgVersion branch organization $
      getEventsForPackage branch $ \events -> do
        let pkgName = branch ^. name
        let pkgOrganizationId = organization ^. organizationId
        let pkgKmId = branch ^. kmId
        let mPpId = branch ^. parentPackageId
        let pkgDescription = versionDto ^. description
        createdPackage <- createPackage pkgName pkgOrganizationId pkgKmId pkgVersion pkgDescription mPpId events
        deleteEventsAtBranch branchUuid
        updateBranchWithParentPackageId branchUuid (createdPackage ^. pId)
        updateBranchIfMigrationIsCompleted branchUuid
        deleteMigratorStateByBranchUuid branchUuid
        recompileKnowledgeModel branch $ return . Right $ createdPackage
  where
    validateVersionFormat pkgVersion callback =
      case isVersionInValidFormat pkgVersion of
        Nothing -> callback
        Just error -> return . Left $ error
    getBranch branchUuid callback = do
      eitherBranch <- findBranchWithEventsById branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error
    getCurrentOrganization callback = do
      eitherOrganization <- getOrganization
      case eitherOrganization of
        Right organization -> callback organization
        Left error -> return . Left $ error
    validateVersion pkgVersion branch organization callback = do
      let pkgOrganizationId = organization ^. organizationId
      let pkgKmId = branch ^. kmId
      eitherMaybePackage <- getTheNewestPackageByOrganizationIdAndKmId pkgOrganizationId pkgKmId
      case eitherMaybePackage of
        Right (Just package) ->
          case isVersionHigher pkgVersion (package ^. version) of
            Nothing -> callback
            Just error -> return . Left $ error
        Right Nothing -> callback
        Left error -> return . Left $ error
    updateBranchIfMigrationIsCompleted branchUuid = do
      eitherMigrationState <- findMigratorStateByBranchUuid branchUuid
      case eitherMigrationState of
        Right migrationState -> do
          let branchParentId = migrationState ^. msBranchParentId
          let targetPackageId = migrationState ^. msTargetPackageId
          updateBranchWithMigrationInfo branchUuid targetPackageId branchParentId
        Left _ -> return ()
    getEventsForPackage branch callback = do
      let branchUuid = U.toString $ branch ^. uuid
      eitherMigrationState <- findMigratorStateByBranchUuid branchUuid
      case eitherMigrationState of
        Right migrationState -> callback $ migrationState ^. msResultEvents
        Left (NotExistsError _) -> callback $ branch ^. events
        Left error -> return . Left $ error
    recompileKnowledgeModel branch callback = do
      let branchUuid = U.toString $ branch ^. uuid
      eitherEventsForUuid <- getEventsForBranchUuid branchUuid
      case eitherEventsForUuid of
        Right eventsForBranchUuid -> do
          recompileKnowledgeModelWithEvents branchUuid eventsForBranchUuid
          callback
        Left error -> return . Left $ error

importPackage :: BS.ByteString -> AppContextM (Either AppError PackageDTO)
importPackage fileContent = do
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
          createdPkg <- createPackage pName pOrganizationId pKmId pVersion pDescription pParentPackageId pEvents
          return . Right $ createdPkg
    Left error -> return . Left . createErrorWithErrorMessage $ error
  where
    validatePackageId pkgPId callback = do
      eitherPackage <- findPackageById pkgPId
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Left . createErrorWithErrorMessage $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgPId
        Left error -> return . Left $ error
    validateParentPackageId pkgPId maybeParentPkgId callback =
      case maybeParentPkgId of
        Just parentPkgId -> do
          eitherPackage <- findPackageById parentPkgId
          case eitherPackage of
            Right _ -> callback
            Left (NotExistsError _) ->
              return . Left . createErrorWithErrorMessage $
              _ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgPId
            Left error -> return . Left $ error
        Nothing -> callback

deletePackagesByQueryParams :: [(Text, Text)] -> AppContextM (Maybe AppError)
deletePackagesByQueryParams queryParams = do
  eitherPackages <- findPackagesFiltered queryParams
  case eitherPackages of
    Right packages -> do
      maybeError <- validatePackagesDeletation (_packagePId <$> packages)
      if isJust maybeError
        then return maybeError
        else do
          deletePackagesFiltered queryParams
          return Nothing
    Left error -> return . Just $ error

deletePackage :: String -> AppContextM (Maybe AppError)
deletePackage pkgPId = do
  eitherPackage <- findPackageById pkgPId
  case eitherPackage of
    Right package -> do
      maybeError <- validatePackageDeletation pkgPId
      if isJust maybeError
        then return maybeError
        else do
          deletePackageById pkgPId
          return Nothing
    Left error -> return . Just $ error

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError (Maybe Package))
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId = do
  eitherPackages <- findPackageByOrganizationIdAndKmId organizationId kmId
  case eitherPackages of
    Right packages ->
      if length packages == 0
        then return . Right $ Nothing
        else do
          let sorted = sortPackagesByVersion packages
          return . Right . Just . head $ sorted
    Left error -> return . Left $ error

getAllPreviousEventsSincePackageId :: String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageId pkgPId = do
  eitherPackage <- findPackageWithEventsById pkgPId
  case eitherPackage of
    Right package ->
      case package ^. parentPackageId of
        Just parentPackageId -> do
          eitherPkgEvents <- getAllPreviousEventsSincePackageId parentPackageId
          case eitherPkgEvents of
            Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
            Left error -> return . Left $ error
        Nothing -> return . Right $ package ^. events
    Left error -> return . Left $ error

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgPId =
      if pkgPId == untilPkgId
        then return . Right $ []
        else do
          eitherPackage <- findPackageWithEventsById pkgPId
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

getEventsForBranchUuid :: String -> AppContextM (Either AppError [Event])
getEventsForBranchUuid branchUuid =
  getBranch $ \branch ->
    case branch ^. parentPackageId of
      Just ppId -> do
        eitherEventsFromPackage <- getAllPreviousEventsSincePackageId ppId
        case eitherEventsFromPackage of
          Right eventsFromPackage -> do
            let eventsFromKM = branch ^. events
            let pkgEvents = eventsFromPackage ++ eventsFromKM
            return . Right $ pkgEvents
          Left error -> return . Left $ error
      Nothing -> return . Right $ branch ^. events
  where
    getBranch callback = do
      eitherBranch <- findBranchWithEventsById branchUuid
      case eitherBranch of
        Right branch -> callback branch
        Left error -> return . Left $ error

getNewerPackages :: String -> AppContextM (Either AppError [Package])
getNewerPackages currentPkgId =
  getPackages $ \packages -> do
    let packagesWithHigherVersion = filter (\pkg -> isNothing $ isVersionHigher (pkg ^. version) pkgVersion) packages
    return . Right . sortPackagesByVersion $ packagesWithHigherVersion
  where
    getPackages callback = do
      eitherPackages <- findPackageByOrganizationIdAndKmId pkgOrganizationId pkgKmId
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

validatePackagesDeletation :: [String] -> AppContextM (Maybe AppError)
validatePackagesDeletation pkgPIdsToDelete = foldl foldOne (return Nothing) (validateOnePackage <$> pkgPIdsToDelete)
  where
    foldOne :: AppContextM (Maybe AppError) -> AppContextM (Maybe AppError) -> AppContextM (Maybe AppError)
    foldOne accIO resultIO = do
      acc <- accIO
      if isJust acc
        then accIO
        else resultIO
    validateOnePackage :: String -> AppContextM (Maybe AppError)
    validateOnePackage pkgPId = do
      eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId pkgPId
      case eitherBranches of
        Right [] -> do
          eitherPkgs <- findPackagesByParentPackageId pkgPId
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

validatePackageDeletation :: String -> AppContextM (Maybe AppError)
validatePackageDeletation pkgPId = do
  eitherBranches <- findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId pkgPId
  case eitherBranches of
    Right [] -> do
      eitherPkgs <- findPackagesByParentPackageId pkgPId
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
