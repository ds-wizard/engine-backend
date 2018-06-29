module Service.Package.PackageService where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID as U

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
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Organization.OrganizationService
import Service.Package.PackageMapper
import Service.Package.PackageUtils
import Service.Package.PackageValidation

getPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageDTO])
getPackagesFiltered queryParams =
  heFindPackagesFiltered queryParams $ \packages -> return . Right . fmap packageToDTO $ packages

getSimplePackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams = do
  heFindPackagesFiltered queryParams $ \packages -> do
    let uniquePackages = makePackagesUnique packages
    return . Right . fmap packageToSimpleDTO $ uniquePackages
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
      find
        (\pkg -> equalSameKmId (newPackage ^. kmId) pkg && equalSameOrganizationId (newPackage ^. organizationId) pkg)
        packages
    hasSameKmId :: Package -> Package -> Bool
    hasSameKmId pkg1 pkg2 = pkg1 ^. kmId == pkg2 ^. kmId
    equalSameKmId :: String -> Package -> Bool
    equalSameKmId pkgKmId pkg = pkgKmId == pkg ^. kmId
    equalSameOrganizationId :: String -> Package -> Bool
    equalSameOrganizationId pkgOrganizationId pkg = pkgOrganizationId == pkg ^. organizationId

getPackageById :: String -> AppContextM (Either AppError PackageDTO)
getPackageById pkgId = heFindPackageById pkgId $ \package -> return . Right . packageToDTO $ package

getPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEventsDTO)
getPackageWithEventsById pkgId =
  heFindPackageWithEventsById pkgId $ \package -> return . Right . packageWithEventsToDTOWithEvents $ package

createPackage :: String -> String -> String -> String -> String -> Maybe String -> [Event] -> AppContextM PackageDTO
createPackage name organizationId kmId version description maybeParentPackageId events = do
  let package = buildPackage name organizationId kmId version description maybeParentPackageId events
  insertPackage package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: String -> String -> VersionDTO -> AppContextM (Either AppError PackageDTO)
createPackageFromKMC branchUuid pkgVersion versionDto =
  heValidateVersionFormat pkgVersion $
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
          case validateIsVersionHigher pkgVersion (package ^. version) of
            Nothing -> callback
            Just error -> return . Left $ error
        Right Nothing -> callback
        Left error -> return . Left $ error
    updateBranchIfMigrationIsCompleted branchUuid = do
      eitherMigrationState <- findMigratorStateByBranchUuid branchUuid
      case eitherMigrationState of
        Right migrationState -> do
          let msBranchParentId = migrationState ^. branchParentId
          let msTargetPackageId = migrationState ^. targetPackageId
          updateBranchWithMigrationInfo branchUuid msTargetPackageId msBranchParentId
        Left _ -> return ()
    getEventsForPackage branch callback = do
      let branchUuid = U.toString $ branch ^. uuid
      eitherMigrationState <- findMigratorStateByBranchUuid branchUuid
      case eitherMigrationState of
        Right migrationState -> callback $ migrationState ^. resultEvents
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
        validateParentPackageId pId pParentPackageId $
        validateKmValidity pId pParentPackageId pEvents $ do
          createdPkg <- createPackage pName pOrganizationId pKmId pVersion pDescription pParentPackageId pEvents
          return . Right $ createdPkg
    Left error -> return . Left . createErrorWithErrorMessage $ error
  where
    validatePackageId pkgId callback = do
      eitherPackage <- findPackageById pkgId
      case eitherPackage of
        Left (NotExistsError _) -> callback
        Right _ -> return . Left . createErrorWithErrorMessage $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId
        Left error -> return . Left $ error
    validateParentPackageId pkgId maybeParentPkgId callback =
      case maybeParentPkgId of
        Just parentPkgId -> do
          eitherPackage <- findPackageById parentPkgId
          case eitherPackage of
            Right _ -> callback
            Left (NotExistsError _) ->
              return . Left . createErrorWithErrorMessage $
              _ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId
            Left error -> return . Left $ error
        Nothing -> callback
    validateKmValidity pkgId maybeParentPkgId pkgEvents callback =
      case maybeParentPkgId of
        Just ppId -> do
          eitherEventsFromPackage <- getAllPreviousEventsSincePackageId ppId
          case eitherEventsFromPackage of
            Right eventsFromPackage -> do
              case compileKnowledgeModelFromScratch $ eventsFromPackage ++ pkgEvents of
                Right _ -> callback
                Left error -> return . Left $ error
            Left error -> return . Left $ error
        Nothing ->
          case compileKnowledgeModelFromScratch $ pkgEvents of
            Right _ -> callback
            Left error -> return . Left $ error

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
deletePackage pkgId =
  hmFindPackageById pkgId $ \package -> do
    maybeError <- validatePackageDeletation pkgId
    if isJust maybeError
      then return maybeError
      else do
        deletePackageById pkgId
        return Nothing

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError (Maybe Package))
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId =
  heFindPackagesByOrganizationIdAndKmId organizationId kmId $ \packages -> do
    if length packages == 0
      then return . Right $ Nothing
      else do
        let sorted = sortPackagesByVersion packages
        return . Right . Just . head $ sorted

getAllPreviousEventsSincePackageId :: String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageId pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. parentPackageId of
      Just parentPackageId -> do
        eitherPkgEvents <- getAllPreviousEventsSincePackageId parentPackageId
        case eitherPkgEvents of
          Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
          Left error -> return . Left $ error
      Nothing -> return . Right $ package ^. events

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return . Right $ []
        else heFindPackageWithEventsById pkgId $ \package ->
               case package ^. parentPackageId of
                 Just parentPackageId -> do
                   eitherPkgEvents <- go parentPackageId
                   case eitherPkgEvents of
                     Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
                     Left error -> return . Left $ error
                 Nothing -> return . Right $ package ^. events

getEventsForBranchUuid :: String -> AppContextM (Either AppError [Event])
getEventsForBranchUuid branchUuid =
  heGetBranch branchUuid $ \branch ->
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

getNewerPackages :: String -> AppContextM (Either AppError [Package])
getNewerPackages currentPkgId =
  getPackages $ \packages -> do
    let packagesWithHigherVersion =
          filter (\pkg -> isNothing $ validateIsVersionHigher (pkg ^. version) pkgVersion) packages
    return . Right . sortPackagesByVersion $ packagesWithHigherVersion
  where
    getPackages callback =
      heFindPackagesByOrganizationIdAndKmId pkgOrganizationId pkgKmId $ \packages -> callback packages
    pkgOrganizationId = T.unpack $ splitPackageId currentPkgId !! 0
    pkgKmId = T.unpack $ splitPackageId currentPkgId !! 1
    pkgVersion = T.unpack $ splitPackageId currentPkgId !! 2
