module Wizard.Service.Template.TemplateService
  ( listTemplates
  , getTemplateByUuid
  , getTemplateByUuidOrFirst
  , loadTemplateFile
  -- Private
  , fitsIntoKMSpec
  , filterTemplates
  -- Helpers
  , heListTemplates
  , heGetTemplateByUuid
  , heGetTemplateByUuidOrFirst
  , heLoadTemplateFile
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.List (find)
import qualified Data.UUID as U
import qualified Text.Ginger as Q

import LensesConfig
import Shared.Model.Error.Error
import Shared.Service.File.FileService
import Shared.Util.Helper (createHeeHelper)
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Constant.Resource
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Package.PackageValidation
import Wizard.Util.List (foldEithersInContext)
import Wizard.Util.Template (mLoadFile)

listTemplates :: Maybe String -> AppContextM (Either AppError [TemplateDTO])
listTemplates mPkgId = do
  folder <- getTemplateFolder
  files <- liftIO $ listFilesWithExtension folder "json"
  eTemplates <- foldEithersInContext ((liftIO . loadJSONFile) . (\f -> folder ++ "/" ++ f) <$> files)
  case mPkgId of
    Nothing -> return eTemplates
    Just pkgId ->
      heValidatePackageIdFormat pkgId $ do
        let pkgIdSplit = splitPackageId pkgId
        case eTemplates of
          Right templates -> return . Right . filterTemplates pkgIdSplit $ templates
          Left error -> return . Left $ error

getTemplateByUuid :: String -> Maybe String -> AppContextM (Either AppError TemplateDTO)
getTemplateByUuid templateUuid mPkgId =
  heListTemplates mPkgId $ \templates ->
    case find (\t -> U.toString (t ^. uuid) == templateUuid) templates of
      Just template -> return . Right $ template
      Nothing -> return . Left . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getTemplateByUuidOrFirst :: Maybe String -> Maybe String -> AppContextM (Either AppError TemplateDTO)
getTemplateByUuidOrFirst mTemplateUuid mPkgId =
  case mTemplateUuid of
    Just templateUuid -> getTemplateByUuid templateUuid mPkgId
    Nothing ->
      heListTemplates mPkgId $ \templates ->
        if not (null templates)
          then return . Right . head $ templates
          else return . Left . GeneralServerError $ _ERROR_SERVICE_TEMPLATE__NO_TEMPLATES_IN_SYSTEM

loadTemplateFile :: String -> AppContextM (Either AppError (Q.Template Q.SourcePos))
loadTemplateFile fileName = do
  folder <- getTemplateFolder
  eTemplate <- liftIO $ Q.parseGingerFile mLoadFile (folder ++ "/" ++ fileName)
  case eTemplate of
    Right template -> return . Right $ template
    Left error ->
      return . Left . GeneralServerError $
      _ERROR_SERVICE_TEMPLATE__LOADING_TEMPLATE_FAILED (Q.formatParserError Nothing error)

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateFolder :: AppContextM String
getTemplateFolder = do
  appConfig <- asks _appContextApplicationConfig
  return $ (appConfig ^. general . templateFolder) ++ documentTemplatesFolder

filterTemplates :: [String] -> [TemplateDTO] -> [TemplateDTO]
filterTemplates pkgIdSplit = filter (filterTemplate pkgIdSplit)
  where
    filterTemplate :: [String] -> TemplateDTO -> Bool
    filterTemplate pkgIdSplit template = foldl (foldOverKmSpec pkgIdSplit) False (template ^. allowedKMs)
    foldOverKmSpec :: [String] -> Bool -> TemplateAllowedKMDTO -> Bool
    foldOverKmSpec pkgIdSplit acc allowedKm = acc || fitsIntoKMSpec pkgIdSplit allowedKm

fitsIntoKMSpec ::
     ( HasOrgId kmSpec (Maybe String)
     , HasKmId kmSpec (Maybe String)
     , HasMinVersion kmSpec (Maybe String)
     , HasMaxVersion kmSpec (Maybe String)
     )
  => [String]
  -> kmSpec
  -> Bool
fitsIntoKMSpec pkgIdSplit kmSpec = heCompareOrgId $ heCompareKmId $ heCompareVersionMin $ heCompareVersionMax True
  where
    heCompareOrgId callback =
      case kmSpec ^. orgId of
        Just orgId -> (head pkgIdSplit == orgId) && callback
        Nothing -> callback
    heCompareKmId callback =
      case kmSpec ^. kmId of
        Just kmId -> ((pkgIdSplit !! 1) == kmId) && callback
        Nothing -> callback
    heCompareVersionMin callback =
      case kmSpec ^. minVersion of
        Just minVersion ->
          case compareVersion (pkgIdSplit !! 2) minVersion of
            LT -> False
            _ -> callback
        Nothing -> callback
    heCompareVersionMax callback =
      case kmSpec ^. maxVersion of
        Just maxVersion ->
          case compareVersion (pkgIdSplit !! 2) maxVersion of
            GT -> False
            _ -> callback
        Nothing -> callback

-- --------------------------------
-- HELPERS
-- --------------------------------
heListTemplates mPkgId = createHeeHelper (listTemplates mPkgId)

-- -----------------------------------------------------
heGetTemplateByUuid templateUuid mPkgId = createHeeHelper (getTemplateByUuid templateUuid mPkgId)

-- -----------------------------------------------------
heGetTemplateByUuidOrFirst mTemplateUuid mPkgId = createHeeHelper (getTemplateByUuidOrFirst mTemplateUuid mPkgId)

-- -----------------------------------------------------
heLoadTemplateFile fileName = createHeeHelper (loadTemplateFile fileName)
