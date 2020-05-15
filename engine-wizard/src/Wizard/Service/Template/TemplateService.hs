module Wizard.Service.Template.TemplateService
  ( getTemplates
  , getTemplatesDto
  , getTemplateByUuid
  -- Private
  , getAllowedPackagesForTemplate
  , fitsIntoKMSpec
  , filterTemplates
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.List (find)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Service.File.FileService
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Constant.Resource
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Template.Template
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Template.TemplateMapper
import Wizard.Util.List (foldEithersInContext)

getTemplates :: Maybe String -> AppContextM [Template]
getTemplates mPkgId = do
  folder <- getTemplateFolder
  files <- liftIO $ listFilesWithName folder "template.json"
  eTemplates <- foldEithersInContext (fmap (liftIO . loadJSONFile) files)
  case eTemplates of
    Left error -> throwError error
    Right templates ->
      case mPkgId of
        Nothing -> return templates
        Just pkgId -> do
          validatePackageIdFormat pkgId
          let pkgIdSplit = splitPackageId pkgId
          return . filterTemplates pkgIdSplit $ templates

getTemplatesDto :: Maybe String -> AppContextM [TemplateDTO]
getTemplatesDto mPkgId = do
  templates <- getTemplates mPkgId
  pkgs <- findPackages
  return . fmap (\tml -> toDTO (getAllowedPackagesForTemplate tml pkgs) tml) $ templates

getTemplateByUuid :: String -> Maybe String -> AppContextM Template
getTemplateByUuid templateUuid mPkgId = do
  templates <- getTemplates mPkgId
  case find (\t -> U.toString (t ^. uuid) == templateUuid) templates of
    Just template -> return template
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateFolder :: AppContextM String
getTemplateFolder = do
  serverConfig <- asks _appContextServerConfig
  return $ (serverConfig ^. general . templateFolder) ++ documentTemplatesFolder

getAllowedPackagesForTemplate :: Template -> [Package] -> [Package]
getAllowedPackagesForTemplate tml = getNewestUniquePackages . filterPackages tml
  where
    filterPackages :: Template -> [Package] -> [Package]
    filterPackages tml = filter (\pkg -> not . null $ filterTemplates (splitPackageId $ pkg ^. pId) [tml])

filterTemplates :: [String] -> [Template] -> [Template]
filterTemplates pkgIdSplit = filter (filterTemplate pkgIdSplit)
  where
    filterTemplate :: [String] -> Template -> Bool
    filterTemplate pkgIdSplit template = foldl (foldOverKmSpec pkgIdSplit) False (template ^. allowedPackages)
    foldOverKmSpec :: [String] -> Bool -> TemplateAllowedPackage -> Bool
    foldOverKmSpec pkgIdSplit acc allowedPackages = acc || fitsIntoKMSpec pkgIdSplit allowedPackages

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
