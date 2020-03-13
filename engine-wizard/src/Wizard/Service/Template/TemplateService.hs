module Wizard.Service.Template.TemplateService
  ( listTemplates
  , getTemplateByUuid
  -- Private
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
import Shared.Service.File.FileService
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Constant.Resource
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Package.PackageValidation
import Wizard.Util.List (foldEithersInContext)

listTemplates :: Maybe String -> AppContextM [TemplateDTO]
listTemplates mPkgId = do
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

getTemplateByUuid :: String -> Maybe String -> AppContextM TemplateDTO
getTemplateByUuid templateUuid mPkgId = do
  templates <- listTemplates mPkgId
  case find (\t -> U.toString (t ^. uuid) == templateUuid) templates of
    Just template -> return template
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateFolder :: AppContextM String
getTemplateFolder = do
  serverConfig <- asks _appContextApplicationConfig
  return $ (serverConfig ^. general . templateFolder) ++ documentTemplatesFolder

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
