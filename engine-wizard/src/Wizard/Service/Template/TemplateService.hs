module Wizard.Service.Template.TemplateService
  ( listTemplates
  , getTemplateByUuid
  , getTemplateByUuidOrFirst
  , loadTemplateFile
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
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Util.List (foldEithersInContext)
import Wizard.Util.Template (mLoadFile)

dmpFolder = "/dmp"

listTemplates :: AppContextM (Either AppError [TemplateDTO])
listTemplates = do
  folder <- getFolder
  files <- liftIO $ listFilesWithExtension folder "json"
  foldEithersInContext $ (liftIO . loadJSONFile) <$> (\f -> folder ++ "/" ++ f) <$> files

getTemplateByUuid :: String -> AppContextM (Either AppError TemplateDTO)
getTemplateByUuid templateUuid =
  heListTemplates $ \templates ->
    case find (\t -> (U.toString $ t ^. uuid) == templateUuid) templates of
      Just template -> return . Right $ template
      Nothing -> return . Left . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getTemplateByUuidOrFirst :: Maybe String -> AppContextM (Either AppError TemplateDTO)
getTemplateByUuidOrFirst mTemplateUuid =
  case mTemplateUuid of
    Just templateUuid -> getTemplateByUuid templateUuid
    Nothing ->
      heListTemplates $ \templates ->
        if length templates > 0
          then return . Right $ templates !! 0
          else return . Left . GeneralServerError $ _ERROR_SERVICE_TEMPLATE__NO_TEMPLATES_IN_SYSTEM

loadTemplateFile :: String -> AppContextM (Either AppError (Q.Template Q.SourcePos))
loadTemplateFile fileName = do
  folder <- getFolder
  eTemplate <- liftIO $ Q.parseGingerFile mLoadFile (folder ++ "/" ++ fileName)
  case eTemplate of
    Right template -> return . Right $ template
    Left error ->
      return . Left . GeneralServerError $
      _ERROR_SERVICE_TEMPLATE__LOADING_TEMPLATE_FAILED (Q.formatParserError Nothing error)

-- --------------------------------
-- PRIVATE
-- --------------------------------
getFolder :: AppContextM String
getFolder = do
  appConfig <- asks _appContextApplicationConfig
  return $ (appConfig ^. general . templateFolder) ++ dmpFolder

-- --------------------------------
-- HELPERS
-- --------------------------------
heListTemplates callback = do
  eTemplates <- listTemplates
  case eTemplates of
    Right templates -> callback templates
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetTemplateByUuid templateUuid callback = do
  eTemplate <- getTemplateByUuid templateUuid
  case eTemplate of
    Right template -> callback template
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetTemplateByUuidOrFirst mTemplateUuid callback = do
  eTemplate <- getTemplateByUuidOrFirst mTemplateUuid
  case eTemplate of
    Right template -> callback template
    Left error -> return . Left $ error

-- -----------------------------------------------------
heLoadTemplateFile fileName callback = do
  eTemplateFile <- loadTemplateFile fileName
  case eTemplateFile of
    Right templateFile -> callback templateFile
    Left error -> return . Left $ error
