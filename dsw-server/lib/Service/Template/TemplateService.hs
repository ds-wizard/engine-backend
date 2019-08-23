module Service.Template.TemplateService
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
import Control.Monad.Reader (liftIO)
import Data.List (find)
import qualified Data.UUID as U
import qualified Text.Ginger as Q

import Api.Resource.Template.TemplateDTO
import Api.Resource.Template.TemplateJM ()
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.File.FileService
import Util.List (foldEithersInContext)
import Util.Template (mLoadFile)

templateFolder = "templates/dmp"

listTemplates :: AppContextM (Either AppError [TemplateDTO])
listTemplates = do
  files <- liftIO $ listFilesWithExtension templateFolder "json"
  foldEithersInContext $ (liftIO . loadJSONFile) <$> (\f -> templateFolder ++ "/" ++ f) <$> files

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
  eTemplate <- liftIO $ Q.parseGingerFile mLoadFile (templateFolder ++ "/" ++ fileName)
  case eTemplate of
    Right template -> return . Right $ template
    Left error ->
      return . Left . GeneralServerError $
      _ERROR_SERVICE_TEMPLATE__LOADING_TEMPLATE_FAILED (Q.formatParserError Nothing error)

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
