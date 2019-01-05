module Service.Template.TemplateService where

import Control.Monad.Reader (liftIO)
import Data.Aeson (Value, decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Strict (HashMap, fromList)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.FromHTML as FromHTML
import Text.Ginger (formatParserError, parseGingerFile)

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Localization
import Model.Context.AppContext
import Model.DataManagementPlan.DataManagementPlan
import Model.Error.Error
import Service.Template.TemplateMapper (heFormatToToHTMLType)
import Service.Template.TemplateUtils (mLoadFile, render)

templateFile = "template/root.html.jinja"

sampleContext :: DataManagementPlanDTO -> HashMap T.Text (HashMap T.Text Value)
sampleContext dmp = fromList [("dmp", fromJust . decode . encode $ dmp)]

generateTemplateInFormat ::
     DataManagementPlanFormat -> DataManagementPlanDTO -> AppContextM (Either AppError BS.ByteString)
generateTemplateInFormat format dmp =
  heFormatToToHTMLType format $ \toHTMLType ->
    heGenerateTemplate dmp $ \document -> do
      eResult <- liftIO . FromHTML.fromHTML toHTMLType . T.unpack $ document
      case eResult of
        Right result -> return . Right . BS.fromStrict $ result
        Left err ->
          return . Left . GeneralServerError $
          _ERROR_SERVICE_TEMPLATE__TRANSFORMATION_FAILED (T.unpack . E.decodeUtf8 $ err)

generateTemplate :: DataManagementPlanDTO -> AppContextM (Either AppError T.Text)
generateTemplate dmp = do
  eTemplate <- liftIO $ parseGingerFile mLoadFile templateFile
  case eTemplate of
    Right template -> return . Right $ render template (sampleContext dmp)
    Left error ->
      return . Left . GeneralServerError $
      _ERROR_SERVICE_TEMPLATE__LOADING_TEMPLATE_FAILED (formatParserError Nothing error)

-- --------------------------------
-- HELPERS
-- --------------------------------
heGenerateTemplate dmp callback = do
  eDocument <- generateTemplate dmp
  case eDocument of
    Right document -> callback document
    Left error -> return . Left $ error
