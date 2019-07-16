module Service.Document.DocumentService
  ( generateDocument
  , generateDocumentInFormat
  -- Helpers
  , heGenerateDocument
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Aeson (Value, decode, encode)
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Strict (HashMap, fromList)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.FromHTML as FromHTML

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Api.Resource.DataManagementPlan.DataManagementPlanJM ()
import Api.Resource.Template.TemplateDTO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.DataManagementPlan.DataManagementPlan
import Model.Error.Error
import Service.Document.DocumentMapper (heFormatToToHTMLType)
import Service.Template.TemplateService
import Util.Template (render)

generateDocument :: TemplateDTO -> DataManagementPlanDTO -> AppContextM (Either AppError T.Text)
generateDocument template dmp = do
  heLoadTemplateFile (template ^. rootFile) $ \templateFile -> return . Right $ render templateFile (sampleContext dmp)

generateDocumentInFormat ::
     DataManagementPlanFormat -> TemplateDTO -> DataManagementPlanDTO -> AppContextM (Either AppError BS.ByteString)
generateDocumentInFormat format template dmp =
  heFormatToToHTMLType format $ \toHTMLType ->
    heGenerateDocument template dmp $ \document -> do
      eResult <- liftIO . FromHTML.fromHTML toHTMLType . T.unpack $ document
      case eResult of
        Right result -> return . Right . BS.fromStrict $ result
        Left err ->
          return . Left . GeneralServerError $
          _ERROR_SERVICE_DOCUMENT__TRANSFORMATION_FAILED (T.unpack . E.decodeUtf8 $ err)

-- --------------------------------
-- PRIVATE
-- --------------------------------
sampleContext :: DataManagementPlanDTO -> HashMap T.Text Value
sampleContext dmp = fromList [("dmp", fromMaybe emptyObject . decode . encode $ dmp)]

-- --------------------------------
-- HELPERS
-- --------------------------------
heGenerateDocument templateUuid dmp callback = do
  eDocument <- generateDocument templateUuid dmp
  case eDocument of
    Right document -> callback document
    Left error -> return . Left $ error
