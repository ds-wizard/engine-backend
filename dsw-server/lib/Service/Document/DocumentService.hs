module Service.Document.DocumentService
  ( createDocumentContext
  , exportDocument
  , generateDocument
  , generateDocumentInFormat
  -- Helpers
  , heCreateDocumentContext
  , heGenerateDocument
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (decode, encode)
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Strict (fromList)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time
import qualified Data.UUID as U
import qualified Text.FromHTML as FromHTML

import Api.Resource.Document.DocumentContextDTO
import Api.Resource.Document.DocumentContextJM ()
import Api.Resource.Template.TemplateDTO
import Database.DAO.Level.LevelDAO
import Database.DAO.Metric.MetricDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.User.UserDAO
import LensesConfig
import Localization.Messages.Internal
import Model.Context.AppContext
import Model.Document.DocumentContext
import Model.Error.Error
import Service.Document.DocumentMapper
import Service.KnowledgeModel.KnowledgeModelService
import Service.Report.ReportGenerator
import Service.Template.TemplateService
import Util.Helper (createHeeHelper)
import Util.Template (render)
import Util.Uuid

createDocumentContext :: String -> AppContextM (Either AppError DocumentContextDTO)
createDocumentContext qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindPackageById (qtn ^. packageId) $ \pkg ->
      heFindMetrics $ \metrics ->
        heFindLevels $ \levels ->
          heFindOrganization $ \org ->
            heCompileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids) $ \km ->
              heCreatedBy (qtn ^. ownerUuid) $ \mCreatedBy -> do
                dswConfig <- asks _appContextAppConfig
                dmpUuid <- liftIO generateUuid
                now <- liftIO getCurrentTime
                let _level =
                      if dswConfig ^. general . levelsEnabled
                        then qtn ^. level
                        else 9999
                report <- generateReport _level metrics km (qtn ^. replies)
                let dmp = fromCreateDTO dmpUuid dswConfig qtn _level km metrics levels report pkg org mCreatedBy now
                return . Right . toDocumentContextDTO $ dmp
  where
    heCreatedBy mOwnerUuid callback =
      case mOwnerUuid of
        Just ownerUuid -> heFindUserById (U.toString ownerUuid) $ \createdBy -> callback . Just $ createdBy
        Nothing -> callback Nothing

exportDocument :: String -> Maybe String -> DocumentFormat -> AppContextM (Either AppError BS.ByteString)
exportDocument qtnUuid mTemplateUuid format = do
  heCreateDocumentContext qtnUuid $ \dmp ->
    case format of
      JSON -> return . Right . encode $ dmp
      otherFormat ->
        heGetTemplateByUuidOrFirst mTemplateUuid $ \template -> generateDocumentInFormat otherFormat template dmp

generateDocumentInFormat ::
     DocumentFormat -> TemplateDTO -> DocumentContextDTO -> AppContextM (Either AppError BS.ByteString)
generateDocumentInFormat format template dmp =
  heFormatToToHTMLType format $ \toHTMLType ->
    heGenerateDocument template dmp $ \document -> do
      eResult <- liftIO . FromHTML.fromHTML toHTMLType . T.unpack $ document
      case eResult of
        Right result -> return . Right . BS.fromStrict $ result
        Left err ->
          return . Left . GeneralServerError $
          _ERROR_SERVICE_DOCUMENT__TRANSFORMATION_FAILED (T.unpack . E.decodeUtf8 $ err)

generateDocument :: TemplateDTO -> DocumentContextDTO -> AppContextM (Either AppError T.Text)
generateDocument template dmp = do
  heLoadTemplateFile (template ^. rootFile) $ \templateFile -> return . Right $ render templateFile context
  where
    context = fromList [("dmp", fromMaybe emptyObject . decode . encode $ dmp)]

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateDocumentContext qtnUuid = createHeeHelper (createDocumentContext qtnUuid)

-- -----------------------------------------------------
heGenerateDocument templateUuid dmp = createHeeHelper (generateDocument templateUuid dmp)
