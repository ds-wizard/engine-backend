module Wizard.Service.Document.DocumentService
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

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper)
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Database.DAO.Organization.OrganizationDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.DocumentContext
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Template.TemplateService
import Wizard.Util.Template (render)

createDocumentContext :: String -> AppContextM (Either AppError DocumentContextDTO)
createDocumentContext qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindPackageById (qtn ^. packageId) $ \pkg ->
      heFindMetrics $ \metrics ->
        heFindLevels $ \levels ->
          heFindOrganization $ \org ->
            heCompileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids) $ \km ->
              heCreatedBy (qtn ^. ownerUuid) $ \mCreatedBy -> do
                appConfig <- asks _appContextApplicationConfig
                dmpUuid <- liftIO generateUuid
                now <- liftIO getCurrentTime
                let _level =
                      if appConfig ^. general . levelsEnabled
                        then qtn ^. level
                        else 9999
                report <- generateReport _level metrics km (qtn ^. replies)
                let dmp = fromCreateDTO dmpUuid appConfig qtn _level km metrics levels report pkg org mCreatedBy now
                return . Right . toDocumentContextDTO $ dmp
  where
    heCreatedBy mOwnerUuid callback =
      case mOwnerUuid of
        Just ownerUuid -> heFindUserById (U.toString ownerUuid) $ \createdBy -> callback . Just $ createdBy
        Nothing -> callback Nothing

exportDocument :: String -> Maybe String -> DocumentFormat -> AppContextM (Either AppError BS.ByteString)
exportDocument qtnUuid mTemplateUuid format =
  heCreateDocumentContext qtnUuid $ \dmp ->
    case format of
      JSON -> return . Right . encode $ dmp
      otherFormat ->
        heGetTemplateByUuidOrFirst mTemplateUuid (Just $ dmp ^. package . pId) $ \template ->
          generateDocumentInFormat otherFormat template dmp

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
generateDocument template dmp =
  heLoadTemplateFile (template ^. rootFile) $ \templateFile -> return . Right $ render templateFile context
  where
    context = fromList [("dmp", fromMaybe emptyObject . decode . encode $ dmp)]

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateDocumentContext qtnUuid = createHeeHelper (createDocumentContext qtnUuid)

-- -----------------------------------------------------
heGenerateDocument templateUuid dmp = createHeeHelper (generateDocument templateUuid dmp)
