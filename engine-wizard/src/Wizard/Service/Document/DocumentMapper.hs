module Wizard.Service.Document.DocumentMapper where

import Control.Lens ((^.))
import Data.Map as M
import Data.Time
import qualified Data.UUID as U
import qualified Text.FromHTML as FromHTML

import LensesConfig
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.AppConfig
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Level.Level
import Wizard.Model.Organization.Organization
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.Level.LevelMapper
import Wizard.Service.Metric.MetricMapper
import qualified Wizard.Service.Organization.OrganizationMapper as ORG_Mapper
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
import Wizard.Service.Report.ReportMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContextDTO :: DocumentContext -> DocumentContextDTO
toDocumentContextDTO dc =
  DocumentContextDTO
    { _documentContextDTOUuid = dc ^. uuid
    , _documentContextDTOConfig = toDocumentContextConfigDTO $ dc ^. config
    , _documentContextDTOQuestionnaireUuid = dc ^. questionnaireUuid
    , _documentContextDTOQuestionnaireName = dc ^. questionnaireName
    , _documentContextDTOQuestionnaireReplies = replies
    , _documentContextDTOQuestionnaireRepliesMap = M.fromList $ (\reply -> (reply ^. path, reply)) <$> replies
    , _documentContextDTOLevel = dc ^. level
    , _documentContextDTOKnowledgeModel = toKnowledgeModelDTO $ dc ^. knowledgeModel
    , _documentContextDTOMetrics = toMetricDTO <$> dc ^. metrics
    , _documentContextDTOLevels = toLevelDTO <$> dc ^. levels
    , _documentContextDTOReport = toReportDTO $ dc ^. report
    , _documentContextDTOPackage = toSimpleDTO (dc ^. package)
    , _documentContextDTOOrganization = ORG_Mapper.toDTO $ dc ^. organization
    , _documentContextDTOCreatedBy = USR_Mapper.toDTO <$> dc ^. createdBy
    , _documentContextDTOCreatedAt = dc ^. createdAt
    , _documentContextDTOUpdatedAt = dc ^. updatedAt
    }
  where
    replies = QTN_Mapper.toReplyDTO <$> dc ^. questionnaireReplies

toDocumentContextConfigDTO :: DocumentContextConfig -> DocumentContextConfigDTO
toDocumentContextConfigDTO config =
  DocumentContextConfigDTO {_documentContextConfigDTOLevelsEnabled = config ^. levelsEnabled}

fromCreateDTO ::
     U.UUID
  -> AppConfig
  -> Questionnaire
  -> Int
  -> KnowledgeModel
  -> [Metric]
  -> [Level]
  -> Report
  -> Package
  -> Organization
  -> Maybe User
  -> UTCTime
  -> DocumentContext
fromCreateDTO dmpUuid appConfig qtn level km metrics levels report pkg org mCreatedBy now =
  DocumentContext
    { _documentContextUuid = dmpUuid
    , _documentContextConfig =
        DocumentContextConfig {_documentContextConfigLevelsEnabled = appConfig ^. general . levelsEnabled}
    , _documentContextQuestionnaireUuid = U.toString $ qtn ^. uuid
    , _documentContextQuestionnaireName = qtn ^. name
    , _documentContextQuestionnaireReplies = qtn ^. replies
    , _documentContextLevel = level
    , _documentContextKnowledgeModel = km
    , _documentContextMetrics = metrics
    , _documentContextLevels = levels
    , _documentContextReport = report
    , _documentContextPackage = pkg
    , _documentContextOrganization = org
    , _documentContextCreatedBy = mCreatedBy
    , _documentContextCreatedAt = now
    , _documentContextUpdatedAt = now
    }

formatToToHTMLType :: DocumentFormat -> Maybe FromHTML.ExportType
formatToToHTMLType HTML = Just FromHTML.HTML
formatToToHTMLType LaTeX = Just FromHTML.LaTeX
formatToToHTMLType Markdown = Just FromHTML.Markdown
formatToToHTMLType Docx = Just FromHTML.Docx
formatToToHTMLType ODT = Just FromHTML.ODT
formatToToHTMLType PDF = Just FromHTML.PDF
formatToToHTMLType RTF = Just FromHTML.RTF
formatToToHTMLType RST = Just FromHTML.RST
formatToToHTMLType AsciiDoc = Just FromHTML.AsciiDoc
formatToToHTMLType DokuWiki = Just FromHTML.DokuWiki
formatToToHTMLType MediaWiki = Just FromHTML.MediaWiki
formatToToHTMLType EPUB2 = Just FromHTML.EPUB2
formatToToHTMLType EPUB3 = Just FromHTML.EPUB3
formatToToHTMLType _ = Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heFormatToToHTMLType format callback =
  case formatToToHTMLType format of
    Just toHTMLType -> callback toHTMLType
    Nothing -> return $ Left . GeneralServerError $ _ERROR_SERVICE_DOCUMENT__UKNOWN_FORMAT
