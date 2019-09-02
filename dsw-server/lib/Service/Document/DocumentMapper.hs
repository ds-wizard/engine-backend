module Service.Document.DocumentMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U
import qualified Text.FromHTML as FromHTML

import Api.Resource.Document.DocumentContextDTO
import LensesConfig
import Localization.Messages.Internal
import Model.Config.AppConfig
import Model.Document.DocumentContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel
import Model.Level.Level
import Model.Organization.Organization
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Model.Report.Report
import Model.User.User
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Level.LevelMapper
import Service.Metric.MetricMapper
import qualified Service.Organization.OrganizationMapper
       as ORG_Mapper
import Service.Package.PackageMapper
import qualified Service.Questionnaire.QuestionnaireMapper
       as QTN_Mapper
import Service.Report.ReportMapper
import qualified Service.User.UserMapper as USR_Mapper

toDocumentContextDTO :: DocumentContext -> DocumentContextDTO
toDocumentContextDTO dc =
  DocumentContextDTO
  { _documentContextDTOUuid = dc ^. uuid
  , _documentContextDTOConfig = toDocumentContextConfigDTO $ dc ^. config
  , _documentContextDTOQuestionnaireUuid = dc ^. questionnaireUuid
  , _documentContextDTOQuestionnaireName = dc ^. questionnaireName
  , _documentContextDTOQuestionnaireReplies = QTN_Mapper.toReplyDTO <$> dc ^. questionnaireReplies
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
fromCreateDTO dmpUuid dswConfig qtn level km metrics levels report pkg org mCreatedBy now =
  DocumentContext
  { _documentContextUuid = dmpUuid
  , _documentContextConfig =
      DocumentContextConfig {_documentContextConfigLevelsEnabled = dswConfig ^. general . levelsEnabled}
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
