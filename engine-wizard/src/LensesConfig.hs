module LensesConfig where

import Control.Lens (makeFields)

import Registry.Api.Resource.Organization.OrganizationDTO
import Shared.Api.Resource.Event.AnswerEventDTO
import Shared.Api.Resource.Event.ChapterEventDTO
import Shared.Api.Resource.Event.ExpertEventDTO
import Shared.Api.Resource.Event.IntegrationEventDTO
import Shared.Api.Resource.Event.KnowledgeModelEventDTO
import Shared.Api.Resource.Event.MoveEventDTO
import Shared.Api.Resource.Event.QuestionEventDTO
import Shared.Api.Resource.Event.ReferenceEventDTO
import Shared.Api.Resource.Event.TagEventDTO
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchWithEventsDTO
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Level.LevelDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO as KM_MigratorConflictDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO as KM_MigratorStateCreateDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO as KM_MigratorStateDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO as KM_MigratorStateDetailDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO as QTN_MigratorStateChangeDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO as QTN_MigratorStateCreateDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO as QTN_MigratorStateDTO
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO
import Wizard.Integration.Resource.Package.PackageDetailIDTO
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Branch.Branch
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentTemplateContext
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Level.Level
import qualified Wizard.Model.Migration.KnowledgeModel.MigratorState as KM_MigratorState
import qualified Wizard.Model.Migration.Questionnaire.MigratorState as QTN_MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Model.Template.Template
import Wizard.Model.User.User

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / BookReference
makeFields ''BookReference

-- Model / Branch
makeFields ''Branch

makeFields ''BranchWithEvents

-- Model / Config
makeFields ''AppConfig

makeFields ''AppConfigOrganization

makeFields ''AppConfigAuth

makeFields ''AppConfigAuthInternal

makeFields ''AppConfigAuthExternal

makeFields ''AppConfigAuthExternalService

makeFields ''AppConfigAuthExternalServiceParameter

makeFields ''AppConfigAuthExternalServiceStyle

makeFields ''AppConfigPrivacyAndSupport

makeFields ''AppConfigDashboard

makeFields ''AppConfigDashboardWidgets

makeFields ''AppConfigLookAndFeel

makeFields ''AppConfigLookAndFeelCustomMenuLink

makeFields ''AppConfigRegistry

makeFields ''AppConfigQuestionnaire

makeFields ''AppConfigQuestionnaireFeedback

makeFields ''AppConfigTemplate

makeFields ''AppConfigSubmission

makeFields ''AppConfigSubmissionService

makeFields ''AppConfigSubmissionServiceSupportedFormat

makeFields ''AppConfigSubmissionServiceRequest

makeFields ''AppConfigSubmissionServiceRequestMultipart

makeFields ''SimpleFeature

makeFields ''ServerConfig

makeFields ''ServerConfigGeneral

makeFields ''ServerConfigDatabase

makeFields ''ServerConfigMessaging

makeFields ''ServerConfigJwt

makeFields ''ServerConfigRoles

makeFields ''ServerConfigMail

makeFields ''ServerConfigRegistry

makeFields ''ServerConfigAnalytics

makeFields ''ServerConfigFeedback

makeFields ''BuildInfoConfig

-- Model / Context
makeFields ''BaseContext

makeFields ''AppContext

-- Model / Document
makeFields ''Document

makeFields ''DocumentMetadata

makeFields ''DocumentContext

makeFields ''DocumentContextConfig

makeFields ''DocumentTemplateContext

-- Model / Event
makeFields ''EventField

makeFields ''AddKnowledgeModelEvent

makeFields ''EditKnowledgeModelEvent

makeFields ''AddChapterEvent

makeFields ''EditChapterEvent

makeFields ''DeleteChapterEvent

makeFields ''AddQuestionEvent

makeFields ''AddOptionsQuestionEvent

makeFields ''AddListQuestionEvent

makeFields ''AddValueQuestionEvent

makeFields ''AddIntegrationQuestionEvent

makeFields ''EditQuestionEvent

makeFields ''EditOptionsQuestionEvent

makeFields ''EditListQuestionEvent

makeFields ''EditValueQuestionEvent

makeFields ''EditIntegrationQuestionEvent

makeFields ''DeleteQuestionEvent

makeFields ''AddAnswerEvent

makeFields ''EditAnswerEvent

makeFields ''DeleteAnswerEvent

makeFields ''AddExpertEvent

makeFields ''EditExpertEvent

makeFields ''DeleteExpertEvent

makeFields ''AddReferenceEvent

makeFields ''AddResourcePageReferenceEvent

makeFields ''AddURLReferenceEvent

makeFields ''AddCrossReferenceEvent

makeFields ''EditReferenceEvent

makeFields ''EditResourcePageReferenceEvent

makeFields ''EditURLReferenceEvent

makeFields ''EditCrossReferenceEvent

makeFields ''DeleteReferenceEvent

makeFields ''AddTagEvent

makeFields ''EditTagEvent

makeFields ''DeleteTagEvent

makeFields ''AddIntegrationEvent

makeFields ''EditIntegrationEvent

makeFields ''DeleteIntegrationEvent

makeFields ''MoveQuestionEvent

makeFields ''MoveAnswerEvent

makeFields ''MoveExpertEvent

makeFields ''MoveReferenceEvent

-- Model / Feedback
makeFields ''Feedback

-- Model / Http
makeFields ''HttpRequest

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''KnowledgeModelEntities

makeFields ''Chapter

makeFields ''Question

makeFields ''OptionsQuestion

makeFields ''ListQuestion

makeFields ''ValueQuestion

makeFields ''IntegrationQuestion

makeFields ''Answer

makeFields ''Expert

makeFields ''Reference

makeFields ''ResourcePageReference

makeFields ''URLReference

makeFields ''CrossReference

makeFields ''Metric

makeFields ''MetricMeasure

makeFields ''Tag

makeFields ''Integration

-- Model / Level
makeFields ''Level

-- Model / Migration / KnowledgeModel
makeFields ''KM_MigratorState.MigratorState

-- Model / Migration / Questionnaire
makeFields ''QTN_MigratorState.MigratorState

-- Model / Package
makeFields ''Package

makeFields ''PackageWithEvents

-- Model / PackageBundle
makeFields ''PackageBundle

-- Model / Questionnaire
makeFields ''Questionnaire

makeFields ''Reply

makeFields ''ReplyValue

makeFields ''IntegrationReplyValue

makeFields ''Label

-- Model / Report
makeFields ''Indication

makeFields ''AnsweredIndication

makeFields ''LevelsAnsweredIndication

makeFields ''MetricSummary

makeFields ''ChapterReport

makeFields ''TotalReport

makeFields ''Report

-- Model / Statistic
makeFields ''InstanceStatistics

-- Model / Template
makeFields ''Template

makeFields ''TemplateAllowedPackage

makeFields ''TemplateFormat

-- Model / User
makeFields ''User

makeFields ''UserSubmissionProps

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKey
makeFields ''ActionKeyDTO

-- Api / Resource / BookReference
makeFields ''BookReferenceDTO

-- Api / Resource / Branch
makeFields ''BranchChangeDTO

makeFields ''BranchCreateDTO

makeFields ''BranchDTO

makeFields ''BranchDetailDTO

makeFields ''BranchWithEventsDTO

-- Api / Resource / Config
makeFields ''AppConfigChangeDTO

makeFields ''ClientConfigDTO

makeFields ''ClientConfigRegistryDTO

makeFields ''ClientConfigQuestionnaireDTO

-- Api / Resource / Document
makeFields ''DocumentDTO

makeFields ''DocumentCreateDTO

makeFields ''DocumentContextDTO

makeFields ''DocumentContextConfigDTO

-- Api / Resource / Event
makeFields ''AddKnowledgeModelEventDTO

makeFields ''EditKnowledgeModelEventDTO

makeFields ''AddChapterEventDTO

makeFields ''EditChapterEventDTO

makeFields ''DeleteChapterEventDTO

makeFields ''AddQuestionEventDTO

makeFields ''AddOptionsQuestionEventDTO

makeFields ''AddListQuestionEventDTO

makeFields ''AddValueQuestionEventDTO

makeFields ''AddIntegrationQuestionEventDTO

makeFields ''EditQuestionEventDTO

makeFields ''EditOptionsQuestionEventDTO

makeFields ''EditListQuestionEventDTO

makeFields ''EditValueQuestionEventDTO

makeFields ''EditIntegrationQuestionEventDTO

makeFields ''DeleteQuestionEventDTO

makeFields ''AddAnswerEventDTO

makeFields ''EditAnswerEventDTO

makeFields ''DeleteAnswerEventDTO

makeFields ''AddExpertEventDTO

makeFields ''EditExpertEventDTO

makeFields ''DeleteExpertEventDTO

makeFields ''AddReferenceEventDTO

makeFields ''AddResourcePageReferenceEventDTO

makeFields ''AddURLReferenceEventDTO

makeFields ''AddCrossReferenceEventDTO

makeFields ''EditReferenceEventDTO

makeFields ''EditResourcePageReferenceEventDTO

makeFields ''EditURLReferenceEventDTO

makeFields ''EditCrossReferenceEventDTO

makeFields ''DeleteReferenceEventDTO

makeFields ''AddTagEventDTO

makeFields ''EditTagEventDTO

makeFields ''DeleteTagEventDTO

makeFields ''AddIntegrationEventDTO

makeFields ''EditIntegrationEventDTO

makeFields ''DeleteIntegrationEventDTO

makeFields ''MoveQuestionEventDTO

makeFields ''MoveAnswerEventDTO

makeFields ''MoveExpertEventDTO

makeFields ''MoveReferenceEventDTO

-- Api / Resource / Feedback
makeFields ''FeedbackDTO

makeFields ''FeedbackCreateDTO

-- Api / Resource / Info
makeFields ''InfoDTO

-- Api / Resource / KnowledgeModel
makeFields ''KnowledgeModelChangeDTO

makeFields ''KnowledgeModelDTO

makeFields ''KnowledgeModelEntitiesDTO

makeFields ''ChapterDTO

makeFields ''QuestionDTO

makeFields ''OptionsQuestionDTO

makeFields ''ListQuestionDTO

makeFields ''ValueQuestionDTO

makeFields ''IntegrationQuestionDTO

makeFields ''AnswerDTO

makeFields ''ExpertDTO

makeFields ''ReferenceDTO

makeFields ''ResourcePageReferenceDTO

makeFields ''URLReferenceDTO

makeFields ''CrossReferenceDTO

makeFields ''MetricDTO

makeFields ''MetricMeasureDTO

makeFields ''TagDTO

makeFields ''IntegrationDTO

-- Model / Level
makeFields ''LevelDTO

-- Api / Resource / Migration / KnowledgeModel
makeFields ''KM_MigratorConflictDTO.MigratorConflictDTO

makeFields ''KM_MigratorStateCreateDTO.MigratorStateCreateDTO

makeFields ''KM_MigratorStateDetailDTO.MigratorStateDetailDTO

makeFields ''KM_MigratorStateDTO.MigratorStateDTO

-- Api / Resource / Migration / Questionnaire
makeFields ''QTN_MigratorStateDTO.MigratorStateDTO

makeFields ''QTN_MigratorStateCreateDTO.MigratorStateCreateDTO

makeFields ''QTN_MigratorStateChangeDTO.MigratorStateChangeDTO

-- Api / Resource / Organization
makeFields ''OrganizationSimpleDTO

-- Api / Resource / Package
makeFields ''PackageDTO

makeFields ''PackageSimpleDTO

makeFields ''PackageDetailDTO

-- Api / Resource / PackageBundle
makeFields ''PackageBundleDTO

-- Api / Resource / Questionnaire
makeFields ''QuestionnaireCreateDTO

makeFields ''QuestionnaireDTO

makeFields ''QuestionnaireDetailDTO

makeFields ''QuestionnaireChangeDTO

makeFields ''ReplyDTO

makeFields ''ReplyValueDTO

makeFields ''IntegrationReplyValueDTO

makeFields ''LabelDTO

-- Api / Resource / Registry
makeFields ''RegistryConfirmationDTO

makeFields ''RegistryCreateDTO

-- Api / Resource / Report
makeFields ''IndicationDTO

makeFields ''AnsweredIndicationDTO

makeFields ''LevelsAnsweredIndicationDTO

makeFields ''MetricSummaryDTO

makeFields ''ChapterReportDTO

makeFields ''TotalReportDTO

makeFields ''ReportDTO

-- Api / Resource / Submission
makeFields ''SubmissionCreateDTO

makeFields ''SubmissionDTO

-- Api / Resource / Template
makeFields ''TemplateDTO

-- Api / Resource / Token
makeFields ''TokenDTO

makeFields ''TokenCreateDTO

-- Api / Resource / Typehint
makeFields ''TypehintDTO

makeFields ''TypehintRequestDTO

-- Api / Resource / User
makeFields ''UserChangeDTO

makeFields ''UserCreateDTO

makeFields ''UserDTO

makeFields ''UserPasswordDTO

makeFields ''UserProfileDTO

makeFields ''UserProfileChangeDTO

makeFields ''UserStateDTO

makeFields ''UserSubmissionPropsDTO

-- Api / Resource / Version
makeFields ''VersionDTO

-- -------------------------------------
-- Integration
-- -------------------------------------
-- Integration / Resource / GitHub
makeFields ''IssueIDTO

-- Integration / Resource / Organization
makeFields ''OrganizationSimpleIDTO

-- Integration / Resource / Package
makeFields ''PackageDetailIDTO

makeFields ''PackageSimpleIDTO

-- Integration / Resource / Typehint
makeFields ''TypehintIDTO

-- -------------------------------------
-- Registry
-- -------------------------------------
-- Api / Resource / Organization
makeFields ''OrganizationDTO
