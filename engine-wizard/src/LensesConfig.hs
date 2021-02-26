module LensesConfig where

import Control.Lens (makeFields)

import Registry.Api.Resource.Organization.OrganizationDTO
import qualified Registry.Api.Resource.Package.PackageSimpleDTO as R_PackageSimpleDTO
import qualified Registry.Api.Resource.Template.TemplateSimpleDTO as R_TemplateSimpleDTO
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Choice.ChoiceEvent
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
import Shared.Model.Package.PackageGroup
import Shared.Model.Package.PackageWithEvents
import Shared.Model.PackageBundle.PackageBundle
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateGroup
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
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
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO as KM_MigratorConflictDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO as KM_MigratorStateCreateDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO as KM_MigratorStateDTO
import qualified Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO as KM_MigratorStateDetailDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO as QTN_MigratorStateChangeDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO as QTN_MigratorStateCreateDTO
import qualified Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO as QTN_MigratorStateDTO
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateSimpleDTO
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
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMDTO
import Wizard.Model.Acl.Acl
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.BookReference.BookReference
import Wizard.Model.Branch.Branch
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Http.HttpRequest
import Wizard.Model.Level.Level
import qualified Wizard.Model.Migration.KnowledgeModel.MigratorState as KM_MigratorState
import qualified Wizard.Model.Migration.Questionnaire.MigratorState as QTN_MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Report.Report
import Wizard.Model.Statistics.InstanceStatistics
import Wizard.Model.User.User
import Wizard.Model.User.UserSuggestion
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / Acl
makeFields ''Group

makeFields ''GroupMembership

makeFields ''QuestionnairePermRecord

makeFields ''Member

-- Model / ActionKey
makeFields ''ActionKey

-- Model / BookReference
makeFields ''BookReference

-- Model / Branch
makeFields ''Branch

makeFields ''BranchWithEvents

-- Model / Cache
makeFields ''ServerCache

-- Model / Common
makeFields ''Page

makeFields ''PageMetadata

makeFields ''Pageable

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

makeFields ''AppConfigQuestionnaireVisibility

makeFields ''AppConfigQuestionnaireSharing

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

makeFields ''ServerConfigLogging

makeFields ''BuildInfoConfig

-- Model / Context
makeFields ''BaseContext

makeFields ''AppContext

-- Model / Document
makeFields ''Document

makeFields ''DocumentMetadata

-- Model / Event
makeFields ''EventField

makeFields ''AddKnowledgeModelEvent

makeFields ''EditKnowledgeModelEvent

makeFields ''AddChapterEvent

makeFields ''EditChapterEvent

makeFields ''DeleteChapterEvent

makeFields ''AddQuestionEvent

makeFields ''AddOptionsQuestionEvent

makeFields ''AddMultiChoiceQuestionEvent

makeFields ''AddListQuestionEvent

makeFields ''AddValueQuestionEvent

makeFields ''AddIntegrationQuestionEvent

makeFields ''EditQuestionEvent

makeFields ''EditOptionsQuestionEvent

makeFields ''EditMultiChoiceQuestionEvent

makeFields ''EditListQuestionEvent

makeFields ''EditValueQuestionEvent

makeFields ''EditIntegrationQuestionEvent

makeFields ''DeleteQuestionEvent

makeFields ''AddAnswerEvent

makeFields ''EditAnswerEvent

makeFields ''DeleteAnswerEvent

makeFields ''AddChoiceEvent

makeFields ''EditChoiceEvent

makeFields ''DeleteChoiceEvent

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

makeFields ''MoveChoiceEvent

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

makeFields ''MultiChoiceQuestion

makeFields ''ListQuestion

makeFields ''ValueQuestion

makeFields ''IntegrationQuestion

makeFields ''Answer

makeFields ''Choice

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

makeFields ''PackageGroup

-- Model / PackageBundle
makeFields ''PackageBundle

-- Model / Questionnaire
makeFields ''Questionnaire

makeFields ''QuestionnaireContent

makeFields ''Reply

makeFields ''ReplyValue

makeFields ''IntegrationReplyType

makeFields ''SetReplyEvent

makeFields ''ClearReplyEvent

makeFields ''SetLevelEvent

makeFields ''SetLabelsEvent

makeFields ''QuestionnaireVersion

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

makeFields ''TemplateFile

makeFields ''TemplateAsset

makeFields ''TemplateGroup

-- Model / User
makeFields ''User

makeFields ''UserSubmissionProps

makeFields ''UserSuggestion

-- Model / Websocket
makeFields ''WebsocketMessage

makeFields ''WebsocketRecord

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKey
makeFields ''ActionKeyDTO

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

-- Api / Resource / Feedback
makeFields ''FeedbackDTO

makeFields ''FeedbackCreateDTO

-- Api / Resource / Info
makeFields ''InfoDTO

-- Api / Resource / KnowledgeModel
makeFields ''KnowledgeModelChangeDTO

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

makeFields ''QuestionnaireContentChangeDTO

makeFields ''QuestionnaireReportDTO

makeFields ''SetReplyEventDTO

makeFields ''ClearReplyEventDTO

makeFields ''SetLevelEventDTO

makeFields ''SetLabelsEventDTO

makeFields ''SetReplyEventChangeDTO

makeFields ''ClearReplyEventChangeDTO

makeFields ''SetLevelEventChangeDTO

makeFields ''SetLabelsEventChangeDTO

makeFields ''QuestionnaireVersionDTO

makeFields ''QuestionnaireVersionChangeDTO

makeFields ''QuestionnaireVersionRevertDTO

-- Api / Resource / Registry
makeFields ''RegistryConfirmationDTO

makeFields ''RegistryCreateDTO

-- Api / Resource / Submission
makeFields ''SubmissionCreateDTO

makeFields ''SubmissionDTO

-- Api / Resource / Template
makeFields ''TemplateSimpleDTO

makeFields ''TemplateDetailDTO

makeFields ''TemplateChangeDTO

makeFields ''TemplateFileChangeDTO

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

makeFields ''UserSuggestionDTO

-- Api / Resource / Version
makeFields ''VersionDTO

-- -------------------------------------
-- Messaging
-- -------------------------------------
-- Messaging / Resource / Questionnaire
makeFields ''QuestionnaireEventMDTO

-- -------------------------------------
-- Integration
-- -------------------------------------
-- Integration / Resource / GitHub
makeFields ''IssueIDTO

-- Integration / Resource / Typehint
makeFields ''TypehintIDTO

-- -------------------------------------
-- Registry
-- -------------------------------------
-- Api / Resource / Organization
makeFields ''OrganizationDTO

-- Api / Resource / Package
makeFields ''R_PackageSimpleDTO.PackageSimpleDTO

-- Api / Resource / Package
makeFields ''R_TemplateSimpleDTO.TemplateSimpleDTO
