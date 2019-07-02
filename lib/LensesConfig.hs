module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.BookReference.BookReferenceDTO
import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Branch.BranchCreateDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchDetailDTO
import Api.Resource.Branch.BranchWithEventsDTO
import Api.Resource.Config.ClientConfigDTO
import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Api.Resource.Event.AnswerEventDTO
import Api.Resource.Event.ChapterEventDTO
import Api.Resource.Event.ExpertEventDTO
import Api.Resource.Event.IntegrationEventDTO
import Api.Resource.Event.KnowledgeModelEventDTO
import Api.Resource.Event.QuestionEventDTO
import Api.Resource.Event.ReferenceEventDTO
import Api.Resource.Event.TagEventDTO
import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO
import Api.Resource.Info.InfoDTO
import Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.KnowledgeModel.PathDTO
import Api.Resource.Level.LevelDTO
import qualified
       Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
       as KM_MigratorConflictDTO
import qualified
       Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
       as KM_MigratorStateCreateDTO
import qualified
       Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
       as KM_MigratorStateDTO
import qualified
       Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO
       as KM_MigratorStateDetailDTO
import qualified
       Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
       as QTN_MigratorStateChangeDTO
import qualified
       Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
       as QTN_MigratorStateCreateDTO
import qualified
       Api.Resource.Migration.Questionnaire.MigratorStateDTO
       as QTN_MigratorStateDTO
import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.PackageBundle.PackageBundleDTO
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Api.Resource.Report.ReportDTO
import Api.Resource.Template.TemplateDTO
import Api.Resource.Token.TokenCreateDTO
import Api.Resource.Token.TokenDTO
import Api.Resource.Typehint.TypehintDTO
import Api.Resource.Typehint.TypehintRequestDTO
import Api.Resource.User.UserChangeDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserProfileChangeDTO
import Api.Resource.User.UserStateDTO
import Api.Resource.Version.VersionDTO
import Integration.Resource.Organization.OrganizationSimpleIDTO
import Integration.Resource.Package.PackageDetailIDTO
import Integration.Resource.Package.PackageSimpleIDTO
import Integration.Resource.Typehint.TypehintIDTO
import Model.ActionKey.ActionKey
import Model.BookReference.BookReference
import Model.Branch.Branch
import Model.Config.AppConfig
import Model.Config.BuildInfoConfig
import Model.Context.AppContext
import Model.Context.BaseContext
import Model.DataManagementPlan.DataManagementPlan
import Model.DataManagementPlan.DataManagementPlanTemplateContext
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.Feedback.Feedback
import Model.Feedback.SimpleIssue
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.Http.HttpRequest
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.Path
import Model.Level.Level
import qualified Model.Migration.KnowledgeModel.MigratorState
       as KM_MigratorState
import qualified Model.Migration.Questionnaire.MigratorState
       as QTN_MigratorState
import Model.Organization.Organization
import Model.Package.Package
import Model.Package.PackageWithEvents
import Model.PackageBundle.PackageBundle
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Model.Statistics.InstanceStatistics
import Model.User.User

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

makeFields ''AppConfigGeneral

makeFields ''AppConfigClient

makeFields ''AppConfigClientDashboard

makeFields ''AppConfigDatabase

makeFields ''AppConfigMessaging

makeFields ''AppConfigJwt

makeFields ''AppConfigRoles

makeFields ''AppConfigMail

makeFields ''AppConfigRegistry

makeFields ''AppConfigAnalytics

makeFields ''AppConfigFeedback

makeFields ''BuildInfoConfig

-- Model / Context
makeFields ''BaseContext

makeFields ''AppContext

-- Model / DataManagementPlan
makeFields ''DataManagementPlan

makeFields ''DataManagementPlanConfig

makeFields ''DataManagementPlanTemplateContext

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

-- Model / Feedback
makeFields ''Feedback

makeFields ''SimpleIssue

-- Model / FilledKnowledgeModel
makeFields ''FilledKnowledgeModel

makeFields ''FilledChapter

makeFields ''FilledQuestion

makeFields ''FilledOptionsQuestion

makeFields ''FilledListQuestion

makeFields ''FilledValueQuestion

makeFields ''FilledIntegrationQuestion

makeFields ''FilledAnswer

makeFields ''FilledAnswerItem

-- Model / Http
makeFields ''HttpRequest

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

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

makeFields ''PathItem

-- Model / Level
makeFields ''Level

-- Model / Migration / KnowledgeModel
makeFields ''KM_MigratorState.MigratorState

-- Model / Migration / Questionnaire
makeFields ''QTN_MigratorState.MigratorState

-- Model / Organization
makeFields ''Organization

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

-- Model / Report
makeFields ''Indication

makeFields ''AnsweredIndication

makeFields ''MetricSummary

makeFields ''ChapterReport

makeFields ''Report

-- Model / Statistic
makeFields ''InstanceStatistics

-- Model / User
makeFields ''User

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
makeFields ''ClientConfigDTO

makeFields ''ClientConfigRegistryDTO

makeFields ''ClientConfigClientDTO

makeFields ''ClientConfigClientDashboardDTO

-- Api / Resource / DataManagementPlan
makeFields ''DataManagementPlanDTO

makeFields ''DataManagementPlanConfigDTO

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

-- Api / Resource / Feedback
makeFields ''FeedbackDTO

makeFields ''FeedbackCreateDTO

-- Api / Resource / FilledKnowledgeModel
makeFields ''FilledKnowledgeModelDTO

makeFields ''FilledChapterDTO

makeFields ''FilledQuestionDTO

makeFields ''FilledOptionsQuestionDTO

makeFields ''FilledListQuestionDTO

makeFields ''FilledValueQuestionDTO

makeFields ''FilledIntegrationQuestionDTO

makeFields ''FilledAnswerDTO

makeFields ''FilledAnswerItemDTO

-- Api / Resource / Info
makeFields ''InfoDTO

-- Api / Resource / KnowledgeModel
makeFields ''KnowledgeModelChangeDTO

makeFields ''KnowledgeModelDTO

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

makeFields ''PathItemDTO

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
makeFields ''OrganizationDTO

makeFields ''OrganizationChangeDTO

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

-- Api / Resource / Report
makeFields ''IndicationDTO

makeFields ''AnsweredIndicationDTO

makeFields ''MetricSummaryDTO

makeFields ''ChapterReportDTO

makeFields ''ReportDTO

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

makeFields ''UserProfileChangeDTO

makeFields ''UserStateDTO

-- Api / Resource / Version
makeFields ''VersionDTO

-- -------------------------------------
-- Integration
-- -------------------------------------
-- Integration / Resource / Organization
makeFields ''OrganizationSimpleIDTO

-- Integration / Resource / Package
makeFields ''PackageDetailIDTO

makeFields ''PackageSimpleIDTO

-- Integration / Resource / Typehint
makeFields ''TypehintIDTO
