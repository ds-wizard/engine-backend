module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.BookReference.BookReferenceDTO
import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Branch.BranchCreateDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchDetailDTO
import Api.Resource.Branch.BranchWithEventsDTO
import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventPathDTO
import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO
import Api.Resource.Info.InfoDTO
import Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.KnowledgeModelBundle.KnowledgeModelBundleDTO
import Api.Resource.Level.LevelDTO
import Api.Resource.Migration.MigratorConflictDTO
import Api.Resource.Migration.MigratorStateCreateDTO
import Api.Resource.Migration.MigratorStateDTO
import Api.Resource.Migration.MigratorStateDetailDTO
import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageWithEventsDTO
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
import Model.Event.EventPath
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
import Model.KnowledgeModelBundle.KnowledgeModelBundle
import Model.Level.Level
import Model.Migrator.MigratorState
import Model.Organization.Organization
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
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

makeFields ''AppConfigDatabase

makeFields ''AppConfigMessaging

makeFields ''AppConfigJwt

makeFields ''AppConfigRoles

makeFields ''AppConfigMail

makeFields ''AppConfigAnalytics

makeFields ''AppConfigFeedback

makeFields ''BuildInfoConfig

-- Model / Context
makeFields ''BaseContext

makeFields ''AppContext

-- Model / DataManagementPlan
makeFields ''DataManagementPlan

makeFields ''DataManagementPlanTemplateContext

-- Model / Event
makeFields ''EventField

makeFields ''EventPathItem

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

-- Model / KnowledgeModelBundle
makeFields ''KnowledgeModelBundle

-- Model / Level
makeFields ''Level

-- Model / Migrator
makeFields ''MigratorConflictDTO

makeFields ''MigratorStateCreateDTO

makeFields ''MigratorStateDetailDTO

makeFields ''MigratorStateDTO

-- Model / Organization
makeFields ''Organization

-- Model / Package
makeFields ''Package

makeFields ''PackageWithEvents

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

-- Api / Resource / DataManagementPlan
makeFields ''DataManagementPlanDTO

-- Api / Resource / Event
makeFields ''EventPathItemDTO

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

-- Api / Resource / KnowledgeModelBundle
makeFields ''KnowledgeModelBundleDTO

-- Model / Level
makeFields ''LevelDTO

-- Api / Resource / Migrator
makeFields ''MigratorState

-- Api / Resource / Organization
makeFields ''OrganizationDTO

makeFields ''OrganizationChangeDTO

-- Api / Resource / Package
makeFields ''PackageDTO

makeFields ''PackageSimpleDTO

makeFields ''PackageWithEventsDTO

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
-- Integration / Resource / Typehint
makeFields ''TypehintIDTO
