module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.Event.AnswerEventDTO
import Api.Resource.Event.ChapterEventDTO
import Api.Resource.Event.ExpertEventDTO
import Api.Resource.Event.IntegrationEventDTO
import Api.Resource.Event.KnowledgeModelEventDTO
import Api.Resource.Event.QuestionEventDTO
import Api.Resource.Event.ReferenceEventDTO
import Api.Resource.Event.TagEventDTO
import Api.Resource.Info.InfoDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationCreateDTO
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Organization.OrganizationStateDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.PackageBundle.PackageBundleDTO
import Model.ActionKey.ActionKey
import Model.Config.AppConfig
import Model.Config.BuildInfoConfig
import Model.Context.AppContext
import Model.Context.BaseContext
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.Organization.Organization
import Model.Package.Package
import Model.Package.PackageWithEvents
import Model.PackageBundle.PackageBundle
import Model.Statistics.InstanceStatistics

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / Config
makeFields ''AppConfig

makeFields ''AppConfigGeneral

makeFields ''AppConfigDatabase

makeFields ''AppConfigMail

makeFields ''AppConfigAnalytics

makeFields ''BuildInfoConfig

-- Model / Context
makeFields ''BaseContext

makeFields ''AppContext

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

-- Model / Organization
makeFields ''Organization

-- Model / Package
makeFields ''Package

makeFields ''PackageWithEvents

-- Model / PackageBundle
makeFields ''PackageBundle

-- Model / Statistics
makeFields ''InstanceStatistics

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKey
makeFields ''ActionKeyDTO

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

-- Api / Resource / Info
makeFields ''InfoDTO

-- Api / Resource / KnowledgeModel
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

-- Api / Resource / Organization
makeFields ''OrganizationDTO

makeFields ''OrganizationCreateDTO

makeFields ''OrganizationChangeDTO

makeFields ''OrganizationStateDTO

-- Api / Resource / Package
makeFields ''PackageDTO

makeFields ''PackageSimpleDTO

makeFields ''PackageDetailDTO

-- Api / Resource / PackageBundle
makeFields ''PackageBundleDTO
