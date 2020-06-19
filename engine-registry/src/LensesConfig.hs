module LensesConfig where

import Control.Lens (makeFields)

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Model.Organization.Organization
import Registry.Model.Statistics.InstanceStatistics
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
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
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
import Shared.Model.Template.Template

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / Config
makeFields ''ServerConfig

makeFields ''ServerConfigGeneral

makeFields ''ServerConfigDatabase

makeFields ''ServerConfigMail

makeFields ''ServerConfigAnalytics

makeFields ''ServerConfigLogging

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

makeFields ''MoveQuestionEvent

makeFields ''MoveAnswerEvent

makeFields ''MoveExpertEvent

makeFields ''MoveReferenceEvent

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

-- Model / Template
makeFields ''Template

makeFields ''TemplateAllowedPackage

makeFields ''TemplateFormat

makeFields ''TemplateFile

makeFields ''TemplateAsset

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

makeFields ''MoveQuestionEventDTO

makeFields ''MoveAnswerEventDTO

makeFields ''MoveExpertEventDTO

makeFields ''MoveReferenceEventDTO

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
