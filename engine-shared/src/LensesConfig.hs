module LensesConfig where

import Control.Lens (makeFields)

import Shared.Api.Resource.Event.AnswerEventDTO
import Shared.Api.Resource.Event.ChapterEventDTO
import Shared.Api.Resource.Event.ExpertEventDTO
import Shared.Api.Resource.Event.IntegrationEventDTO
import Shared.Api.Resource.Event.KnowledgeModelEventDTO
import Shared.Api.Resource.Event.MoveEventDTO
import Shared.Api.Resource.Event.QuestionEventDTO
import Shared.Api.Resource.Event.ReferenceEventDTO
import Shared.Api.Resource.Event.TagEventDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Shared.Api.Resource.Package.PackageDTO
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.Context.AppContext
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

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / Context
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

-- Model / Package
makeFields ''Package

makeFields ''PackageWithEvents

-- Model / PackageBundle
makeFields ''PackageBundle

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
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

-- Api / Resource / Package
makeFields ''PackageDTO

-- Api / Resource / PackageBundle
makeFields ''PackageBundleDTO
