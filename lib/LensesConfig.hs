module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.Event.EventDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.ActionKey.ActionKey
import Model.Config.DSWConfig
import Model.Context.AppContext
import Model.Event.Answer.AnswerEvent
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / Config
makeFields ''AppConfigClient

makeFields ''AppConfigWeb

makeFields ''AppConfigDatabase

makeFields ''AppConfigJwt

makeFields ''AppConfigRoles

makeFields ''AppConfigMail

makeFields ''BuildInfo

makeFields ''DSWConfig

-- Model / Config
makeFields ''AppContext

-- Model / Event
makeFields ''EventField

makeFields ''AddKnowledgeModelEvent

makeFields ''EditKnowledgeModelEvent

makeFields ''AddChapterEvent

makeFields ''EditChapterEvent

makeFields ''DeleteChapterEvent

makeFields ''AddQuestionEvent

makeFields ''EditQuestionEvent

makeFields ''DeleteQuestionEvent

makeFields ''AddAnswerEvent

makeFields ''EditAnswerEvent

makeFields ''DeleteAnswerEvent

makeFields ''AddExpertEvent

makeFields ''EditExpertEvent

makeFields ''DeleteExpertEvent

makeFields ''AddReferenceEvent

makeFields ''EditReferenceEvent

makeFields ''DeleteReferenceEvent

makeFields ''AddFollowUpQuestionEvent

makeFields ''EditFollowUpQuestionEvent

makeFields ''DeleteFollowUpQuestionEvent

makeFields ''AddAnswerItemTemplateQuestionEvent

makeFields ''EditAnswerItemTemplateQuestionEvent

makeFields ''DeleteAnswerItemTemplateQuestionEvent

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''Chapter

makeFields ''Question

makeFields ''Answer

makeFields ''AnswerItemTemplate

makeFields ''AnswerItemTemplatePlain

makeFields ''AnswerItemTemplatePlainWithIds

makeFields ''Expert

makeFields ''Reference

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKeyDTO
makeFields ''ActionKeyDTO

-- Api / Event / EventDTO
makeFields ''AddKnowledgeModelEventDTO

makeFields ''EditKnowledgeModelEventDTO

makeFields ''AddChapterEventDTO

makeFields ''EditChapterEventDTO

makeFields ''DeleteChapterEventDTO

makeFields ''AddQuestionEventDTO

makeFields ''EditQuestionEventDTO

makeFields ''DeleteQuestionEventDTO

makeFields ''AddAnswerEventDTO

makeFields ''EditAnswerEventDTO

makeFields ''DeleteAnswerEventDTO

makeFields ''AddAnswerItemTemplateQuestionEventDTO

makeFields ''EditAnswerItemTemplateQuestionEventDTO

makeFields ''DeleteAnswerItemTemplateQuestionEventDTO

makeFields ''AddExpertEventDTO

makeFields ''EditExpertEventDTO

makeFields ''DeleteExpertEventDTO

makeFields ''AddReferenceEventDTO

makeFields ''EditReferenceEventDTO

makeFields ''DeleteReferenceEventDTO

makeFields ''AddFollowUpQuestionEventDTO

makeFields ''EditFollowUpQuestionEventDTO

makeFields ''DeleteFollowUpQuestionEventDTO

-- Api / Resource / KnowledgeModelDTO
makeFields ''KnowledgeModelDTO

makeFields ''ChapterDTO

makeFields ''QuestionDTO

makeFields ''AnswerDTO

makeFields ''AnswerItemTemplateDTO

makeFields ''AnswerItemTemplatePlainDTO

makeFields ''AnswerItemTemplatePlainWithIdsDTO

makeFields ''ExpertDTO

makeFields ''ReferenceDTO
