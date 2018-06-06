module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchWithStateDTO
import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventPathDTO
import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO
import Api.Resource.Info.InfoDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageWithEventsDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.User.UserChangeDTO
import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Api.Resource.User.UserPasswordDTO
import Api.Resource.User.UserProfileChangeDTO
import Api.Resource.User.UserStateDTO
import Model.ActionKey.ActionKey
import Model.Branch.Branch
import Model.Config.DSWConfig
import Model.Context.AppContext
import Model.DataManagementPlan.DataManagementPlan
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.EventPath
import Model.Event.Expert.ExpertEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.KnowledgeModel.KnowledgeModel
import Model.Organization.Organization
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Model.User.User

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / Config
makeFields ''AppConfigEnvironment

makeFields ''AppConfigClient

makeFields ''AppConfigWeb

makeFields ''AppConfigDatabase

makeFields ''AppConfigJwt

makeFields ''AppConfigRoles

makeFields ''AppConfigMail

makeFields ''BuildInfo

makeFields ''DSWConfig

-- Model / Config
makeFields ''Branch

makeFields ''BranchWithEvents

makeFields ''BranchWithKM

-- Model / Config
makeFields ''AppContext

-- Model / DataManagementPlan
makeFields ''DataManagementPlan

-- Model / Event
makeFields ''EventField

makeFields ''EventPathItem

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

-- Model / FilledKnowledgeModel
makeFields ''FilledKnowledgeModel

makeFields ''FilledChapter

makeFields ''FilledQuestion

makeFields ''FilledAnswer

makeFields ''FilledAnswerItem

-- Model / Organization
makeFields ''Organization

-- Model / Questionnaire
makeFields ''Questionnaire

makeFields ''QuestionnaireReply

-- Model / Package
makeFields ''Package

makeFields ''PackageWithEvents

-- Model / User
makeFields ''User

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKeyDTO
makeFields ''ActionKeyDTO

-- Api / Branch / BranchDTO
makeFields ''BranchDTO

makeFields ''BranchWithStateDTO

-- Api / DataManagementPlan
makeFields ''DataManagementPlanDTO

-- Api / Event / EventPathDTO
makeFields ''EventPathItemDTO

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

makeFields ''AddExpertEventDTO

makeFields ''EditExpertEventDTO

makeFields ''DeleteExpertEventDTO

makeFields ''AddReferenceEventDTO

makeFields ''EditReferenceEventDTO

makeFields ''DeleteReferenceEventDTO

-- Api / Resource / InfoDTO
makeFields ''InfoDTO

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

-- Model / FilledKnowledgeModel
makeFields ''FilledKnowledgeModelDTO

makeFields ''FilledChapterDTO

makeFields ''FilledQuestionDTO

makeFields ''FilledAnswerDTO

makeFields ''FilledAnswerItemDTO

-- Model / OrganizationDTO
makeFields ''OrganizationDTO

makeFields ''OrganizationChangeDTO

-- Model / QuestionnaireDTO
makeFields ''QuestionnaireCreateDTO

makeFields ''QuestionnaireDTO

makeFields ''QuestionnaireReplyDTO

makeFields ''QuestionnaireDetailDTO

-- Model / UserDTO
makeFields ''PackageDTO

makeFields ''PackageSimpleDTO

makeFields ''PackageWithEventsDTO

-- Model / UserDTO
makeFields ''UserChangeDTO

makeFields ''UserCreateDTO

makeFields ''UserDTO

makeFields ''UserPasswordDTO

makeFields ''UserProfileChangeDTO

makeFields ''UserStateDTO
