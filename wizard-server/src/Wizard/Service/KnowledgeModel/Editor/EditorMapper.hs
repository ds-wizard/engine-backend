module Wizard.Service.KnowledgeModel.Editor.EditorMapper where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelRawEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

toList :: KnowledgeModelEditor -> Maybe String -> KnowledgeModelEditorState -> KnowledgeModelEditorList
toList editor mForkOfPackageId state =
  KnowledgeModelEditorList
    { uuid = editor.uuid
    , name = editor.name
    , kmId = editor.kmId
    , version = editor.version
    , state = state
    , previousPackageId = editor.previousPackageId
    , forkOfPackageId = mForkOfPackageId
    , createdBy = editor.createdBy
    , createdAt = editor.createdAt
    , updatedAt = editor.updatedAt
    }

toDetailDTO :: KnowledgeModelEditor -> [KnowledgeModelEditorEvent] -> [KnowledgeModelEditorReply] -> KnowledgeModel -> Maybe String -> Maybe KnowledgeModelPackage -> KnowledgeModelEditorState -> KnowledgeModelEditorDetailDTO
toDetailDTO editor kmEditorEvents kmEditorReplies knowledgeModel mForkOfPackageId mForkOfPackage state =
  KnowledgeModelEditorDetailDTO
    { uuid = editor.uuid
    , name = editor.name
    , kmId = editor.kmId
    , version = editor.version
    , description = editor.description
    , readme = editor.readme
    , license = editor.license
    , state = state
    , previousPackageId = editor.previousPackageId
    , forkOfPackageId = mForkOfPackageId
    , forkOfPackage = fmap toSimpleDTO mForkOfPackage
    , events = fmap (\KnowledgeModelEditorEvent {..} -> KnowledgeModelEvent {..}) kmEditorEvents
    , replies = M.fromList . fmap (\KnowledgeModelEditorReply {..} -> (path, Reply {..})) $ kmEditorReplies
    , knowledgeModel = knowledgeModel
    , createdBy = editor.createdBy
    , createdAt = editor.createdAt
    , updatedAt = editor.updatedAt
    }

fromCreateDTO :: KnowledgeModelEditorCreateDTO -> U.UUID -> Maybe KnowledgeModelPackage -> U.UUID -> U.UUID -> UTCTime -> KnowledgeModelEditor
fromCreateDTO dto uuid mPreviousPkg createdBy tenantUuid now =
  KnowledgeModelEditor
    { uuid = uuid
    , name = dto.name
    , kmId = dto.kmId
    , version = dto.version
    , description = maybe "" (.description) mPreviousPkg
    , readme = maybe "" (.readme) mPreviousPkg
    , license = maybe "" (.license) mPreviousPkg
    , previousPackageId = dto.previousPackageId
    , metamodelVersion = knowledgeModelMetamodelVersion
    , squashed = True
    , createdBy = Just createdBy
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: KnowledgeModelEditorChangeDTO -> KnowledgeModelEditor -> UTCTime -> KnowledgeModelEditor
fromChangeDTO dto editor bUpdatedAt =
  KnowledgeModelEditor
    { uuid = editor.uuid
    , name = dto.name
    , kmId = dto.kmId
    , version = dto.version
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , previousPackageId = editor.previousPackageId
    , metamodelVersion = editor.metamodelVersion
    , squashed = editor.squashed
    , createdBy = editor.createdBy
    , tenantUuid = editor.tenantUuid
    , createdAt = editor.createdAt
    , updatedAt = bUpdatedAt
    }

toKnowledgeModelEditorEvent :: U.UUID -> U.UUID -> KnowledgeModelEvent -> KnowledgeModelEditorEvent
toKnowledgeModelEditorEvent knowledgeModelEditorUuid tenantUuid KnowledgeModelEvent {..} = KnowledgeModelEditorEvent {..}

toKnowledgeModelEditorRawEvent :: U.UUID -> U.UUID -> KnowledgeModelRawEvent -> KnowledgeModelEditorRawEvent
toKnowledgeModelEditorRawEvent knowledgeModelEditorUuid tenantUuid KnowledgeModelRawEvent {..} = KnowledgeModelEditorRawEvent {..}

toKnowledgeModelEvent :: KnowledgeModelEditorEvent -> KnowledgeModelEvent
toKnowledgeModelEvent KnowledgeModelEditorEvent {..} = KnowledgeModelEvent {..}

toKnowledgeModelRawEvent :: KnowledgeModelEditorRawEvent -> KnowledgeModelRawEvent
toKnowledgeModelRawEvent KnowledgeModelEditorRawEvent {..} = KnowledgeModelRawEvent {..}

toReplies :: [KnowledgeModelEditorReply] -> M.Map String Reply
toReplies = M.fromList . fmap toReply

toReply :: KnowledgeModelEditorReply -> (String, Reply)
toReply KnowledgeModelEditorReply {..} = (path, Reply {..})
