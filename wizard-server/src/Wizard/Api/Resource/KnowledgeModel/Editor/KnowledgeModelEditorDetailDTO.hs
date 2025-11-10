module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.Questionnaire.QuestionnaireReply

data KnowledgeModelEditorDetailDTO = KnowledgeModelEditorDetailDTO
  { uuid :: U.UUID
  , name :: String
  , kmId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , state :: KnowledgeModelEditorState
  , previousPackageId :: Maybe String
  , forkOfPackageId :: Maybe String
  , forkOfPackage :: Maybe KnowledgeModelPackageSimpleDTO
  , createdBy :: Maybe U.UUID
  , events :: [KnowledgeModelEvent]
  , replies :: M.Map String Reply
  , knowledgeModel :: KnowledgeModel
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq KnowledgeModelEditorDetailDTO where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.kmId == b.kmId
      && a.version == b.version
      && a.description == b.description
      && a.readme == b.readme
      && a.license == b.license
      && a.state == b.state
      && a.previousPackageId == b.previousPackageId
      && a.forkOfPackageId == b.forkOfPackageId
      && a.forkOfPackage == b.forkOfPackage
      && a.createdBy == b.createdBy
      && a.events == b.events
      && a.replies == b.replies
      && a.knowledgeModel == b.knowledgeModel
