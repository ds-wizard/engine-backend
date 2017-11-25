module Database.Migration.Branch.Data.KnowledgeModel where

import Data.Maybe
import qualified Data.UUID as U

import Model.Event.KnowledgeModel.AddKnowledgeModelEvent

akm =
  AddKnowledgeModelEvent
  { _akmUuid = fromJust (U.fromString "00e3d7c9-4f71-45b0-91d4-343470e34711")
  , _akmKmUuid = fromJust (U.fromString "f9bad7dd-ce47-43f3-b65b-d4a5d51ef528")
  , _akmName = "My Knowledge Model"
  }
