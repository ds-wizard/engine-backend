module Model.KnowledgeModel.Path where

import qualified Data.UUID as U
import GHC.Generics

_EVENT_PATH_ITEM__KM = "km"

_EVENT_PATH_ITEM__CHAPTER = "chapter"

_EVENT_PATH_ITEM__QUESTION = "question"

_EVENT_PATH_ITEM__ANSWER = "answer"

_EVENT_PATH_ITEM__EXPERT = "expert"

_EVENT_PATH_ITEM__REFERENCE = "reference"

data PathItem = PathItem
  { _pathItemPType :: String
  , _pathItemUuid :: U.UUID
  } deriving (Show, Eq, Generic)

type Path = [PathItem]

showPathShort path =
  unwords $
  (\pathItem -> "[" ++ (_pathItemPType pathItem) ++ ": " ++ U.toString (_pathItemUuid pathItem) ++ "]") <$> path
