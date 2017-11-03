module Fixtures.KnowledgeModel.KnowledgeModels where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Fixtures.KnowledgeModel.Chapters
import Model.KnowledgeModel.KnowledgeModel

km1 :: KnowledgeModel
km1 =
 KnowledgeModel
 { _kmUuid = fromJust $ U.fromString "ff672529-e837-4201-b7b1-7ada557d9725"
 , _kmName = "My Knowledge Model"
 , _kmChapters = [chapter1, chapter2]
 }

km1WithChangeProperties :: KnowledgeModel
km1WithChangeProperties =
 KnowledgeModel
 { _kmUuid = km1 ^. kmUuid
 , _kmName = "EDITED: My Knowledge Model"
 , _kmChapters = [chapter2, chapter1]
 }

km1WithoutChapters :: KnowledgeModel
km1WithoutChapters =
 KnowledgeModel
 { _kmUuid = km1 ^. kmUuid
 , _kmName = "My Knowledge Model"
 , _kmChapters = []
 }

  