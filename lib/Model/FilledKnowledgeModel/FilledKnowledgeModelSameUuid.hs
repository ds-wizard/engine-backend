module Model.FilledKnowledgeModel.FilledKnowledgeModelSameUuid where

import Control.Lens

import LensesConfig
import Model.Common
import Model.FilledKnowledgeModel.FilledKnowledgeModel

instance SameUuid FilledKnowledgeModel FilledKnowledgeModel where
  equalsUuid km1 km2 = km1 ^. uuid == km2 ^. uuid

instance SameUuid FilledChapter FilledChapter where
  equalsUuid ch1 ch2 = ch1 ^. uuid == ch2 ^. uuid

instance SameUuid FilledQuestion FilledQuestion where
  equalsUuid q1 q2 = q1 ^. uuid == q2 ^. uuid

instance SameUuid FilledAnswer FilledAnswer where
  equalsUuid ans1 ans2 = ans1 ^. uuid == ans2 ^. uuid
