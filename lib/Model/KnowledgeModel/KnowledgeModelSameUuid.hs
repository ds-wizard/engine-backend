module Model.KnowledgeModel.KnowledgeModelSameUuid where

import Control.Lens

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

instance SameUuid KnowledgeModel KnowledgeModel where
  equalsUuid km1 km2 = km1 ^. uuid == km2 ^. uuid

instance SameUuid Chapter Chapter where
  equalsUuid ch1 ch2 = ch1 ^. uuid == ch2 ^. uuid

instance SameUuid Question Question where
  equalsUuid q1 q2 = q1 ^. uuid == q2 ^. uuid

instance SameUuid Answer Answer where
  equalsUuid ans1 ans2 = ans1 ^. uuid == ans2 ^. uuid

instance SameUuid Expert Expert where
  equalsUuid exp1 exp2 = exp1 ^. uuid == exp2 ^. uuid

instance SameUuid Reference Reference where
  equalsUuid ref1 ref2 = ref1 ^. uuid == ref2 ^. uuid
