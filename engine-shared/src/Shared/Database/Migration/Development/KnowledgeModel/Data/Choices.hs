module Shared.Database.Migration.Development.KnowledgeModel.Data.Choices where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

q11_choice1 :: Choice
q11_choice1 =
  Choice {uuid = u' "43d4ffe7-c19b-486f-8e04-9d76d7ea333a", aLabel = "Choice 1", annotations = []}

q11_choice1Edited :: Choice
q11_choice1Edited =
  q11_choice1 {aLabel = "EDITED: Choice 1", annotations = [MapEntry "newAnnotation" "someValue"]}

q11_choice2 :: Choice
q11_choice2 =
  Choice {uuid = u' "24b6d097-1be1-44d1-95bd-e7cf50052093", aLabel = "Choice 2", annotations = []}

q11_choice3 :: Choice
q11_choice3 =
  Choice {uuid = u' "deb3d004-bff0-48d8-ab7e-25e18fa9508a", aLabel = "Choice 3", annotations = []}
