module Shared.Database.Migration.Development.KnowledgeModel.Data.Choices where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

q11_choice1 :: Choice
q11_choice1 =
  Choice {_choiceUuid = u' "43d4ffe7-c19b-486f-8e04-9d76d7ea333a", _choiceLabel = "Choice 1", _choiceAnnotations = []}

q11_choice1Edited :: Choice
q11_choice1Edited =
  q11_choice1 {_choiceLabel = "EDITED: Choice 1", _choiceAnnotations = [MapEntry "newAnnotation" "someValue"]}

q11_choice2 :: Choice
q11_choice2 =
  Choice {_choiceUuid = u' "24b6d097-1be1-44d1-95bd-e7cf50052093", _choiceLabel = "Choice 2", _choiceAnnotations = []}

q11_choice3 :: Choice
q11_choice3 =
  Choice {_choiceUuid = u' "deb3d004-bff0-48d8-ab7e-25e18fa9508a", _choiceLabel = "Choice 3", _choiceAnnotations = []}
