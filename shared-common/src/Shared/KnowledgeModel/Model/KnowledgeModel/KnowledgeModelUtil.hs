module Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelUtil where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Prelude hiding (id)
import qualified Prelude

import Shared.Common.Util.Map (insertFlipped)
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

isItListQuestion :: KnowledgeModel -> U.UUID -> Bool
isItListQuestion km itemUuid =
  case M.lookup itemUuid (getQuestionsM km) of
    Just (ListQuestion' _) -> True
    _ -> False

makeParentMap :: KnowledgeModel -> M.Map U.UUID U.UUID
makeParentMap km = M.union chMap . M.union qMap $ ansMap
  where
    chMap :: M.Map U.UUID U.UUID
    chMap = foldr chMapGo M.empty (getChaptersL km)
    chMapGo :: Chapter -> M.Map U.UUID U.UUID -> M.Map U.UUID U.UUID
    chMapGo ch = M.union (foldr (insertFlipped ch.uuid) M.empty ch.questionUuids)
    -- -------
    qMap :: M.Map U.UUID U.UUID
    qMap = foldr qMapGo M.empty (getQuestionsL km)
    qMapGo :: Question -> M.Map U.UUID U.UUID -> M.Map U.UUID U.UUID
    qMapGo (OptionsQuestion' q) = M.union (foldr (insertFlipped q.uuid) M.empty q.answerUuids)
    qMapGo (ListQuestion' q) = M.union (foldr (insertFlipped q.uuid) M.empty q.itemTemplateQuestionUuids)
    qMapGo _ = Prelude.id
    -- -------
    ansMap :: M.Map U.UUID U.UUID
    ansMap = foldr ansMapGo M.empty (getAnswersL km)
    ansMapGo :: Answer -> M.Map U.UUID U.UUID -> M.Map U.UUID U.UUID
    ansMapGo ans = M.union (foldr (insertFlipped ans.uuid) M.empty ans.followUpUuids)
