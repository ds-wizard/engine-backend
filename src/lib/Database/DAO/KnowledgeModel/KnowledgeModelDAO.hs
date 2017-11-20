module Database.DAO.KnowledgeModel.KnowledgeModelDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, modify, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Common.Types
import Database.BSON.Branch.BranchWithKM
import Database.BSON.KnowledgeModel.KnowledgeModel
import Database.DAO.Branch.BranchDAO
import Database.DAO.Common
import Model.Branch.Branch
import Model.KnowledgeModel.KnowledgeModel

findKnowledgeModelByBranchId :: Context -> String -> IO (Either AppError BranchWithKM)
findKnowledgeModelByBranchId context branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeKMS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeKMS

updateKnowledgeModelByBranchId :: Context -> String -> KnowledgeModel -> IO ()
updateKnowledgeModelByBranchId context branchUuid km = do
  let action = modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["knowledgeModel" =: (toBSON km)]]
  runMongoDBPoolDef action (context ^. ctxDbPool)
