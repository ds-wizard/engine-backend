module Database.DAO.KnowledgeModel.KnowledgeModelDAO where

import Control.Lens ((^.))
import Database.MongoDB ((=:), findOne, modify, select)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Database.BSON.Branch.BranchWithKM ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.DAO.Branch.BranchDAO
import Database.DAO.Common
import Model.Branch.Branch
import Model.KnowledgeModel.KnowledgeModel

findBranchWithKMByBranchId :: Context -> String -> IO (Either AppError BranchWithKM)
findBranchWithKMByBranchId context branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchWithKMS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeBranchWithKMS

updateKnowledgeModelByBranchId :: Context -> String -> Maybe KnowledgeModel -> IO ()
updateKnowledgeModelByBranchId context branchUuid km = do
  let action = modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["knowledgeModel" =: (km)]]
  runMongoDBPoolDef action (context ^. ctxDbPool)
