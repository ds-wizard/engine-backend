module Database.DAO.KnowledgeModel.KnowledgeModelDAO where

import Database.MongoDB ((=:), findOne, modify, select)

import Database.BSON.Branch.BranchWithKM ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.DAO.Branch.BranchDAO
import Database.DAO.Common
import Model.Branch.Branch
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel

findBranchWithKMByBranchId :: String -> AppContextM (Either AppError BranchWithKM)
findBranchWithKMByBranchId branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchWithKMS <- runDB action
  return . deserializeMaybeEntity $ maybeBranchWithKMS

updateKnowledgeModelByBranchId :: String -> Maybe KnowledgeModel -> AppContextM ()
updateKnowledgeModelByBranchId branchUuid km = do
  let action = modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["knowledgeModel" =: (km)]]
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindBranchWithKMByBranchId branchUuid callback = do
  eitherBranchWithKM <- findBranchWithKMByBranchId branchUuid
  case eitherBranchWithKM of
    Right branchWithKM -> callback branchWithKM
    Left error -> return . Left $ error
