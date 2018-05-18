module Service.KnowledgeModel.KnowledgeModelApplicator where

import Common.Error
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.Context.AppContext
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Migrator.Applicator

createKnowledgeModel :: [Event] -> Either AppError KnowledgeModel
createKnowledgeModel = runApplicator Nothing

recompileKnowledgeModelWithEvents :: String -> [Event] -> AppContextM (Either AppError KnowledgeModel)
recompileKnowledgeModelWithEvents branchUuid eventsForBranchUuid = do
  let eitherNewKM = runApplicator Nothing eventsForBranchUuid
  case eitherNewKM of
    Right newKM -> do
      updateKnowledgeModelByBranchId branchUuid (Just newKM)
      return . Right $ newKM
    Left error -> return . Left $ error
