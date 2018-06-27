module Service.KnowledgeModel.KnowledgeModelApplicator where

import Common.Error
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.Context.AppContext
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Migrator.Applicator.Applicator

createKnowledgeModel :: [Event] -> Either AppError KnowledgeModel
createKnowledgeModel = runApplicator Nothing

compileKnowledgeModelFromScratch :: [Event] -> Either AppError KnowledgeModel
compileKnowledgeModelFromScratch = runApplicator Nothing

recompileKnowledgeModelWithEvents :: String -> [Event] -> AppContextM (Either AppError KnowledgeModel)
recompileKnowledgeModelWithEvents branchUuid eventsForBranchUuid =
  case compileKnowledgeModelFromScratch eventsForBranchUuid of
    Right newKM -> do
      updateKnowledgeModelByBranchId branchUuid (Just newKM)
      return . Right $ newKM
    Left error -> return . Left $ error
