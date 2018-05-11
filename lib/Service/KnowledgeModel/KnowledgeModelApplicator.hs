module Service.KnowledgeModel.KnowledgeModelApplicator where

import Common.Context
import Common.Error
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Migrator.Applicator

createKnowledgeModel :: [Event] -> Either AppError KnowledgeModel
createKnowledgeModel events = runApplicator Nothing events

recompileKnowledgeModelWithEvents :: Context -> String -> [Event] -> IO (Either AppError KnowledgeModel)
recompileKnowledgeModelWithEvents context branchUuid eventsForBranchUuid = do
  let eitherNewKM = runApplicator Nothing eventsForBranchUuid
  case eitherNewKM of
    Right newKM -> do
      updateKnowledgeModelByBranchId context branchUuid (Just newKM)
      return . Right $ newKM
    Left error -> return . Left $ error
