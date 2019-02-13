module Service.KnowledgeModel.KnowledgeModelApplicator where

import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.Context.AppContext
import Model.Error.Error
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

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateKnowledgeModel events callback = do
  let eitherKm = createKnowledgeModel $ events
  case eitherKm of
    Right km -> callback km
    Left error -> return . Left $ error

-- -----------------------------------------------------
heCompileKnowledgeModelFromScratch events callback =
  case compileKnowledgeModelFromScratch events of
    Right km -> callback km
    Left error -> return . Left $ error

hmCompileKnowledgeModelFromScratch events callback =
  case compileKnowledgeModelFromScratch events of
    Right km -> callback km
    Left error -> return . Just $ error
