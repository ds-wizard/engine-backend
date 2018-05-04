module Service.KnowledgeModel.KnowledgeModelApplicator where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Context
import Common.Error
import Common.Types
import Common.Uuid
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelMapper
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
