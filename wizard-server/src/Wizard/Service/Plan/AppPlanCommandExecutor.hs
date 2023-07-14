module Wizard.Service.Plan.AppPlanCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Plan.AppPlanMapper
import Wizard.Service.Plan.AppPlanService
import Wizard.Util.Logger
import WizardLib.Public.Model.PersistentCommand.Plan.CreateOrUpdatePlanCommand
import WizardLib.Public.Model.PersistentCommand.Plan.DeletePlanCommand

cComponent = "plan"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreatePlanName = cCreatePlan command
  | command.function == cUpdatePlanName = cUpdatePlan command
  | command.function == cDeletePlanName = cDeletePlan command

cCreatePlanName = "createPlan"

cCreatePlan :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreatePlan persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePlanCommand
  case eCommand of
    Right command -> do
      let reqDto = fromCommandToChangeDTO command
      createPlanWithUuid command.appUuid command.uuid reqDto
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdatePlanName = "updatePlan"

cUpdatePlan :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdatePlan persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePlanCommand
  case eCommand of
    Right command -> do
      let reqDto = fromCommandToChangeDTO command
      modifyPlan command.appUuid command.uuid reqDto
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeletePlanName = "deletePlan"

cDeletePlan :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeletePlan persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeletePlanCommand
  case eCommand of
    Right command -> do
      deletePlan command.appUuid command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
