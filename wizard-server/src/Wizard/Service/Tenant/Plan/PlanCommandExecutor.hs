module Wizard.Service.Tenant.Plan.PlanCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.Plan.PlanMapper
import Wizard.Service.Tenant.Plan.PlanService
import WizardLib.Public.Model.PersistentCommand.Tenant.Plan.CreateOrUpdatePlanCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Plan.DeletePlanCommand

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
      createPlanWithUuid command.uuid command.tenantUuid reqDto
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdatePlanName = "updatePlan"

cUpdatePlan :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdatePlan persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdatePlanCommand
  case eCommand of
    Right command -> do
      let reqDto = fromCommandToChangeDTO command
      modifyPlan command.tenantUuid command.uuid reqDto
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeletePlanName = "deletePlan"

cDeletePlan :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeletePlan persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeletePlanCommand
  case eCommand of
    Right command -> do
      deletePlan command.tenantUuid command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
