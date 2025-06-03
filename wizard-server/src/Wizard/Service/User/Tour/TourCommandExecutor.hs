module Wizard.Service.User.Tour.TourCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.User.Tour.TourService
import WizardLib.Public.Model.PersistentCommand.User.Tour.DeleteToursCommand

cComponent = "tour"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cDeleteToursName = cDeleteTours command

cDeleteToursName = "deleteTours"

cDeleteTours :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteTours persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeleteToursCommand
  case eCommand of
    Right command -> do
      deleteTours command.userUuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
