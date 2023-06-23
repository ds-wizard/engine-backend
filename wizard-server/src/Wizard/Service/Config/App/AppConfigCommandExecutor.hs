module Wizard.Service.Config.App.AppConfigCommandExecutor where

import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.PersistentCommand.Config.InvokeClientCssCompilationCommand
import Wizard.Service.Config.App.AppConfigService
import Wizard.Util.Logger

cComponent = "AppConfig"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cInvokeClientCssCompilationName = cInvokeClientCssCompilation command

recompileCssInAllApplications :: AppContextM ()
recompileCssInAllApplications =
  runInTransaction $ do
    apps <- findApps
    traverse_ (\a -> recompileCssInApplication a.uuid) apps

recompileCssInApplication :: U.UUID -> AppContextM ()
recompileCssInApplication appUuid = do
  pUuid <- liftIO generateUuid
  user <- getCurrentUser
  now <- liftIO getCurrentTime
  let command =
        toPersistentCommand
          pUuid
          cComponent
          cInvokeClientCssCompilationName
          (BSL.unpack . encode $ InvokeClientCssCompilationCommand appUuid)
          1
          True
          Nothing
          appUuid
          (Just . U.toString $ user.uuid)
          now
  insertPersistentCommand command
  return ()

cInvokeClientCssCompilationName = "invokeClientCssCompilation"

cInvokeClientCssCompilation :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cInvokeClientCssCompilation persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String InvokeClientCssCompilationCommand
  case eCommand of
    Right command -> do
      appConfig <- getAppConfigByUuid command.appUuid
      updatedAppConfig <- invokeClientCssCompilation appConfig appConfig
      modifyAppConfig updatedAppConfig
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
