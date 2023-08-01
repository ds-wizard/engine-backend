module Wizard.Service.Context.ContextService where

import qualified Control.Exception.Base as E
import Control.Monad (when)
import Control.Monad.Reader (ask, asks, liftIO)
import Data.Aeson (Value (..), toJSON)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.ContextResult
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.App.AppDAO
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.User.UserMapper as UM
import Wizard.Util.Context

runFunctionForAllApps :: String -> AppContextM (ContextResult, Maybe String) -> AppContextM ()
runFunctionForAllApps functionName function = do
  apps <- findApps
  traverse_ (\app -> runFunctionUnderDifferentUserAndApp Nothing app.uuid functionName function) apps

runFunctionUnderDifferentUserAndApp
  :: Maybe User -> U.UUID -> String -> AppContextM (ContextResult, Maybe String) -> AppContextM ()
runFunctionUnderDifferentUserAndApp mUser appUuid functionName function = do
  logInfoI
    _CMP_SERVICE
    ( f'
        "Running '%s' with app ('%s') and user ('%s') started"
        [functionName, U.toString appUuid, show . fmap (U.toString . ((.uuid))) $ mUser]
    )
  context <- ask
  newTraceUuid <- liftIO generateUuid
  eResult <- liftIO . E.try $ runAppContextWithAppContext function (updateContext mUser appUuid newTraceUuid context)
  let (resultState, mReturnedMessage) =
        case eResult :: Either E.SomeException (Either String (ContextResult, Maybe String)) of
          Right (Right (resultState, mErrorMessage)) -> (resultState, mErrorMessage)
          Right (Left error) -> (ErrorContextResult, Just error)
          Left exception -> (ErrorContextResult, Just . show $ exception)
  case resultState of
    SuccessContextResult ->
      logInfoI
        _CMP_SERVICE
        ( f'
            "Running '%s' with app ('%s') and user ('%s') finished successfully. It returns: '%s''"
            [functionName, U.toString appUuid, show . fmap (U.toString . ((.uuid))) $ mUser, show mReturnedMessage]
        )
    ErrorContextResult -> do
      logInfoI
        _CMP_SERVICE
        ( f'
            "Running '%s' with app ('%s') and user ('%s') failed. The reason is: '%s''"
            [functionName, U.toString appUuid, show . fmap (U.toString . ((.uuid))) $ mUser, show mReturnedMessage]
        )
      sendToSentry mUser appUuid functionName mReturnedMessage

-- --------------------------------
-- PRIVATE
-- --------------------------------
updateContext :: Maybe User -> U.UUID -> U.UUID -> AppContext -> AppContext
updateContext mUser aUuid newTraceUuid context =
  context
    { currentAppUuid = aUuid
    , currentUser = fmap UM.toDTO mUser
    , traceUuid = newTraceUuid
    }

sendToSentry :: Maybe User -> U.UUID -> String -> Maybe String -> AppContextM ()
sendToSentry mUser appUuid functionName mErrorMessage = do
  serverConfig <- asks serverConfig
  when
    serverConfig.sentry.enabled
    ( do
        let sentryDsn = serverConfig.sentry.dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks buildInfoConfig
        let buildVersion = buildInfoConfig.version
        let message = fromMaybe "" mErrorMessage
        liftIO $
          register
            sentryService
            "persistentCommandLogger"
            Error
            message
            (recordUpdate buildVersion mUser appUuid functionName mErrorMessage)
    )

recordUpdate :: String -> Maybe User -> U.UUID -> String -> Maybe String -> SentryRecord -> SentryRecord
recordUpdate buildVersion mUser appUuid functionName mErrorMessage record =
  let userUuid = maybe "anonymous" (U.toString . (.uuid)) mUser
   in record
        { srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              [("sentry.interfaces.User", toJSON $ HashMap.fromList [("id", String . T.pack $ userUuid)])]
        , srTags = HashMap.fromList [("function", functionName), ("appUuid", U.toString appUuid)]
        , srExtra =
            HashMap.fromList
              [ ("function", String . T.pack $ functionName)
              , ("appUuid", String . T.pack . U.toString $ appUuid)
              , ("lastErrorMessage", String . T.pack . fromMaybe "" $ mErrorMessage)
              ]
        }
