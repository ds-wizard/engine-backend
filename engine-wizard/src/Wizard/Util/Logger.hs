module Wizard.Util.Logger
  ( logDebugU
  , logInfoU
  , logWarnU
  , logErrorU
  , LogLevel(..)
  , module Shared.Constant.Component
  , module Shared.Util.Logger
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Logger (logWithoutLoc)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (Value(..), toJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.UUID as U
import Prelude hiding (log)
import System.Log.Raven (initRaven, register, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel(Error), SentryRecord(..))

import LensesConfig
import Shared.Constant.Component
import Shared.Util.Logger
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext

logDebugU :: String -> String -> AppContextM ()
logDebugU = logU LevelDebug

logInfoU :: String -> String -> AppContextM ()
logInfoU = logU LevelInfo

logWarnU :: String -> String -> AppContextM ()
logWarnU = logU LevelWarn

logErrorU :: String -> String -> AppContextM ()
logErrorU component message = do
  logU LevelError component message
  sendToSentry component message

-- ---------------------------------------------------------------------------
logU :: LogLevel -> String -> String -> AppContextM ()
logU logLevel component message = do
  mUser <- asks _appContextCurrentUser
  traceUuid <- asks _appContextTraceUuid
  let mUserUuid = fmap (U.toString . _userDTOUuid) mUser
  let mTraceUuid = Just . U.toString $ traceUuid
  let record = createLogRecord logLevel mUserUuid mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

sendToSentry :: String -> String -> AppContextM ()
sendToSentry component message = do
  mUser <- asks _appContextCurrentUser
  traceUuid <- asks _appContextTraceUuid
  serverConfig <- asks _appContextServerConfig
  when
    (serverConfig ^. sentry . enabled)
    (do let sentryDsn = serverConfig ^. sentry . dsn
        sentryService <- liftIO $ initRaven sentryDsn id sendRecord stderrFallback
        buildInfoConfig <- asks _appContextBuildInfoConfig
        let buildVersion = buildInfoConfig ^. version
        appUuid <- asks _appContextAppUuid
        liftIO $
          register sentryService "messageLogger" Error message (recordUpdate buildVersion mUser traceUuid appUuid))

recordUpdate :: String -> Maybe UserDTO -> U.UUID -> U.UUID -> SentryRecord -> SentryRecord
recordUpdate buildVersion mUser traceUuid appUuid record =
  let userUuid = T.pack . maybe "" (U.toString . _userDTOUuid) $ mUser
      userEmail = T.pack . maybe "" _userDTOEmail $ mUser
   in record
        { srRelease = Just buildVersion
        , srInterfaces =
            HashMap.fromList
              [ ( "sentry.interfaces.User"
                , toJSON $ HashMap.fromList [("id", String userUuid), ("email", String userEmail)])
              ]
        , srTags = HashMap.fromList [("traceUuid", U.toString traceUuid), ("appUuid", U.toString appUuid)]
        , srExtra =
            HashMap.fromList
              [ ("traceUuid", String . T.pack . U.toString $ traceUuid)
              , ("appUuid", String . T.pack . U.toString $ appUuid)
              ]
        }
