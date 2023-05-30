module WizardLib.Public.Service.Cache.Common where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Util.Logger

logCacheWarmBefore :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> m ()
logCacheWarmBefore cacheName = logDebugI _CMP_CACHE (f' "Warm (before): %s" [cacheName])

logCacheWarmAfter :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> m ()
logCacheWarmAfter cacheName = logDebugI _CMP_CACHE (f' "Warm (after): %s" [cacheName])

logCacheAddBefore :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheAddBefore cacheName key = logDebugI _CMP_CACHE (f' "Add (before): %s, key: { %s }" [cacheName, key])

logCacheAddAfter :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheAddAfter cacheName key = logDebugI _CMP_CACHE (f' "Add (after): %s, key: { %s }" [cacheName, key])

logCacheGetBefore :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheGetBefore cacheName key = logDebugI _CMP_CACHE (f' "Get (before): '%s', key: { %s }" [cacheName, key])

logCacheGetFound :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheGetFound cacheName key = logDebugI _CMP_CACHE (f' "Get (found): '%s', key: { %s }" [cacheName, key])

logCacheGetMissed :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheGetMissed cacheName key = logDebugI _CMP_CACHE (f' "Get (missed): '%s', key: { %s }" [cacheName, key])

logCacheModifyBefore :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheModifyBefore cacheName key = logDebugI _CMP_CACHE (f' "Modify (before): %s, key: { %s }" [cacheName, key])

logCacheModifyAfter :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheModifyAfter cacheName key = logDebugI _CMP_CACHE (f' "Modify (after): %s, key: { %s }" [cacheName, key])

logCacheDeleteAllBefore :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> m ()
logCacheDeleteAllBefore cacheName = logDebugI _CMP_CACHE (f' "Delete All (before): '%s'" [cacheName])

logCacheDeleteAllFinished :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> m ()
logCacheDeleteAllFinished cacheName = logDebugI _CMP_CACHE (f' "Delete All (finished): '%s'" [cacheName])

logCacheDeleteBefore :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheDeleteBefore cacheName key = logDebugI _CMP_CACHE (f' "Delete (before): '%s', key: { %s }" [cacheName, key])

logCacheDeleteFinished :: (MonadReader s m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> String -> m ()
logCacheDeleteFinished cacheName key = logDebugI _CMP_CACHE (f' "Delete (finished): '%s', key: { %s }" [cacheName, key])
