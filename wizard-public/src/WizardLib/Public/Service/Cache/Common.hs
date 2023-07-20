module WizardLib.Public.Service.Cache.Common where

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

logCacheWarmBefore :: AppContextC s sc m => String -> m ()
logCacheWarmBefore cacheName = logDebugI _CMP_CACHE (f' "Warm (before): %s" [cacheName])

logCacheWarmAfter :: AppContextC s sc m => String -> m ()
logCacheWarmAfter cacheName = logDebugI _CMP_CACHE (f' "Warm (after): %s" [cacheName])

logCacheAddBefore :: AppContextC s sc m => String -> String -> m ()
logCacheAddBefore cacheName key = logDebugI _CMP_CACHE (f' "Add (before): %s, key: { %s }" [cacheName, key])

logCacheAddAfter :: AppContextC s sc m => String -> String -> m ()
logCacheAddAfter cacheName key = logDebugI _CMP_CACHE (f' "Add (after): %s, key: { %s }" [cacheName, key])

logCacheGetBefore :: AppContextC s sc m => String -> String -> m ()
logCacheGetBefore cacheName key = logDebugI _CMP_CACHE (f' "Get (before): '%s', key: { %s }" [cacheName, key])

logCacheGetFound :: AppContextC s sc m => String -> String -> m ()
logCacheGetFound cacheName key = logDebugI _CMP_CACHE (f' "Get (found): '%s', key: { %s }" [cacheName, key])

logCacheGetMissed :: AppContextC s sc m => String -> String -> m ()
logCacheGetMissed cacheName key = logDebugI _CMP_CACHE (f' "Get (missed): '%s', key: { %s }" [cacheName, key])

logCacheModifyBefore :: AppContextC s sc m => String -> String -> m ()
logCacheModifyBefore cacheName key = logDebugI _CMP_CACHE (f' "Modify (before): %s, key: { %s }" [cacheName, key])

logCacheModifyAfter :: AppContextC s sc m => String -> String -> m ()
logCacheModifyAfter cacheName key = logDebugI _CMP_CACHE (f' "Modify (after): %s, key: { %s }" [cacheName, key])

logCacheDeleteAllBefore :: AppContextC s sc m => String -> m ()
logCacheDeleteAllBefore cacheName = logDebugI _CMP_CACHE (f' "Delete All (before): '%s'" [cacheName])

logCacheDeleteAllFinished :: AppContextC s sc m => String -> m ()
logCacheDeleteAllFinished cacheName = logDebugI _CMP_CACHE (f' "Delete All (finished): '%s'" [cacheName])

logCacheDeleteBefore :: AppContextC s sc m => String -> String -> m ()
logCacheDeleteBefore cacheName key = logDebugI _CMP_CACHE (f' "Delete (before): '%s', key: { %s }" [cacheName, key])

logCacheDeleteFinished :: AppContextC s sc m => String -> String -> m ()
logCacheDeleteFinished cacheName key = logDebugI _CMP_CACHE (f' "Delete (finished): '%s', key: { %s }" [cacheName, key])
