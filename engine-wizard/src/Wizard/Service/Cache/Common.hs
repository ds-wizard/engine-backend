module Wizard.Service.Cache.Common where

import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Util.Logger

logCacheWarmBefore :: String -> AppContextM ()
logCacheWarmBefore cacheName = logDebugU _CMP_CACHE (f' "Warm (before): %s" [cacheName])

logCacheWarmAfter :: String -> AppContextM ()
logCacheWarmAfter cacheName = logDebugU _CMP_CACHE (f' "Warm (after): %s" [cacheName])

logCacheAddBefore :: String -> String -> AppContextM ()
logCacheAddBefore cacheName key = logDebugU _CMP_CACHE (f' "Add (before): %s, key: { %s }" [cacheName, key])

logCacheAddAfter :: String -> String -> AppContextM ()
logCacheAddAfter cacheName key = logDebugU _CMP_CACHE (f' "Add (after): %s, key: { %s }" [cacheName, key])

logCacheGetBefore :: String -> String -> AppContextM ()
logCacheGetBefore cacheName key = logDebugU _CMP_CACHE (f' "Get (before): '%s', key: { %s }" [cacheName, key])

logCacheGetFound :: String -> String -> AppContextM ()
logCacheGetFound cacheName key = logDebugU _CMP_CACHE (f' "Get (found): '%s', key: { %s }" [cacheName, key])

logCacheGetMissed :: String -> String -> AppContextM ()
logCacheGetMissed cacheName key = logDebugU _CMP_CACHE (f' "Get (missed): '%s', key: { %s }" [cacheName, key])

logCacheModifyBefore :: String -> String -> AppContextM ()
logCacheModifyBefore cacheName key = logDebugU _CMP_CACHE (f' "Modify (before): %s, key: { %s }" [cacheName, key])

logCacheModifyAfter :: String -> String -> AppContextM ()
logCacheModifyAfter cacheName key = logDebugU _CMP_CACHE (f' "Modify (after): %s, key: { %s }" [cacheName, key])

logCacheDeleteBefore :: String -> String -> AppContextM ()
logCacheDeleteBefore cacheName key = logDebugU _CMP_CACHE (f' "Delete (before): '%s', key: { %s }" [cacheName, key])

logCacheDeleteFinished :: String -> String -> AppContextM ()
logCacheDeleteFinished cacheName key = logDebugU _CMP_CACHE (f' "Delete (finished): '%s', key: { %s }" [cacheName, key])
