module Wizard.Cache.CacheFactory where

import qualified Data.Cache as C

import Shared.Common.Util.Clock
import Wizard.Model.Cache.ServerCache

createServerCache serverConfig = do
  let [dataExp, websocketExp] = fmap (Just . fromHoursToTimeSpec) [serverConfig.cache.dataExpiration, serverConfig.cache.websocketExpiration]
  knowledgeModelEditorWebsocket <- C.newCache websocketExp
  projectWebsocket <- C.newCache websocketExp
  user <- C.newCache dataExp
  userToken <- C.newCache dataExp
  return ServerCache {..}
