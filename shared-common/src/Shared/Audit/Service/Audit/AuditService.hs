module Shared.Audit.Service.Audit.AuditService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Audit.Database.DAO.Audit.AuditDAO
import Shared.Audit.Service.Audit.AuditMapper
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Uuid

logAudit :: AppContextC s sc m => String -> String -> String -> m ()
logAudit component action entity = logAuditWithBody component action entity M.empty

logAuditWithBody :: AppContextC s sc m => String -> String -> String -> M.Map String String -> m ()
logAuditWithBody component action entity body = do
  aUuid <- liftIO generateUuid
  tenantUuid <- asks (.tenantUuid')
  createdBy <- asks (.identity')
  now <- liftIO getCurrentTime
  let audit = toAudit aUuid component action entity body (fmap (fromJust . U.fromString) createdBy) tenantUuid now
  insertAudit audit
  return ()
