module Wizard.Service.User.UserAudit where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditUserCreateByAdmin :: UserDTO -> AppContextM ()
auditUserCreateByAdmin userDto =
  logAuditWithBody
    "user"
    "createByAdmin"
    (U.toString $ userDto ^. uuid)
    (M.fromList [("firstName", userDto ^. firstName), ("lastName", userDto ^. lastName), ("email", userDto ^. email)])
