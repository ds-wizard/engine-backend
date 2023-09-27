module Shared.Common.Constant.Tenant where

import qualified Data.UUID as U

import Shared.Common.Util.Uuid

defaultTenantUuid :: U.UUID
defaultTenantUuid = U.nil

differentTenantUuid :: U.UUID
differentTenantUuid = u' "d9e73946-faa6-449d-83e4-2e38371b7bfa"
