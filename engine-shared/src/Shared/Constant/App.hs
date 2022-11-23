module Shared.Constant.App where

import qualified Data.UUID as U

import Shared.Util.Uuid

defaultAppUuid :: U.UUID
defaultAppUuid = U.nil

differentAppUuid :: U.UUID
differentAppUuid = u' "d9e73946-faa6-449d-83e4-2e38371b7bfa"
