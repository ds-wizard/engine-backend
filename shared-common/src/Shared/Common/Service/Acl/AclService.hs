module Shared.Common.Service.Acl.AclService (
  AclContext (..),
  module Shared.Common.Constant.Acl,
) where

import Shared.Common.Constant.Acl

class AclContext m where
  checkPermission :: String -> m ()
