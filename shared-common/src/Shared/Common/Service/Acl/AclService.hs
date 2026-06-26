module Shared.Common.Service.Acl.AclService where

class AclContext m where
  hasPermission :: String -> m Bool
  checkPermission :: String -> m ()
