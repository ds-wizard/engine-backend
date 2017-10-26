module KMMigration.Model.UserManagement where

type Role = String

data User = User
  { _uId :: Int
  , _uName :: String
  , _uSurname :: String
  , _uPasswordHash :: String
  , _uSalt :: String
  -- , _uRole :: Role
--   , _permissions :: [Permission]
  }
-- class Permission p where
--     name :: p -> String
-- data CreateUserPermission = CreateUserPermission
-- instance Permission CreateUserPermission where
--     name = "CREATE_USER_PERMISSION"
-- getPermissions :: User -> [Permission]
-- getPermissions user = []
-- checkPermission :: User -> Permission -> Bool
-- checkPermission user perm = getPermissions user
