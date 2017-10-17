module Api.Resources.User.UserDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data UserDTO = UserDTO
  { _udtoUuid :: UUID
  , _udtoName :: String
  , _udtoSurname :: String
  , _udtoRole :: Role
  , _udtoPermissions :: [Permission]
  }

makeLenses ''UserDTO

instance ToJSON UserDTO where
  toJSON UserDTO {..} =
    object
      [ "uuid" .= _udtoUuid
      , "name" .= _udtoName
      , "surname" .= _udtoSurname
      , "role" .= _udtoRole
      , "permissions" .= _udtoPermissions
      ]


-- instance Functor f => ToJSON (f UserDTO) where
--   toJSON dtos = fmap toJSON dtos