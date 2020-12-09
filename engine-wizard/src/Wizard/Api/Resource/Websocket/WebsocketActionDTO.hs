module Wizard.Api.Resource.Websocket.WebsocketActionDTO where

import GHC.Generics

import Shared.Model.Error.Error

data Success_ServerActionDTO a =
  Success_ServerActionDTO
    { _success_ServerActionDTOData :: a
    }
  deriving (Show, Eq, Generic)

data Error_ServerActionDTO =
  Error_ServerActionDTO
    { _error_ServerActionDTOData :: AppError
    }
  deriving (Show, Eq, Generic)
