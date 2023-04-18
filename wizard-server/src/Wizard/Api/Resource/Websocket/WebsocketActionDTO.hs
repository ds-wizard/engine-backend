module Wizard.Api.Resource.Websocket.WebsocketActionDTO where

import GHC.Generics

import Shared.Common.Model.Error.Error

data Success_ServerActionDTO a = Success_ServerActionDTO
  { aData :: a
  }
  deriving (Show, Eq, Generic)

data Error_ServerActionDTO = Error_ServerActionDTO
  { aData :: AppError
  }
  deriving (Show, Eq, Generic)
