module Shared.Common.Model.Context.ContextResult where

import GHC.Generics

data ContextResult
  = SuccessContextResult
  | ErrorContextResult
  deriving (Show, Eq, Generic)
