module Shared.Model.Error.Error where

import qualified Data.Map.Strict as M
import Network.HTTP.Types.Status

import Shared.Model.Localization.LocaleRecord

data AppError
  = AcceptedError
  | FoundError String
  | ValidationError [LocaleRecord] (M.Map String [LocaleRecord])
  | UserError LocaleRecord
  | UnauthorizedError LocaleRecord
  | ForbiddenError LocaleRecord
  | NotExistsError LocaleRecord
  | GeneralServerError String
  | HttpClientError Status String
  deriving (Show, Eq)
