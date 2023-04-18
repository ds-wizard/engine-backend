module Shared.Common.Model.Error.Error where

import qualified Data.Map.Strict as M
import Network.HTTP.Types.Status

import Shared.Common.Model.Localization.LocaleRecord

data AppError
  = AcceptedError
  | FoundError String
  | ValidationError [LocaleRecord] (M.Map String [LocaleRecord])
  | UserError LocaleRecord
  | SystemLogError LocaleRecord
  | UnauthorizedError LocaleRecord
  | ForbiddenError LocaleRecord
  | NotExistsError LocaleRecord
  | LockedError
  | GeneralServerError String
  | HttpClientError Status String
  deriving (Show, Eq)
