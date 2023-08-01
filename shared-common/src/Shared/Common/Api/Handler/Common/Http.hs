module Shared.Common.Api.Handler.Common.Http where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.UUID as U
import GHC.Records
import Servant (Header, Headers, addHeader)

import Shared.Common.Model.Common.Sort
import Shared.Common.Util.String (splitOn)

parseSortQuery :: Maybe String -> [Sort]
parseSortQuery Nothing = []
parseSortQuery (Just query) =
  case splitOn "," query of
    [by, "asc"] -> [Sort {by = by, direction = Ascending}]
    [by, "desc"] -> [Sort {by = by, direction = Descending}]
    _ -> []

addTraceUuidHeader :: (MonadReader s m, HasField "traceUuid'" s U.UUID, MonadIO m) => a -> m (Headers '[Header "x-trace-uuid" String] a)
addTraceUuidHeader result = do
  context <- ask
  let traceUuid = context.traceUuid'
  return $ addHeader (U.toString traceUuid) result
