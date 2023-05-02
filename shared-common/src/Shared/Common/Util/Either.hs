module Shared.Common.Util.Either where

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither errorValue = maybe (Left errorValue) Right
