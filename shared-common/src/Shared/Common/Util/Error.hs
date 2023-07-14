module Shared.Common.Util.Error where

import Control.Monad.Error.Class (MonadError, catchError)

tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
