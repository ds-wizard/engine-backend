module Shared.Common.Util.Error where

import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, catchError)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M

import Shared.Common.Model.Error.Error

tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

mergeErrors :: AppError -> AppError -> AppError
mergeErrors (ValidationError oldFormErrors oldFieldErrors) (ValidationError newFormErrors newFieldErrors) =
  ValidationError (oldFormErrors ++ newFormErrors) (M.foldlWithKey (\oldMap newKey newValue -> M.insertWith (++) newKey newValue oldMap) oldFieldErrors newFieldErrors)
mergeErrors oldError newError = newError

isEmptyError :: AppError -> Bool
isEmptyError (ValidationError formErrors fieldErrors) = null formErrors && M.null fieldErrors
isEmptyError _ = False

combineErrors :: MonadError AppError m => [m ()] -> m ()
combineErrors functions = do
  error <- foldl run (return $ ValidationError [] M.empty) functions
  unless
    (isEmptyError error)
    (throwError error)
  where
    run :: MonadError AppError m => m AppError -> m () -> m AppError
    run errorM function = do
      oldError <- errorM
      eResult <- tryError function
      case eResult of
        Right result -> return oldError
        Left newError -> return $ mergeErrors oldError newError

combineErrorsWithContext :: MonadError AppError m => [acc] -> [[acc] -> m [acc]] -> m ()
combineErrorsWithContext acc functions = do
  (_, error) <- foldl run (return (acc, ValidationError [] M.empty)) functions
  unless
    (isEmptyError error)
    (throwError error)
  where
    run :: MonadError AppError m => m ([acc], AppError) -> ([acc] -> m [acc]) -> m ([acc], AppError)
    run context function = do
      (oldAcc, oldError) <- context
      eResult <- tryError (function oldAcc)
      case eResult of
        Right newAcc -> return (oldAcc ++ newAcc, oldError)
        Left newError -> return (oldAcc, mergeErrors oldError newError)
