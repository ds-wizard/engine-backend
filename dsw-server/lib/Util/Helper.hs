module Util.Helper where

createHeeHelper fn callback = do
  eitherResult <- fn
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error

createHemHelper fn callback = do
  eitherResult <- fn
  case eitherResult of
    Right result -> callback result
    Left error -> return . Just $ error

createHmeHelper fn callback = do
  eitherResult <- fn
  case eitherResult of
    Nothing -> callback
    Just error -> return . Left $ error

createHmmHelper fn callback = do
  eitherResult <- fn
  case eitherResult of
    Nothing -> callback
    Just error -> return . Just $ error
