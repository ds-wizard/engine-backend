module Wizard.Util.Cache where

getFromCacheOrDb getFromCache addToCache findInDb key = do
  mRecord <- getFromCache key
  case mRecord of
    Just record -> return record
    Nothing -> do
      user <- findInDb key
      addToCache user
      return user
