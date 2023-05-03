module Shared.Common.Util.Cache where

getFromCacheOrDb getFromCache addToCache findInDb key = do
  mRecord <- getFromCache key
  case mRecord of
    Just record -> return record
    Nothing -> do
      user <- findInDb key
      addToCache user
      return user

getFromCacheOrDb' getFromCache addToCache findInDb key = do
  mRecord <- getFromCache key
  case mRecord of
    Just record -> return . Just $ record
    Nothing -> do
      mUser <- findInDb key
      case mUser of
        Just user -> do
          addToCache user
          return . Just $ user
        Nothing -> return Nothing
