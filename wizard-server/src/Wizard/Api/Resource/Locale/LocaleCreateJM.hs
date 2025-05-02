module Wizard.Api.Resource.Locale.LocaleCreateJM where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Servant.Multipart

import Wizard.Api.Resource.Locale.LocaleCreateDTO

instance FromMultipart Mem LocaleCreateDTO where
  fromMultipart form =
    LocaleCreateDTO
      <$> fmap T.unpack (lookupInput "name" form)
      <*> fmap T.unpack (lookupInput "description" form)
      <*> fmap T.unpack (lookupInput "code" form)
      <*> fmap T.unpack (lookupInput "localeId" form)
      <*> fmap T.unpack (lookupInput "version" form)
      <*> fmap T.unpack (lookupInput "license" form)
      <*> fmap T.unpack (lookupInput "readme" form)
      <*> fmap T.unpack (lookupInput "recommendedAppVersion" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "wizardContent" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "mailContent" form)
