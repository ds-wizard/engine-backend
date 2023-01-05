module Wizard.Api.Resource.PackageBundle.PackageBundleFileJM where

import qualified Data.Text as T
import Servant.Multipart

import Wizard.Api.Resource.PackageBundle.PackageBundleFileDTO

instance FromMultipart Mem PackageBundleFileDTO where
  fromMultipart form =
    PackageBundleFileDTO
      <$> fmap (T.unpack . fdFileName) (lookupFile "file" form)
      <*> fmap (T.unpack . fdFileCType) (lookupFile "file" form)
      <*> wrapMaybe (lookupInput "rootElement" form)
      <*> wrapMaybe (fmap T.unpack (lookupInput "name" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "organizationId" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "kmId" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "version" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "previousPackageId" form))
      <*> fmap fdPayload (lookupFile "file" form)

wrapMaybe :: Either String a -> Either String (Maybe a)
wrapMaybe eResult =
  case eResult of
    Right result -> Right (Just result)
    Left _ -> Right Nothing
