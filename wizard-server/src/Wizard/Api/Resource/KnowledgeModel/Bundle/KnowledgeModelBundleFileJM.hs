module Wizard.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleFileJM where

import qualified Data.Text as T
import Servant.Multipart

import Shared.Coordinate.Util.Coordinate
import Wizard.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile

instance FromMultipart Mem KnowledgeModelBundleFile where
  fromMultipart form =
    KnowledgeModelBundleFile
      <$> fmap (T.unpack . fdFileName) (lookupFile "file" form)
      <*> fmap (T.unpack . fdFileCType) (lookupFile "file" form)
      <*> wrapMaybe (lookupInput "rootElement" form)
      <*> wrapMaybe (fmap T.unpack (lookupInput "name" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "organizationId" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "kmId" form))
      <*> wrapMaybe (fmap T.unpack (lookupInput "version" form))
      <*> wrapMaybe (mergeEither parseCoordinateT (lookupInput "previousPackageId" form))
      <*> fmap fdPayload (lookupFile "file" form)

wrapMaybe :: Either String a -> Either String (Maybe a)
wrapMaybe eResult =
  case eResult of
    Right result -> Right (Just result)
    Left _ -> Right Nothing

mergeEither :: (b -> Either String a) -> Either String b -> Either String a
mergeEither mapFn eResult =
  case eResult of
    Right result -> mapFn result
    Left error -> Left error
