module Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateJM where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Servant.Multipart

import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO

instance FromMultipart Mem KnowledgeModelLocaleCreateDTO where
  fromMultipart form =
    KnowledgeModelLocaleCreateDTO
      <$> fmap T.unpack (lookupInput "name" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "poContent" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "jsonContent" form)
