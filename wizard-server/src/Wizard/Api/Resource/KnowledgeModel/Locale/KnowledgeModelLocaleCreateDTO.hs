module Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO where

import qualified Data.ByteString.Char8 as BS
import GHC.Generics

data KnowledgeModelLocaleCreateDTO = KnowledgeModelLocaleCreateDTO
  { name :: String
  , poContent :: BS.ByteString
  , jsonContent :: BS.ByteString
  }
  deriving (Show, Eq, Generic)
