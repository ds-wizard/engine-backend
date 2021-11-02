module Wizard.Integration.Http.Submission.ResponseMapper
  ( toUploadDocumentResponse
  ) where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Wizard.Integration.Http.Common.ResponseMapper

toUploadDocumentResponse :: Response BSL.ByteString -> Either String (Maybe String)
toUploadDocumentResponse response = Right $ extractResponseHeader "Location" response
