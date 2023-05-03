module Wizard.Integration.Http.Submission.ResponseMapper (
  toUploadDocumentResponse,
) where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Common.Integration.Http.Common.ResponseMapper

toUploadDocumentResponse :: Response BSL.ByteString -> Either String (Maybe String)
toUploadDocumentResponse response = Right $ extractResponseHeader "Location" response
