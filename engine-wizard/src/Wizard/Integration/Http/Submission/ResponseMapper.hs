module Wizard.Integration.Http.Submission.ResponseMapper
  ( toUploadDocumentResponse
  ) where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Model.Error.Error
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Integration.Http.Common.ResponseMapper

toUploadDocumentResponse :: Response BSL.ByteString -> Either AppError SubmissionDTO
toUploadDocumentResponse response =
  let mLocation = extractResponseHeader "Location" response
   in Right $ SubmissionDTO {_submissionDTOLocation = mLocation}
