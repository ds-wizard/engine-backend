module Wizard.Integration.Http.Submission.Runner (
  uploadDocument,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M

import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Submission.RequestMapper
import Wizard.Integration.Http.Submission.ResponseMapper
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext

uploadDocument
  :: AppConfigSubmissionServiceRequest
  -> M.Map String String
  -> BS.ByteString
  -> AppContextM (Either String (Maybe String))
uploadDocument reqTemplate variables reqBody =
  runRequest' (toUploadDocumentRequest reqTemplate variables reqBody) toUploadDocumentResponse
