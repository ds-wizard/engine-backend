module Shared.Bootstrap.S3 where

import Shared.S3.Common
import Shared.Util.Logger

setupS3Client serverConfig manager = do
  logInfo _CMP_LOCALIZATION "start creating S3 client"
  s3Client <- createS3Client serverConfig manager
  logInfo _CMP_LOCALIZATION "S3 client created"
  return s3Client
