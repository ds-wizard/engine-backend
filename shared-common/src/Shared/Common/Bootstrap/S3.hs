module Shared.Common.Bootstrap.S3 where

import Shared.Common.S3.Common
import Shared.Common.Util.Logger

setupS3Client serverConfig manager = do
  logInfo _CMP_S3 "start creating S3 client"
  s3Client <- createS3Client serverConfig manager
  logInfo _CMP_S3 "S3 client created"
  return s3Client
