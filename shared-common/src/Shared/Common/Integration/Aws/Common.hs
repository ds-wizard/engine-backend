module Shared.Common.Integration.Aws.Common where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWSAuth
import Control.Monad.Reader (ask, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString.Char8 as BS
import Data.Int
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified System.IO as IO

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext

runAwsRequest :: String -> String -> String -> (AWSAuth.Env -> ResourceT IO response) -> IO response
runAwsRequest awsAccessKeyId awsSecretAccessKey awsRegion function = do
  let accessKeyId = AWSAuth.AccessKey $ BS.pack awsAccessKeyId
  let secretAccessKey = AWSAuth.SecretKey $ BS.pack awsSecretAccessKey
  logger <- AWS.newLogger AWS.Trace IO.stdout
  envWithoutRegion <- AWS.newEnv (return . AWSAuth.fromKeys accessKeyId secretAccessKey)
  let env = envWithoutRegion {AWS.region = AWS.Region' (T.pack awsRegion)}
  AWS.runResourceT (function env)

runAwsRequestWithContext :: AppContextC s sc m => (AWSAuth.Env -> ResourceT IO response) -> m response
runAwsRequestWithContext function = do
  context <- ask
  liftIO $
    runAwsRequest
      context.serverConfig'.aws'.awsAccessKeyId
      context.serverConfig'.aws'.awsSecretAccessKey
      context.serverConfig'.aws'.awsRegion
      function

utcTimeToAwsTime :: UTCTime -> Int64
utcTimeToAwsTime = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
