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

runAwsRequest :: AppContextC s sc m => (AWSAuth.Env -> ResourceT IO response) -> m response
runAwsRequest function = do
  context <- ask
  liftIO $ do
    let accessKeyId = AWSAuth.AccessKey $ BS.pack context.serverConfig'.aws'.awsAccessKeyId
    let secretAccessKey = AWSAuth.SecretKey $ BS.pack context.serverConfig'.aws'.awsSecretAccessKey
    logger <- AWS.newLogger AWS.Trace IO.stdout
    envWithoutRegion <- AWS.newEnv (return . AWSAuth.fromKeys accessKeyId secretAccessKey)
    let env = envWithoutRegion {AWS.region = AWS.Region' (T.pack context.serverConfig'.aws'.awsRegion)}
    AWS.runResourceT (function env)

utcTimeToAwsTime :: UTCTime -> Int64
utcTimeToAwsTime = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
