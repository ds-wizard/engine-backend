module Shared.Common.Integration.Aws.Lambda where

import qualified Amazonka as AWS
import Amazonka.Lambda
import Amazonka.Lambda.Invoke
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Shared.Common.Integration.Aws.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

invokeLambda :: AppContextC s sc m => String -> BS.ByteString -> m Bool
invokeLambda functionArn payload = do
  let request =
        Invoke'
          { clientContext = Nothing
          , invocationType = Just InvocationType_Event
          , logType = Nothing
          , qualifier = Nothing
          , functionName = T.pack functionArn
          , payload = payload
          }
  logInfoI _CMP_INTEGRATION (show request)
  response <- runAwsRequest (`AWS.send` request)
  logInfoI _CMP_INTEGRATION (show response)
  return $ response.statusCode == 200
