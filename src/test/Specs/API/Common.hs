module Specs.API.Common where

import Data.Aeson (Value(..), object, (.=))
import Data.ByteString
import Network.HTTP.Types.Header
import Network.Wai (Application)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Web.Scotty as S

import Application
import Context
import DSPConfig

startWebApp :: Context -> DSPConfig -> IO Application
startWebApp context dspConfig = S.scottyApp (createEndpoints context dspConfig)

reqAuthHeader :: Header
reqAuthHeader =
  ( "Authorization"
  , "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyVXVpZCI6ImVjNmY4ZTkwLTJhOTEtNDllYy1hYTNmLTllYWIyMjY3ZmM2NiIsInJvbGVzIjpbIkFERF9DSEFQVEVSIiwiRURJVF9DSEFQVEVSIiwiREVMRVRFX0NIQVBURVIiXX0.5LmuMLSg8HoYGkH1QMECJksEXvsNXoRJ128pLEXXnVg")

reqCtHeader :: Header
reqCtHeader = ("Content-Type", "application/json")

resCtHeader = "Content-Type" <:> "application/json"

resCorsHeaders =
  [ "Access-Control-Allow-Credential" <:> "true"
  , "Access-Control-Allow-Headers" <:>
    "Origin, X-Requested-With, Content-Type, Accept, Authorization"
  , "Access-Control-Allow-Methods" <:> "OPTIONS, HEAD, GET, POST, PUT, DELETE"
  , "Access-Control-Allow-Origin" <:> "*"
  ]

createAuthTest method url headers body =
  it "HTTP 401 UNAUTHORIZED" $ do
    let expBody =
          [json|
    {
      status: 401,
      error: "Unauthorized"
    }
    |]
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expStatus = 401
    request method url headers body `shouldRespondWith`
      expBody {matchHeaders = expHeaders, matchStatus = expStatus}

fakeLogState :: String -> IO ()
fakeLogState _ = return ()
