module Wizard.Specs.Websocket.Common where

import Control.Concurrent.Async
import Control.Lens ((^.))
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Network.Socket
import Network.WebSockets hiding (serverPort)
import Network.WebSockets.Stream hiding (close)

import LensesConfig
import Wizard.Bootstrap.Web
import Wizard.Util.Context

runWebserver appContext = race_ (runWebServer (baseContextFromAppContext appContext))

createConnection appContext reqUrl = do
  let port = read (show $ appContext ^. serverConfig . general . serverPort) :: PortNumber
  let host =
        if port == 80
          then "localhost"
          else "localhost:" ++ show port
  let addr =
        AddrInfo
          { addrFlags = []
          , addrFamily = AF_INET
          , addrSocketType = Stream
          , addrProtocol = 6
          , addrAddress = SockAddrInet port (tupleToHostAddress (127, 0, 0, 1))
          , addrCanonName = Nothing
          }
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock NoDelay 1
  connect sock (addrAddress addr)
  stream <- makeSocketStream sock
  connection <- newClientConnection stream host reqUrl defaultConnectionOptions []
  return (connection, sock)

closeConnection connection = sendClose connection (T.pack "close")

closeSockets = traverse_ close

sendMessage connection content = sendTextData connection (encode content)
