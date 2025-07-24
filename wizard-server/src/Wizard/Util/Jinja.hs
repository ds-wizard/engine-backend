module Wizard.Util.Jinja (renderJinjaSingle, renderJinjaBatch, verifyJinja) where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.String
import GHC.Generics
import System.Exit (exitFailure)

-- FFI to dynamic rendering function
foreign import ccall "render_jinja"
  c_render_jinja :: CString -> IO CString

-- FFI to C's free function
foreign import ccall "free_string"
  c_free_string :: CString -> IO ()

data JinjaInput = JinjaInput
  { template :: String
  , contexts :: [Value]
  }
  deriving (Generic, Show)

instance ToJSON JinjaInput
instance FromJSON JinjaInput

data JinjaResult = JinjaResult
  { result :: String
  , message :: Maybe String
  , ok :: Bool
  }
  deriving (Generic, Show)

instance ToJSON JinjaResult
instance FromJSON JinjaResult

renderJinjaSingle :: String -> Value -> IO (Either String String)
renderJinjaSingle templateStr itemContext = do
  let inputStructure = JinjaInput templateStr [itemContext]
  results <- renderJinja' inputStructure
  case results of
    Left err -> return $ Left err
    Right (r : _) ->
      return $
        if r.ok
          then Right (r.result)
          else Left $ fromMaybe "Unknown rendering error" (r.message)
    _ -> return $ Left "No results returned from Jinja rendering"

renderJinjaBatch :: String -> [Value] -> IO [Either String String]
renderJinjaBatch templateStr itemContexts = do
  let inputStructure = JinjaInput templateStr itemContexts
  results <- renderJinja' inputStructure
  case results of
    Left err -> return [Left err]
    Right jinjaResults -> return $ map processResult jinjaResults
  where
    processResult r =
      if r.ok
        then Right (r.result)
        else Left $ fromMaybe "Unknown rendering error" (r.message)

renderJinja' :: JinjaInput -> IO (Either String [JinjaResult])
renderJinja' inputStructure = do
  let input = BS.toStrict (encode inputStructure)

  BS.useAsCString input $ \cinput -> do
    resultCStr <- c_render_jinja cinput
    if resultCStr == nullPtr
      then return $ Left "Failed to render Jinja template"
      else do
        resultBS <- BS.packCString resultCStr
        c_free_string resultCStr
        let decoded = decodeStrict resultBS :: Maybe [JinjaResult]
        case decoded of
          Just results -> return $ Right results
          Nothing -> return $ Left "Failed to decode Jinja results"

verifyJinja :: IO ()
verifyJinja = do
  let template = "Testing Jinja rendering: {{ var }}"
      context = object ["var" .= ("passed" :: String)]
  result <- renderJinjaSingle template context
  putStrLn "Verifying Jinja rendering:"
  case result of
    Left err -> do
      putStrLn $ "- error: " ++ err
      exitFailure
    Right output -> putStrLn $ "- success: " ++ output
