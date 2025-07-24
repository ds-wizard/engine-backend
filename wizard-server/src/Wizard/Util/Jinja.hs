module Wizard.Util.Jinja (renderJinjaSingle, renderJinjaMultiple, renderJinjaBatch, verifyJinja) where

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
  { templates :: [String]
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
renderJinjaSingle template itemContext = do
  let inputStructure = JinjaInput [template] [itemContext]
  eResults <- renderJinja' inputStructure
  case eResults of
    Left err -> return $ Left err
    Right (r : _) ->
      return $
        if r.ok
          then Right (r.result)
          else Left $ fromMaybe "Unknown rendering error" (r.message)
    _ -> return $ Left "No results returned from Jinja rendering"

renderJinjaMultiple :: [String] -> Value -> IO [Either String String]
renderJinjaMultiple templates itemContext = do
  let inputStructure = JinjaInput templates [itemContext]
  eResults <- renderJinja' inputStructure
  case eResults of
    Left err -> return [Left err]
    Right jinjaResults -> return $ map processResult jinjaResults
  where
    processResult r =
      if r.ok
        then Right (r.result)
        else Left $ fromMaybe "Unknown rendering error" (r.message)

renderJinjaBatch :: String -> [Value] -> IO [Either String String]
renderJinjaBatch template itemContexts = do
  let inputStructure = JinjaInput [template] itemContexts
  eResults <- renderJinja' inputStructure
  case eResults of
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
        let mResults = decodeStrict resultBS :: Maybe [JinjaResult]
        case mResults of
          Just results -> return $ Right results
          Nothing -> return $ Left "Failed to decode Jinja results"

verifyJinja :: IO ()
verifyJinja = do
  let template = "Testing Jinja rendering: {{ var }}"
      context = object ["var" .= ("passed" :: String)]
  eResult <- renderJinjaSingle template context
  putStrLn "Verifying Jinja rendering:"
  case eResult of
    Left err -> do
      putStrLn $ "- error: " ++ err
      exitFailure
    Right output -> putStrLn $ "- success: " ++ output
