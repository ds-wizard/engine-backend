module Shared.Common.Service.File.FileService where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import System.Path (fileList)

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String (isSuffixOf)

listFilesWithName :: String -> String -> IO [FilePath]
listFilesWithName folder name = do
  files <- fileList folder
  return $ filter (isSuffixOf name) files

listFilesWithExtension :: String -> String -> IO [FilePath]
listFilesWithExtension folder extension = do
  files <- fileList folder
  return $ filter (isSuffixOf ("." ++ extension)) files

loadJSONFile :: (FromJSON a) => FilePath -> IO (Either AppError a)
loadJSONFile path = do
  content <- B.readFile path
  case eitherDecode content of
    Right obj -> return . Right $ obj
    Left error -> return . Left . GeneralServerError . _ERROR_SERVICE_FILE__CANT_READ_JSON $ path
