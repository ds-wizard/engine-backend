module Service.File.FileService where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.IO (FilePath)

import Localization
import Model.Error.Error

listFilesWithExtension :: String -> String -> IO [FilePath]
listFilesWithExtension folder extension = do
  allFiles <- listDirectory folder
  return $ filterFilesWithExtension allFiles extension

filterFilesWithExtension :: [FilePath] -> String -> [FilePath]
filterFilesWithExtension files extension = filter (\f -> T.isSuffixOf (T.pack $ "." ++ extension) (T.pack f)) files

loadJSONFile :: (FromJSON a) => FilePath -> IO (Either AppError a)
loadJSONFile path = do
  content <- B.readFile $ path
  case eitherDecode content of
    Right obj -> return . Right $ obj
    Left error -> return . Left . GeneralServerError . _ERROR_SERVICE_FILE__CANT_READ_JSON $ path
