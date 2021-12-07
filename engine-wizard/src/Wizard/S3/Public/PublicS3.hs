module Wizard.S3.Public.PublicS3 where

import qualified Data.ByteString.Char8 as BS

import Shared.S3.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

folderName = "public"

putPublicContent :: String -> Maybe String -> BS.ByteString -> AppContextM String
putPublicContent configName = createPutObjectFn (f' "%s/%s" [folderName, configName])

makePublicLink :: String -> AppContextM String
makePublicLink = createMakePublicLink folderName

removePublic :: String -> AppContextM ()
removePublic configName = createRemoveObjectFn (f' "%s/%s" [folderName, configName])
