module Wizard.S3.Public.PublicS3 where

import qualified Data.ByteString.Char8 as BS

import Shared.Common.S3.Common
import Shared.Common.Util.Logger
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "public"

putPublicContent :: String -> Maybe String -> BS.ByteString -> AppContextM String
putPublicContent configName mContentType = createPutObjectFn (f' "%s/%s" [folderName, configName]) mContentType Nothing

makePublicLink :: String -> AppContextM String
makePublicLink configName = createMakePublicLink (f' "%s/%s" [folderName, configName])

removePublic :: String -> AppContextM ()
removePublic configName = createRemoveObjectFn (f' "%s/%s" [folderName, configName])
