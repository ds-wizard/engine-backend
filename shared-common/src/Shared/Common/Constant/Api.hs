module Shared.Common.Constant.Api where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.Header (HeaderName)

authorizationHeaderName :: String
authorizationHeaderName = "Authorization"

xTraceUuidHeaderName :: String
xTraceUuidHeaderName = "x-trace-uuid"

xUserCountHeaderName :: String
xUserCountHeaderName = "x-user-count"

xKnowledgeModelPackageCountHeaderName :: String
xKnowledgeModelPackageCountHeaderName = "x-knowledge-model-package-count"

xProjectCountHeaderName :: String
xProjectCountHeaderName = "x-project-count"

xKnowledgeModelEditorCountHeaderName :: String
xKnowledgeModelEditorCountHeaderName = "x-knowledge-model-editor-count"

xDocCountHeaderName :: String
xDocCountHeaderName = "x-doc-count"

xTmlCountHeaderName :: String
xTmlCountHeaderName = "x-tml-count"

contentTypeHeaderJSON :: (HeaderName, BS.ByteString)
contentTypeHeaderJSON = ("Content-Type", "application/json")
