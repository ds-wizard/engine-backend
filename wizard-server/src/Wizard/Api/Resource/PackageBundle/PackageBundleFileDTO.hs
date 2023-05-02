module Wizard.Api.Resource.PackageBundle.PackageBundleFileDTO where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T

data PackageBundleFileDTO = PackageBundleFileDTO
  { fileName :: String
  , contentType :: String
  , rootElement :: Maybe T.Text
  , name :: Maybe String
  , organizationId :: Maybe String
  , kmId :: Maybe String
  , version :: Maybe String
  , previousPackageId :: Maybe String
  , content :: BSL.ByteString
  }
