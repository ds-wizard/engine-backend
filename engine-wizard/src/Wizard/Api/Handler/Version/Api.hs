module Wizard.Api.Handler.Version.Api where

import Servant

import Wizard.Api.Handler.Version.Detail_Version_Detail_PUT
import Wizard.Model.Context.BaseContext

type VersionAPI = Detail_Version_Detail_PUT

versionApi :: Proxy VersionAPI
versionApi = Proxy

versionServer :: ServerT VersionAPI BaseContextM
versionServer = detail_version_detail_PUT
