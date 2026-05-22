module Registry.Specs.API.UserEmailLink.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common
import Registry.Specs.API.UserEmailLink.List_POST

userEmailLinkAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "ACTION KEY API Spec" $ list_POST appContext
