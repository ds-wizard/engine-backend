module Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds where

import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle

openIdClientDefinitionParameter :: OpenIdClientParameter
openIdClientDefinitionParameter =
  OpenIdClientParameter
    { name = "hd2"
    , value = "google.com"
    }

openIdClientDefinitionStyle :: OpenIdClientStyle
openIdClientDefinitionStyle =
  OpenIdClientStyle
    { icon = Just "fa-google"
    , background = Just "#000"
    , color = Just "#FFF"
    }
