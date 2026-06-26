module WizardLib.Public.Api.Resource.User.RoleSimpleJM where

import Data.Aeson

import WizardLib.Public.Model.User.RoleSimple

instance ToJSON RoleSimple

instance FromJSON RoleSimple
