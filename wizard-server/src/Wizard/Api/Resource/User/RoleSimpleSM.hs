module Wizard.Api.Resource.User.RoleSimpleSM where

import Data.Swagger

import WizardLib.Public.Api.Resource.User.RoleSimpleJM ()
import WizardLib.Public.Model.User.RoleSimple

instance ToSchema RoleSimple
