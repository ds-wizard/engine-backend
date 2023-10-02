module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO where

import Data.Time
import GHC.Generics

import RegistryLib.Model.Organization.OrganizationSimple
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

data DocumentTemplateSimpleDTO = DocumentTemplateSimpleDTO
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , nonEditable :: Bool
  , state :: DocumentTemplateState
  , organization :: Maybe OrganizationSimple
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
