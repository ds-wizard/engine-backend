module Service.Organization.OrganizationMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Organization.OrganizationDTO
import Common.Types
import Model.Organization.Organization

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
  { _orgdtoUuid = organization ^. orgUuid
  , _orgdtoName = organization ^. orgName
  , _orgdtoNamespace = organization ^. orgNamespace
  }

fromDTO :: OrganizationDTO -> Organization
fromDTO dto =
  Organization
  { _orgUuid = dto ^. orgdtoUuid
  , _orgName = dto ^. orgdtoName
  , _orgNamespace = dto ^. orgdtoNamespace
  }
