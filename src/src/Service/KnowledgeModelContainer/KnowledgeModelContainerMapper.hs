module Service.KnowledgeModelContainer.KnowledgeModelContainerMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Common.Types
import Model.KnowledgeModelContainer.KnowledgeModelContainer

toDTO :: KnowledgeModelContainer -> KnowledgeModelContainerDTO
toDTO kmc =
  KnowledgeModelContainerDTO
  { _kmcdtoKmContainerUuid = kmc ^. kmcKmContainerUuid
  , _kmcdtoName = kmc ^. kmcName
  , _kmcdtoShortname = kmc ^. kmcShortname
  , _kmcdtoParentPackageName = kmc ^. kmcParentPackageName
  , _kmcdtoParentPackageVersion = kmc ^. kmcParentPackageVersion
  }

fromDTO :: KnowledgeModelContainerDTO -> KnowledgeModelContainer
fromDTO dto =
  KnowledgeModelContainer
  { _kmcKmContainerUuid = dto ^. kmcdtoKmContainerUuid
  , _kmcName = dto ^. kmcdtoName
  , _kmcShortname = dto ^. kmcdtoShortname
  , _kmcParentPackageName = dto ^. kmcdtoParentPackageName
  , _kmcParentPackageVersion = dto ^. kmcdtoParentPackageVersion
  }
