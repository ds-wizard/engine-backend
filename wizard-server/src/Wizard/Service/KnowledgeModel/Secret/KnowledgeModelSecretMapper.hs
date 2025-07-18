module Wizard.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper where

import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeDTO
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret

fromCreateDTO :: KnowledgeModelSecretChangeDTO -> U.UUID -> U.UUID -> UTCTime -> KnowledgeModelSecret
fromCreateDTO dto uuid tenantUuid now =
  KnowledgeModelSecret
    { uuid = uuid
    , name = dto.name
    , value = dto.value
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: KnowledgeModelSecret -> KnowledgeModelSecretChangeDTO -> UTCTime -> KnowledgeModelSecret
fromChangeDTO kmSecret dto now =
  KnowledgeModelSecret
    { uuid = kmSecret.uuid
    , name = dto.name
    , value = dto.value
    , tenantUuid = kmSecret.tenantUuid
    , createdAt = kmSecret.createdAt
    , updatedAt = now
    }
