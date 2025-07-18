module Wizard.Database.Migration.Development.KnowledgeModelSecret.Data.KnowledgeModelSecrets where

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeDTO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret
import Wizard.Model.Tenant.Tenant

kmSecret1 :: KnowledgeModelSecret
kmSecret1 =
  KnowledgeModelSecret
    { uuid = u' "171635b5-d5e7-4bba-8dd0-93765866aea1"
    , name = "mySecret1"
    , value = "mySecretValue1"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

kmSecret1Edited :: KnowledgeModelSecret
kmSecret1Edited =
  KnowledgeModelSecret
    { uuid = u' "171635b5-d5e7-4bba-8dd0-93765866aea1"
    , name = "EDITED_mySecret1"
    , value = "EDITED_mySecretValue1"
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

kmSecret1ChangeDTO :: KnowledgeModelSecretChangeDTO
kmSecret1ChangeDTO =
  KnowledgeModelSecretChangeDTO
    { name = kmSecret1Edited.name
    , value = kmSecret1Edited.value
    }

kmSecretDifferent :: KnowledgeModelSecret
kmSecretDifferent =
  KnowledgeModelSecret
    { uuid = u' "eb884df9-9ba1-4bf5-85d8-74f3e433cdfb"
    , name = "mySecret1"
    , value = "mySecretValue1"
    , tenantUuid = differentTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }
