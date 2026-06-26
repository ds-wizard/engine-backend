module Wizard.Database.Migration.Development.User.Data.Roles where

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Tenant
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RolePermission

adminRole :: Role
adminRole =
  Role
    { uuid = u' "a0000000-0000-0000-0000-000000000001"
    , name = "Admin"
    , permissions =
        [ -- Shared
          _LOCALES_MANAGE_ROLE_PERMISSION
        , _SETTINGS_MANAGE_ROLE_PERMISSION
        , _USERS_MANAGE_ROLE_PERMISSION
        , -- Wizard
          _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
        , _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
        , _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
        , _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
        , _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION
        , _PROJECTS_COMMENT_ROLE_PERMISSION
        , _PROJECTS_EDIT_ROLE_PERMISSION
        , _PROJECTS_MANAGE_ROLE_PERMISSION
        , _PROJECTS_VIEW_ROLE_PERMISSION
        ]
    , isAdmin = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

dataStewardRole :: Role
dataStewardRole =
  Role
    { uuid = u' "a0000000-0000-0000-0000-000000000002"
    , name = "Data Steward"
    , permissions =
        [ _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION
        , _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
        , _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
        , _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
        , _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
        ]
    , isAdmin = False
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

researcherRole :: Role
researcherRole =
  Role
    { uuid = u' "a0000000-0000-0000-0000-000000000003"
    , name = "Researcher"
    , permissions = []
    , isAdmin = False
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

deletableRole :: Role
deletableRole =
  researcherRole
    { uuid = u' "a0000000-0000-0000-0000-0000000000ff"
    , name = "Temporary Role"
    , permissions = []
    , isAdmin = False
    }

differentAdminRole :: Role
differentAdminRole =
  adminRole
    { uuid = u' "a0000000-0000-0000-0000-000000000011"
    , tenantUuid = differentTenant.uuid
    }

differentDataStewardRole :: Role
differentDataStewardRole =
  dataStewardRole
    { uuid = u' "a0000000-0000-0000-0000-000000000012"
    , tenantUuid = differentTenant.uuid
    }

differentResearcherRole :: Role
differentResearcherRole =
  researcherRole
    { uuid = u' "a0000000-0000-0000-0000-000000000013"
    , tenantUuid = differentTenant.uuid
    }
