module Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages where

import Data.Maybe (fromJust)
import Data.Time

import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Tenant

globalRemotePackage :: KnowledgeModelPackageSimpleDTO
globalRemotePackage =
  KnowledgeModelPackageSimpleDTO
    { pId = globalKmPackage.pId
    , name = globalKmPackage.name
    , organizationId = globalKmPackage.organizationId
    , kmId = globalKmPackage.kmId
    , version = globalKmPackage.version
    , description = globalKmPackage.description
    , organization =
        OrganizationSimple
          { organizationId = orgGlobalSimple.organizationId
          , name = orgGlobalSimple.name
          , logo = Just orgLogo
          }
    , createdAt = globalKmPackage.createdAt
    }

globalNetherlandsPackage :: KnowledgeModelPackageSimpleDTO
globalNetherlandsPackage =
  KnowledgeModelPackageSimpleDTO
    { pId = netherlandsKmPackageV2.pId
    , name = netherlandsKmPackageV2.name
    , organizationId = netherlandsKmPackageV2.organizationId
    , kmId = netherlandsKmPackageV2.kmId
    , version = netherlandsKmPackageV2.version
    , description = netherlandsKmPackageV2.description
    , organization =
        OrganizationSimple
          { organizationId = orgNetherlandsSimple.organizationId
          , name = orgNetherlandsSimple.name
          , logo = Just orgLogo
          }
    , createdAt = globalKmPackage.createdAt
    }

differentPackage :: KnowledgeModelPackage
differentPackage =
  KnowledgeModelPackage
    { pId = "global:different:1.0.0"
    , name = "Different Knowledge Model"
    , organizationId = "global"
    , kmId = "different"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "Empty package"
    , readme = "# Different Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

differentPackageEvents :: [KnowledgeModelPackageEvent]
differentPackageEvents = fmap (toPackageEvent differentPackage.pId differentTenant.uuid) [a_km1]
