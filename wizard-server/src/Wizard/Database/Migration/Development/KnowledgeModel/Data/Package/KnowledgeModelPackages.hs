module Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages where

import Data.Maybe (fromJust)
import Data.Time

import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Tenant.Tenant
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

globalRemotePackage :: KnowledgeModelPackageSimpleDTO
globalRemotePackage =
  KnowledgeModelPackageSimpleDTO
    { uuid = globalKmPackage.uuid
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

globalKmPackageDetailDto :: KnowledgeModelPackageDetailDTO
globalKmPackageDetailDto =
  toDetailDTO
    globalKmPackage
    True
    [globalRegistryPackage]
    [globalRegistryOrganization, nlRegistryOrganization]
    [(globalKmPackageEmpty.uuid, globalKmPackageEmpty.version), (globalKmPackage.uuid, globalKmPackage.version)]
    (Just $ "https://registry-test.ds-wizard.org/knowledge-models/" ++ show (createCoordinate globalKmPackage))

globalNetherlandsPackage :: KnowledgeModelPackageSimpleDTO
globalNetherlandsPackage =
  KnowledgeModelPackageSimpleDTO
    { uuid = netherlandsKmPackageV2.uuid
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

globalNetherlandsPackageDetailDto :: KnowledgeModelPackageDetailDTO
globalNetherlandsPackageDetailDto =
  toDetailDTO
    netherlandsKmPackageV2
    True
    [nlRegistryPackage]
    [globalRegistryOrganization, nlRegistryOrganization]
    [(netherlandsKmPackage.uuid, netherlandsKmPackage.version), (netherlandsKmPackageV2.uuid, netherlandsKmPackageV2.version)]
    (Just $ "https://registry-test.ds-wizard.org/knowledge-models/" ++ show (createCoordinate netherlandsKmPackageV2))

germanyPackageSuggestion :: KnowledgeModelPackageSuggestion
germanyPackageSuggestion = toSuggestion germanyKmPackage

differentPackage :: KnowledgeModelPackage
differentPackage =
  KnowledgeModelPackage
    { uuid = u' "5dc5209a-03ad-425d-9aa0-6ad2aef94563"
    , name = "Different Knowledge Model"
    , organizationId = "global"
    , kmId = "different"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = knowledgeModelMetamodelVersion
    , description = "Empty package"
    , readme = "# Different Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageUuid = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , public = False
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

differentPackageEvents :: [KnowledgeModelPackageEvent]
differentPackageEvents = fmap (toPackageEvent differentPackage.uuid differentTenant.uuid) [a_km1]
