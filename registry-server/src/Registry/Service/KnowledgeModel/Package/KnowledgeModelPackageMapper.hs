module Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import qualified Registry.Service.Organization.OrganizationMapper as OM
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import qualified RegistryLib.Model.Organization.Organization as Organization
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

toSimpleDTO :: KnowledgeModelPackage -> Organization.Organization -> KnowledgeModelPackageSimpleDTO
toSimpleDTO pkg org =
  KnowledgeModelPackageSimpleDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , description = pkg.description
    , createdAt = pkg.createdAt
    , organization = OM.toSimpleDTO org
    }

toDetailDTO :: KnowledgeModelPackage -> [String] -> Organization.Organization -> KnowledgeModelPackageDetailDTO
toDetailDTO pkg versions org =
  KnowledgeModelPackageDetailDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , metamodelVersion = pkg.metamodelVersion
    , previousPackageId = pkg.previousPackageId
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , versions = versions
    , organization = OM.toSimpleDTO org
    , createdAt = pkg.createdAt
    }
