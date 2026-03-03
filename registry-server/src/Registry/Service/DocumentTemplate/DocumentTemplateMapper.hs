module Registry.Service.DocumentTemplate.DocumentTemplateMapper where

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Service.DocumentTemplate.DocumentTemplateUtil
import qualified Registry.Service.Organization.OrganizationMapper as OM_Mapper
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Model.Organization.Organization
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

toSimpleDTO :: [Organization] -> DocumentTemplate -> DocumentTemplateSimpleDTO
toSimpleDTO orgs dt =
  DocumentTemplateSimpleDTO
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , description = dt.description
    , organization = fmap OM_Mapper.toSimpleDTO . selectOrganizationByOrgId dt $ orgs
    , createdAt = dt.createdAt
    }

toDetailDTO :: DocumentTemplate -> [String] -> Organization -> DocumentTemplateDetailDTO
toDetailDTO dt versions org =
  DocumentTemplateDetailDTO
    { uuid = dt.uuid
    , name = dt.name
    , organizationId = dt.organizationId
    , templateId = dt.templateId
    , version = dt.version
    , metamodelVersion = dt.metamodelVersion
    , description = dt.description
    , readme = dt.readme
    , license = dt.license
    , versions = versions
    , organization = OM_Mapper.toSimpleDTO org
    , createdAt = dt.createdAt
    }
