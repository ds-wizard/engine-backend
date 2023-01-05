module Registry.Service.DocumentTemplate.DocumentTemplateMapper where

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Registry.Model.Organization.Organization
import Registry.Service.DocumentTemplate.DocumentTemplateUtil
import qualified Registry.Service.Organization.OrganizationMapper as OM_Mapper
import Shared.Model.DocumentTemplate.DocumentTemplate

toSimpleDTO :: [Organization] -> DocumentTemplate -> DocumentTemplateSimpleDTO
toSimpleDTO orgs tml =
  DocumentTemplateSimpleDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , description = tml.description
    , organization = fmap OM_Mapper.toSimpleDTO . selectOrganizationByOrgId tml $ orgs
    , createdAt = tml.createdAt
    }

toDetailDTO :: DocumentTemplate -> [String] -> Organization -> DocumentTemplateDetailDTO
toDetailDTO tml versions org =
  DocumentTemplateDetailDTO
    { tId = tml.tId
    , name = tml.name
    , organizationId = tml.organizationId
    , templateId = tml.templateId
    , version = tml.version
    , metamodelVersion = tml.metamodelVersion
    , description = tml.description
    , readme = tml.readme
    , license = tml.license
    , versions = versions
    , organization = OM_Mapper.toSimpleDTO org
    , createdAt = tml.createdAt
    }
