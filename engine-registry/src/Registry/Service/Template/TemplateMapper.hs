module Registry.Service.Template.TemplateMapper where

import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Model.Organization.Organization
import qualified Registry.Service.Organization.OrganizationMapper as OM_Mapper
import Registry.Service.Template.TemplateUtil
import Shared.Model.Template.Template

toSimpleDTO :: [Organization] -> Template -> TemplateSimpleDTO
toSimpleDTO orgs template =
  TemplateSimpleDTO
    { tId = template.tId
    , name = template.name
    , organizationId = template.organizationId
    , templateId = template.templateId
    , version = template.version
    , description = template.description
    , organization = fmap OM_Mapper.toSimpleDTO . selectOrganizationByOrgId template $ orgs
    , createdAt = template.createdAt
    }

toDetailDTO :: Template -> [String] -> Organization -> TemplateDetailDTO
toDetailDTO template versions org =
  TemplateDetailDTO
    { tId = template.tId
    , name = template.name
    , organizationId = template.organizationId
    , templateId = template.templateId
    , version = template.version
    , metamodelVersion = template.metamodelVersion
    , description = template.description
    , readme = template.readme
    , license = template.license
    , versions = versions
    , organization = OM_Mapper.toSimpleDTO org
    , createdAt = template.createdAt
    }
