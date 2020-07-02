module Registry.Service.Template.TemplateMapper where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Model.Organization.Organization
import qualified Registry.Service.Organization.OrganizationMapper as OM_Mapper
import Shared.Model.Template.Template

toSimpleDTO :: Template -> Organization -> TemplateSimpleDTO
toSimpleDTO template org =
  TemplateSimpleDTO
    { _templateSimpleDTOTId = template ^. tId
    , _templateSimpleDTOName = template ^. name
    , _templateSimpleDTOOrganizationId = template ^. organizationId
    , _templateSimpleDTOTemplateId = template ^. templateId
    , _templateSimpleDTOVersion = template ^. version
    , _templateSimpleDTODescription = template ^. description
    , _templateSimpleDTOOrganization = OM_Mapper.toSimpleDTO org
    , _templateSimpleDTOCreatedAt = template ^. createdAt
    }

toDetailDTO :: Template -> [String] -> Organization -> TemplateDetailDTO
toDetailDTO template versions org =
  TemplateDetailDTO
    { _templateDetailDTOTId = template ^. tId
    , _templateDetailDTOName = template ^. name
    , _templateDetailDTOOrganizationId = template ^. organizationId
    , _templateDetailDTOTemplateId = template ^. templateId
    , _templateDetailDTOVersion = template ^. version
    , _templateDetailDTOMetamodelVersion = template ^. metamodelVersion
    , _templateDetailDTODescription = template ^. description
    , _templateDetailDTOReadme = template ^. readme
    , _templateDetailDTOLicense = template ^. license
    , _templateDetailDTOVersions = versions
    , _templateDetailDTOOrganization = OM_Mapper.toSimpleDTO org
    , _templateDetailDTOCreatedAt = template ^. createdAt
    }
