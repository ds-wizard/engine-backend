module Registry.Database.Migration.Development.Template.Data.Templates where

import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Service.Template.TemplateMapper
import Shared.Database.Migration.Development.Template.Data.Templates

commonWizardTemplateSimpleDTO :: TemplateSimpleDTO
commonWizardTemplateSimpleDTO = toSimpleDTO commonWizardTemplate orgGlobal

commonWizardTemplateDetailDTO :: TemplateDetailDTO
commonWizardTemplateDetailDTO = toDetailDTO commonWizardTemplate ["1.0.0"] orgGlobal
