module Registry.Service.DocumentTemplate.DocumentTemplateService where

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Context.AppContext
import Registry.Service.DocumentTemplate.DocumentTemplateMapper
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Common.Model.Common.SemVer2Tuple
import WizardLib.Common.Util.Coordinate
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO hiding (findDocumentTemplatesFiltered)
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil

getDocumentTemplates :: [(String, String)] -> Maybe SemVer2Tuple -> AppContextM [DocumentTemplateSimpleDTO]
getDocumentTemplates queryParams mMetamodelVersion = do
  tmls <- findDocumentTemplatesFiltered queryParams mMetamodelVersion
  orgs <- findOrganizations
  return . fmap (toSimpleDTO orgs) . chooseTheNewest . groupDocumentTemplates $ tmls

getDocumentTemplateById :: String -> AppContextM DocumentTemplateDetailDTO
getDocumentTemplateById tId = do
  resolvedId <- resolveDocumentTemplateId tId
  tml <- findDocumentTemplateById resolvedId
  versions <- getDocumentTemplateVersions tml
  org <- findOrganizationByOrgId tml.organizationId
  return $ toDetailDTO tml versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getDocumentTemplateVersions :: DocumentTemplate -> AppContextM [String]
getDocumentTemplateVersions tml = do
  allTmls <- findDocumentTemplatesByOrganizationIdAndKmId tml.organizationId tml.templateId
  return . fmap (.version) $ allTmls
