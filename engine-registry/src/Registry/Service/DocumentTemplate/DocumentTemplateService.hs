module Registry.Service.DocumentTemplate.DocumentTemplateService where

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Registry.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Context.AppContext
import Registry.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO hiding (findDocumentTemplatesFiltered)
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.DocumentTemplateUtil
import Shared.Util.Coordinate

getDocumentTemplates :: [(String, String)] -> Maybe Int -> AppContextM [DocumentTemplateSimpleDTO]
getDocumentTemplates queryParams mMetamodelVersion = do
  tmpls <- findDocumentTemplatesFiltered queryParams mMetamodelVersion
  orgs <- findOrganizations
  return . fmap (toSimpleDTO orgs) . chooseTheNewest . groupDocumentTemplates $ tmpls

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
