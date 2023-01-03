module Registry.Service.Template.TemplateService where

import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.DAO.Template.TemplateDAO
import Registry.Model.Context.AppContext
import Registry.Service.Template.TemplateMapper
import Shared.Database.DAO.Template.TemplateDAO hiding (findTemplatesFiltered)
import Shared.Model.Template.Template
import Shared.Service.Template.TemplateUtil
import Shared.Util.Coordinate

getTemplates :: [(String, String)] -> Maybe Int -> AppContextM [TemplateSimpleDTO]
getTemplates queryParams mMetamodelVersion = do
  tmpls <- findTemplatesFiltered queryParams mMetamodelVersion
  orgs <- findOrganizations
  return . fmap (toSimpleDTO orgs) . chooseTheNewest . groupTemplates $ tmpls

getTemplateById :: String -> AppContextM TemplateDetailDTO
getTemplateById tId = do
  resolvedId <- resolveTemplateId tId
  tml <- findTemplateById resolvedId
  versions <- getTemplateVersions tml
  org <- findOrganizationByOrgId tml.organizationId
  return $ toDetailDTO tml versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateVersions :: Template -> AppContextM [String]
getTemplateVersions tml = do
  allTmls <- findTemplatesByOrganizationIdAndKmId tml.organizationId tml.templateId
  return . fmap (.version) $ allTmls
