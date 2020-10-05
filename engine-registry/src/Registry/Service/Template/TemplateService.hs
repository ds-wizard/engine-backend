module Registry.Service.Template.TemplateService where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Model.Context.AppContext
import Registry.Service.Template.TemplateMapper
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Model.Template.Template
import Shared.Service.Template.TemplateUtil
import Shared.Util.Identifier

getTemplates :: [(String, String)] -> AppContextM [TemplateSimpleDTO]
getTemplates queryParams = do
  tmpls <- findTemplatesFiltered queryParams
  orgs <- findOrganizations
  return . fmap (toSimpleDTO orgs) . chooseTheNewest . groupTemplates $ tmpls

getTemplateById :: String -> AppContextM TemplateDetailDTO
getTemplateById tId = do
  tml <- findTemplateById tId
  versions <- getTemplateVersions tml
  org <- findOrganizationByOrgId (tml ^. organizationId)
  return $ toDetailDTO tml versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTemplateVersions :: Template -> AppContextM [String]
getTemplateVersions tml = do
  allTmls <- findTemplatesByOrganizationIdAndKmId (tml ^. organizationId) (tml ^. templateId)
  return . fmap _templateVersion $ allTmls
