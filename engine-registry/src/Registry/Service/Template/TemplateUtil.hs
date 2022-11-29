module Registry.Service.Template.TemplateUtil where

import qualified Data.List as L

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)
