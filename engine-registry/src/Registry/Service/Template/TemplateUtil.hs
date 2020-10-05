module Registry.Service.Template.TemplateUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig

selectOrganizationByOrgId tml = L.find (\org -> (org ^. organizationId) == (tml ^. organizationId))
