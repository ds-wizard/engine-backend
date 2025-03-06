module WizardLib.Public.Database.DAO.ExternalLink.ExternalLinkUsageDAO where

import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.ExternalLink.ExternalLinkUsage ()
import WizardLib.Public.Model.ExternalLink.ExternalLinkUsage

entityName = "external_link_usage"

findExternalLinkUsages :: AppContextC s sc m => m [ExternalLinkUsage]
findExternalLinkUsages = createFindEntitiesFn entityName

insertExternalLinkUsage :: AppContextC s sc m => ExternalLinkUsage -> m Int64
insertExternalLinkUsage = createInsertFn entityName

deleteExternalLinkUsages :: AppContextC s sc m => m Int64
deleteExternalLinkUsages = createDeleteEntitiesFn entityName
