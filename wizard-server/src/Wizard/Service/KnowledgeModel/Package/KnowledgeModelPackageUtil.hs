module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Localization.Messages.Public

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> p.organizationId == pkg.organizationId && p.kmId == pkg.kmId)

selectOrganizationByOrgId pkg = L.find (\org -> org.organizationId == pkg.organizationId)

checkViewPermissionToKnowledgeModelPackage :: Maybe U.UUID -> AppContextM ()
checkViewPermissionToKnowledgeModelPackage Nothing = return ()
checkViewPermissionToKnowledgeModelPackage (Just pkgUuid) = do
  pkg <- findPackageByUuid pkgUuid
  mCurrentUser <- asks currentUser
  unless
    (pkg.public || isJust mCurrentUser)
    (throwError . ForbiddenError $ _ERROR_SERVICE_USER__MISSING_USER)
