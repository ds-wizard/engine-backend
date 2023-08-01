module Wizard.Service.Branch.Collaboration.CollaborationAcl where

import Wizard.Model.Context.AclContext

checkViewPermission _ = checkPermission _KM_PERM

checkEditPermission _ = checkPermission _KM_PERM
