module Wizard.Service.Branch.Collaboration.CollaborationAcl where

import Wizard.Service.Acl.AclService

checkViewPermission _ = checkPermission _KM_PERM

checkEditPermission _ = checkPermission _KM_PERM
