module Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationAcl where

import Wizard.Model.Context.AclContext

checkViewPermission _ = checkPermission _KM_PERM

checkEditPermission _ = checkPermission _KM_PERM
