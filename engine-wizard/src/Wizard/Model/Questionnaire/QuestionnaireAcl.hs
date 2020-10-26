module Wizard.Model.Questionnaire.QuestionnaireAcl where

import GHC.Generics

import Wizard.Model.Acl.Acl

data QuestionnairePermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordMember :: Member
    , _questionnairePermRecordPerms :: [String]
    }
  deriving (Generic, Eq, Show)

ownerPermissions = [_VIEW_PERM, _EDIT_PERM, _ADMIN_PERM]

editorPermissions = [_VIEW_PERM, _EDIT_PERM]

viewerPermissions = [_VIEW_PERM]
