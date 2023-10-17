module Wizard.Model.Questionnaire.QuestionnairePerm where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Acl.Acl

data QuestionnairePermType
  = UserQuestionnairePermType
  | UserGroupQuestionnairePermType
  deriving (Show, Eq, Generic, Read)

data QuestionnairePerm = QuestionnairePerm
  { questionnaireUuid :: U.UUID
  , memberType :: QuestionnairePermType
  , memberUuid :: U.UUID
  , perms :: [String]
  , tenantUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

ownerPermissions = [_VIEW_PERM, _COMMENT_PERM, _EDIT_PERM, _ADMIN_PERM]

editorPermissions = [_VIEW_PERM, _COMMENT_PERM, _EDIT_PERM]

commentatorPermissions = [_VIEW_PERM, _COMMENT_PERM]

viewerPermissions = [_VIEW_PERM]
