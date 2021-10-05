module Wizard.Model.Questionnaire.QuestionnaireAcl where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Acl.Acl

data QuestionnairePermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid :: U.UUID
    , _questionnairePermRecordQuestionnaireUuid :: U.UUID
    , _questionnairePermRecordPerms :: [String]
    , _questionnairePermRecordMember :: Member
    }
  deriving (Generic, Eq, Show)

ownerPermissions = [_VIEW_PERM, _COMMENT_PERM, _EDIT_PERM, _ADMIN_PERM]

editorPermissions = [_VIEW_PERM, _COMMENT_PERM, _EDIT_PERM]

commentatorPermissions = [_VIEW_PERM, _COMMENT_PERM]

viewerPermissions = [_VIEW_PERM]
