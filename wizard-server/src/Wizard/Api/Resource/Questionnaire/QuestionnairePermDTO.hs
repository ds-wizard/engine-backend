module Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO where

import qualified Data.UUID as U
import GHC.Generics
import GHC.Records

import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.Questionnaire.QuestionnairePerm

data QuestionnairePermDTO = QuestionnairePermDTO
  { questionnaireUuid :: U.UUID
  , member :: MemberDTO
  , perms :: [String]
  }
  deriving (Generic, Eq, Show)

instance Ord QuestionnairePermDTO where
  compare a b = compare a.member.uuid b.member.uuid

instance QuestionnairePermC QuestionnairePermDTO

instance HasField "memberType" QuestionnairePermDTO QuestionnairePermType where
  getField perm =
    case perm.member of
      UserMemberDTO {} -> UserQuestionnairePermType
      UserGroupMemberDTO {} -> UserGroupQuestionnairePermType

instance HasField "memberUuid" QuestionnairePermDTO U.UUID where
  getField perm = perm.member.uuid
