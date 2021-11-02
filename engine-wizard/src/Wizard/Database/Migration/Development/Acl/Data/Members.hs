module Wizard.Database.Migration.Development.Acl.Data.Members where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Acl.Acl

bioGroupMember :: Member
bioGroupMember = GroupMember {_groupMemberGId = bioGroup ^. gId}

albertMember :: Member
albertMember = UserMember {_userMemberUuid = userAlbert ^. uuid}

nikolaMember :: Member
nikolaMember = UserMember {_userMemberUuid = userNikola ^. uuid}

charlesMember :: Member
charlesMember = UserMember {_userMemberUuid = userCharles ^. uuid}
