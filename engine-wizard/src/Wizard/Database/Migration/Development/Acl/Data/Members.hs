module Wizard.Database.Migration.Development.Acl.Data.Members where

import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Acl.Acl
import Wizard.Model.User.User

bioGroupMember :: Member
bioGroupMember = GroupMember {gId = bioGroup.gId}

albertMember :: Member
albertMember = UserMember {uuid = userAlbert.uuid}

nikolaMember :: Member
nikolaMember = UserMember {uuid = userNikola.uuid}

charlesMember :: Member
charlesMember = UserMember {uuid = userCharles.uuid}
