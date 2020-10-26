module Wizard.Database.Migration.Development.Acl.Data.Groups where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Model.Acl.Acl

bioGroup :: Group
bioGroup =
  Group {_groupGId = "biogroup", _groupName = "Bio Group", _groupDescription = "Some description about bio group"}

plantGroup :: Group
plantGroup =
  Group {_groupGId = "plantgroup", _groupName = "Plant Group", _groupDescription = "Some description about plant group"}

animalGroup :: Group
animalGroup =
  Group
    {_groupGId = "animalgroup", _groupName = "Animal Group", _groupDescription = "Some description about animal group"}

ownerBioGroup :: GroupMembership
ownerBioGroup =
  GroupMembership {_groupMembershipGType = OwnerGroupMembershipType, _groupMembershipGroupId = bioGroup ^. gId}

ownerPlantGroup :: GroupMembership
ownerPlantGroup =
  GroupMembership {_groupMembershipGType = OwnerGroupMembershipType, _groupMembershipGroupId = plantGroup ^. gId}

ownerAnimalGroup :: GroupMembership
ownerAnimalGroup =
  GroupMembership {_groupMembershipGType = OwnerGroupMembershipType, _groupMembershipGroupId = animalGroup ^. gId}

memberBioGroup :: GroupMembership
memberBioGroup =
  GroupMembership {_groupMembershipGType = MemberGroupMembershipType, _groupMembershipGroupId = bioGroup ^. gId}

memberPlantGroup :: GroupMembership
memberPlantGroup =
  GroupMembership {_groupMembershipGType = MemberGroupMembershipType, _groupMembershipGroupId = plantGroup ^. gId}

memberAnimalGroup :: GroupMembership
memberAnimalGroup =
  GroupMembership {_groupMembershipGType = MemberGroupMembershipType, _groupMembershipGroupId = animalGroup ^. gId}
