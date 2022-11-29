module Wizard.Database.Migration.Development.Acl.Data.Groups where

import Wizard.Model.Acl.Acl

bioGroup :: Group
bioGroup =
  Group {gId = "biogroup", name = "Bio Group", description = "Some description about bio group"}

plantGroup :: Group
plantGroup =
  Group {gId = "plantgroup", name = "Plant Group", description = "Some description about plant group"}

animalGroup :: Group
animalGroup =
  Group
    { gId = "animalgroup"
    , name = "Animal Group"
    , description = "Some description about animal group"
    }

ownerBioGroup :: GroupMembership
ownerBioGroup =
  GroupMembership {gType = OwnerGroupMembershipType, groupId = bioGroup.gId}

ownerPlantGroup :: GroupMembership
ownerPlantGroup =
  GroupMembership {gType = OwnerGroupMembershipType, groupId = plantGroup.gId}

ownerAnimalGroup :: GroupMembership
ownerAnimalGroup =
  GroupMembership {gType = OwnerGroupMembershipType, groupId = animalGroup.gId}

memberBioGroup :: GroupMembership
memberBioGroup =
  GroupMembership {gType = MemberGroupMembershipType, groupId = bioGroup.gId}

memberPlantGroup :: GroupMembership
memberPlantGroup =
  GroupMembership {gType = MemberGroupMembershipType, groupId = plantGroup.gId}

memberAnimalGroup :: GroupMembership
memberAnimalGroup =
  GroupMembership {gType = MemberGroupMembershipType, groupId = animalGroup.gId}
