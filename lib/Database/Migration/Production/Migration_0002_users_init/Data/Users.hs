module Database.Migration.Production.Migration_0002_users_init.Data.Users where

import qualified Data.Bson as BSON

userAlbert now =
  [ "uuid" BSON.=: "3ef96067-8e7a-479e-ae18-1be47f0e2a5d"
  , "name" BSON.=: "Albert"
  , "surname" BSON.=: "Einstein"
  , "email" BSON.=: "albert.einstein@example.com"
  , "passwordHash" BSON.=: "sha256|17|KOj9LS2y8IXDvo0DG8EW8A==|rduRLWmC7xAKKPAV0DHK2LQiaptQ4Xn3cWZgwuXmqMc=" -- password
  , "role" BSON.=: "ADMIN"
  , "permissions" BSON.=:
    [ "UM_PERM"
    , "ORG_PERM"
    , "KM_PERM"
    , "KM_UPGRADE_PERM"
    , "KM_PUBLISH_PERM"
    , "PM_READ_PERM"
    , "PM_WRITE_PERM"
    , "QTN_PERM"
    , "DMP_PERM"
    ]
  , "active" BSON.=: True
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

userNikola now =
  [ "uuid" BSON.=: "30d48cf4-8c8a-496f-bafe-585bd238f798"
  , "name" BSON.=: "Nikola"
  , "surname" BSON.=: "Tesla"
  , "email" BSON.=: "nikola.tesla@example.com"
  , "passwordHash" BSON.=: "sha256|17|Nwafc2BQvbcbYdV/2m/xVQ==|Mjgj3wrtK21qIoSmz8ODiro8Yr6Upc6V27whAobIz5k=" -- password
  , "role" BSON.=: "DATASTEWARD"
  , "permissions" BSON.=: ["KM_PERM", "KM_UPGRADE_PERM", "KM_PUBLISH_PERM", "PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
  , "active" BSON.=: True
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

userIsaac now =
  [ "uuid" BSON.=: "e1c58e52-0824-4526-8ebe-ec38eec67030"
  , "name" BSON.=: "Isaac"
  , "surname" BSON.=: "Newton"
  , "email" BSON.=: "isaac.newton@example.com"
  , "passwordHash" BSON.=: "sha256|17|lWASjBQx215ktNe7mjaWHg==|btUAw+oFeBVR9bDXmoVGLMSIrGOjbs+CxC6SR7FqouQ=" -- password
  , "role" BSON.=: "RESEARCHER"
  , "permissions" BSON.=: ["PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
  , "active" BSON.=: True
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]
