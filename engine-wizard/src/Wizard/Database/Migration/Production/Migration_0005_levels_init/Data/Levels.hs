module Wizard.Database.Migration.Production.Migration_0005_levels_init.Data.Levels where

import qualified Data.Bson as BSON

level1 now =
  [ "level" BSON.=: 1
  , "title" BSON.=: "Before Submitting the Proposal"
  , "description" BSON.=: "..."
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

level2 now =
  [ "level" BSON.=: 2
  , "title" BSON.=: "Before Submitting the DMP"
  , "description" BSON.=: ""
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

level3 now =
  [ "level" BSON.=: 3
  , "title" BSON.=: "Before Finishing the Project"
  , "description" BSON.=: ""
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]
