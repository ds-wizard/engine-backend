module Database.Migration.Production.Migration_0004_metrics_init.Data.Metrics where

import qualified Data.Bson as BSON

metricF now =
  [ "uuid" BSON.=: "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"
  , "title" BSON.=: "Findability"
  , "abbreviation" BSON.=: "F"
  , "description" BSON.=: "..."
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricA now =
  [ "uuid" BSON.=: "0feac7e6-add4-4723-abae-be5ce7864c63"
  , "title" BSON.=: "Accessibility"
  , "abbreviation" BSON.=: "A"
  , "description" BSON.=: ""
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricI now =
  [ "uuid" BSON.=: "a42bded3-a085-45f8-b384-32b4a77c8385"
  , "title" BSON.=: "Interoperability"
  , "abbreviation" BSON.=: "I"
  , "description" BSON.=: ""
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricR now =
  [ "uuid" BSON.=: "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"
  , "title" BSON.=: "Reusability"
  , "abbreviation" BSON.=: "R"
  , "description" BSON.=: ""
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricG now =
  [ "uuid" BSON.=: "8845fe2b-79df-4138-baea-3a035bf5e249"
  , "title" BSON.=: "Good DMP Practice"
  , "abbreviation" BSON.=: "G"
  , "description" BSON.=: ""
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricO now =
  [ "uuid" BSON.=: "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"
  , "title" BSON.=: "Openness"
  , "abbreviation" BSON.=: "O"
  , "description" BSON.=: ""
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]
