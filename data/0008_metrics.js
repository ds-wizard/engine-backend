var MIGRATION_NAME = "0008_metrics"
logStartMigrationProcess()

// ------------------------------------------------------------------------
// CHANGES
// ------------------------------------------------------------------------

// Insert default organization
startChange("Insert metrics")
logInsert(METRIC_COL, "Insert base metrics")
db.getCollection("metrics").insertMany(
  [
    {
        "uuid" : "8db30660-d4e5-4c0a-bf3e-553f3f0f997a",
        "title" : "Findability",
        "abbreviation" : "∀",
        "description" : "...",
        "references" : [],
    },
    {
        "uuid" : "0feac7e6-add4-4723-abae-be5ce7864c63",
        "title" : "Accessibility",
        "abbreviation" : "",
        "description" : "",
        "references" : [],
    },
    {
        "uuid" : "a42bded3-a085-45f8-b384-32b4a77c8385",
        "title" : "Interoperability",
        "abbreviation" : "I",
        "description" : "",
        "references" : [],
    },
    {
        "uuid" : "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7",
        "title" : "Reusability",
        "abbreviation" : "R",
        "description" : "",
        "references" : [],
    },
    {
        "uuid" : "8845fe2b-79df-4138-baea-3a035bf5e249",
        "title" : "Good DMP Practice",
        "abbreviation" : "",
        "description" : "",
        "references" : [],
    },
    {
        "uuid" : "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0",
        "title" : "Openness",
        "abbreviation" : "O",
        "description" : "",
        "references" : [],
    }
  ])

logInsert(METRIC_COL, "Add timestamps")
db.getCollection('metrics').updateMany(
   { },
   { $set:
      {
        createdAt: new Date(),
        updatedAt: new Date()
      }
   }
)

endChange("Insert metrics")

// ------------------------------------------------------------------------
// FINALIZATION
// ------------------------------------------------------------------------
logEndMigrationProcess()
