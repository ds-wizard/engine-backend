var MIGRATION_NAME = "0001_init"
logStartMigrationProcess()

// ------------------------------------------------------------------------
// CHANGES
// ------------------------------------------------------------------------
// Drop organizations
logDelete(ORGANIZATION_COL, "All")
db.organizations.deleteMany({})

// Drop users
logDelete(USER_COL, "All")
db.users.deleteMany({})

// ------------------------------------------------------------------------
// FINALIZATION
// ------------------------------------------------------------------------
logEndMigrationProcess()
