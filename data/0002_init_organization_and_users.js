var MIGRATION_NAME = "0002_init_organization_and_users"
logStartMigrationProcess()

// ------------------------------------------------------------------------
// CHANGES
// ------------------------------------------------------------------------

// Insert default organization
startChange("Init organizations")
logInsert(ORGANIZATION_COL, "Elixir Amsterdam")
db.organizations.update({"uuid" : "d0619a24-db8a-48e1-a033-0d4ef8b8da78"}, {
    "uuid" : "d0619a24-db8a-48e1-a033-0d4ef8b8da78",
    "name" : "Elixir Amsterdam",
    "organizationId" : "elixir.nl.amsterdam"
}, { upsert: true });
endChange("Init organizations")

// Insert default users
startChange("Init users")
logInsert(USER_COL, "Albert Einstein")
db.users.update({"uuid" : "3ef96067-8e7a-479e-ae18-1be47f0e2a5d"}, {
    "uuid" : "3ef96067-8e7a-479e-ae18-1be47f0e2a5d",
    "name" : "Albert",
    "surname" : "Einstein",
    "email" : "albert.einstein@example.com",
    "passwordHash" : "sha256|17|KOj9LS2y8IXDvo0DG8EW8A==|rduRLWmC7xAKKPAV0DHK2LQiaptQ4Xn3cWZgwuXmqMc=", // password
    "role" : "ADMIN",
    "permissions" : [
        "UM_PERM",
        "ORG_PERM",
        "KM_PERM",
        "KM_UPGRADE_PERM",
        "KM_PUBLISH_PERM",
        "PM_READ_PERM",
        "PM_WRITE_PERM",
        "QTN_PERM",
        "DMP_PERM"
    ],
    "isActive" : true
}, { upsert: true });

logInsert(USER_COL, "Nikola Tesla")
db.users.update({"uuid" : "30d48cf4-8c8a-496f-bafe-585bd238f798"}, {
    "uuid" : "30d48cf4-8c8a-496f-bafe-585bd238f798",
    "name" : "Nikola",
    "surname" : "Tesla",
    "email" : "nikola.tesla@example.com",
    "passwordHash" : "sha256|17|Nwafc2BQvbcbYdV/2m/xVQ==|Mjgj3wrtK21qIoSmz8ODiro8Yr6Upc6V27whAobIz5k=",
    "role" : "DATASTEWARD",
    "permissions" : [
        "KM_PERM",
        "KM_UPGRADE_PERM",
        "KM_PUBLISH_PERM",
        "PM_READ_PERM",
        "QTN_PERM",
        "DMP_PERM"
    ],
    "isActive" : true
}, { upsert: true });

logInsert(USER_COL, "Isaac Newton")
db.users.update({"uuid" : "e1c58e52-0824-4526-8ebe-ec38eec67030"}, {
    "uuid" : "e1c58e52-0824-4526-8ebe-ec38eec67030",
    "name" : "Isaac",
    "surname" : "Newton",
    "email" : "isaac.newton@example.com",
    "passwordHash" : "sha256|17|lWASjBQx215ktNe7mjaWHg==|btUAw+oFeBVR9bDXmoVGLMSIrGOjbs+CxC6SR7FqouQ=",
    "role" : "RESEARCHER",
    "permissions" : [
        "PM_READ_PERM",
        "QTN_PERM",
        "DMP_PERM"
    ],
    "isActive" : true
}, { upsert: true });





endChange("Init users")

// ------------------------------------------------------------------------
// FINALIZATION
// ------------------------------------------------------------------------
logEndMigrationProcess()
