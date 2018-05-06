// Database
DATABASE_NAME="dsw-server"

ORGANIZATION_COL = "Organizations"
USER_COL = "Users"
ACTION_KEY_COL = "Action Keys"
PACKAGE_COL = "Packages"
BRANCHE_COL = "Organizations"
QUESTIONNAIRE_COL = "Questionnaires"

// Colors
var NO_COLOR = "\033[0m";
var BLACK_COLOR = "\033[0;30m";
var DARK_GRAY_COLOR = "\033[1;30m";
var LIGHT_GRAY_COLOR = "\033[0;37m";
var WHITE_COLOR = "\033[1;37m";

var RED_COLOR = "\033[0;31m";
var LIGHT_RED_COLOR = "\033[1;31m";
var BROWN_ORANGE_COLOR = "\033[0;33m";
var YELLOW_COLOR = "\033[1;33m";

var GREEN_COLOR = "\033[0;32m";
var LIGHT_GREEN_COLOR = "\033[1;32m";
var CYAN_COLOR = "\033[0;36m";
var LIGHT_CYAN_COLOR = "\033[1;36m";
var BLUE_COLOR = "\033[0;34m";
var LIGHT_BLUE_COLOR = "\033[1;34m";
var PURPLE_COLOR = "\033[0;35m";
var LIGHT_PURPLE_COLOR = "\033[1;35m";

// 1. Helper functions
db.system.js.save({
   _id: "log",
   value : function(message) { print(MIGRATION_NAME + ": " + message) }
})

db.system.js.save({
   _id: "logColor",
   value : function(color, message) { print(color + MIGRATION_NAME + ": " + message + NO_COLOR) }
})

db.system.js.save({
   _id: "logStartMigrationProcess",
   value : function() {
     logColor(LIGHT_CYAN_COLOR, "-------------------------------------------------")
     log("Starting migrations");
   }
})
db.system.js.save({
   _id: "logEndMigrationProcess",
   value : function() {
     log("Migrations successfully ended")
     logColor(YELLOW_COLOR, "-------------------------------------------------")
   }
})

db.system.js.save({
   _id: "startChange",
   value : function(name) { log("start: " + name) }
})
db.system.js.save({
   _id: "endChange",
   value : function(name) { log("end: " + name) }
})

db.system.js.save({
   _id: "logInsert",
   value : function(entity, message) {
     logColor(LIGHT_GREEN_COLOR, "Insert: (" + entity + ") " + message);
   }
})

db.system.js.save({
   _id: "logDelete",
   value : function(entity, message) {
     logColor(LIGHT_RED_COLOR, "Delete: (" + entity + ") " + message);
   }
})

// 2. Load functions
db.loadServerScripts();

// 3. Connection
var conn = new Mongo();
var db = conn.getDB(DATABASE_NAME);
