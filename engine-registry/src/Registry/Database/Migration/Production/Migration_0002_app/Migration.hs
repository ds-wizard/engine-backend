module Registry.Database.Migration.Production.Migration_0002_app.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 2, mmName = "Add App", mmDescription = "Add app column for package and template"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addForeignKey dbPool "package"
  addForeignKey dbPool "template"
  addForeignKey dbPool "template_asset"
  addForeignKey dbPool "template_file"

addForeignKey dbPool table = do
  let sql =
        f'
          "alter table %s \
           \  add app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null;"
          [table]
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------
f' :: String -> [String] -> String
f' str terms =
  case str of
    '%':'s':rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ f' rest (drop 1 terms)
    '%':'%':'s':rest -> '%' : 's' : f' rest terms
    a:rest -> a : f' rest terms
    [] -> []
