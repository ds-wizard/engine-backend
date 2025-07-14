module Registry.Database.Migration.Production.Migration_0014_documentTemplateMetamodel.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 14, mmName = "DT metamodel", mmDescription = "Document template metamodel as tuple"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createSemVer2TupleType dbPool
  changeMetamodelVersionColumnTypeInDocumentTemplate dbPool

createSemVer2TupleType dbPool = do
  let sql =
        "CREATE TYPE sem_ver_2_tuple AS ( \
        \    major INT, \
        \    minor INT \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeMetamodelVersionColumnTypeInDocumentTemplate dbPool = do
  let sql =
        "ALTER TABLE document_template \
        \ALTER COLUMN metamodel_version \
        \    TYPE sem_ver_2_tuple \
        \    USING ROW (metamodel_version, 0);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
