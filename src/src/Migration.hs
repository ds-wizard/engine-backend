module Migration where

import Context
import Database.Migration.User.UserMigration as UM

runMigration context dspConfig = do
  putStrLn "MIGRATION: started"
  UM.runMigration context dspConfig
  putStrLn "MIGRATION: ended"
