module Main where

import Control.Monad (guard)
import Data.Aeson (Value, decode, encode, toJSON)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust, isJust)
import System.Environment

import Wizard.Metamodel.Migrator.EventMigrator as EventMigrator
import Wizard.Metamodel.Util.List (foldEither)

getVersionArgs :: IO (Int, Int)
getVersionArgs = do
  args <- getArgs
  guard $ length args == 2
  let ints = map read args :: [Int]
  return (head ints, ints !! 1)

main :: IO ()
main = do
  args <- getVersionArgs
  let oldMetamodelVersion = fst args
  let kmMetamodelVersion = snd args
  contents <- LB.getContents
  let val = decode contents :: Maybe [Value]
  guard $ isJust val
  let events = fromJust val
  let updatedEvents = EventMigrator.migrate oldMetamodelVersion kmMetamodelVersion <$> events
  case foldEither updatedEvents of
    Right newEvents -> LB.writeFile "output.json" $ encode . toJSON . concat $ newEvents
    Left error -> putStrLn $ "Error occured: " ++ error
