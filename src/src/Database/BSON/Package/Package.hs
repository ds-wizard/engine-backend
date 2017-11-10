module Database.BSON.Package.Package where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Package.Package

instance ToBSON Package where
  toBSON package =
    [ "uuid" BSON.=: package ^. pkgId
    , "name" BSON.=: (package ^. pkgName)
    , "shortName" BSON.=: (package ^. pkgShortName)
    , "version" BSON.=: (package ^. pkgVersion)
    , "description" BSON.=: (package ^. pkgDescription)
    , "parentPackage" BSON.=: (package ^. pkgParentPackage)
    ]

instance FromBSON Package where
  fromBSON doc = do
    pkgId <- BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    shortName <- BSON.lookup "shortName" doc
    version <- BSON.lookup "version" doc
    description <- BSON.lookup "description" doc
    parentPackage <- BSON.lookup "parentPackage" doc
    return
      Package
      { _pkgId = pkgId
      , _pkgName = name
      , _pkgShortName = shortName
      , _pkgVersion = version
      , _pkgDescription = description
      , _pkgParentPackage = parentPackage
      }
