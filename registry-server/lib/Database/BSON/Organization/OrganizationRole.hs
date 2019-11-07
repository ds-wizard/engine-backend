module Database.BSON.Organization.OrganizationRole where

import qualified Data.Bson as BSON

import Model.Organization.Organization

instance BSON.Val OrganizationRole where
  val AdminRole = BSON.String "AdminRole"
  val UserRole = BSON.String "UserRole"
  cast' (BSON.String "AdminRole") = Just AdminRole
  cast' (BSON.String "UserRole") = Just UserRole
  cast' _ = Nothing
