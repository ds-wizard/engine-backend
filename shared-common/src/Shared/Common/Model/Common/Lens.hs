module Shared.Common.Model.Common.Lens where

import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.MapEntry

class HasUuid' entity where
  getUuid :: entity -> U.UUID
  setUuid :: entity -> U.UUID -> entity

class HasEntityUuid' entity where
  getEntityUuid :: entity -> U.UUID
  setEntityUuid :: entity -> U.UUID -> entity

class HasParentUuid' entity where
  getParentUuid :: entity -> U.UUID
  setParentUuid :: entity -> U.UUID -> entity

class HasAnnotations' entity where
  getAnnotations :: entity -> [MapEntry String String]
  setAnnotations :: entity -> [MapEntry String String] -> entity

class HasCreatedAt' entity where
  getCreatedAt :: entity -> UTCTime
  setCreatedAt :: entity -> UTCTime -> entity

class HasCreatedBy' entity where
  getCreatedBy :: entity -> Maybe U.UUID
  setCreatedBy :: entity -> Maybe U.UUID -> entity

class HasTitle' entity where
  getTitle :: entity -> String
  setTitle :: entity -> String -> entity

class HasText' entity where
  getText :: entity -> Maybe String
  setText :: entity -> Maybe String -> entity

class HasProps' entity fieldType where
  getProps :: entity -> fieldType
  setProps :: entity -> fieldType -> entity
