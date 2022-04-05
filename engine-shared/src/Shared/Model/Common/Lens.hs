module Shared.Model.Common.Lens where

import Data.Time
import qualified Data.UUID as U
import Shared.Model.Common.MapEntry

class HasUuid' entity where
  uuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

class HasEntityUuid' entity where
  entityUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

class HasParentUuid' entity where
  parentUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

class HasExpertUuids' entity fieldType where
  expertUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasReferenceUuids' entity fieldType where
  referenceUuids' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasProps' entity fieldType where
  props' :: Functor f => (fieldType -> f fieldType) -> entity -> f entity

class HasAnnotations' entity where
  annotations' :: Functor f => ([MapEntry String String] -> f [MapEntry String String]) -> entity -> f entity

class HasCreatedAt' entity where
  createdAt' :: Functor f => (UTCTime -> f UTCTime) -> entity -> f entity

class HasCreatedBy' entity where
  createdBy' :: Functor f => (Maybe U.UUID -> f (Maybe U.UUID)) -> entity -> f entity
