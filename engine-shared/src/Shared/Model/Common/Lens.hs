module Shared.Model.Common.Lens where

import qualified Data.UUID as U

class HasUuid' entity where
  uuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

class HasEntityUuid' entity where
  entityUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

class HasParentUuid' entity where
  parentUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity
