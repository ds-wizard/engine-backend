module Shared.Model.Common.Page where

import Control.Applicative
import qualified Data.List as L
import GHC.Generics

import Shared.Model.Common.PageMetadata

data Page entity = Page
  { name :: String
  , metadata :: PageMetadata
  , entities :: [entity]
  }
  deriving (Show, Eq, Generic)

instance Functor Page where
  fmap _ (Page name page []) = Page name page []
  fmap f (Page name page (x : xs)) = Page name page (f x : fmap f xs)

instance Foldable Page where
  elem e = L.elem e . entities
  foldl fn e = L.foldl fn e . entities
  foldl' fn e = L.foldl' fn e . entities
  foldl1 fn = L.foldl1 fn . entities
  foldr fn e = L.foldr fn e . entities
  foldr1 fn = L.foldr1 fn . entities
  length = L.length . entities
  maximum = L.maximum . entities
  minimum = L.minimum . entities
  null = L.null . entities
  product = L.product . entities
  sum = L.sum . entities

instance Traversable Page where
  traverse f (Page name metadata entities) = Page name metadata <$> L.foldr cons_f (pure []) entities
    where
      cons_f x = liftA2 (:) (f x)

filterP :: (entity -> Bool) -> Page entity -> Page entity
filterP fn (Page name metadata entities) =
  let filteredEntities = filter fn entities
      updatedMetadata = metadata {totalElements = length filteredEntities}
   in Page name updatedMetadata filteredEntities

mapMaybeP :: (a -> Maybe b) -> Page a -> Page b
mapMaybeP _ (Page name metadata []) = Page name metadata []
mapMaybeP f (Page name metadata (x : xs)) =
  let Page name' metadata' xs' = mapMaybeP f (Page name metadata xs)
   in case f x of
        Nothing -> Page name' (metadata' {totalElements = totalElements metadata' - 1}) xs'
        Just x' -> Page name' metadata' (x' : xs')
