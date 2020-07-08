module Shared.Model.Common.Page where

import Control.Applicative
import qualified Data.List as L
import GHC.Generics

import Shared.Model.Common.PageMetadata

data Page entity =
  Page
    { _pageName :: String
    , _pageMetadata :: PageMetadata
    , _pageEntities :: [entity]
    }
  deriving (Show, Eq, Generic)

instance Functor Page where
  fmap _ (Page name page []) = Page name page []
  fmap f (Page name page (x:xs)) = Page name page (f x : fmap f xs)

instance Foldable Page where
  elem e = L.elem e . _pageEntities
  foldl fn e = L.foldl fn e . _pageEntities
  foldl' fn e = L.foldl' fn e . _pageEntities
  foldl1 fn = L.foldl1 fn . _pageEntities
  foldr fn e = L.foldr fn e . _pageEntities
  foldr1 fn = L.foldr1 fn . _pageEntities
  length = L.length . _pageEntities
  maximum = L.maximum . _pageEntities
  minimum = L.minimum . _pageEntities
  null = L.null . _pageEntities
  product = L.product . _pageEntities
  sum = L.sum . _pageEntities

instance Traversable Page where
  traverse f (Page name metadata entities) = Page name metadata <$> L.foldr cons_f (pure []) entities
    where
      cons_f x = liftA2 (:) (f x)

filterP :: (entity -> Bool) -> Page entity -> Page entity
filterP fn (Page name metadata entities) =
  let filteredEntities = filter fn entities
      updatedMetadata = metadata {_pageMetadataTotalElements = length filteredEntities}
   in Page name updatedMetadata filteredEntities
