module Wizard.Service.Owl.Diff.Accessor.Accessor where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

getDiffEntities :: HasAnnotations' entity => [entity] -> [entity] -> [entity]
getDiffEntities oldEntities newEntities =
  foldl
    ( \acc oldEntity ->
        case L.find (isSameEntity oldEntity) newEntities of
          Just _ -> acc
          Nothing -> acc ++ [oldEntity]
    )
    []
    oldEntities

getExistingEntities :: HasAnnotations' entity => [entity] -> [entity] -> [(entity, entity)]
getExistingEntities oldEntities newEntities =
  foldl
    ( \acc oldEntity ->
        case L.find (isSameEntity oldEntity) newEntities of
          Just newEntity -> acc ++ [(oldEntity, newEntity)]
          Nothing -> acc
    )
    []
    oldEntities

isSameEntity :: HasAnnotations' entity => entity -> entity -> Bool
isSameEntity entity1 entity2 = getAnnotations entity1 == getAnnotations entity2

diffListField (oldKm, newKm) oldList newList getEntitiesMFn =
  let accessorFn km uuid = M.lookup uuid (getEntitiesMFn km)
      oldEntities = mapMaybe (accessorFn oldKm) oldList
      newEntities = mapMaybe (accessorFn newKm) newList
      isNotInOldList newEntity =
        not $ foldl (\acc oldEntity -> acc || isSameEntity oldEntity newEntity) False oldEntities
   in if foldl (\acc newEntity -> isNotInOldList newEntity) False newEntities
        then ChangedValue newList
        else NothingChanged
