module Wizard.Service.Owl.Diff.Accessor.Accessor where

import Control.Lens (Lens', (^.))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import Shared.Model.Event.EventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

getDiffEntities :: HasAnnotations' entity => [entity] -> [entity] -> [entity]
getDiffEntities oldEntities newEntities =
  foldl
    (\acc oldEntity ->
       case L.find (isSameEntity oldEntity) newEntities of
         Just _ -> acc
         Nothing -> acc ++ [oldEntity])
    []
    oldEntities

getExistingEntities :: HasAnnotations' entity => [entity] -> [entity] -> [(entity, entity)]
getExistingEntities oldEntities newEntities =
  foldl
    (\acc oldEntity ->
       case L.find (isSameEntity oldEntity) newEntities of
         Just newEntity -> acc ++ [(oldEntity, newEntity)]
         Nothing -> acc)
    []
    oldEntities

isSameEntity :: HasAnnotations' entity => entity -> entity -> Bool
isSameEntity entity1 entity2 = entity1 ^. annotations' == entity2 ^. annotations'

diffListField ::
     HasAnnotations' entity
  => (KnowledgeModel, KnowledgeModel)
  -> [U.UUID]
  -> [U.UUID]
  -> Lens' KnowledgeModel (M.Map U.UUID entity)
  -> EventField [U.UUID]
diffListField (oldKm, newKm) oldList newList entitiesM =
  let accessorFn km uuid = M.lookup uuid (km ^. entitiesM)
      oldEntities = mapMaybe (accessorFn oldKm) oldList
      newEntities = mapMaybe (accessorFn newKm) newList
      isNotInOldList newEntity =
        not $ foldl (\acc oldEntity -> acc || isSameEntity oldEntity newEntity) False oldEntities
   in if foldl (\acc newEntity -> isNotInOldList newEntity) False newEntities
        then ChangedValue newList
        else NothingChanged
