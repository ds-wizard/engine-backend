module Wizard.Service.KnowledgeModel.Package.Event.KnowledgeModelPackageEventService where

import qualified Data.UUID as U

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

getAllPreviousEventsSincePackageId :: U.UUID -> AppContextM [KnowledgeModelEvent]
getAllPreviousEventsSincePackageId pkgUuid = do
  package <- findPackageByUuid pkgUuid
  packageEvents <- findPackageEvents pkgUuid
  case package.previousPackageUuid of
    Just previousPackageUuid -> do
      pkgEvents <- getAllPreviousEventsSincePackageId previousPackageUuid
      return $ pkgEvents ++ fmap toEvent packageEvents
    Nothing -> return (fmap toEvent packageEvents)

getAllPreviousEventsSincePackageIdAndUntilPackageId :: U.UUID -> U.UUID -> AppContextM [KnowledgeModelEvent]
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgUuid untilPkgUuid = go sincePkgUuid
  where
    go pkgUuid =
      if pkgUuid == untilPkgUuid
        then return []
        else do
          package <- findPackageByUuid pkgUuid
          packageEvents <- findPackageEvents pkgUuid
          case package.previousPackageUuid of
            Just previousPackageUuid -> do
              pkgEvents <- go previousPackageUuid
              return $ pkgEvents ++ fmap toEvent packageEvents
            Nothing -> return (fmap toEvent packageEvents)
