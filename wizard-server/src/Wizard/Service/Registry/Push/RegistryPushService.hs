module Wizard.Service.Registry.Push.RegistryPushService where

import Shared.Common.Util.Logger
import Shared.Coordinate.Util.Coordinate
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService as DocumentTemplateBundleService
import qualified Wizard.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService as KnowledgeModelBundleService
import qualified Wizard.Service.Locale.Bundle.LocaleBundleService as LocaleBundleService

pushKnowledgeModelBundle :: String -> AppContextM ()
pushKnowledgeModelBundle pkgId = do
  logInfoI _CMP_SERVICE (f' "Pushing knowledge model bundle with the id ('%s') to registry" [pkgId])
  bundle <- KnowledgeModelBundleService.exportBundle pkgId
  uploadKnowledgeModelBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing knowledge model bundle with the id ('%s') successfully completed" [pkgId])

pushDocumentTemplateBundle :: String -> AppContextM ()
pushDocumentTemplateBundle tmlId = do
  logInfoI _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') to registry" [tmlId])
  bundle <- DocumentTemplateBundleService.exportBundle tmlId
  uploadDocumentTemplateBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') successfully completed" [tmlId])

pushLocaleBundle :: String -> AppContextM ()
pushLocaleBundle lId = do
  logInfoI _CMP_SERVICE (f' "Pushing locale bundle with the id ('%s') to registry" [lId])
  coordinate <- parseCoordinate lId
  locale <- findLocaleByCoordinate coordinate
  (coordinate, bundle) <- LocaleBundleService.exportBundle locale.uuid
  uploadLocaleBundle bundle
  logInfoI _CMP_SERVICE (f' "Pushing locale bundle with the id ('%s') successfully completed" [lId])
