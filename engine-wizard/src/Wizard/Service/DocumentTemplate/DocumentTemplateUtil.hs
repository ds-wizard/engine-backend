module Wizard.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.List as L

import Shared.Constant.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Package.Package
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Registry.RegistryTemplate

computeDocumentTemplateState :: [RegistryTemplate] -> DocumentTemplate -> DocumentTemplateState
computeDocumentTemplateState tmlsFromRegistry tml =
  if not (isDocumentTemplateSupported False tml)
    then UnsupportedMetamodelVersionDocumentTemplateState
    else case selectDocumentTemplateByOrgIdAndTmlId tml tmlsFromRegistry of
      Just tmlFromRegistry ->
        case compareVersion tmlFromRegistry.remoteVersion tml.version of
          LT -> UnpublishedDocumentTemplateState
          EQ -> UpToDateDocumentTemplateState
          GT -> OutdatedDocumentTemplateState
      Nothing -> UnknownDocumentTemplateState

computeDocumentTemplateState' :: Bool -> DocumentTemplateList -> DocumentTemplateState
computeDocumentTemplateState' registryEnabled tml
  | not (isDocumentTemplateSupported False tml) = UnsupportedMetamodelVersionDocumentTemplateState
  | registryEnabled = tml.state
  | otherwise = UnknownDocumentTemplateState

selectDocumentTemplateByOrgIdAndTmlId tml =
  L.find (\t -> t.organizationId == tml.organizationId && t.templateId == tml.templateId)

selectOrganizationByOrgId tml = L.find (\org -> org.organizationId == tml.organizationId)

getUsablePackagesForDocumentTemplate tml = chooseTheNewest . groupPackages . filterPackages tml
  where
    filterPackages tml = filter (\pkg -> not . null $ filterDocumentTemplates (Just pkg.pId) [tml])

isDocumentTemplateSupported True tml = True
isDocumentTemplateSupported False tml = tml.metamodelVersion == documentTemplateMetamodelVersion

isDocumentTemplateInPhase (Just phase) tml = tml.phase == phase
isDocumentTemplateInPhase _ _ = True

filterDocumentTemplates mPkgId tmls =
  case mPkgId of
    Just pkgId -> filter (filterDocumentTemplate . splitCoordinate $ pkgId) tmls
    Nothing -> tmls
  where
    filterDocumentTemplate pkgIdSplit template = fitsIntoKMSpecs pkgIdSplit template.allowedPackages
