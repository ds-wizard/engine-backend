module Wizard.Database.Migration.Production.Migration where

import Database.PostgreSQL.Migration.Entity

import qualified Wizard.Database.Migration.Production.Migration_0001_init.Migration as M_0001
import qualified Wizard.Database.Migration.Production.Migration_0002_projectTemplate.Migration as M_0002
import qualified Wizard.Database.Migration.Production.Migration_0003_metricsAndPhases.Migration as M_0003
import qualified Wizard.Database.Migration.Production.Migration_0004_questionnaireEventsSquash.Migration as M_0004
import qualified Wizard.Database.Migration.Production.Migration_0005_documentMetadata.Migration as M_0005
import qualified Wizard.Database.Migration.Production.Migration_0006_passwordHash.Migration as M_0006
import qualified Wizard.Database.Migration.Production.Migration_0007_bookReference.Migration as M_0007
import qualified Wizard.Database.Migration.Production.Migration_0008_packageFkAndBase64.Migration as M_0008
import qualified Wizard.Database.Migration.Production.Migration_0009_adminOperationsAndSubmission.Migration as M_0009
import qualified Wizard.Database.Migration.Production.Migration_0010_app.Migration as M_0010
import qualified Wizard.Database.Migration.Production.Migration_0011_app_2.Migration as M_0011
import qualified Wizard.Database.Migration.Production.Migration_0012_projectTagging.Migration as M_0012
import qualified Wizard.Database.Migration.Production.Migration_0013_branchWebsocket.Migration as M_0013
import qualified Wizard.Database.Migration.Production.Migration_0014_appLimit.Migration as M_0014
import qualified Wizard.Database.Migration.Production.Migration_0015_prefab.Migration as M_0015
import qualified Wizard.Database.Migration.Production.Migration_0016_appPlan.Migration as M_0016
import qualified Wizard.Database.Migration.Production.Migration_0017_integrationYaml.Migration as M_0017
import qualified Wizard.Database.Migration.Production.Migration_0018_dropDocumentQueue.Migration as M_0018
import qualified Wizard.Database.Migration.Production.Migration_0019_dashboardAndAudit.Migration as M_0019
import qualified Wizard.Database.Migration.Production.Migration_0020_persistentCommandCreatedBy.Migration as M_0020
import qualified Wizard.Database.Migration.Production.Migration_0021_qtnImporter.Migration as M_0021
import qualified Wizard.Database.Migration.Production.Migration_0022_optimizeProjectList.Migration as M_0022
import qualified Wizard.Database.Migration.Production.Migration_0023_createdBy.Migration as M_0023
import qualified Wizard.Database.Migration.Production.Migration_0024_commonFn_and_token_and_locale.Migration as M_0024
import qualified Wizard.Database.Migration.Production.Migration_0025_locale_2.Migration as M_0025
import qualified Wizard.Database.Migration.Production.Migration_0026_locale_3.Migration as M_0026
import qualified Wizard.Database.Migration.Production.Migration_0027_localePerm.Migration as M_0027
import qualified Wizard.Database.Migration.Production.Migration_0028_instanceConfigMail.Migration as M_0028
import qualified Wizard.Database.Migration.Production.Migration_0029_documentTemplateEditor.Migration as M_0029
import qualified Wizard.Database.Migration.Production.Migration_0030_twoFactorAuth.Migration as M_0030
import qualified Wizard.Database.Migration.Production.Migration_0031_component.Migration as M_0031
import qualified Wizard.Database.Migration.Production.Migration_0032_branchFix.Migration as M_0032
import qualified Wizard.Database.Migration.Production.Migration_0033_apiKey.Migration as M_0033
import qualified Wizard.Database.Migration.Production.Migration_0034_unification.Migration as M_0034
import qualified Wizard.Database.Migration.Production.Migration_0035_persistentCommandDestination.Migration as M_0035
import qualified Wizard.Database.Migration.Production.Migration_0036_branchCreatedBy.Migration as M_0036
import qualified Wizard.Database.Migration.Production.Migration_0037_pkgAndDocReadOnly.Migration as M_0037
import qualified Wizard.Database.Migration.Production.Migration_0038_admin.Migration as M_0038
import qualified Wizard.Database.Migration.Production.Migration_0039_tenant.Migration as M_0039
import qualified Wizard.Database.Migration.Production.Migration_0040_qtnAction.Migration as M_0040
import qualified Wizard.Database.Migration.Production.Migration_0041_upgradeTmlMetamodel.Migration as M_0041
import qualified Wizard.Database.Migration.Production.Migration_0042_isOutdated.Migration as M_0042
import qualified Wizard.Database.Migration.Production.Migration_0043_temporaryFile.Migration as M_0043
import qualified Wizard.Database.Migration.Production.Migration_0044_signalBridge.Migration as M_0044
import qualified Wizard.Database.Migration.Production.Migration_0045_analytics.Migration as M_0045
import qualified Wizard.Database.Migration.Production.Migration_0046_tenantFeature.Migration as M_0046
import qualified Wizard.Database.Migration.Production.Migration_0047_qtnCommentAssignedTo.Migration as M_0047
import qualified Wizard.Database.Migration.Production.Migration_0048_jsonb.Migration as M_0048
import qualified Wizard.Database.Migration.Production.Migration_0049_questionnaireFile.Migration as M_0049
import qualified Wizard.Database.Migration.Production.Migration_0050_kmValidation.Migration as M_0050
import qualified Wizard.Database.Migration.Production.Migration_0051_branchState.Migration as M_0051
import qualified Wizard.Database.Migration.Production.Migration_0052_questionnaireFileFix.Migration as M_0052
import qualified Wizard.Database.Migration.Production.Migration_0053_qtnEventsRefactoring.Migration as M_0053
import qualified Wizard.Database.Migration.Production.Migration_0054_userLocale.Migration as M_0054
import qualified Wizard.Database.Migration.Production.Migration_0055_localeFix.Migration as M_0055
import qualified Wizard.Database.Migration.Production.Migration_0056_tour.Migration as M_0056
import qualified Wizard.Database.Migration.Production.Migration_0057_tenantConfigSubmission.Migration as M_0057

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions =
  [ M_0001.definition
  , M_0002.definition
  , M_0003.definition
  , M_0004.definition
  , M_0005.definition
  , M_0006.definition
  , M_0007.definition
  , M_0008.definition
  , M_0009.definition
  , M_0010.definition
  , M_0011.definition
  , M_0012.definition
  , M_0013.definition
  , M_0014.definition
  , M_0015.definition
  , M_0016.definition
  , M_0017.definition
  , M_0018.definition
  , M_0019.definition
  , M_0020.definition
  , M_0021.definition
  , M_0022.definition
  , M_0023.definition
  , M_0024.definition
  , M_0025.definition
  , M_0026.definition
  , M_0027.definition
  , M_0028.definition
  , M_0029.definition
  , M_0030.definition
  , M_0031.definition
  , M_0032.definition
  , M_0033.definition
  , M_0034.definition
  , M_0035.definition
  , M_0036.definition
  , M_0037.definition
  , M_0038.definition
  , M_0039.definition
  , M_0040.definition
  , M_0041.definition
  , M_0042.definition
  , M_0043.definition
  , M_0044.definition
  , M_0045.definition
  , M_0046.definition
  , M_0047.definition
  , M_0048.definition
  , M_0049.definition
  , M_0050.definition
  , M_0051.definition
  , M_0052.definition
  , M_0053.definition
  , M_0054.definition
  , M_0055.definition
  , M_0056.definition
  , M_0057.definition
  ]
