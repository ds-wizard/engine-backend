module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.EventMigratorSpec where

import Test.Hspec

import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0001.MigrationSpec as M0001Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0002.MigrationSpec as M0002Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0003.MigrationSpec as M0003Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0004.MigrationSpec as M0004Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0005.MigrationSpec as M0005Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0006.MigrationSpec as M0006Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0007.MigrationSpec as M0007Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0008.MigrationSpec as M0008Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0009.MigrationSpec as M0009Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0010.MigrationSpec as M0010Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0011.MigrationSpec as M0011Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0014.MigrationSpec as M0014Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0016.MigrationSpec as M0016Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0017.MigrationSpec as M0017Spec
import qualified Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0018.MigrationSpec as M0018Spec

eventMigratorSpec :: Spec
eventMigratorSpec = do
  describe "Migration #0001" M0001Spec.spec
  describe "Migration #0002" M0002Spec.spec
  describe "Migration #0003" M0003Spec.spec
  describe "Migration #0004" M0004Spec.spec
  describe "Migration #0005" M0005Spec.spec
  describe "Migration #0006" M0006Spec.spec
  describe "Migration #0007" M0007Spec.spec
  describe "Migration #0008" M0008Spec.spec
  describe "Migration #0009" M0009Spec.spec
  describe "Migration #0010" M0010Spec.spec
  describe "Migration #0011" M0011Spec.spec
  describe "Migration #0014" M0014Spec.spec
  describe "Migration #0016" M0016Spec.spec
  describe "Migration #0017" M0017Spec.spec
  describe "Migration #0018" M0018Spec.spec
