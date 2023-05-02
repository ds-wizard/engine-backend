import Test.Hspec

import qualified Migration0001.MigrationSpec as M0001Spec
import qualified Migration0002.MigrationSpec as M0002Spec
import qualified Migration0003.MigrationSpec as M0003Spec
import qualified Migration0004.MigrationSpec as M0004Spec
import qualified Migration0005.MigrationSpec as M0005Spec
import qualified Migration0006.MigrationSpec as M0006Spec
import qualified Migration0007.MigrationSpec as M0007Spec
import qualified Migration0008.MigrationSpec as M0008Spec
import qualified Migration0009.MigrationSpec as M0009Spec
import qualified Migration0010.MigrationSpec as M0010Spec
import qualified Migration0011.MigrationSpec as M0011Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
