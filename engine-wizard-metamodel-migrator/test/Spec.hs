import Test.Hspec

import qualified Migration0011.MigrationSpec as M0011Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Migration #0011" M0011Spec.spec
