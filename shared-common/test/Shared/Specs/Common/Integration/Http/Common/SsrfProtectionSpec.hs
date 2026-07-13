module Shared.Specs.Common.Integration.Http.Common.SsrfProtectionSpec where

import Data.Word (Word16, Word8)
import Network.Socket (SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import Test.Hspec

import Shared.Common.Integration.Http.Common.SsrfProtection

v4 :: (Word8, Word8, Word8, Word8) -> SockAddr
v4 t = SockAddrInet 80 (tupleToHostAddress t)

v6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> SockAddr
v6 t = SockAddrInet6 80 0 (tupleToHostAddress6 t) 0

ssrfProtectionSpec =
  describe "SsrfProtection" $ do
    describe "isConnectionAllowed without an allowlist" $ do
      let blocks addr = isConnectionAllowed [] addr `shouldBe` False
      let allows addr = isConnectionAllowed [] addr `shouldBe` True
      it "blocks the cloud metadata address 169.254.169.254" $ blocks (v4 (169, 254, 169, 254))
      it "blocks loopback 127.0.0.1" $ blocks (v4 (127, 0, 0, 1))
      it "blocks private 10.0.0.1" $ blocks (v4 (10, 0, 0, 1))
      it "blocks private 172.16.0.1" $ blocks (v4 (172, 16, 0, 1))
      it "blocks private 192.168.1.1" $ blocks (v4 (192, 168, 1, 1))
      it "blocks CGNAT 100.64.0.1" $ blocks (v4 (100, 64, 0, 1))
      it "blocks 0.0.0.0" $ blocks (v4 (0, 0, 0, 0))
      it "allows public 8.8.8.8" $ allows (v4 (8, 8, 8, 8))
      it "allows public 1.1.1.1" $ allows (v4 (1, 1, 1, 1))
      it "blocks IPv6 loopback ::1" $ blocks (v6 (0, 0, 0, 0, 0, 0, 0, 1))
      it "blocks IPv6 link-local fe80::1" $ blocks (v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1))
      it "blocks IPv6 unique-local fc00::1" $ blocks (v6 (0xfc00, 0, 0, 0, 0, 0, 0, 1))
      it "allows public IPv6 2606:4700::1111" $ allows (v6 (0x2606, 0x4700, 0, 0, 0, 0, 0, 0x1111))
      it "blocks the IPv4-mapped loopback ::ffff:127.0.0.1" $ blocks (v6 (0, 0, 0, 0, 0, 0xffff, 0x7f00, 0x0001))
    describe "isConnectionAllowed with an allowlist" $ do
      it "allows an address inside an allowlisted CIDR (10.0.0.0/8)" $ do
        rules <- buildAllowRules ["10.0.0.0/8"]
        isConnectionAllowed rules (v4 (10, 1, 2, 3)) `shouldBe` True
      it "still blocks a private address outside the allowlisted CIDR" $ do
        rules <- buildAllowRules ["10.0.0.0/8"]
        isConnectionAllowed rules (v4 (127, 0, 0, 1)) `shouldBe` False
      it "allows an allowlisted exact IP (192.168.5.5)" $ do
        rules <- buildAllowRules ["192.168.5.5"]
        isConnectionAllowed rules (v4 (192, 168, 5, 5)) `shouldBe` True
      it "does not allow a neighbor of an allowlisted exact IP" $ do
        rules <- buildAllowRules ["192.168.5.5"]
        isConnectionAllowed rules (v4 (192, 168, 5, 6)) `shouldBe` False
