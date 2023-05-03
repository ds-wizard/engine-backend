module Shared.Common.Constant.DummyRsaPrivateKey where

import qualified Crypto.PubKey.RSA as RSA
import Data.Maybe (fromJust)

import Shared.Common.Util.Crypto

dummyRsaPrivateKey :: RSA.PrivateKey
dummyRsaPrivateKey =
  fromJust . readRSAPrivateKey $
    "-----BEGIN RSA PRIVATE KEY-----\n\
    \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\n\
    \-----END RSA PRIVATE KEY-----\n"
