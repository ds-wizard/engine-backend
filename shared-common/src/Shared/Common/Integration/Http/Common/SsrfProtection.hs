{- | SSRF protection for outbound integration requests whose destination URL is
user-influenced. Blocks connections to non-public IP ranges (loopback,
private, link-local incl. the cloud metadata address, ULA, multicast) at the
connection level, so it is resistant to redirects and DNS rebinding. Specific
internal hosts/CIDRs can be permitted via an operator allowlist.
-}
module Shared.Common.Integration.Http.Common.SsrfProtection (
  buildAllowRules,
  ssrfRestriction,
  isConnectionAllowed,
  AllowRule,
) where

import qualified Control.Exception as E
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word16, Word32, Word8)
import Network.HTTP.Client.Restricted (ConnectionRestricted, Restriction, addressRestriction, connectionRestricted)
import Network.Socket (
  AddrInfo (..),
  SockAddr (..),
  defaultHints,
  getAddrInfo,
  hostAddress6ToTuple,
  hostAddressToTuple,
 )
import Text.Read (readMaybe)

-- | A resolved allowlist rule matched against the address we are about to connect to.
data AllowRule
  = -- | IPv4 base (logical big-endian Word32) + CIDR prefix length (/32 = exact)
    AllowV4 Word32 Int
  | -- | exact IPv6 (eight hextets)
    AllowV6 (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)

data ResolvedIp
  = V4 Word32
  | V6 (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
  | Other

{- | Build allowlist rules from config entries. An entry is either an IPv4 CIDR
(`10.0.0.0/8`), or a literal IP / hostname which is resolved once here (the
allowlist is operator-trusted, so build-time resolution is acceptable).
-}
buildAllowRules :: [String] -> IO [AllowRule]
buildAllowRules = fmap concat . traverse parseEntry

parseEntry :: String -> IO [AllowRule]
parseEntry entry
  | '/' `elem` entry = return (maybe [] pure (parseCidrV4 entry))
  | otherwise = resolveHost entry

parseCidrV4 :: String -> Maybe AllowRule
parseCidrV4 s =
  case break (== '/') s of
    (ipStr, '/' : prefixStr) -> do
      base <- parseV4 ipStr
      prefix <- readMaybe prefixStr
      if prefix >= 0 && prefix <= 32 then Just (AllowV4 base prefix) else Nothing
    _ -> Nothing

parseV4 :: String -> Maybe Word32
parseV4 s =
  case traverse readOctet (splitOn '.' s) of
    Just [a, b, c, d] -> Just (packV4 (a, b, c, d))
    _ -> Nothing
  where
    readOctet o = case readMaybe o of
      Just n | n >= 0 && n <= 255 -> Just (fromIntegral (n :: Int) :: Word8)
      _ -> Nothing

resolveHost :: String -> IO [AllowRule]
resolveHost hostName = do
  eAddrs <- E.try (getAddrInfo (Just defaultHints) (Just hostName) Nothing) :: IO (Either E.SomeException [AddrInfo])
  case eAddrs of
    Left _ -> return []
    Right addrs -> return (concatMap (toRule . resolveSockAddr . addrAddress) addrs)
  where
    toRule (V4 w) = [AllowV4 w 32]
    toRule (V6 t) = [AllowV6 t]
    toRule Other = []

{- | The restriction handed to http-client-restricted. Allows a connection when
the target address is explicitly allowlisted or is a public address; denies
everything else (loopback / private / link-local / metadata / ULA / …).
-}
ssrfRestriction :: [AllowRule] -> Restriction
ssrfRestriction rules = addressRestriction check
  where
    check :: AddrInfo -> Maybe ConnectionRestricted
    check addrInfo =
      if isConnectionAllowed rules (addrAddress addrInfo)
        then Nothing
        else Just (connectionRestricted ("SSRF protection: refused connection to non-public address " ++) addrInfo)

{- | Core decision behind 'ssrfRestriction': may we open a connection to this
resolved socket address? Allowed when the address is explicitly allowlisted or
is a public address; denied otherwise. Kept pure and exported so the
classification can be tested without opening a live connection.
-}
isConnectionAllowed :: [AllowRule] -> SockAddr -> Bool
isConnectionAllowed rules sockAddr =
  let ip = resolveSockAddr sockAddr
   in isAllowed rules ip || not (isBlocked ip)

isAllowed :: [AllowRule] -> ResolvedIp -> Bool
isAllowed rules ip = any matches rules
  where
    matches (AllowV4 base prefix) = case ip of
      V4 w -> maskEq prefix w base
      _ -> False
    matches (AllowV6 t') = case ip of
      V6 t -> t == t'
      _ -> False
    maskEq prefix a b =
      let sh = 32 - prefix
       in (a `shiftR` sh) == (b `shiftR` sh)

isBlocked :: ResolvedIp -> Bool
isBlocked (V4 w) = isBlockedV4 w
isBlocked (V6 t) = isBlockedV6 t
isBlocked Other = True

isBlockedV4 :: Word32 -> Bool
isBlockedV4 w =
  let a = fromIntegral (w `shiftR` 24) :: Word8
      b = fromIntegral (w `shiftR` 16) :: Word8
   in a == 0 -- 0.0.0.0/8 "this network"
        || a == 10 -- 10.0.0.0/8 private
        || a == 127 -- 127.0.0.0/8 loopback
        || a >= 224 -- 224.0.0.0/4 multicast + 240.0.0.0/4 reserved + 255.255.255.255
        || (a == 169 && b == 254) -- 169.254.0.0/16 link-local (incl. 169.254.169.254 metadata)
        || (a == 172 && b >= 16 && b <= 31) -- 172.16.0.0/12 private
        || (a == 192 && b == 168) -- 192.168.0.0/16 private
        || (a == 100 && b >= 64 && b <= 127) -- 100.64.0.0/10 CGNAT

isBlockedV6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Bool
isBlockedV6 (h0, h1, h2, h3, h4, h5, h6, h7) =
  allZeroExceptLast -- :: and ::1
    || (h0 .&. 0xffc0) == 0xfe80 -- fe80::/10 link-local
    || (h0 .&. 0xfe00) == 0xfc00 -- fc00::/7 unique local
    || (h0 .&. 0xff00) == 0xff00 -- ff00::/8 multicast
  where
    allZeroExceptLast = h0 == 0 && h1 == 0 && h2 == 0 && h3 == 0 && h4 == 0 && h5 == 0 && h6 == 0 && (h7 == 0 || h7 == 1)

resolveSockAddr :: SockAddr -> ResolvedIp
resolveSockAddr (SockAddrInet _ ha) = V4 (packV4 (hostAddressToTuple ha))
resolveSockAddr (SockAddrInet6 _ _ ha6 _) =
  case hostAddress6ToTuple ha6 of
    (0, 0, 0, 0, 0, 0xffff, hi, lo) -> V4 ((fromIntegral hi `shiftL` 16) .|. fromIntegral lo) -- ::ffff:a.b.c.d
    t -> V6 t
resolveSockAddr _ = Other

packV4 :: (Word8, Word8, Word8, Word8) -> Word32
packV4 (a, b, c, d) =
  (fromIntegral a `shiftL` 24)
    .|. (fromIntegral b `shiftL` 16)
    .|. (fromIntegral c `shiftL` 8)
    .|. fromIntegral d

splitOn :: Char -> String -> [String]
splitOn sep s =
  case break (== sep) s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitOn sep rest
