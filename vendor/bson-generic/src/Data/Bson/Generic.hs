module Data.Bson.Generic
  ( ToBSON(..)
  , FromBSON(..)
  , ObjectKey(..)
  , keyLabel
  ) where

import Control.Monad
import qualified Data.Bson as BSON (lookup)
import Data.Bson
import Data.Char (toLower)
import qualified Data.Text as TS (pack)
import Data.Typeable
import GHC.Generics

------------------------------------------------------------------------------
-- TO BSON
------------------------------------------------------------------------------
class ToBSON a where
  toBSON :: a -> Document
  default toBSON :: (Generic a, GToBSON (Rep a)) =>
    a -> Document
  toBSON a = genericToBSON "" (from a)

class GToBSON f where
  genericToBSON :: String -> f a -> Document

-- | Unit type -> Empty document
instance GToBSON U1 where
  genericToBSON _ U1 = []

-- | Sum of types
instance (GToBSON a, GToBSON b) => GToBSON (a :*: b) where
  genericToBSON constructorName (x :*: y) = genericToBSON constructorName x ++ genericToBSON constructorName y

-- | Product of types
instance (GToBSON a, GToBSON b) => GToBSON (a :+: b) where
  genericToBSON constructorName (L1 x) = genericToBSON constructorName x
  genericToBSON constructorName (R1 x) = genericToBSON constructorName x

-- | Datatype information tag
instance (GToBSON a) => GToBSON (D1 c a) where
  genericToBSON constructorName (M1 x) = genericToBSON constructorName x

-- | Constructor tag
instance (GToBSON a, Constructor c) => GToBSON (C1 c a) where
  genericToBSON constructorName c@(M1 x) = genericToBSON (conName c) x

-- | Selector tag
instance (Val a, Selector s) => GToBSON (S1 s (K1 i a)) where
  genericToBSON constructorName s@(M1 (K1 x)) = [TS.pack (removePrefix fieldPrefix $ selName s) =: x]
    where
      fieldPrefix = "_" ++ lowerFirst entityType
      entityType = constructorName

-- | ObjectKey special treatment
instance (Selector s) => GToBSON (S1 s (K1 i ObjectKey)) where
  genericToBSON _ (M1 (K1 (ObjectKey (Just key)))) = [keyLabel =: key]
  genericToBSON _ _ = []

-- | Constants
instance (ToBSON a) => GToBSON (K1 i a) where
  genericToBSON _ (K1 x) = toBSON x

------------------------------------------------------------------------------
-- FROM BSON
------------------------------------------------------------------------------
class FromBSON a where
  fromBSON :: Document -> Maybe a
  default fromBSON :: (Generic a, GFromBSON (Rep a)) =>
    Document -> Maybe a
  fromBSON doc = maybe Nothing (Just . to) (genericFromBSON "" doc)

class GFromBSON f where
  genericFromBSON :: String -> Document -> Maybe (f a)

-- | Unit type -> Empty document
instance GFromBSON U1 where
  genericFromBSON _ _ = Just U1

-- | Sum of types
instance (GFromBSON a, GFromBSON b) => GFromBSON (a :*: b) where
  genericFromBSON constructorName doc = do
    x <- genericFromBSON constructorName doc
    y <- genericFromBSON constructorName doc
    return $ x :*: y

-- | Product of types
instance (GFromBSON a, GFromBSON b) => GFromBSON (a :+: b) where
  genericFromBSON constructorName doc = left `mplus` right
    where
      left = L1 <$> genericFromBSON constructorName doc
      right = R1 <$> genericFromBSON constructorName doc

-- | Datatype information tag
instance (GFromBSON a, Constructor c) => GFromBSON (C1 c a) where
  genericFromBSON constructorName doc = M1 <$> genericFromBSON (conName (undefined :: M1 C c a r)) doc

-- | Constructor tag
instance (GFromBSON a) => GFromBSON (M1 D c a) where
  genericFromBSON constructorName doc = M1 <$> genericFromBSON constructorName doc

-- | Selector tag
instance (Val a, Selector s) => GFromBSON (S1 s (K1 i a)) where
  genericFromBSON constructorName doc = M1 . K1 <$> BSON.lookup sname doc
    where
      sname = TS.pack . removePrefix fieldPrefix . selName $ (undefined :: S1 s (K1 i a) r)
      fieldPrefix = "_" ++ lowerFirst constructorName

-- | ObjectKey special treatment
instance (Selector s) => GFromBSON (S1 s (K1 i ObjectKey)) where
  genericFromBSON _ doc = Just . M1 . K1 $ ObjectKey (BSON.lookup keyLabel doc)

------------------------------------------------------------------------------
-- ObjectKey
------------------------------------------------------------------------------
newtype ObjectKey =
  ObjectKey
    { unObjectKey :: Maybe ObjectId
    }
  deriving (Generic, Typeable, Show, Eq)

instance FromBSON ObjectKey

instance ToBSON ObjectKey

------------------------------------------------------------------------------
-- Other
------------------------------------------------------------------------------
instance (FromBSON a, ToBSON a, Typeable a, Show a, Eq a) => Val a where
  val x = Doc $ toBSON x
  cast' (Doc x) = fromBSON x
  cast' _ = Nothing

------------------------------------------------------------------------------
keyLabel :: Label
keyLabel = TS.pack "_id"

------------------------------------------------------------------------------  
removePrefix :: String -> String -> String
removePrefix fieldPrefix = bsonSpecialFields . lowerFirst . drop (length fieldPrefix)

bsonSpecialFields :: String -> String
bsonSpecialFields "aType" = "type"
bsonSpecialFields "pType" = "type"
bsonSpecialFields "bundleId" = "id"
bsonSpecialFields "intId" = "id"
bsonSpecialFields "aId" = "id"
bsonSpecialFields "dId" = "id"
bsonSpecialFields "iId" = "id"
bsonSpecialFields "pId" = "id"
bsonSpecialFields "sId" = "id"
bsonSpecialFields field = field

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [toLower c]
lowerFirst (s:str) = toLower s : str
