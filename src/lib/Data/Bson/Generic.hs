{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

------------------------------------------------------------------------------
-- | Examples
--
-- > data Test0 = A | B | C deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test0
-- > instance FromBSON Test0
-- >
-- > (fromBSON $ toBSON A) :: Maybe Test0
--
--
-- > data Test1 = Test1 String String deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test1
-- > instance FromBSON Test1
-- >
-- > (fromBSON $ toBSON $ Test1 "aa" "bb") :: Maybe Test1
--
--
-- > data Test2 = Test2 { test20 :: String, test21 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test2
-- > instance FromBSON Test2
-- >
-- > (fromBSON $ toBSON $ Test2 "aa" "bb") :: Maybe Test2
--
--
-- > data Test3 = Test3 { test30 :: Test2, test31 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test3
-- > instance FromBSON Test3
-- >
-- > (fromBSON $ toBSON $ Test3 (Test2 "aa" "bb") "cc") :: Maybe Test3
--
--
-- > data Test4 = Test4 { test4Key :: ObjectKey, test4 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test4
-- > instance FromBSON Test4
-- >
-- > (fromBSON $ toBSON $ Test4 (ObjectKey . Just $ unsafePerformIO genObjectId) "something") :: Maybe Test4
-- > (fromBSON $ toBSON $ Test4 (ObjectKey Nothing) "something") :: Maybe Test4
--
--
-- > data Comment = Comment { author :: String, comments :: [Comment] } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Comment
-- > instance FromBSON Comment
-- >
-- > (fromBSON $ toBSON $ Comment "Joe1" [Comment "Joe2" [], Comment "Joe3" [Comment "Joe4" [], Comment "Joe5" []]]) :: Maybe Comment
--
--
-- Representation
--
-- > toBSON $ Test2 "aa" "bb"
-- >
-- > [ test20: "aa", test21: "bb" ]
--
-- > toBSON $ Test3 (Test2 "aa" "bb") "cc"
-- >
-- > [ test30: [ test20: "aa", test21: "bb"], test31: "cc" ]
--
-- > toBSON $ Test4 (ObjectKey . Just $ unsafePerformIO genObjectId) "something"
-- >
-- > [ _id: 4f226c27900faa06ab000001, test4: "something" ]
--
-- > toBSON $ Test4 (ObjectKey Nothing) "something"
-- >
-- > [ test4: "something" ]
--
-- > toBSON $ Comment "Joe1" [ Comment "Joe2" []
-- >                         , Comment "Joe3" [ Comment "Joe4" []
-- >                                          , Comment "Joe5" []
-- >                                          ]
-- >                         ]
-- >
-- > [ author: "Joe1", comments: [ [ author: "Joe2", comments: []]
-- >                             , [ author: "Joe3", comments: [ [ author: "Joe4", comments: []]
-- >                                                           , [ author: "Joe5", comments: []]
-- >                                                           ]]
-- >                             ]]
module Data.Bson.Generic
  ( ToBSON(..)
  , FromBSON(..)
  , ObjectKey(..)
  , keyLabel
  , constructorLabel
  ) where

------------------------------------------------------------------------------
import GHC.Generics

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad

------------------------------------------------------------------------------
import qualified Data.Bson as BSON (lookup)
import Data.Bson
import qualified Data.Text as TS (pack)
import Data.Typeable

------------------------------------------------------------------------------
keyLabel :: Label
keyLabel = TS.pack "_id"

constructorLabel :: Label
constructorLabel = TS.pack "_co"

------------------------------------------------------------------------------
newtype ObjectKey = ObjectKey
  { unObjectKey :: Maybe ObjectId
  } deriving (Generic, Typeable, Show, Eq)

instance FromBSON ObjectKey

instance ToBSON ObjectKey

------------------------------------------------------------------------------
instance (FromBSON a, ToBSON a, Typeable a, Show a, Eq a) =>
         Val a where
  val x = Doc $ toBSON x
  cast' (Doc x) = fromBSON x
  cast' _ = Nothing

------------------------------------------------------------------------------
-- TO BSON
class ToBSON a where
  toBSON :: a -> Document
  default toBSON :: (Generic a, GConstructorCount (Rep a), GToBSON (Rep a)) =>
    a -> Document
  toBSON a = genericToBSON (constructorCount a) (from a)

class GToBSON f where
  genericToBSON :: Int -> f a -> Document

-- | Unit type -> Empty document
instance GToBSON U1 where
  genericToBSON _ U1 = []

-- | Sum of types
instance (GToBSON a, GToBSON b) =>
         GToBSON (a :*: b) where
  genericToBSON n (x :*: y) = genericToBSON n x ++ genericToBSON n y

-- | Product of types
instance (GToBSON a, GToBSON b) =>
         GToBSON (a :+: b) where
  genericToBSON n (L1 x) = genericToBSON n x
  genericToBSON n (R1 x) = genericToBSON n x

-- | Datatype information tag
instance (GToBSON a) =>
         GToBSON (D1 c a) where
  genericToBSON n (M1 x) = genericToBSON n x

-- | Constructor tag
instance (GToBSON a, Constructor c) =>
         GToBSON (C1 c a) where
  genericToBSON 0 (M1 x) = genericToBSON 0 x
  genericToBSON 1 (M1 x) = genericToBSON 1 x
  genericToBSON n c@(M1 x) =
    genericToBSON n x ++ [constructorLabel =: conName c]

-- | Selector tag
instance (Val a, Selector s) =>
         GToBSON (S1 s (K1 i a)) where
  genericToBSON _ s@(M1 (K1 x)) = [TS.pack (selName s) =: x]

-- | ObjectKey special treatment
instance (Selector s) =>
         GToBSON (S1 s (K1 i ObjectKey)) where
  genericToBSON _ (M1 (K1 (ObjectKey (Just key)))) = [keyLabel =: key]
  genericToBSON _ _ = []

-- | Constants
instance (ToBSON a) =>
         GToBSON (K1 i a) where
  genericToBSON _ (K1 x) = toBSON x

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- FROM BSON
class FromBSON a where
  fromBSON :: Document -> Maybe a
  default fromBSON :: (Generic a, GConstructorCount (Rep a), GFromBSON (Rep a)) =>
    Document -> Maybe a
  fromBSON doc =
    maybe
      Nothing
      (Just . to)
      (genericFromBSON (constructorCount (undefined :: a)) doc)

class GFromBSON f where
  genericFromBSON :: Int -> Document -> Maybe (f a)

instance GFromBSON U1 where
  genericFromBSON _ _ = Just U1

instance (GFromBSON a, GFromBSON b) =>
         GFromBSON (a :*: b) where
  genericFromBSON n doc = do
    x <- genericFromBSON n doc
    y <- genericFromBSON n doc
    return $ x :*: y

instance (GFromBSON a, GFromBSON b) =>
         GFromBSON (a :+: b) where
  genericFromBSON n doc = left `mplus` right
    where
      left = L1 <$> genericFromBSON n doc
      right = R1 <$> genericFromBSON n doc

instance (GFromBSON a, Constructor c) =>
         GFromBSON (C1 c a) where
  genericFromBSON 0 doc = M1 <$> genericFromBSON 0 doc
  genericFromBSON 1 doc = M1 <$> genericFromBSON 0 doc
  genericFromBSON n doc = do
    cname <- BSON.lookup constructorLabel doc
    if cname == conName (undefined :: M1 C c a r)
      then M1 <$> genericFromBSON n doc
      else Nothing

instance (GFromBSON a) =>
         GFromBSON (M1 D c a) where
  genericFromBSON n doc = M1 <$> genericFromBSON n doc

instance (Val a, Selector s) =>
         GFromBSON (S1 s (K1 i a)) where
  genericFromBSON _ doc = M1 . K1 <$> BSON.lookup sname doc
    where
      sname = TS.pack . selName $ (undefined :: S1 s (K1 i a) r)

-- | ObjectKey special treatment
instance (Selector s) =>
         GFromBSON (S1 s (K1 i ObjectKey)) where
  genericFromBSON _ doc = Just . M1 . K1 $ ObjectKey (BSON.lookup keyLabel doc)

------------------------------------------------------------------------------
-- CONVENIENCE
class GConstructorCount f where
  gconstructorCount :: f a -> Int

instance GConstructorCount V1 where
  gconstructorCount _ = 0

instance (GConstructorCount a) =>
         GConstructorCount (D1 d a) where
  gconstructorCount (M1 x) = gconstructorCount x

instance (Constructor c) =>
         GConstructorCount (C1 c a) where
  gconstructorCount _ = 1

instance (GConstructorCount a, GConstructorCount b) =>
         GConstructorCount (a :+: b) where
  gconstructorCount (_ :: (a :+: b) r) =
    gconstructorCount (undefined :: a r) + gconstructorCount (undefined :: b r)

constructorCount
  :: (Generic a, GConstructorCount (Rep a))
  => a -> Int
constructorCount x = gconstructorCount $ from x
