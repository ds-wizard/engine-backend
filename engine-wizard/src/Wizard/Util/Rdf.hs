module Wizard.Util.Rdf where

import Data.RDF
import qualified Data.Text as T

import Shared.Util.List (headSafe)

rdfType = Just (unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

rdfsDomain = Just (unode "http://www.w3.org/2000/01/rdf-schema#domain")

rdfsRange = Just (unode "http://www.w3.org/2000/01/rdf-schema#range")

rdfsComment = Just (unode "http://www.w3.org/2000/01/rdf-schema#comment")

owlObjectProperty = Just (unode "http://www.w3.org/2002/07/owl#ObjectProperty")

owlDataTypeProperty = Just (unode "http://www.w3.org/2002/07/owl#DatatypeProperty")

data RdfClass
  = RdfClass T.Text [RdfDataType] [RdfObject]
  deriving (Show)

data RdfObject
  = RdfObject T.Text (Maybe T.Text) RdfClass
  deriving (Show)

data RdfDataType
  = RdfDataType T.Text T.Text
  deriving (Show)

subUriOf t =
  let (UNode val) = subjectOf t
   in val

objUriOf t =
  let (UNode val) = objectOf t
   in val

obValjOf t =
  case objectOf t of
    (LNode (PlainL val)) -> val
    (LNode (PlainLL val _)) -> val
    (LNode (TypedL val _)) -> val

resolveClass :: RDF TList -> T.Text -> RdfClass
resolveClass graph rootElement =
  let relationships = fmap subUriOf $ query graph Nothing rdfsDomain (Just (unode rootElement))
   in RdfClass rootElement (resolveDataTypes graph relationships) (resolveObjects graph relationships)

resolveObjects :: RDF TList -> [T.Text] -> [RdfObject]
resolveObjects graph relationships =
  let isObject r = not . null $ query graph (Just (unode r)) rdfType owlObjectProperty
      createObject o = RdfObject o (resolveComment graph o) (resolveClass graph (resolveRange graph o))
   in fmap createObject . filter isObject $ relationships

resolveDataTypes :: RDF TList -> [T.Text] -> [RdfDataType]
resolveDataTypes graph relationships =
  let isDataType r = not . null $ query graph (Just (unode r)) rdfType owlDataTypeProperty
      createDataType d = RdfDataType d (resolveRange graph d)
   in fmap createDataType . filter isDataType $ relationships

resolveRange :: RDF TList -> T.Text -> T.Text
resolveRange graph v = head . fmap objUriOf $ query graph (Just (unode v)) rdfsRange Nothing

resolveComment :: RDF TList -> T.Text -> Maybe T.Text
resolveComment graph o = fmap obValjOf . headSafe $ query graph (Just (unode o)) rdfsComment Nothing
