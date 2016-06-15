{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.JSON.TokenStream.ToStream
    ( ToStream(..)
    , object
    , pair
    , pairs
    , array
    , item
    , items
    )
     where

import           GHC.Generics

import           Data.Coerce
import qualified Data.List                   as List
import           Data.Monoid                 ((<>))
import           Data.Proxy
import           Data.Tagged
import           Data.Text                   (Text)
import qualified Data.Text                   as Text


import           Data.JSON.TokenStream.Token

data Items = Empty
         | Items Encoding

instance Monoid Items where
  mempty = Empty
  {-# INLINE mempty #-}
  Empty `mappend` v = v
  v `mappend` Empty = v
  Items a `mappend` Items b = Items (a <> encodeComma <> b)


newtype Pairs = Pairs Items
  deriving (Monoid)

newtype List = List Items
  deriving (Monoid)

class ToStream a where
  toStream :: a -> Encoding
  default toStream :: (Generic a, GToStream Zero (Rep a)) => a -> Encoding
  toStream = genericToStream defaultOptions

object :: Pairs -> Encoding
object (Pairs v) = case v of
  Empty -> encodeObjectEmpty
  Items elems -> wrapObject elems
{-# INLINE object #-}

pair :: ToStream a => Text -> a -> Pairs
pair t v = Pairs (Items (encodeKey t <> toStream v))
{-# INLINE pair #-}

pairs :: (Foldable f, ToStream a) => f (Text,a) -> Pairs
pairs = foldMap (uncurry pair)
{-# INLINE pairs #-}

array :: List -> Encoding
array (List v) = case v of
  Empty -> encodeListEmpty
  Items elems -> wrapList elems
{-# INLINE array #-}

item :: ToStream a => a -> List
item = List . Items . coerce . toStream
{-# INLINE item #-}

items :: (Foldable f, ToStream a) => f a -> List
items = foldMap item

instance ToStream Encoding where
  toStream = id
  {-# INLINE toStream #-}

instance ToStream Int where
  toStream = encodeInt
  {-# INLINE toStream #-}

instance ToStream Double where
  toStream = encodeDouble
  {-# INLINE toStream #-}

instance ToStream Text where
  toStream = encodeText
  {-# INLINE toStream #-}

instance ToStream a => ToStream (Maybe a) where
  toStream Nothing = encodeNull
  toStream (Just v) = toStream v
  {-# INLINE toStream #-}

instance {-# OVERLAPPABLE #-} ToStream a => ToStream [a] where
  toStream [] = encodeListEmpty
  toStream (x:xs) = wrapList $ toStream x
                             <> foldr go mempty xs
    where
     go !v s = encodeComma <> toStream v <> s

instance {-# OVERLAPPING #-} ToStream String where
  toStream = toStream . Text.pack

-- | Options that specify how to encode\/decode your datatype to\/from JSON.
data Options = Options
    { fieldLabelModifier     :: String -> String
    , constructorTagModifier :: String -> String
    , allNullaryToStringTag  :: Bool
    , omitNothingFields      :: Bool
    , sumEncoding            :: SumEncoding
    , unwrapUnaryRecords     :: Bool
    }

data SumEncoding =
    TaggedObject { tagFieldName      :: String
                 , contentsFieldName :: String
                 }
  | ObjectWithSingleField
  | TwoElemArray
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
                 { fieldLabelModifier      = id
                 , constructorTagModifier  = id
                 , allNullaryToStringTag   = True
                 , omitNothingFields       = False
                 , sumEncoding             = defaultTaggedObject
                 , unwrapUnaryRecords      = False
                 }

defaultTaggedObject :: SumEncoding
defaultTaggedObject = TaggedObject
                      { tagFieldName      = "tag"
                      , contentsFieldName = "contents"
                      }

------ Generics stolen from aeson

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance (Constructor c) => GetConName (C1 c a) where
    getConName = conName

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

data Zero
data One

proxyZero :: Proxy Zero
proxyZero = Proxy

proxyOne :: Proxy One
proxyOne  = Proxy

class ProductSize f where
    productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1


class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) 'False
instance AllNullary (a :.: b) 'False
instance AllNullary (K1 i c) 'False
instance AllNullary Par1 'False
instance AllNullary U1 'True

class    And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And 'True  'True  'True
instance And 'False 'False 'False
instance And 'False 'True  'False
instance And 'True  'False 'False

genericToStream :: (Generic a, GToStream Zero (Rep a))
              => Options -> a -> Encoding
genericToStream opts =
  gToStream opts proxyZero undefined undefined . from


class GToStream arity f where
    gToStream :: Options -> Proxy arity
            -> (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding



class ToStream1 f where
    liftToStream :: (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

    default liftToStream :: (Generic1 f, GToStream One (Rep1 f))
                       => (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding
    liftToStream = genericLiftToStream defaultOptions

    liftToStreamList :: (a -> Encoding) -> ([a] -> Encoding) -> [f a] -> Encoding
    liftToStreamList f g = listStream (liftToStream f g)

listStream :: (a -> Encoding) -> [a] -> Encoding
listStream _ [] = encodeListEmpty
listStream f (x:xs) =
    encodeListBegin  <> f x
    <> foldr (\v acc -> encodeComma <> f v <> acc) encodeListEnd xs

{-# INLINE listStream #-}


class IsRecord (f :: * -> *) isRecord | f -> isRecord
  where
    isUnary :: f a -> Bool
    isUnary = const True

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
  where isUnary = const False
instance {-# OVERLAPPING #-} IsRecord (M1 S NoSelector f) 'False

instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) 'True
instance IsRecord Par1 'True
instance IsRecord (f :.: g) 'True
instance IsRecord U1 'False
  where isUnary = const False

genericLiftToStream :: (Generic1 f, GToStream One (Rep1 f))
                  => Options -> (a -> Encoding) -> ([a] -> Encoding)
                  -> f a -> Encoding
genericLiftToStream opts tj tjl = gToStream opts proxyOne tj tjl . from1

class RecordToEncoding arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    recordToEncoding :: Options -> Proxy arity
                     -> (a -> Encoding) -> ([a] -> Encoding)
                     -> f a -> (Encoding, Maybe Encoding)

instance ( RecordToEncoding    arity a
         , RecordToEncoding    arity b
         ) => RecordToEncoding arity (a :*: b) where
    recordToEncoding opts pa te tel (a :*: b) | omitNothingFields opts =
      (mconcat $ List.intersperse encodeComma $
        filter (not . nullEncoding)
        [ fst (recordToEncoding opts pa te tel a)
        , fst (recordToEncoding opts pa te tel b) ]
      , Nothing)
    recordToEncoding opts pa te tel (a :*: b) =
      (fst (recordToEncoding opts pa te tel a) <> encodeComma <>
       fst (recordToEncoding opts pa te tel b),
       Nothing)

nullEncoding :: Encoding -> Bool
nullEncoding (Encoding s) = s TkEnd == TkEnd

instance (Selector s, GToStream arity a) => RecordToEncoding arity (S1 s a) where
    recordToEncoding = fieldToEncoding

instance {-# OVERLAPPING #-} (Selector s, ToStream a) =>
  RecordToEncoding arity (S1 s (K1 i (Maybe a))) where
    recordToEncoding opts _ _ _ (M1 k1) | omitNothingFields opts
                                        , K1 Nothing <- k1 = (mempty, Nothing)
    recordToEncoding opts pa te tel m1 = fieldToEncoding opts pa te tel m1

fieldToEncoding :: (Selector s, GToStream arity a)
                => Options -> Proxy arity
                -> (p -> Encoding) -> ([p] -> Encoding)
                -> S1 s a p -> (Encoding, Maybe Encoding)
fieldToEncoding opts pa te tel m1 =
  let key = encodeKeyString (fieldLabelModifier opts $ selName m1)
      value = gToStream opts pa te tel (unM1 m1)
  in  (key <> value, Just value)

--------------------------------------------------------------------------------

class ConsToEncoding arity f where
    consToEncoding :: Options -> Proxy arity
                   -> (a -> Encoding) -> ([a] -> Encoding)
                   -> f a -> Encoding

class ConsToEncoding' arity f isRecord where
    consToEncoding' :: Options -> Proxy arity
                    -> Bool -- ^ Are we a record with one field?
                    -> (a -> Encoding) -> ([a] -> Encoding)
                    -> f a -> Tagged isRecord Encoding

instance ( IsRecord                f isRecord
         , ConsToEncoding'   arity f isRecord
         ) => ConsToEncoding arity f where
    consToEncoding opts pa te tel =
        (unTagged :: Tagged isRecord Encoding -> Encoding)
      . consToEncoding' opts pa (isUnary (undefined :: f a)) te tel

instance (RecordToEncoding arity f) => ConsToEncoding' arity f 'True where
    consToEncoding' opts pa isUn te tel x =
      let (enc, mbVal) = recordToEncoding opts pa te tel x
      in case (unwrapUnaryRecords opts, isUn, mbVal) of
           (True, True, Just val) -> Tagged val
           _ -> Tagged $ wrapObject enc

instance GToStream arity f => ConsToEncoding' arity f 'False where
    consToEncoding' opts pa _ te tel = Tagged . gToStream opts pa te tel


--------------------------------------------------------------------------------

class EncodeProduct arity f where
    encodeProduct :: Options -> Proxy arity
                  -> (a -> Encoding) -> ([a] -> Encoding)
                  -> f a -> Encoding

instance ( EncodeProduct    arity a
         , EncodeProduct    arity b
         ) => EncodeProduct arity (a :*: b) where
    encodeProduct opts pa te tel (a :*: b) | omitNothingFields opts =
        mconcat $ List.intersperse encodeComma $
        filter (not . nullEncoding)
        [encodeProduct opts pa te tel a, encodeProduct opts pa te tel b]
    encodeProduct opts pa te tel (a :*: b) =
      encodeProduct opts pa te tel a <> encodeProduct opts pa te tel b

instance {-# OVERLAPPABLE #-} (GToStream arity a) => EncodeProduct arity a where
    encodeProduct opts pa te tel a = gToStream opts pa te tel a


--------------------------------------------------------------------------------

class ObjectWithSingleFieldEnc arity f where
    objectWithSingleFieldEnc :: Options -> Proxy arity
                             -> (a -> Encoding) -> ([a] -> Encoding)
                             -> f a -> Encoding

instance ( ObjectWithSingleFieldEnc    arity a
         , ObjectWithSingleFieldEnc    arity b
         ) => ObjectWithSingleFieldEnc arity (a :+: b) where
    objectWithSingleFieldEnc opts pa te tel (L1 x) =
      objectWithSingleFieldEnc opts pa te tel x
    objectWithSingleFieldEnc opts pa te tel (R1 x) =
      objectWithSingleFieldEnc opts pa te tel x

instance ( GToStream    arity a
         , ConsToEncoding arity a
         , Constructor c
         ) => ObjectWithSingleFieldEnc arity (C1 c a) where
    objectWithSingleFieldEnc opts pa te tel v =
      wrapObject $
        encodeKeyString
            (constructorTagModifier opts
                         (conName (undefined :: t c a p)))
         <> gToStream opts pa te tel v


--------------------------------------------------------------------------------
-- Generic toEncoding

instance {-# OVERLAPPABLE #-} (GToStream arity a) => GToStream arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToStream opts pa te tel = gToStream opts pa te tel . unM1

instance ToStream a => GToStream arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToStream _opts _ _ _ = coerce . toStream . unK1

instance GToStream One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    gToStream _opts _ te _ = te . unPar1

instance ToStream1 f => GToStream One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToStream1 instance:
    gToStream _opts _ te tel = coerce . liftToStream te tel . unRec1

instance GToStream arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToStream _opts _ _ _ _ = encodeListEmpty

instance (ConsToEncoding arity a) => GToStream arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToStream':
    gToStream opts pa te tel = consToEncoding opts pa te tel . unM1

instance ( EncodeProduct  arity a
         , EncodeProduct  arity b
         ) => GToStream arity (a :*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gToStream opts pa te tel p = wrapList $ encodeProduct opts pa te tel p

instance ( AllNullary           (a :+: b) allNullary
         , SumToEncoding  arity (a :+: b) allNullary
         ) => GToStream arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToStream':
    gToStream opts pa te tel
        = (unTagged :: Tagged allNullary Encoding -> Encoding)
        . sumToEncoding opts pa te tel

instance (ToStream1 f, GToStream One g) => GToStream One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToStream opts pa te tel =
      let gte = gToStream opts pa te tel in
      liftToStream gte (listStream gte) . unComp1

--------------------------------------------------------------------------------

class SumToEncoding arity f allNullary where
    sumToEncoding :: Options -> Proxy arity
                  -> (a -> Encoding) -> ([a] -> Encoding)
                  -> f a -> Tagged allNullary Encoding

instance ( GetConName                     f
         , TaggedObjectEnc          arity f
         , ObjectWithSingleFieldEnc arity f
         , TwoElemArrayEnc          arity f
         ) => SumToEncoding arity f 'True where
    sumToEncoding opts pa te tel
        | allNullaryToStringTag opts = Tagged . coerce . toStream .
                                       constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToEncoding opts pa te tel

instance ( TwoElemArrayEnc          arity f
         , TaggedObjectEnc          arity f
         , ObjectWithSingleFieldEnc arity f
         ) => SumToEncoding arity f 'False where
    sumToEncoding opts pa te tel = Tagged . nonAllNullarySumToEncoding opts pa te tel

nonAllNullarySumToEncoding :: ( TwoElemArrayEnc          arity f
                              , TaggedObjectEnc          arity f
                              , ObjectWithSingleFieldEnc arity f
                              ) => Options -> Proxy arity
                                -> (a -> Encoding) -> ([a] -> Encoding)
                                -> f a -> Encoding
nonAllNullarySumToEncoding opts pa te tel =
    case sumEncoding opts of
      TaggedObject{..}      ->
        taggedObjectEnc opts pa tagFieldName contentsFieldName te tel
      ObjectWithSingleField -> objectWithSingleFieldEnc opts pa te tel
      TwoElemArray          -> twoElemArrayEnc opts pa te tel

--------------------------------------------------------------------------------

class TaggedObjectEnc arity f where
    taggedObjectEnc :: Options -> Proxy arity
                    -> String -> String
                    -> (a -> Encoding) -> ([a] -> Encoding)
                    -> f a -> Encoding

instance ( TaggedObjectEnc    arity a
         , TaggedObjectEnc    arity b
         ) => TaggedObjectEnc arity (a :+: b) where
    taggedObjectEnc opts pa tagFieldName contentsFieldName te tel (L1 x) =
        taggedObjectEnc opts pa tagFieldName contentsFieldName te tel x
    taggedObjectEnc opts pa tagFieldName contentsFieldName te tel (R1 x) =
        taggedObjectEnc opts pa tagFieldName contentsFieldName te tel x

instance ( IsRecord               a isRecord
         , TaggedObjectEnc' arity a isRecord
         , Constructor c
         ) => TaggedObjectEnc arity (C1 c a) where
    taggedObjectEnc opts pa tagFieldName contentsFieldName te tel v =
        wrapObject $
           (encodeKeyString tagFieldName <>
           (coerce $ toStream (constructorTagModifier opts (conName (undefined :: t c a p))))) <>
            ((unTagged :: Tagged isRecord (Encoding) -> Encoding) .
             taggedObjectEnc' opts pa contentsFieldName te tel . unM1 $ v)


wrapObject :: Encoding -> Encoding
wrapObject o = encodeObjectBegin <> o <> encodeObjectEnd

wrapList :: Encoding -> Encoding
wrapList o = encodeListBegin <> o <> encodeListEnd

class TaggedObjectEnc' arity f isRecord where
    taggedObjectEnc' :: Options -> Proxy arity
                     -> String -> (a -> Encoding) -> ([a] -> Encoding)
                     -> f a -> Tagged isRecord Encoding

instance {-# OVERLAPPING #-} TaggedObjectEnc' arity U1 'False where
    taggedObjectEnc' _ _ _ _ _ _ = Tagged mempty

instance (RecordToEncoding arity f) => TaggedObjectEnc' arity f 'True where
    taggedObjectEnc' opts pa _ te tel =
        Tagged . (encodeComma <>) . fst
                   . recordToEncoding opts pa te tel

instance (GToStream arity f) => TaggedObjectEnc' arity f 'False where
    taggedObjectEnc' opts pa contentsFieldName te tel =
        Tagged . (\z -> encodeComma
                  <> encodeKeyString contentsFieldName
                  <> z) .
        gToStream opts pa te tel

--------------------------------------------------------------------------------

class TwoElemArrayEnc arity f where
    twoElemArrayEnc :: Options -> Proxy arity
                    -> (a -> Encoding) -> ([a] -> Encoding)
                    -> f a -> Encoding

instance ( TwoElemArrayEnc    arity a
         , TwoElemArrayEnc    arity b
         ) => TwoElemArrayEnc arity (a :+: b) where
    twoElemArrayEnc opts pa te tel (L1 x) = twoElemArrayEnc opts pa te tel x
    twoElemArrayEnc opts pa te tel (R1 x) = twoElemArrayEnc opts pa te tel x

instance ( GToStream    arity a
         , ConsToEncoding arity a
         , Constructor c
         ) => TwoElemArrayEnc arity (C1 c a) where
    twoElemArrayEnc opts pa te tel x = wrapList $
      coerce (toStream (constructorTagModifier opts (conName (undefined :: t c a p))))
        <> encodeComma <> gToStream opts pa te tel x


------ Instances

instance ToStream Bool where
  toStream = encodeBool

instance (ToStream a, ToStream b) => ToStream (a,b) where
  toStream (a,b) = wrapList $
    coerce (toStream a) <> encodeComma <> coerce (toStream b)

instance (ToStream a, ToStream b,ToStream c) => ToStream (a,b,c) where
  toStream (a,b,c) = wrapList $
    coerce (toStream a)
    <> encodeComma
    <> coerce (toStream b)
    <> encodeComma
    <> coerce (toStream c)

instance (ToStream a, ToStream b) => ToStream (Either a b) where
  toStream v = case v of
    Left l -> wrapObject $
              encodeKey "Left" <> encodeComma <> coerce(toStream l)
    Right r -> wrapObject $
               encodeKey "Right" <> encodeComma <> coerce(toStream r)
