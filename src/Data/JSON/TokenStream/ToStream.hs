{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Data.JSON.TokenStream.ToStream
    ( ToStream(..)
    , object
    , pair
    , pairs
    , array
    , item
    , items
    , valueToEncoding
    )
     where

import           Data.Coerce
import           Data.JSON.TokenStream.Token
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

newtype Value = Value Encoding
  deriving (Monoid)

newtype Pairs = Pairs (Maybe Encoding)

instance Monoid Pairs where
  mempty = Pairs Nothing
  {-# INLINE mempty #-}

  Pairs Nothing `mappend` v = v
  v `mappend` Pairs Nothing = v
  Pairs (Just a) `mappend` Pairs (Just b) = Pairs $ Just (a <> encodeComma <> b)
  {-# INLINE mappend #-}

newtype Items = Items (Maybe Encoding)

instance Monoid Items where
  mempty = Items Nothing
  {-# INLINE mempty #-}

  Items Nothing `mappend` v = v
  v `mappend` Items Nothing = v
  Items (Just a) `mappend` Items (Just b) = Items $ Just (a <> encodeComma <> b)
  {-# INLINE mappend #-}


class ToStream a where
  toStream :: a -> Value

object :: Pairs -> Value
object (Pairs v) = Value $ case v of
  Nothing -> encodeObjectBegin <> encodeObjectEnd
  Just elems -> encodeObjectBegin <> elems <> encodeObjectEnd
{-# INLINE object #-}

pair :: ToStream a => Text -> a -> Pairs
pair t v = Pairs (Just (encodeKey t <> vs))
  where
    Value vs = toStream v
{-# INLINE pair #-}

pairs :: (Foldable f, ToStream a) => f (Text,a) -> Pairs
pairs = foldMap (uncurry pair)
{-# INLINE pairs #-}

array :: Items -> Value
array (Items v) = Value $ case v of
  Nothing -> encodeListBegin <> encodeListEnd
  Just elems -> encodeListBegin <> elems <> encodeListEnd
{-# INLINE array #-}

item :: ToStream a => a -> Items
item = Items . Just . coerce . toStream
{-# INLINE item #-}

items :: (Foldable f, ToStream a) => f a -> Items
items = foldMap item

instance ToStream Value where
  toStream = id
  {-# INLINE toStream #-}

instance ToStream Int where
  toStream = Value . encodeInt
  {-# INLINE toStream #-}

instance ToStream Double where
  toStream = Value . encodeDouble
  {-# INLINE toStream #-}

instance ToStream Text where
  toStream = Value . encodeText
  {-# INLINE toStream #-}

instance ToStream a => ToStream (Maybe a) where
  toStream Nothing = Value encodeNull
  toStream (Just v) = toStream v
  {-# INLINE toStream #-}

instance {-# OVERLAPPABLE #-} ToStream a => ToStream [a] where
  toStream [] = Value (encodeListBegin <> encodeListEnd)
  toStream (x:xs) = Value ( encodeListBegin
                          <> valueToEncoding (toStream x)
                          <> foldr go mempty xs
                          <> encodeListEnd)
    where
     go !v s = encodeComma <> valueToEncoding (toStream v) <> s

instance {-# OVERLAPPING #-} ToStream String where
  toStream = toStream . Text.pack

valueToEncoding :: Value -> Encoding
valueToEncoding = coerce
