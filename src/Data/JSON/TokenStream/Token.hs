module Data.JSON.TokenStream.Token
    ( Tokens (..)
    , Encoding (..)
    , encodeInt
    , encodeDouble
    , encodeText
    , encodeBool
    , encodeKey
    , encodeNull
    , encodeComma
    , encodeListBegin
    , encodeListEnd
    , encodeObjectBegin
    , encodeObjectEnd
    ) where

import           Data.Text (Text)

newtype Encoding = Encoding (Tokens -> Tokens)

data Tokens
    = TkInt    {-# UNPACK #-} !Int    Tokens -- Scientific?
    | TkDouble {-# UNPACK #-} !Double Tokens -- same
    | TkString {-# UNPACK #-} !Text   Tokens
    | TkBool   {-# UNPACK #-} !Bool   Tokens
    | TkKey    {-# UNPACK #-} !Text   Tokens
    | TkNull                          Tokens
    | TkComma                         Tokens
    | TkListBegin                     Tokens
    | TkListEnd                       Tokens
    | TkObjectBegin                   Tokens
    | TkObjectEnd                     Tokens
    | TkEnd
    deriving (Eq, Ord, Show)


instance Monoid Encoding where
  mempty = Encoding (\ts -> ts)
  {-# INLINE mempty #-}

  Encoding b1 `mappend` Encoding b2 = Encoding (\ts -> b1 (b2 ts))
  {-# INLINE mappend #-}

  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

encodeInt :: Int -> Encoding
encodeInt = Encoding . TkInt

encodeDouble :: Double -> Encoding
encodeDouble = Encoding . TkDouble

encodeText :: Text -> Encoding
encodeText = Encoding . TkString

encodeBool :: Bool -> Encoding
encodeBool = Encoding . TkBool

encodeKey :: Text -> Encoding
encodeKey = Encoding . TkKey

encodeNull :: Encoding
encodeNull = Encoding TkNull

encodeComma :: Encoding
encodeComma = Encoding TkComma

encodeListBegin :: Encoding
encodeListBegin = Encoding TkListBegin

encodeListEnd :: Encoding
encodeListEnd = Encoding TkListEnd

encodeObjectBegin :: Encoding
encodeObjectBegin = Encoding TkObjectBegin

encodeObjectEnd :: Encoding
encodeObjectEnd = Encoding TkObjectEnd
