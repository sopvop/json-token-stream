module Data.JSON.TokenStream.Token
    ( Tokens (..)
    , Encoding (..)
    , encodeInt
    , encodeDouble
    , encodeText
    , encodeString
    , encodeBool
    , encodeKey
    , encodeKeyString
    , encodeNull
    , encodeComma
    , encodeListEmpty
    , encodeListBegin
    , encodeListEnd
    , encodeObjectEmpty
    , encodeObjectBegin
    , encodeObjectEnd
    ) where

import           Data.Text (Text)

newtype Encoding = Encoding (Tokens -> Tokens)

data Tokens
    = TkInt    {-# UNPACK #-} !Int    Tokens -- Scientific?
    | TkDouble {-# UNPACK #-} !Double Tokens -- same
    | TkString {-# UNPACK #-} !String Tokens
    | TkText   {-# UNPACK #-} !Text   Tokens
    | TkBool                  !Bool   Tokens
    | TkKey    {-# UNPACK #-} !Text   Tokens
    | TkKeyString  {-# UNPACK #-} !String   Tokens
    | TkNull                          Tokens
    | TkComma                         Tokens
    | TkListEmpty                     Tokens
    | TkListBegin                     Tokens
    | TkListEnd                       Tokens
    | TkObjectEmpty                   Tokens
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
encodeText = Encoding . TkText

encodeString :: String -> Encoding
encodeString = Encoding . TkString

encodeBool :: Bool -> Encoding
encodeBool = Encoding . TkBool

encodeKey :: Text -> Encoding
encodeKey = Encoding . TkKey

encodeKeyString :: String -> Encoding
encodeKeyString = Encoding . TkKeyString

encodeNull :: Encoding
encodeNull = Encoding TkNull

encodeComma :: Encoding
encodeComma = Encoding TkComma

encodeListEmpty :: Encoding
encodeListEmpty = Encoding TkListEmpty

encodeListBegin :: Encoding
encodeListBegin = Encoding TkListBegin

encodeListEnd :: Encoding
encodeListEnd = Encoding TkListEnd

encodeObjectEmpty :: Encoding
encodeObjectEmpty = Encoding TkObjectEmpty

encodeObjectBegin :: Encoding
encodeObjectBegin = Encoding TkObjectBegin

encodeObjectEnd :: Encoding
encodeObjectEnd = Encoding TkObjectEnd
