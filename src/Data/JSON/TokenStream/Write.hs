{-# LANGUAGE BangPatterns #-}
module Data.JSON.TokenStream.Write
    ( toBuilder
    , toLazyByteString
    , toStrictByteString
    )
    where

import           Data.Char                    (ord)
import           Data.Word

import           Data.ByteString              (ByteString)
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder      as B
import           Data.ByteString.Builder.Prim ((>$<), (>*<))
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy         as BL

import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text

import           Data.Monoid                  ((<>))

import           Data.JSON.TokenStream.Token

toLazyByteString :: Encoding -> BL.ByteString
toLazyByteString = B.toLazyByteString . toBuilder

toStrictByteString :: Encoding -> ByteString
toStrictByteString = BL.toStrict . B.toLazyByteString . toBuilder

toBuilder :: Encoding -> Builder
toBuilder = \(Encoding stream) -> step mempty (stream TkEnd)
   where
    step !b !tok = case tok of
      TkInt !i s -> step (b <> B.intDec i) s
      TkDouble !d s -> step (b <> B.doubleDec d) s
      TkString !t s -> step (b <> string t) s
      TkText   !t s -> step (b <> text t) s
      TkBool !v s -> step (b <> bool v) s
      TkKey  !v s -> step (b <> text v <> colon) s
      TkKeyString  !v s -> step (b <> string v <> colon) s
      TkNull s -> step (b <> null_) s
      TkComma s -> step (b <> comma) s
      TkListEmpty s -> step (b <> BP.primBounded (ascii2 ('[',']')) ()) s
      TkListBegin s -> step (b <> B.char8 '[') s
      TkListEnd s -> step (b <> B.char8 ']') s
      TkObjectEmpty s -> step (b <> BP.primBounded (ascii2 ('{','}')) ()) s
      TkObjectBegin s -> step (b <> B.char8 '{') s
      TkObjectEnd s -> step (b <> B.char8 '}') s
      TkEnd -> b


colon :: Builder
colon = B.char8 ':'

comma :: Builder
comma = B.char8 ','

null_ :: Builder
null_ = BP.primBounded (ascii4 ('n',('u',('l','l')))) ()

-- | Encode a JSON boolean.
bool :: Bool -> Builder
bool = BP.primBounded (BP.condB id (ascii4 ('t',('r',('u','e'))))
                                   (ascii5 ('f',('a',('l',('s','e'))))))

-- | Encode a JSON string.
string :: String -> Builder
string t = B.char8 '"' <> BP.primMapListBounded go t <> B.char8 '"'
  where go = BP.condB (> '\x7f') BP.charUtf8 (c2w >$< escapeAscii)

-- | Encode a JSON string.
text :: Text -> Builder
text t = B.char8 '"' <> unquoted t <> B.char8 '"'

-- | Encode a JSON string, without enclosing quotes.
unquoted :: Text -> Builder
unquoted = Text.encodeUtf8BuilderEscaped escapeAscii

escapeAscii :: BP.BoundedPrim Word8
escapeAscii =
    BP.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
    BP.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
    BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
    BP.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
    BP.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
    BP.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
    (BP.liftFixedToBounded hexEscape) -- fallback for chars < 0x20
  where
    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 >*< BP.char8 >*< BP.word16HexFixed
{-# INLINE escapeAscii #-}

c2w :: Char -> Word8
c2w c = fromIntegral (ord c)

ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ (const cs) BP.>$< BP.char7 >*< BP.char7
{-# INLINE ascii2 #-}

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii5 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ (const cs) >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}
