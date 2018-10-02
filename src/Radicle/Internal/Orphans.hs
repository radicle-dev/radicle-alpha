{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Orphans () where

import           Codec.Serialise (Serialise(..))
import           Codec.Serialise.Decoding (Decoder)
import           Data.Scientific
import           Prelude (fail)
import           Protolude
import           Text.Megaparsec
                 ( ErrorFancy
                 , ErrorItem
                 , ParseError(..)
                 , Pos
                 , SourcePos
                 , mkPos
                 , unPos
                 )

instance Serialise Scientific where
    encode = encode . (show :: Scientific -> Text)
    decode = decode >>= either fail pure . readEither

instance (Serialise a) => Serialise (ErrorItem a)
instance (Serialise a) => Serialise (ErrorFancy a)
instance Serialise SourcePos
instance Serialise Pos where
    encode = encode . unPos
    decode = mkPos <$> decode

instance {-# OVERLAPPABLE #-} (Serialise a, Ord a, Serialise b, Ord b) => Serialise (ParseError a b)
instance {-# OVERLAPPING #-} (Serialise a, Ord a) => Serialise (ParseError a Void) where
    encode = encode . unvoidParseError
      where
        unvoidParseError x = case x of
            FancyError{} -> panic "impossible"
            _            -> x
    decode = (decode :: Decoder s (ParseError a ())) >>= \case
        TrivialError a b c -> pure $ TrivialError a b c
        _ -> fail "Expecting Trivial Error"
