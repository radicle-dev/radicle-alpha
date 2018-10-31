{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Radicle.Internal.Doc where

import           Protolude hiding (Any)

import           Codec.Serialise (Serialise(..))
import           Data.Copointed (Copointed(..))
import qualified Data.Default as Default
import           Language.Haskell.TH.Quote
import           Text.Pandoc

data Docd a = Docd (Maybe Text) a
  deriving (Show, Read, Functor, Foldable, Traversable, Generic)

instance Serialise a => Serialise (Docd a)

instance Copointed Docd where
  copoint (Docd _ x) = x

doc :: Docd a -> Maybe Text
doc (Docd d _) = d

instance Eq a => Eq (Docd a) where
  Docd _ x == Docd _ y = x == y

instance Ord a => Ord (Docd a) where
  compare (Docd _ x) (Docd _ y) = compare x y

noDocs :: [(a, c)] -> [(a, Maybe b, c)]
noDocs = fmap $ \(x,y) -> (x, Nothing, y)

-- | A quasiquoter that checks if a string is valid pandoc-markdown, then
-- returns the markdown with better formatting.
md :: QuasiQuoter
md = QuasiQuoter
    { quoteExp = \s -> [| case checkPandocMd s of
        Left e  -> panic $ "Not valid pandoc-markdown: " <> show e
        Right s' -> s' |]
    , quoteType = err
    , quotePat = err
    , quoteDec = err
    }
  where
    err = panic "pan only works for expressions"

checkPandocMd :: Text -> Either PandocError Text
checkPandocMd s = runPure $ do
  p <- readMarkdown Default.def s
  writeMarkdown Default.def p
