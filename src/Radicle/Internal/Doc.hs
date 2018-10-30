{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Radicle.Internal.Doc where

import           Protolude hiding (Any)

import           Codec.Serialise (Serialise(..))
import           Data.Copointed (Copointed(..))
import qualified Data.Default as Default
import           Data.Pointed (Pointed(..))
import           Language.Haskell.TH.Quote
import           Text.Pandoc

type Blocks = [Block]
type Inlines = [Inline]

data Pan = Blocks Blocks | Inlines Inlines
  deriving (Eq, Ord, Show, Read)

instance Semigroup Pan where
  Blocks x <> Blocks y = Blocks (x <> y)
  Inlines x <> Inlines y = Inlines (x <> [Space] <> y)
  Blocks x <> Inlines y = Blocks (x <> [Para y])
  Inlines x <> Blocks y = Blocks (Para x : y)

class ToPan a where
  pan :: a -> Pan

instance ToPan Pan where
  pan = identity

blocks :: ToPan a => a -> Blocks
blocks x = case pan x of
  Blocks bs  -> bs
  Inlines is -> [Para is]

instance ToPan Blocks where
  pan = Blocks

instance ToPan a => ToPan (Maybe a) where
  pan Nothing  = Inlines []
  pan (Just x) = pan x

data Described a = Desc (Maybe Pan) a
  deriving (Eq, Ord, Generic, Show, Read)

instance Serialise (Described a) where
  encode = notImplemented
  decode = notImplemented

instance ToPan a => ToPan (Described a) where
  pan (Desc (Just d) x) = d <> pan x
  pan (Desc _ x)        = pan x

data Value
  = Fun Function
  | String
  | Boolean
  | Atom
  | Doc
  | Any
  | Other Pan
  deriving (Eq, Ord, Generic, Show, Read)

instance ToPan Value where
  pan = \case
      Fun f -> pan f
      String -> typ "string"
      Boolean -> typ "boolean"
      Atom -> typ "atom"
      Doc -> typ "doc"
      Any -> typ "any"
      Other pd -> pd
    where

typ :: Text -> Pan
typ t = Inlines [Str "A ", Code nullAttr (toS t), Str "."]

data Function = Function
  { parameters :: Described Params
  , output     :: Described Value
  } deriving (Eq, Ord, Show, Read)

instance ToPan Function where
  pan Function{..} = Blocks $
       blocks (typ "function") <>
       [ BulletList
         [ blocks
             (Inlines [Strong [Str "Parameters:"]] <> pan parameters)
         , blocks
             (Inlines [Strong [Str "Returns:"]] <> pan output)
         ]
       ]

data Params
  = Fixed [Described Param]
  | Variable
  deriving (Eq, Ord, Show, Read)

instance ToPan Params where
  pan Variable   = notImplemented
  pan (Fixed ps) = Blocks [ DefinitionList (definitionItem <$> ps) ]

data Param = Param
  { name  :: Text
  , value :: Value
  } deriving (Eq, Ord, Show, Read)

definitionItem :: Described Param -> ([Inline], [[Block]])
definitionItem (Desc d_ Param{..}) =
  ( [Code nullAttr (toS name)]
  , [blocks (pan d_ <> pan value)]
  )

data Docd a = Docd (Maybe (Described Value)) a
  deriving (Show, Read, Functor, Foldable, Traversable, Generic)

instance Pointed Docd where
  point = Docd Nothing

instance Copointed Docd where
  copoint (Docd _ x) = x

doc :: Docd a -> Maybe (Described Value)
doc (Docd d _) = d

type DocVal = Maybe (Described Value)

instance Serialise a => Serialise (Docd a) where
  encode (Docd _ x) = encode x
  decode = Docd Nothing <$> decode

instance Eq a => Eq (Docd a) where
  Docd _ x == Docd _ y = x == y

instance Ord a => Ord (Docd a) where
  compare (Docd _ x) (Docd _ y) = compare x y

noDocs :: [(a,c)] -> [(a, Maybe b, c)]
noDocs = fmap $ \(x,y) -> (x, Nothing, y)

named :: Text -> Described Value -> Described Value
named name (Desc d_ v) = Desc (n <> d_) v
  where n = Just $ Inlines [Code nullAttr (toS name)]

fun :: Text -> Pan -> Params -> Pan -> Value -> Described Value
fun name desc params yDesc yDoc =
  named name $
    Desc (Just desc) $ Fun $ Function
      (Desc Nothing params)
      (Desc (Just yDesc) yDoc)

funFixed :: Text
         -> Pan
         -> [(Text, Pan, Value)]
         -> Pan
         -> Value
         -> Described Value
funFixed name desc ps yDesc yDoc =
  fun name desc (Fixed (para <$> ps)) yDesc yDoc
  where
    para (xName, xDesc, xDoc) =
      Desc (Just xDesc)
      Param{ name = xName
           , value = xDoc
           }

mdPan :: Text -> Either Text Pan
mdPan t = case runPure (readMarkdown Default.def t) of
  Left e                     -> Left (show e)
  Right (Pandoc _ [Para is]) -> Right $ Inlines is
  Right (Pandoc _ bs)        -> Right $ Blocks bs

toMD :: ToPan d => d -> Either PandocError Text
toMD dv = runPure $
  writeMarkdown Default.def (Pandoc nullMeta (blocks dv))

-- | A quasiquoter that produces 'Pan' from markdown.
md :: QuasiQuoter
md = QuasiQuoter
    { quoteExp = \s -> [| case mdPan s of
        Left e  -> panic $ "Not valid pandoc-markdown: " <> e
        Right p -> p |]
    , quoteType = err
    , quotePat = err
    , quoteDec = err
    }
  where err = panic "pan only works for expressions"
