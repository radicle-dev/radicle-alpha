{-# LANGUAGE DuplicateRecordFields #-}

module Radicle.Internal.Doc where

import           Protolude

import           Codec.Serialise (Serialise(..))
import           Data.Copointed (Copointed(..))
import qualified Data.Default as Default
import           Data.Pointed (Pointed(..))
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
  pan (Desc _ x)         = pan x

data Value
  = Fun Function
  | String
  | Boolean
  | Other Pan
  deriving (Eq, Ord, Generic, Show, Read)

instance ToPan Value where
  pan = \case
      Fun f -> pan f
      String -> typ "string"
      Boolean -> typ "boolean"
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
         [ blocks (Inlines [Str "Parameters:"] <> pan parameters)
         , blocks (Inlines [Str "Returns:"] <> pan output)
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

fun1 :: Text
     -> Text
     -> Text
     -> Text
     -> Value
     -> Text
     -> Value
     -> Maybe (Described Value)
fun1 name ds_ xDesc xName xDoc yDesc yDoc = do
  ds <- md ds_
  let n = Blocks [Para [Code nullAttr (toS name)]]
  pure $
    Desc (Just (n <> ds)) $ Fun $ Function
      (Desc Nothing
            (Fixed [Desc (md xDesc)
                         Param{ name = xName
                              , value = xDoc}]))
      (Desc (md yDesc) yDoc)

md :: Text -> Maybe Pan
md t = case runPure (readMarkdown Default.def t) of
  Left _                     -> Nothing
  Right (Pandoc _ [Para is]) -> Just (Inlines is)
  Right (Pandoc _ bs)        -> Just (Blocks bs)

toMD :: ToPan d => d -> Either PandocError Text
toMD dv = runPure $
  writeMarkdown Default.def (Pandoc nullMeta (blocks dv))
