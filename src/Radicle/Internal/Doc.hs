{-# LANGUAGE DuplicateRecordFields #-}

module Radicle.Internal.Doc where

import           Protolude

import           Codec.Serialise (Serialise(..))
import           Data.Copointed (Copointed(..))
import qualified Data.Default as Default
import           Data.Pointed (Pointed(..))
import           Text.Pandoc

type Blocks = [Block]

class ToBlocks a where
  blocks :: a -> Blocks

instance ToBlocks Blocks where
  blocks = identity

instance ToBlocks a => ToBlocks (Maybe a) where
  blocks Nothing = []
  blocks (Just x) = blocks x

data Described a = Desc (Maybe Blocks) a
  deriving (Eq, Ord, Generic, Show, Read)

instance Serialise (Described a) where
  encode = notImplemented
  decode = notImplemented

instance ToBlocks a => ToBlocks (Described a) where
  blocks (Desc (Just bs) x) = blocks x <> bs
  blocks (Desc _ x)         = blocks x

data Value
  = Fun Function
  | String
  | Boolean
  | Other Blocks
  deriving (Eq, Ord, Generic, Show, Read)

instance ToBlocks Value where
  blocks = \case
      Fun f -> blocks f
      String -> ty "A string."
      Boolean -> ty "A boolean."
      Other pd -> pd
    where
      ty t = [Plain [Str t]]

data Function = Function
  { parameters :: Described Params
  , output     :: Described Value
  } deriving (Eq, Ord, Show, Read)

instance ToBlocks Function where
  blocks Function{..} =
       [ Para [Str "A Function:"]
       , BulletList
         [ Para [Str "Parameters:"] : blocks parameters
         , Para [Str "Returns:"] : blocks output
         ]
       ]

data Params
  = Fixed [Described Param]
  | Variable
  deriving (Eq, Ord, Show, Read)

instance ToBlocks Params where
  blocks Variable = [] -- TODO
  blocks (Fixed ps) = [ DefinitionList (definitionItem <$> ps) ]

data Param = Param
  { name  :: Text
  , value :: Value
  } deriving (Eq, Ord, Show, Read)

definitionItem :: Described Param -> ([Inline], [[Block]])
definitionItem (Desc d_ Param{..}) =
  ( [Code nullAttr (toS name)]
  , [blocks value, blocks d_]
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
  let n = [Para [Str (toS name)]]
  pure $
    Desc (Just (n <> ds)) $ Fun $ Function
      (Desc Nothing
            (Fixed [Desc (md xDesc)
                         Param{ name = xName
                              , value = xDoc}]))
      (Desc (md yDesc) yDoc)

md :: Text -> Maybe Blocks
md t = case runPure (readMarkdown Default.def t) of
  Left _              -> Nothing
  Right (Pandoc _ bs) -> Just bs

toMD :: ToBlocks d => d -> Either PandocError Text
toMD dv = runPure $
  writeMarkdown Default.def (Pandoc nullMeta (blocks dv))
