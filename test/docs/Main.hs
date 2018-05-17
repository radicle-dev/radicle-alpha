module Main (main) where

import           Hpack.Config
import           System.FilePath.Glob (glob)
import           Test.DocTest

main :: IO ()
main = do
  Right pkg <- fmap decodeResultPackage <$> readPackageConfig defaultDecodeOptions
  let Just exts = sectionDefaultExtensions <$> packageLibrary pkg
  srcs <- glob "src/**/*.hs"
  doctest $ srcs ++ fmap ("-X" ++) exts
