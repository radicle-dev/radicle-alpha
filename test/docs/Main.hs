module Main (main) where

import           Protolude

import           Hpack.Config
import           System.FilePath.Glob (glob)
import           Test.DocTest

main :: IO ()
main = do
  Right pkg <- fmap decodeResultPackage <$> readPackageConfig defaultDecodeOptions
  exts <- case sectionDefaultExtensions <$> packageLibrary pkg of
      Just es -> pure es
      Nothing -> die "No default-extensions in package.yaml"
  srcs <- glob "src/**/*.hs"
  doctest $ srcs ++ fmap ("-X" ++) exts
