module Main (main) where

import           System.FilePath.Glob (glob)
import           Test.DocTest
import Hpack.Config

main :: IO ()
main = do
  Right pkg <- fmap decodeResultPackage <$> readPackageConfig defaultDecodeOptions
  let Just exts = fmap sectionDefaultExtensions $ packageLibrary pkg
  srcs <- glob "src/**/*.hs"
  doctest $ srcs ++ fmap ("-X" ++) exts
