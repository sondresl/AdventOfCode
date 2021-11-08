{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import Data.Aeson.Lens
import Data.Aeson
import Control.Lens

main :: IO ()
main = do
  input <- (read :: String -> Object) <$> readFile "../data/12.in"
  print input
