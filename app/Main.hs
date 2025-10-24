{-# LANGUAGE OverloadedStrings #-}
module Main where

import Enigma
import Options.Generic
import Options

main :: IO ()
main = do
  opts <- getRecord "Enigma simulator"
  let (config, pos3) = enigmaOptionsToConfig opts
  contents <- getContents
  putStr $ simpleRunEnigma config pos3 contents
