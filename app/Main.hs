{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
module Main where

import Enigma
import Options.Generic
import Options

main :: IO ()
main = do
  opts <- getRecord "Enigma simulator"
  let (config, pos3, ring3) = enigmaOptionsToConfig opts
  if opts.streamly.unHelpful
  then putChars $ simpleRunEnigmaStream config ring3 pos3 readChars
  else do
    contents <- getContents
    putStr $ simpleRunEnigma config ring3 pos3 contents
