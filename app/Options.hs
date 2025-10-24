{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass, DataKinds, TypeOperators, DuplicateRecordFields, OverloadedRecordDot #-}
module Options where

import Enigma
import Options.Generic
import qualified Options.Applicative          as Options
import qualified Options.Applicative.Types    as Options

data EnigmaOptions = EnigmaOptions
  { pos         :: (Char, Char, Char)    <?> "Starting positions for the rotors (e.g. AAA)"
  , plugboard   :: [(Char, Char)]        <?> "Plugboard settings as pairs of characters (e.g. A-B C-D)"
  , leftRotor   :: Maybe EnigmaRotor     <?> "Set the left rotor"
  , midRotor    :: Maybe EnigmaRotor     <?> "Set the middle rotor"
  , rightRotor  :: Maybe EnigmaRotor     <?> "Set the right rotor"
  , reflector   :: Maybe EnigmaReflector <?> "Set the reflector"
  } deriving (Generic, Show)

instance ParseField (Char, Char, Char) where
  metavar _ = "<3 CHAR STRING>"
  readField = do
    s <- Options.readerAsk
    case s of
      [a, b, c] -> return (a, b, c)
      _         -> Options.readerAbort (Options.ShowHelpText (Just "Must be 3 characters e.g. AAA"))
instance ParseFields (Char, Char, Char)

instance ParseField (Char, Char) where
  metavar _ = "<2 CHAR STRING>"
  readField = do
    s <- Options.readerAsk
    case s of
      [a, b] -> return (a, b)
      _      -> Options.readerAbort (Options.ShowHelpText (Just "Must be 2 characters e.g. AB"))
instance ParseRecord EnigmaOptions

data EnigmaRotor
  = RotorI | RotorII | RotorIII | RotorIV | RotorV | RotorVI | RotorVII | RotorVIII
  | CustomRotor String String
  deriving (Generic, Show, Read, ParseField)

toRotor :: EnigmaRotor -> RotorConfig
toRotor RotorI    = rotorI
toRotor RotorII   = rotorII
toRotor RotorIII  = rotorIII
toRotor RotorIV   = rotorIV
toRotor RotorV    = rotorV
toRotor RotorVI   = rotorVI
toRotor RotorVII  = rotorVII
toRotor RotorVIII = rotorVIII
toRotor (CustomRotor wiring notches) = fromWiringAndNotch wiring notches

data EnigmaReflector
  = ReflectorA
  | ReflectorB
  | ReflectorC
  | ReflectorBThin
  | ReflectorCThin
  | CustomReflector String
  deriving (Generic, Show, Read, ParseField)

toReflector :: EnigmaReflector -> Reflector
toReflector ReflectorA     = reflA
toReflector ReflectorB     = reflB
toReflector ReflectorC     = reflC
toReflector ReflectorBThin = reflBThin
toReflector ReflectorCThin = reflCThin
toReflector (CustomReflector wiring) = reflectorFromString wiring

enigmaOptionsToConfig :: EnigmaOptions -> (EnigmaConfig, (Char, Char, Char))
enigmaOptionsToConfig opts =
  ( EnigmaConfig
      { leftRotor  = maybe rotorI   toRotor  opts.leftRotor.unHelpful
      , midRotor   = maybe rotorII  toRotor  opts.midRotor.unHelpful
      , rightRotor = maybe rotorIII toRotor  opts.rightRotor.unHelpful
      , reflector  = maybe reflB toReflector opts.reflector.unHelpful
      , plugboard  = mkPlugboard opts.plugboard.unHelpful
      }
  , opts.pos.unHelpful
  )
