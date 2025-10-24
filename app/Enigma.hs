{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Enigma where
import           Data.Char (isAsciiUpper, toUpper, ord, chr)
import qualified Data.IntSet as IS
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import           Data.Maybe
import           Data.Bifunctor (first)

newtype Letter = Letter { intLetter :: Int } deriving newtype (Show, Eq, Ord, Enum, Num)
newtype Pos    = Pos    { intPos    :: Int } deriving newtype (Show, Eq, Ord, Enum, Num)
newtype Ring   = Ring   { intRing   :: Int } deriving newtype (Show, Eq, Ord, Enum, Num)
newtype Reflector = Reflector { vecReflector :: Vector Int } deriving newtype (Show, Eq)
newtype Plugboard = Plugboard { vecPlugboard :: Vector Int } deriving newtype (Show, Eq)

mod26 :: Int -> Int
mod26 x = let r = x `mod` 26 in if r < 0 then r + 26 else r

(+.) :: Int -> Int -> Int
(+.) a b = mod26 (a + b)

(-.) :: Int -> Int -> Int
(-.) a b = mod26 (a - b)

charToIndex :: Char -> Int
charToIndex = intLetter . fromMaybe (error "bad char") . toLetter

toLetter :: Char -> Maybe Letter
toLetter c | isAsciiUpper u = Just (Letter (ord u - ord 'A'))
           | otherwise      = Nothing
  where u = toUpper c

letterToChar :: Letter -> Char
letterToChar (Letter i) = chr (ord 'A' + mod26 i)

toPos  :: Char -> Maybe Pos
toPos  c = Pos  . intLetter <$> toLetter c

toRing :: Char -> Maybe Ring
toRing c = Ring . intLetter <$> toLetter c

--------------------------------------------------------------------------------
-- 1) Config: immutable wiring data
--------------------------------------------------------------------------------

-- | A rotor: wiring forward & backward, notch positions.
data RotorConfig = RotorConfig
  { fwd   :: Vector Int -- ^ 0..25 -> 0..25
  , rev   :: Vector Int -- ^ inverse mapping
  , notch :: IS.IntSet  -- ^ set of positions that trigger carry
  } deriving (Eq, Show)

-- | Whole-machine constants.
data EnigmaConfig = EnigmaConfig
  { leftRotor  :: RotorConfig
  , midRotor   :: RotorConfig
  , rightRotor :: RotorConfig
  , reflector  :: Reflector
  , plugboard  :: Plugboard
  , removeNonLetters :: Bool
  , spacedOutput     :: Maybe Int
  } deriving (Eq, Show)

defaultEnigmaConfig :: EnigmaConfig
defaultEnigmaConfig = EnigmaConfig
  { leftRotor  = rotorI
  , midRotor   = rotorII
  , rightRotor = rotorIII
  , reflector  = reflB
  , plugboard  = mkPlugboard []
  , removeNonLetters = False
  , spacedOutput     = Nothing
  }

-- Smart constructor from a 26-letter wiring and notch letters.
fromWiringAndNotch :: String -> String -> RotorConfig
fromWiringAndNotch w ns =
  let fw     = V.fromList (map charToIndex w)
      rv     = V.generate (V.length fw) (\i -> fromJust $ V.findIndex (== i) fw)
      notchs = IS.fromList (map charToIndex ns)
  in  RotorConfig fw rv notchs

-- | Historical rotors / reflectors for Enigma I.
-- https://en.wikipedia.org/wiki/Enigma_rotor_details
rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII :: RotorConfig
rotorI    = fromWiringAndNotch "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Q"
rotorII   = fromWiringAndNotch "AJDKSIRUXBLHWTMCQGZNPYFVOE" "E"
rotorIII  = fromWiringAndNotch "BDFHJLCPRTXVZNYEIWGAKMUSQO" "V"
rotorIV   = fromWiringAndNotch "ESOVPZJAYQUIRHXLNFTGKDCMWB" "J"
rotorV    = fromWiringAndNotch "VZBRGITYUPSDNHLXAWMJQOFECK" "Z"
rotorVI   = fromWiringAndNotch "JPGVOUMFYQBENHZRDKASXLICTW" "ZM"
rotorVII  = fromWiringAndNotch "NZJHGRCXMYSWBOUFAIVLPEKQDT" "ZM"
rotorVIII = fromWiringAndNotch "FKQHTLXOCBJSPDZRAMEWNIUYGV" "ZM"

reflA, reflB, reflC, reflBThin, reflCThin :: Reflector
reflA     = reflectorFromString "EJMZALYXVBWFCRQUONTSPIKHGD"
reflB     = reflectorFromString "YRUHQSLDPXNGOKMIEBFZCWVJAT"
reflC     = reflectorFromString "FVPJIAOYEDRZXWGCTKUQSBNMHL"
reflBThin = reflectorFromString "ENKQAUYWJICOPBLMDXZVFTHRGS"
reflCThin = reflectorFromString "RDOBJNTKVEHMLFCWZAXGYIPSUQ"

reflectorFromString :: String -> Reflector
reflectorFromString = Reflector . V.fromList . map charToIndex

-- | Plugboard as a total 26-map (Vector Int), symmetric by construction.
mkPlugboard :: [(Char, Char)] -> Plugboard
mkPlugboard pairs = Plugboard $ V.generate 26 id V.// ixy V.// [ (b, a) | (a, b) <- ixy ]
  where ixy = [ (charToIndex c1, charToIndex c2) | (c1, c2) <- pairs ]

--------------------------------------------------------------------------------
-- 2) State: evolving positions/rings (threaded purely)
--------------------------------------------------------------------------------

data RotorState = RotorState
  { pos  :: !Pos
  , ring :: !Ring
  } deriving (Eq, Show)

data EnigmaState = EnigmaState
  { leftS  :: !RotorState
  , midS   :: !RotorState
  , rightS :: !RotorState
  , count  :: !Int
  } deriving (Eq, Show)

defaultRotorState :: RotorState
defaultRotorState = RotorState (Pos 0) (Ring 0)

defaultEnigmaState :: EnigmaState
defaultEnigmaState = EnigmaState defaultRotorState defaultRotorState defaultRotorState 0

setPositions :: (Char, Char, Char) -> EnigmaState -> EnigmaState
setPositions (l, m, r) st@EnigmaState{..} =
  st { leftS  = leftS  { pos = fromMaybe (err 'L') (toPos l) }
     , midS   = midS   { pos = fromMaybe (err 'M') (toPos m) }
     , rightS = rightS { pos = fromMaybe (err 'R') (toPos r) }
     }
  where err w = error ("Bad position for " <> [w])

setRings :: (Char, Char, Char) -> EnigmaState -> EnigmaState
setRings (l, m, r) st@EnigmaState{..} =
  st { leftS  = leftS  { ring = fromMaybe (err 'L') (toRing l) }
     , midS   = midS   { ring = fromMaybe (err 'M') (toRing m) }
     , rightS = rightS { ring = fromMaybe (err 'R') (toRing r) }
     }
  where err w = error ("Bad ring for " <> [w])

--------------------------------------------------------------------------------
-- 3) Core signal path (config + state -> new state + output)
--------------------------------------------------------------------------------

-- Effective shift k = pos - ring, then conjugate wiring by k.
throughFow :: RotorConfig -> RotorState -> Letter -> Letter
throughFow RotorConfig{..} RotorState{..} x =
  let k  = intPos pos - intRing ring
      y0 = intLetter x +. k
      y1 = fwd ! y0
  in  Letter (y1 -. k)

throughRev :: RotorConfig -> RotorState -> Letter -> Letter
throughRev RotorConfig{..} RotorState{..} x =
  let k  = intPos pos - intRing ring
      y0 = intLetter x +. k
      y1 = rev ! y0
  in  Letter (y1 -. k)

throughPlugboard :: Plugboard -> Letter -> Letter
throughPlugboard (Plugboard pb) x = Letter (pb ! intLetter x)

throughReflector :: Reflector -> Letter -> Letter
throughReflector (Reflector rf) x = Letter (rf ! intLetter x)

atNotch :: RotorConfig -> RotorState -> Bool
atNotch RotorConfig{..} RotorState{..} = IS.member (intPos pos) notch

stepRotor :: RotorState -> RotorState
stepRotor s = s { pos = Pos (intPos (pos s) +. 1) }

advance :: EnigmaConfig -> EnigmaState -> EnigmaState
advance EnigmaConfig{..} st@EnigmaState{..} =
  let stepL = atNotch midRotor   midS
      stepM = atNotch rightRotor rightS || atNotch midRotor midS
      l'    = if stepL then stepRotor leftS  else leftS
      m'    = if stepM then stepRotor midS   else midS
      r'    = stepRotor rightS
  in  st { leftS = l', midS = m', rightS = r' }

incCount :: EnigmaState -> EnigmaState
incCount st = st { count = count st + 1 }

enigmaStep :: EnigmaConfig -> EnigmaState -> Char -> (EnigmaState, Maybe Char)
enigmaStep cfg@EnigmaConfig{..} st c =
  case toLetter c of
    Nothing -> if removeNonLetters then (st, Nothing) else (st, Just c) -- pass through non-letters without advancing
    Just x0 ->
      let st1 = advance cfg st
          EnigmaState{..} = st1
          x1 = throughPlugboard plugboard           x0
          x2 = throughFow       rightRotor  rightS  x1
          x3 = throughFow       midRotor    midS    x2
          x4 = throughFow       leftRotor   leftS   x3
          x5 = throughReflector reflector           x4
          x6 = throughRev       leftRotor   leftS   x5
          x7 = throughRev       midRotor    midS    x6
          x8 = throughRev       rightRotor  rightS  x7
          x9 = throughPlugboard plugboard           x8
      in (st1, Just $ letterToChar x9)

runEnigma :: EnigmaConfig -> EnigmaState -> String -> (EnigmaState, String)
runEnigma cfg = go
  where
    go st []     = (st, [])
    go st (x:xs) =
      let (st' , my) = first incCount $ enigmaStep cfg st x
          (st'', ys) = go st' xs
      in (st'', spacedAppend st'.count my ys)
    spacedAppend cnt my ys = case (my, spacedOutput cfg) of
      (Nothing, _       ) -> ys
      (Just y, Nothing)   -> y : ys
      (Just y, Just n )   -> y :
        if cnt `mod` n == 0
        then ' ' : ys
        else       ys

simpleRunEnigma :: EnigmaConfig -> (Char, Char, Char) -> String -> String
simpleRunEnigma cfg (p0, p1, p2) input =
  let state0 = setRings     ('A','A','A')
             . setPositions (p0, p1, p2)
             $ defaultEnigmaState
      (_, out) = runEnigma cfg state0 input
  in out
