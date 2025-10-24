## Enigma simulator cli in Haskell

Play with Enigma machine from your terminal!

```bash
enigma --pos AAA
```

Reads from stdin and encodes/decodes it using Enigma machine with initial rotor positions set to `AAA`. Example usage:

```bash
enigma --pos FGB
# and type input text followed by Enter
```

```bash
echo "hello world" | enigma --pos AAA

cat input.txt | enigma --pos XYZ > output.txt

cat input.txt | enigma --pos EFG --removeNonLetters --spaced 5

cat input.txt | enigma --pos VER | enigma --pos YLO | enigma --pos NGK | enigma --pos EYS
# chaining multiple enigma commands to increase security (and use a longer key)
# Note that in this case decoding must be done in reverse order
```

Optional configuration flags:

```plaintext
--leftRotor <rotor>
--middleRotor <rotor>
--rightRotor <rotor>
  where <rotor> can be RotorI, RotorII, RotorIII, RotorIV, RotorV, RotorVI, RotorVII, RotorVIII, or "CustomRotor \"<wiring>\" \"<notchPositions>\""

--reflector <reflector>
  where <reflector> can be ReflectorA, ReflectorB, ReflectorC, ReflectorBThin, ReflectorCThin, or "CustomReflector \"<wiring>\""

--plugboard <connections>
  where <connections> is a 2 character string representing a pair of letters to be swapped, multiple connections can be provided by multiple --plugboard flags, e.g. --plugboard AB --plugboard CD

--removeNonLetters
  removes non-letter characters from input
--spaced Int
  outputs a space every Int characters, useful in conjunction with --removeNonLetters
```

## Build From Source

Install haskell tools from ghcup (recommended) or just install GHC and Cabal.

See link [GHCup installation](https://www.haskell.org/ghcup/)

```bash
git clone https://github.com/Eiko-Tokura/Enigma.git
cd Enigma
cabal build
# the build file lives in ./dist-newstyle
```

Or use `cabal install` to install the executable to your local path:

```bash
git clone https://github.com/Eiko-Tokura/Enigma.git
cd Enigma
cabal install
```

## Performance

I tested it on 10000000 random letters input (9.5MB file), it runs in `2.1s` on my lunar lake laptop, with constant memory usage around `12MB`. The performance seems very decent (though not meaningful XD).
