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
echo "HELLO WORLD" | enigma --pos AAA

cat input.txt | enigma --pos XYZ > output.txt
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
