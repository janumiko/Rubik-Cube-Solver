# Haskell-Rubik-Cube-Solver

### Usage

Compile haskell

```
ghc Main.hs
```

Run executable with input file
```
./Main inputfile
```

where input file has format seen below:
```
Down Color Color Color Color Yellow Color Color Color Color
Up Color Color Color Color White Color Color Color Color
Left Color Color Color Color Green Color Color Color Color
Right Color Color Color Color Blue Color Color Color Color
Front Color Color Color Color Red Color Color Color Color
Back Color Color Color Color Orange Color Color Color Color
```

The middle colors must be in positons like above (red - front, up - white, down - yellow).

Order of colors should match one seen in configuration

![configuration](https://github.com/bubuss2/Haskell-Rubik-Cube-Solver-/blob/main/configuration.png)
