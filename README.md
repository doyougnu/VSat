# DISCLAIMER
This project is deprecated, and this repository serves only as an archive. See
[this repo](https://github.com/lambda-land/VSat-Papers/tree/master/SPLC2020) for
instructions on running this version of the solver. See [this
repo](https://github.com/doyougnu/VSmt) for next iteration of variational
satisfiability solving.

# What
This project provides variational sat solving using the [Choice
Calculus](http://web.engr.oregonstate.edu/~walkiner/projects/choice-calculus.html)
built on top of the
excellent [Data.SBV](https://hackage.haskell.org/package/sbv-7.5/docs/Data-SBV.html)
library.

# Using the Heroku Server (Recommended for non-haskell users)
If you do not feel confident installing from source and are just trying to use
the tool then you can send your requests to a heroku server I've spun up. Be forewarned that your **first request will take longer than normal** due to the dyno being spun up from a sleep state.
- url: `https://vsatcc.herokuapp.com/`.
- supported solvers: Only `z3` at this time
- available routes: see the available routes section below

# Building from Source
You'll need to install `stack` and the sat solver you want to use, the following
are supported: [z3](https://github.com/Z3Prover/z3/wiki),
[Yices](http://yices.csl.sri.com/), [mathsat](http://mathsat.fbk.eu/),
[boolector](https://boolector.github.io/),
[abc](https://people.eecs.berkeley.edu/~alanmi/abc/),
[cvc4](http://cvc4.cs.stanford.edu/web/). Stack will take care of most of the
installation process by installing a sandboxed `ghc` and the `cabal` build tool
for you. Please refer to the sat solver home pages to install the one you'd like
to use for your OS.

## Installing Stack

### Windows
The Haskell Stack tool provides a 64-bit installer you can find
[here](https://docs.haskellstack.org/en/stable/README/#how-to-install). I'm
avoiding linking to it so that this page stays in sync with the latest stack
version.

### Mac
On any Unix system you can simple run:
```
curl -sSL https://get.haskellstack.org/ | sh
```

The more popular way is just to use homebrew:
```
brew install stack
```

### Linux Distros

#### Ubuntu, Debian
Stack will definitely be in a package that you can grab, although the official
packages tend to be out of data. You'll need to run the following to ensure you
have all required dependencies:

```
sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg
```

and now you can run:

```
sudo apt-get install haskell-stack
```

and now make sure you are on the latest stable release:
```
stack upgrade --binary-only
```

#### CentOS, Fedora

Make sure you have the dependencies:

```
## swap yum for dnf if on older versions (<= 21)
sudo dnf install perl make automake gcc gmp-devel libffi zlib xz tar git gnupg
```

Now install stack

```
## CentOS
dnf install stack

## Fedora
sudo dnf copr enable petersen/stack ## enable the unofficial Copr repo
dnf install stack
```

and now make sure you upgrade stack to latest stable

```
## CentOS and Fedora
stack upgrade
```

#### NixOS
Stack is in `systemPackages`, so you can just add `stack` to that section of
your `configuration.nix` or you can run `nix-env -i stack` if you do things in
an ad-hoc manner. Using `stack` inside of nixOS is slightly more tricky than
non-pure distros. All you'll need to do is edit either the `stack.yaml` file
in the github repo and tell stack you are in a nix environment, like so:

```
## uncomment the following lines to tell stack you are in a nix environment
# nix:
  # enable: true
  # pure: false
  # packages: [ z3, pkgconfig ]
```

Notice that you'll need to pass in the extra packages for the tool. In this case
I'm using `z3` so I need to tell stack to look for it, and `pkgconfig` which you
should almost always pass in.

## Installing Haskell Using Stack
You just need to run the following:
```
stack setup # this will download and install GHC, and a package index
git clone <this-repo> ~/path/you/want/to/build/in && cd /path/you/want/to/build/in
stack build # this will build the exectuable, go get some coffee, trust me
```

## Running the VSMT solver
### Starting the local server
To run the local server you need to build the project and then execute the
binary that results from the build, like so:
```
cd /to/haskell/directory/
stack build                # build the binary
stack exec vsat            # execute the binary
```

on my system this looks like:

```
➜  haskell git:(master) ✗ pwd
/home/doyougnu/Research/VSat/haskell
➜  haskell git:(master) ✗ stack build

Warning: Specified pattern "README.md" for extra-source-files does not match any files
vsat-0.1.0.0: unregistering (local file changes: src/Server.hs)
vsat-0.1.0.0: configure (lib + exe)
Configuring vsat-0.1.0.0...
vsat-0.1.0.0: build (lib + exe)
Preprocessing library for vsat-0.1.0.0..
Building library for vsat-0.1.0.0..
[ 1 of 14] Compiling SAT              ( src/SAT.hs, .stack-work/dist/x86_64-linux-nix/Cabal-2.0.1.0/build/SAT.o )
...
Linking .stack-work/dist/x86_64-linux-nix/Cabal-2.0.1.0/build/vsat/vsat ...

vsat-0.1.0.0: copy/register
Installing library in /home/doyougnu/Research/VSat/haskell/.stack-work/install/x86_64-linux-nix/lts-11.14/8.2.2/lib/x86_64-linux-ghc-8.2.2/vsat-0.1.0.0-Aj9r5QEWrvTKTjvHnt9QFe
Installing executable vsat in /home/doyougnu/Research/VSat/haskell/.stack-work/install/x86_64-linux-nix/lts-11.14/8.2.2/bin
Registering library for vsat-0.1.0.0..

➜  haskell git:(master) ✗ stack exec vsat
Spock is running on port 8080               # server now running on localhost:8080
```

### Available routes
There are only 4 routes available at this time but it is trivially easy to add
more and I will do so upon request (open an issue on the repo). These are:

```
localhost:8080/sat                     # run the VSMT solver with default config
localhost:8080/satWith                 # run solver with custom config
localhost:8080/prove                   # run prover with default config
localhost:8080/proveWith               # run the prover with custom config
```

The default config uses `z3` and turns on the most useful optimizations. These
optimizations are trivially some reordering to maximize sharing in the
variational expressions. You can view them in the `Opts.hs` file. If you want to
customize the configuration see the `Sending a Request` section.

### Sending a Request
To send a request I recommend using a helpful tool like
[postman](https://www.getpostman.com/), you can `cURL` if you really want. In
any case the tool expects an object with two fields, `settings`, and
`proposition` with `settings` being an optional field. I've just used Haskell's
generics to generate the JSON parser so it is tightly coupled to the solver AST,
this is open to change in the future but for right now it is sufficient. Here
are some explicit examples:

```
####### Request to localhost:8080/sat
{"settings":null,"proposition":{"tag":"LitB","contents":true}}

# Response
[
    {
        "model": "[There are no variables bound by the model.]"
    }
]

######## the proposition
a ∨ kejtjbsjshvouk

# expands to in JSON
{
    "tag": "Opn",
    "contents": [
        "Or",
        [
            {
                "tag": "RefB",
                "contents": {
                    "varName": "a"
                }
            },
            {
                "tag": "RefB",
                "contents": {
                    "varName": "kejtjbsjshvouk"
                }
            }
        ]
    ]
}

# Request to localhost:8080/satWith
{
    "settings": {
        "seed": 1234,
        "solver": "Z3",
        "optimizations": [
            "MoveLeft",
            "Shrink"
        ]
    }
    ,"proposition": {
        "tag": "Opn",
        "contents": [
            "Or",
            [
                {
                    "tag": "RefB",
                    "contents": {
                        "varName": "a"
                    }
                },
                {
                    "tag": "RefB",
                    "contents": {
                        "varName": "kejtjbsjshvouk"
                    }
                }
            ]
        ]
    }
}

# Response
[
    {
        "model": "  a              = False :: Bool\n  kejtjbsjshvouk = False :: Bool"
    }
]
```

As you can see these propositions, once expanded in JSON, can get quite large.
Here is a non trivial example, for more examples check the examples folder:

```
# The prop
((-17 > 93.52511917955651) ∧ ((-6 < |pccfjtjnkhfapjwtopwwxym|) ↔ ((DD≺zgmpwfdv , vrkpyxv≻) ∧ bifdhcpwh))) ∧ pevwtpjw

# Expands to
{
    "settings": {
        "seed": 1234,
        "solver": "Z3",
        "optimizations": []
    },
    "proposition": {
        "tag": "Opn",
        "contents": [
            "And",
            [
                {
                    "tag": "Opn",
                    "contents": [
                        "And",
                        [
                            {
                                "tag": "OpIB",
                                "contents": [
                                    "GT",
                                    {
                                        "tag": "LitI",
                                        "contents": {
                                            "tag": "I",
                                            "contents": -17
                                        }
                                    },
                                    {
                                        "tag": "LitI",
                                        "contents": {
                                            "tag": "D",
                                            "contents": 93.52511917955651
                                        }
                                    }
                                ]
                            },
                            {
                                "tag": "OpBB",
                                "contents": [
                                    "BiImpl",
                                    {
                                        "tag": "OpIB",
                                        "contents": [
                                            "LT",
                                            {
                                                "tag": "LitI",
                                                "contents": {
                                                    "tag": "I",
                                                    "contents": -6
                                                }
                                            },
                                            {
                                                "tag": "OpI",
                                                "contents": [
                                                    "Abs",
                                                    {
                                                        "tag": "Ref",
                                                        "contents": [
                                                            "RefI",
                                                            {
                                                                "varName": "pccfjtjnkhfapjwtopwwxym"
                                                            }
                                                        ]
                                                    }
                                                ]
                                            }
                                        ]
                                    },
                                    {
                                        "tag": "Opn",
                                        "contents": [
                                            "And",
                                            [
                                                {
                                                    "tag": "ChcB",
                                                    "contents": [
                                                        {
                                                            "dimName": "DD"
                                                        },
                                                        {
                                                            "tag": "RefB",
                                                            "contents": {
                                                                "varName": "zgmpwfdv"
                                                            }
                                                        },
                                                        {
                                                            "tag": "RefB",
                                                            "contents": {
                                                                "varName": "vrkpyxv"
                                                            }
                                                        }
                                                    ]
                                                },
                                                {
                                                    "tag": "RefB",
                                                    "contents": {
                                                        "varName": "bifdhcpwh"
                                                    }
                                                }
                                            ]
                                        ]
                                    }
                                ]
                            }
                        ]
                    ]
                },
                {
                    "tag": "RefB",
                    "contents": {
                        "varName": "pevwtpjw"
                    }
                }
            ]
        ]
    }
}

# The Response
[
    {
        "isSat": "Unsatisfiable"
    },
    {
        "\"DD\"": {
            "L": null,
            "R": null
        }
    }
]
```

Notice that the response is parameterized by the choice dimension `DD`. The `L`
tag corresponds to setting `DD` to `true`, and the `R` to the `false` branch.

### Available Settings
The last two examples show all possible settings. Setting the seed for the
solver is the only field that can be omitted. If you do not include a mandatory
field then you'll receive a response saying so. For a list of available
optimizations see below, for a list of available solvers see the `building from
source` section above.

### Available Optimizations
The optimizations that are available are given by the `Opts` data type. Order is
important here, in general you want anything that reduces terms to be closer to
the tail of the input list, and anything that shuffles terms to be closer to the
head. Here is a description of each, expect this to change substantially in the
coming weeks:



#### MoveRight
  Moves every choice to the right of any commutative operator. This is should
  _decrease_ run times because it maximizes sharing in the AST.

#### MoveLeft
  Conversely, MoveLeft _minimizes_ sharing because the recursive solving
  algorithm is forced to start with a left fold. This should almost always be
  turned off. I leave it here for testing purposes.

#### Shrink
  This uses basic `C_2` logic equivalences to reduce the size of terms. Things
  like `false /\ __ == false`. This can be a costly optimization if your
  proposition is not likely to have tautologies available for reduction because
  it actually makes trivial calls to the sat solver instead of manipulating the
  AST directly.

#### Atomize
  Atomization is the process of driving variational terms as far down the AST as
  possible. This, taken in conjunction with `toCNF` and `moveLeft` maximizes the
  amount of sharing among plain terms that is possible for any given
  proposition. You can think of this as sorting the AST such that all the
  variational terms are as close to the leftmost leaves as possible, and all
  plain terms are as close to the right most leaves as possible. Thus, when the
  VSMTsolver algorithm solves the prop the assertion stack in the sat solver
  will be maximized with plain terms. The name comes from breaking apart large
  choice expressions into smaller ones via choice distribution laws. This is
  part of defaults and should always be turned on in conjunction with `toCNF`
  and `moveLeft`.

#### CNF
  This manipulates the prop's AST to remove implications, equivalences and
  xor's. It then transforms the prop in [conjuctive normal
  form](https://en.wikipedia.org/wiki/Conjunctive_normal_form). Use this if
  you're going to turn on structural optimizations like `Atomize` and
  `MoveLeft`.

#### Prune
  This inspects choice expressions to look for nested equivalent dimensions, it
  then cleaves off expressions that are impossible to reach. This is part of
  defaults, should almost always be turned on, and should be the last
  optimization in the optimization list passed in such that it is processed
  first by the tool


## Generating JSON and running the tool in a REPL
This following only applies if you have a local build up and running. If you are
unsure about what your particular proposition should look like when encoded or
if you want to view non-trivial examples, then you can pull up a REPL and
experiment with the tool. Simply `cd` to the `haskell` folder and run `stack
repl` like so:

```
~ cd ~/Research/VSat/haskell
➜  haskell git:(master) ✗ stack ghci

Warning: Specified pattern "README.md" for extra-source-files does not match any files
vsat-0.1.0.0: initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: vsat
Using main module: 1. Package `vsat' component exe:vsat with main-is file: /home/doyougnu/Research/VSat/haskell/app/Main.hs
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
...
```

Now you should have a prompt that looks like this:

```
Ok, 14 modules loaded.
Loaded GHCi configuration from /run/user/1729/ghci15604/ghci-script
*Main Api Config Json Opts Run SAT Server Utils V VProp.Core VProp.Gen VProp.SBV VProp.Types>
```

and you can change it to something more user friendly:

```
*Main Api Config Json Opts Run SAT Server Utils V VProp.Core VProp.Gen VProp.SBV VProp.Types> :set prompt "-> "
->
```

Now from the repl you can generate propositions, encode them to JSON, decode
them, or run the tool:

```hs
-- Generating a random proposition
-> generatedProp <- genVProp :: IO (VProp Var Var)
-> generatedProp
(|-23.923303271170415| ≠ 12.278288137198295 - signum DD≺spcpvvdpbme, xzxvyxaauolkvdha≻) ∨ (DD≺|signum ukvjsdf|, jrakfejhhbhfwmdoqlnppmzyjpmhiu - kfhcfaruar + 0≻ ≥ |18| / ¬(-23))

-- encoding it to JSON
-> let x = encodePretty generatedProp
-> :t x
x :: Data.ByteString.Lazy.Internal.ByteString

-- Pretty printing it
-- "B.putStrLn x" will also work
-> pprint x
{
    "tag": "Opn",
    "contents": [
        "Or",
        [
            {
                "tag": "OpIB",
                "contents": [
                    "NEQ",
                    {
                        "tag": "OpI",
                        "contents": [
                            "Abs",
                            {
                                "tag": "LitI",
                                "contents": {
                                    "tag": "D",
                                    "contents": -23.923303271170415
                                }
                            }
                        ]
                    },
                    {
...
...

-- decoding it
-> decode x :: Maybe (VProp Var Var)
Just (|-23.923303271170415| ≠ 12.278288137198295 - signum DD≺spcpvvdpbme, xzxvyxaauolkvdha≻) ∨ (DD≺|signum ukvjsdf|, jrakfejhhbhfwmdoqlnppmzyjpmhiu - kfhcfaruar + 0≻ ≥ |18| / ¬(-23))


- Running the solver directly
--> prove (bimap show show generatedProp)
VChc "DD" (Plain (Just Falsifiable. Counter-example:
  spcpvvdpbme                    =                     NaN :: Double
  xzxvyxaauolkvdha               =                     NaN :: Double
  ukvjsdf                        = 2.2250738585072034e-308 :: Double
  jrakfejhhbhfwmdoqlnppmzyjpmhiu =                       0 :: Int64
  kfhcfaruar                     =                     NaN :: Double)) (Plain (Just Falsifiable. Counter-example:
  spcpvvdpbme                    =                    NaN :: Double
  xzxvyxaauolkvdha               =                    NaN :: Double
  ukvjsdf                        =                    NaN :: Double
  jrakfejhhbhfwmdoqlnppmzyjpmhiu =             -134489152 :: Int64
  kfhcfaruar                     = -1.3003684354267704e10 :: Double))
...
...
```

## Known Issues
### Doubles and modulus lead to non-linear constraints
Be careful with modulus operator and the `Double` numeric type. These can easily
lead to non-linear constraints and consume all the memory available. This
behavior will depend on the solver, and I've only tested with `z3` which will
happily accept and then eventually blow up once it runs out of memory.

## Opening Issues
If you would like to raise an issue with this project please do so on the
[github](https://github.com/doyougnu/VSat). The github is the homepage for this
tool.
