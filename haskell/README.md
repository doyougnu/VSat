# What
This project provides variational sat solving using the [Choice
Calculus](http://web.engr.oregonstate.edu/~walkiner/projects/choice-calculus.html)
built on top of the
excellent[Data.SBV](https://hackage.haskell.org/package/sbv-7.5/docs/Data-SBV.html)
library.

# Building from Source (The only method currently)
You'll need to install `haskell`, `stack` and the sat solver you want to use, the following are supported: [z3](https://github.com/Z3Prover/z3/wiki), [Yices](http://yices.csl.sri.com/), [mathsat](http://mathsat.fbk.eu/), [boolector](https://boolector.github.io/), [abc](https://people.eecs.berkeley.edu/~alanmi/abc/), [cvc4](http://cvc4.cs.stanford.edu/web/).

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
an ad-hoc manner.
