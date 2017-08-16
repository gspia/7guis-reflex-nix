# 7guis-reflex-nix

This is based on  [7guis-reflex](https://github.com/themoritz/7guis-reflex)
which, in turn, is another 
[reflex-dom](https://hackage.haskell.org/package/reflex-dom)
implementation of the [7GUIs](https://github.com/eugenkiss/7guis/wiki).

7GUIs demonstrates, how to implement
- a counter
- graph object(s) with undo and redo
- "mini-excel"
- etc.


## Build with GHCJS

Here are the steps shortly:
- Install reflex-platform.
- Get these sources with git clone and cd to it.
- Update the paths to reflex-platform on files ./workOnGHCJS.sh 
  and ./workOnGHC.sh (last lines).


Then to build this with ghcjs

  $ ./workOnGHCJS.sh
  $ cabal configure --ghcjs
  $ cabal build

Open file dist/build/7guis-reflex/7guis-reflex.jsexe/index.html with your 
browser (ctrl-o).

## Build with GHC

  $ ./workOnGHC.sh
  $ cabal configure --builddir=dist-ghc
  $ cabal build --builddir=dist-ghc

Start the program with

  $ dist-ghc/build/7guis-reflex/7guis-reflex


## Notes

Webkit2gtk implementation seems to be a bit fragile (e.g. try "Inspect element"
or resizing the window, at least on Mint (aug 2017)).

Why to make it work for both ghc and ghcjs? Hopefully the working ghc would mean 
that ghcid can also be used, thus helping the workflow a lot. (And in this way 
there is no need for separate executables thus simplifign cabal a tiny bit. Nor
there is a need for separate main-files.)

The cells-example ("mini-excel") has something waiting for a fix: the first 
keypress works differently than the following ones. (Timer: when it has reached
the end, how to stop updating the DOM?)

Svg-elements and mouse-clicks are not working nicely together (the 
scroll-position seems to affect, can be seen with ghcjs and ghc). With 
`wrapDomEvent` it works ok.


## Nix notes

The default.nix was obtained with `cabal2nix`. There are some difficulties
on how the cabal2nix reads cabal-files, which is the reason for two versions
of default.nix-files.

The two shell-scripts are used to copy an appropriate default.nix in place so 
that the `work-on` of reflex-platform would initialize the correct packages 
into nix-shell.

(If it is possible to make conditional default.nix, e.g. by checking somehow 
if the environment is for ghc or ghcjs, we could get rid of the shell scripts. 
Nix-expressions are new to me at the moment (aug 2017) so I don't know yet, how 
to do it. -> things to do then)

There was a compilation problem with file-embed-package with ghc but not with 
ghcjs (this may not be a nix-problem).

About conventions, see:
[ad hoc -env](https://nixos.org/nixpkgs/manual/#how-to-create-ad-hoc-environments-for-nix-shell)
[own pkgs](https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages)

The nix2-directory contains things not working.

