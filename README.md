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


## Notes

Webkit2gtk implementation seems to be a bit fragile (e.g. try "Inspect element"
or resizing the window, at least on Mint as of writing these notes).

Why to make it work for both ghc and ghcjs? Hopefully the working ghc would mean 
that ghcid can also be used, thus helping the workflow a lot. (And in this way 
there is no need for separate executables thus simplifying cabal a tiny bit. Nor
there is a need for separate main-files.)

The cells-example ("mini-excel") has something waiting for a fix: the first 
keypress works differently than the following ones. (Timer: when it has reached
the end, how to stop updating the DOM?)

Svg-elements and mouse-clicks are not working nicely together (the 
scroll-position seems to affect, can be seen with ghcjs and ghc). With 
`wrapDomEvent` it works ok.


## Installation and building (mostly TBD/WIP-things ATM)


First, 
```
git clone https://github.com/gspia/7guis-reflex-nix.git
```
after which you can:
```
nix-build 
```
or 
```
nix-shell 
nix-shell --argstr compiler ghcjs
nix-build --argstr compiler ghc
```


### TBD/WIP


First, 
```
git clone https://github.com/gspia/7guis-reflex-nix.git
```
after which you have the following four options.

  1) use the work-on -script provided at reflex-platform
  2) nix-build with ghcjs as default compiler
  3) nix-shell with ghc as default compiler
  4) nix-shell with ghcjs 


The first one requires obtaining
[reflex-platform](https://github.com/reflex-frp/reflex-platform).  

The following commands work (items 1,2 and 3 above)
```
path-to-reflex-platform/work-on ghc ./7guis-reflex.nix
nix-build
nix-shell
```
while
```
path-to-reflex-platform/work-on ghcjs ./7guis-reflex.nix
path-to-reflex-platform/work-on ghcjs ./7guis-reflex.nix --argstr "compiler" "ghcjs" --argstr "ghcjs-base" "ghcjs-base"
```
don't. Note that
```
nix-shell --argstr "compiler" "ghcjs" 
```
does work, this is the item 4) above. In this case, in nix-shell, it is 
possible to
```
cabal configure --ghcjs
cabal build
```
and results can be found somewhere from dist-directory.

If using 1), that is, `path-to-reflex-platform/work-on ghc ./7guis-reflex.nix`
then you can start 
```
./dev-server.sh
```
which starts ghcid and auto-updates web page (results) at localhost:8000.
```
cabal configure
cabal build
```
can be used (there are two exes, one is the same that dev-server is 
using, the wai-one, and another using webkit2gtk).

The item 2) makes a result-directory and there you may find
the index.html and js-files.
