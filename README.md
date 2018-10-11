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


## Installation and building 

First, get the repo with `git clone` and `cd` into the directory, and after that make sure that the reflex-platform is in place:

```
git submodule update --init --recursive
```

To build with GHC, use the `nix-shell` command to enter the sandbox shell and use cabal (which is supplied by the sandbox):

```
nix-shell -A shells.ghc
cabal new-build all
```

To build with GHCJS:

```
nix-shell -A shells.ghcjs
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

For further information, see the following
- [project-development documentation](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
- [blanket project derivation (default.nix)](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix)
- [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton)

Note that if you have already obtained repo but want to update the 
reflex-platform, you can try, e.g.,

```
git submodule foreach "(git checkout develop; git pull --recurse-submodules)&"
```

(Note that the above command gets the develop-branch of the platform.)

