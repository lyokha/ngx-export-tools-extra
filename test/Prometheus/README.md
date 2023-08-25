##### Building and installation

The build tool requires Cabal, [*patchelf*](https://github.com/NixOS/patchelf),
and utility *nhm-tool* which is shipped with package *ngx-export-distribution*.

```ShellSession
$ make
$ sudo make install
```

With ghc older than *8.10.6*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

By default, package *ngx-export-tools-extra* gets installed from *Hackage*. To
build it locally, augment stanza *packages* inside
[*cabal.project*](cabal.project) according to the commentary attached to it.

