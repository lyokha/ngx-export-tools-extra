##### Building and installation

The build tool requires Cabal, [*patchelf*](https://github.com/NixOS/patchelf),
and utility *nhm-tool* which is shipped with package
[*ngx-export-distribution*](https://hackage.haskell.org/package/ngx-export-distribution).

Before building the project, tune the *constraints* stanza in
[*cabal.project*](cabal.project). Currently, it should look similar to

```Cabal Config
constraints: ngx-export-tools-extra +snapaggregateserver
```

This line enforces building the Snap aggregate server. To disable this,
replace *+snapaggregateserver* by *-snapaggregateserver*. To let Cabal deduce
whether to build Snap automatically, remove the constraint.

Now run

```ShellSession
$ make
$ sudo make install
```

With ghc older than *9.0.1*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

By default, package *ngx-export-tools-extra* gets installed from *Hackage*. To
build it locally, augment stanza *packages* inside
[*cabal.project*](cabal.project) according to the commentary attached to it.

