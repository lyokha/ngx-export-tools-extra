##### Building and installation

The build tool requires Cabal, [*patchelf*](https://github.com/NixOS/patchelf),
and utility *nhm-tool* which is shipped with package
[*ngx-export-distribution*](https://hackage.haskell.org/package/ngx-export-distribution).

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

##### Testing with dnsmasq

This works in *Fedora 40*, may require some tweaks in other Linux distributions.

Before testing, backup files */etc/resolv.conf* and */etc/dnsmasq.conf*.

Put name server *127.0.0.1* into */etc/resolv.conf*.

```ShellSession
$ sudo unlink /etc/resolv.conf
$ sudo echo nameserver 127.0.0.1 > /etc/resolv.conf
```

Copy *dnsmasq.conf* from this directory into */etc/dnsmasq.conf* and (re)start
service *dnsmasq*.

```ShellSession
$ sudo cp dnsmasq.conf /etc/dnsmasq.conf
$ sudo systemctl restart dnsmasq
```

Note that *dnsmasq.conf* contains line *server=8.8.8.8* to not lose ability to
resolve names in the system while running tests. Remove it if not needed.

Run tests with *nginx.conf* and then restore */etc/resolv.conf* and
*/etc/dnsmasq.conf* from the backups.

