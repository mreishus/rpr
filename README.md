# rpr

## Ncurses on Arch

In order to get ncurses to build on arch, I had to do this workaround:

```
cd /usr/include
mkdir ncursesw
cd ncursesw
ln -s ../ncurses.h ./ncurses.h
ln -s ../panel.h ./panel.h
```

It seems that arch installs the ncurses wide headers in a different place 
than debian etc, and the configure script for haskell ncurses doesn't pick
it up.  This is a quick and dirty hack, there should probably be a better solution.
