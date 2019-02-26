# rpr - Pressure Stall Information Display

rpr is a command line client, using ncurses, that displays the current
pressure stall information.

## Pressure Stall?

Pressure Stall Information is a new interface in Linux 4.20 that helps
diagnose overloaded systems by displaying a "pressure" metric for CPU,
Memory, and IO of the computer.

References:

* https://lwn.net/Articles/759658/
* https://lwn.net/Articles/759781/
* https://facebookmicrosites.github.io/psi/docs/overview
* https://major.io/2019/01/27/using-the-pressure-stall-information-interface-in-kernel-4.20/

## Warning

**rpr is in pre-alpha status.**

## Screenshot

![Screenshot](/doc/rpr-screenshot-2019-02-26.png?raw=true "Screenshot")

## TODO

* Warning/error if the linux kernel is too old and does not contain PSI.
* Display load average and # of running processes.
* Colors

## Haskell Advice Welcome

This is the first program I've written in Haskell on my own, after doing
a fair number of katas and exercises.  Any suggestions on cleaning up
the code are welcome.

## Installation

### Prereqs for using

* Linux 4.20 or later
* ncurses

### Binaries

No binaries are available yet.  Coming when this project is more mature.

### Prereqs for Building

* `stack` - The haskell tool stack.
* `ncurses` - development libraries

### Build and Install in Bash
```
stack build
mkdir -p ~/bin
cp `find . -name rpr-exe -type f | grep dist` ~/bin/rpr
```

### Build and Install in Fish

```
stack build
mkdir -p ~/bin
cp (find . -name rpr-exe -type f | grep dist) ~/bin/rpr
```

### Build error re: Ncurses on Arch

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
