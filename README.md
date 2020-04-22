Compact Hexadecimal Interpretive Programming and Operating System
========

CHIPOS is the monitor program and operating system for Michael J. Bauer's 1979 DREAM 6800 computer.

It is designed to run from a 1K EPROM (like the 2708) mapped to memory addresses `C000`–`C3FF`.

If you don't own a DREAM 6800 computer, see my emulator project [DRÖM](https://tobiasvl.github.com/drom) (work in progress) or [MAME](https://mamedev.org).

Build instructions
------------------

Assemble using [AS](http://john.ccac.rwth-aachen.de:8000/as/):

```
as -cpu 6800 CHIPOS68.asm
p2bin -l 0 CHIPOS68.p
```

The CHIPOS code in this repo is also available for download on [Michael J. Bauer's DREAM 6800 website](http://www.mjbauer.biz/DREAM6800.htm). Michael has stated to me in private correspondence that the code is in the public domain.
