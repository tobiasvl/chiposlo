Compact Hexadecimal Interpretive Programming and Operating System with Logical Operators (CHIPOSLO)
========

This is a version of the monitor program and operating system <acronym title="Compact Hexadecimal Interpretive Programming and Operating System">CHIPOS</acronym> for [Michael J. Bauer's 1979 <acronym title="Domestic Recreational Educational and Adaptive Microcomputer">DREAM</acronym> 6800 computer](http://www.mjbauer.biz/DREAM6800.htm), with added support for four CHIP-8 instructions that appeared in other CHIP-8 interpreters.

It is designed to run from a 1K EPROM (like the 2708) mapped to memory addresses `C000`–`C3FF`.

Replacing CHIPOS with CHIPOSLO will allow you to run more of the original CHIP-8 programs for the COSMAC VIP computer on your DREAM, or even more [modern CHIP-8 programs](https://johnearnest.github.io/chip8Archive/?sort=platform) written in the decades since.

If you don't own a DREAM 6800 computer, see my emulator project [DRÖM](https://github.com/tobiasvl/drom) or [MAME](https://mamedev.org).

Features
--------

* 1024 bytes, fits on a 2708 EPROM (like CHIPOS)
* Adds four new logical/arithmetic instructions to your DREAM 6800's CHIP-8 interpreter
* Lets you run more programs and games from the vast, historical CHIP-8 software library
* Almost completely compatible[*](#compatibility-notes) with CHIPOS!

Background
----------

CHIPOS was a monitor program and operating system for the 1979 microcomputer DREAM 6800, written by Michael J. Bauer. It came with an interpreter for the CHIP-8 language, which was originally created by RCA's Joe Weisbecker for the COSMAC VIP computer in 1977.

Weisbecker's original CHIP-8 interpreter for the VIP supported four undocumented arithmetic/logic instructions:

* `8XY3`: `VX = VX XOR VY`
* `8XY6`: `VX = VY >> 1`
* `8XY7`: `VX = VY - VX`
* `8XYE`: `VX = VY << 1`

(You can read more about why these instructions worked in [Laurence Scotford's disassembly of the VIP's CHIP-8 interpreter](https://laurencescotford.com/chip-8-on-the-cosmac-vip-arithmetic-and-logic-instructions/).)

These instructions were discovered around 1978 and detailed in the COSMAC VIP's newsletter, [VIPER issue #2](https://archive.org/details/viper_1_02/page/n2/mode/1up). Bauer was not aware of them, and the interpreter that came with his CHIPOS did not include them.

Most CHIP-8 interpreters since, including CHIP-48 and SUPER-CHIP from 1990/1991, support these extra instructions (although they often introduced other incompatibilities with the original VIP implementation). Now they're finally available on the DREAM as well!

Build instructions
------------------

Assemble using [AS](http://john.ccac.rwth-aachen.de:8000/as/):

```
as -cpu 6800 CHIPOSLO.asm
p2bin -l 0 CHIPOSLO.p
```

The code is a modified version of the CHIPOS code available for download on [Michael J. Bauer's DREAM 6800 website](http://www.mjbauer.biz/DREAM6800.htm). Michael J. Bauer has graciously related to me in private correspondence that the code is in the public domain.

The commit history in this repository details the changes I've made (the initial commit contains [the original CHIPOS code](CHIPOS_asm68_listing.pdf)).


[foo](chipos_manual.pdf)

Compatibility notes
-------------------

All the software details in the [CHIPOS software manual](chipos_manual.pdf) still apply, and the subroutines in the ["CHIPOS SUBROUTINES (& Calling Sequences)" manual](http://www.mjbauer.biz/CHIPOS_calls.pdf) are still located at the same addresses, so any programs that call them should hopefully still work.

However, some of the scratchpad parameter addresses listed at the bottom of the latter manual have moved to compress some code. These are:

* `0040`–`0045` are reserved as scratch (used to construct a temporary logic subroutine)
* `DDPAT` has moved from `0008` to `0050` (in order to let `0A` pull double duty as temporary VX storage and the opcode for `CLV` as a no-op)
* `RNDX` has moved from `002C` to `0047` (in order to let `RNDX+1` pull double duty as the opcode for `ASLA`)

This might affect programs that do any of the following:

* Use any memory in the range `0047` through `0055`
* Initializes `RNDX` as optional input to the `RANDOM` subroutine at `C132`
* Call `LETDSP` and expect the resulting hex digit to be output at `0008` rather than `0047`

I am not aware of any affected programs, but if you know of any, please [create an issue](https://github.com/tobiasvl/chiposlo/issues/new). I want CHIPOSLO to be as compatible as possible.
