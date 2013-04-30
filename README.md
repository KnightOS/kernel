# KnightOS Kernel

This is the kernel that powers KnightOS. It draws inspiration from Unix and offers multitasking, a tree-based
filesystem, and more, for Texas Instruments calculators. The kernel is written entirely z80 assembly.

## Usage

To use this kernel, you need the following things installed:

* make
* Mono (or Microsoft.NET)

Run `make` to build the kernel for TI-84+, or `make [target]` for any other platform. Supported platforms are:

* TI-73 [TI73]
* TI-73 Explorer [TI73]
* TI-83+ [TI83p]
* TI-83+ SE [TI83pSE]
* TI-84+ [TI84p]
* TI-84+ SE [TI84pSE]
* TI-84 Pocket.fr [TI84p]
* TI-84 Plus Pocket SE [TI84pSE]

The make target is included in [brackets] next to the platform name. On Windows, build KnightOS with cygwin.

This will produce a kernel ROM file in `bin/` without a filesystem, and a kernel include file in `bin/inc/kernel.inc`.