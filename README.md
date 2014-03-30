# KnightOS Kernel

The KnightOS kernel is a kernel for Texas Instruments calculators. It offers many unix-like features for
z80 systems, including:

* A tree-based filesystem
* Multitasking (up to 32 concurrent processes)
* Dynamic memory management
* Vanity functions for TI displays, keypads, etc

This kernel is the basis of [KnightOS](https://github.com/KnightSoft/KnightOS), which is a good resource
for others hoping to implement a userspace.

## Compiling

If you have a pre-compiled kernel image, skip this section.

The kernel uses an entirely open-source purpose-built toolchain, most of which can be found
[here](https://github.com/KnightSoft). In order to build it, you'll need GNU make and Mono installed. On
Windows systems, build the kernel with cygwin and Microsoft.NET (you may also be able to build with Mono
on Windows). When building with cygwin, also ensure that git for cygwin is installed.

The kernel needs to be rebuilt for any system you'd like to target (different calculator models). For each
supported calculator model, use the given make target:

| Model                | `make` Target |
| -------------------- | ------------- |
| TI-73                | TI73          |
| TI-73 Explorer       | TI73          |
| TI-83+               | TI83p         |
| TI-83+ SE            | TI83pSE       |
| TI-84+               | TI84p         |
| TI-84+ SE            | TI84pSE       |
| TI-84+ CSE           | TI84pCSE      |
| TI-84 Pocket.fr      | TI84p         |
| TI-84 Plus Pocket SE | TI84pSE       |

Simply run `make [target]` to build the kernel. The default target, when omitted, is `TI84pSE`. This will
produce a kernel image in the `bin/` directory. This will also generate a `kernel.inc` file, which you
can use to link your userspace with the kernel. Run `make clean` before trying to switch platforms.

## Usage

The basic kernel image doesn't do anything on its own. If you were to send it to a calculator, you'd get
a kernel error upon booting. It has no filesystem and you need to provide the userspace. In order to do
so, you should use the [BuildFS](https://github.com/KnightSoft/BuildFS) tool to build the filesystem and
embed it into a kernel image (aka "ROM file"). You can then use a tool like
[CreateUpgrade](https://github.com/KnightSoft/CreateUpgrade) to build and sign an OS upgrade file (an
8xu or 73u file) that you can send to a real calculator.

At a bare minimum, you'll need to write an init program. A basic one is supplied here:

    .nolist
    #include <kernel.inc>
    .list
        .db 0, 20 ; Flags, stack size
    .org 0
    start:
        ; Get an LCD lock so we can use it
        pcall(getLcdLock)
        ; Allocate a 768-byte buffer for drawing things on
        pcall(allocScreenBuffer)
        pcall(clearBuffer)
        ; Relative-load the address of our message
        kld(hl, testMessage)
        ; Draw it to our buffer
        pcall(drawStr)
        ; Copy our buffer to the LCD
        pcall(fastCopy)
        ; Loop forever
        jr $
    testMessage:
        .db "It works!", 0 ; The kernel uses the Windows-1252 character set

Assemble this and use BuildFS to patch the kernel image with a filesystem. You should be
able to boot up the kernel in an emulator now, and on hardware if you use CreateUpgrade to
make an upgrade file. We suggest using [sass](https://github.com/KnightSoft/sass) to
assemble your userspace, but you may use any assembler you wish. To build this init file,
try this:

    mkdir -p temp/bin/
    [mono] sass.exe --include "path/to/kernel.inc/folder/" --encoding "Windows-1252" init.asm temp/bin/init

Include `mono` if you are on Linux or want to use Mono to run the toolchain.

Then, you can patch the kernel image with your filesystem like so:

    [mono] BuildFS.exe 77 kernel.rom temp/

Note the use of `77` here - BuildFS accepts a page number to start the FAT on here. You
should use these page numbers:

Model   | Page Number
------- | -----------
TI73    | 17
TI83p   | 17
TI83pSE | 77
TI84p   | 37
TI84pSE | 77

After this completes, you'll get a ROM file you can use with emulators. Read the CreateUpgrade
documentation if you'd like to turn this into an upgrade file for use on real calculators.

You should be able to take things from here. Make sure you read over the
[documentation](http://knightos.org/documentation) for more information.

## Versioning

The kernel uses semantic versioning. Version numbers are indicated by the latest git tag, and
take the form of `major.minor.patch`. "Patch" is updated when bugs are fixed and for very
minor changes. "Minor" is updated for new features and major non-breaking changes. "Major" is
updated with breaking changes. The current version is 0.4.0 and version 1.0.0 is scheduled for
sometime within the next couple of months, after the filesystem is made writable and a few
other issues are dealt with.

## Getting Help

You're free to ask questions about the kernel (or the userspace) in
[#knightos](http://webchat.freenode.net/?channels=knightos&uio=d4) on irc.freenode.net. We're
not always listening, so stick around - it may be a while before your question is answered.

## Contributing

Contributions to the kernel should follow our
[contribution guidelines](https://github.com/KnightSoft/kernel/blob/master/CONTRIBUTING.md).
All offers are welcome, but not all will be accepted.

## Licensing

The kernel uses the permissive
[MIT license](https://github.com/KnightSoft/kernel/blob/master/LICENSE). It permits use and
modification in most scenarios, including commercial.
