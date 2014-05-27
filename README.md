# KnightOS Kernel

The KnightOS kernel is a kernel for Texas Instruments calculators. It offers many unix-like features for
z80 systems, including:

* A tree-based filesystem
* Multitasking (up to 32 concurrent processes)
* Dynamic memory management

This kernel is the basis of [KnightOS](https://github.com/KnightOS/KnightOS), which is a good resource
for others hoping to implement a userspace.

This project is only useful to systems programmers. Please look into KnightOS instead if you are not a
systems programmer.

## Compiling

If you have a pre-compiled kernel image, skip this section.

The toolchain for the kernel was built mostly from scratch, since old z80 tools are, well, old. The new
toolchain supports a lot of the kernel's needs on newer platforms, and works well on Linux, Mac, and
Windows. You'll need to install:

* Mono (or Microsoft.NET on Windows)
* GNU Make
* [sass](https://github.com/KnightOS/sass) (aur/sass)
* [mktiupgrade](https://github.com/KnightOS/mktiupgrade) (aur/mktiupgrade)
* [genkfs](https://github.com/KnightOS/genkfs) (aur/genkfs)
* [mkrom](https://github.com/KnightOS/mkrom) (aur/mkrom)
* [wabbitemu](https://wabbit.codeplex.com/) (aur/wabbitemu) [optional]

The last one is only strictly neccessary if you hope to build a userspace on top of the kernel. On Windows,
install Cygwin and perform the build from there. Windows users should install sass into their %PATH%.

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

The kernel does not do anything on its own. Instead, it forms the basis for more complex systems. Upon
booting, the kernel loads up the filesystem and runs `/bin/init`. You need to provide this init program
yourself. Here is a simple example init program, which can be assembled with the assembler of your choice:

    #include "kernel.inc"
        ; Program header
        .db "KEXC"
        .db KEXC_STACK_SIZE
        .dw 20
        .db KEXC_ENTRY_POINT
        .dw start
        .db KEXC_HEADER_END
    start:
        pcall(getLcdLock)
        pcall(allocScreenBuffer)
        kld(hl, message)
        ld de, 0
        pcall(drawStr)
        pcall(fastCopy)
        jr $
    message:
        .db "Hello, userspace!", 0

When you compile the kernel, you'll get a ROM file with an empty filesystem. To build the filesystem, you
will need to make an example on your own system to build it from. Then, you can use
[genkfs](https://github.com/KnightOS/genkfs) to generate and write a filesystem to the ROM. If you wish
to build an OS upgrade that you can send to your calculator, use
[mktiupgrade](https://github.com/KnightOS/mktiupgrade).

When you build the kernel, in addition to the ROM file, you will receive a kernel upgrade file (this will
be the .73u, .8xu, or .8cu file in the output directory). This can be used on any KnightOS system to
upgrade the kernel without touching the userspace filesystem.

## Kernel API

The kernel offers an API to userspace to interact with things like threads, memory, hardware, the
filesystem, and more. The API is documented through special comments in the source code, which are
extracted to generate the [online API reference](http://www.knightos.org/documentation/reference/).

## Versioning

The kernel uses semantic versioning. Version numbers are indicated by the latest git tag, and
take the form of `major.minor.patch`. "Patch" is updated when bugs are fixed and for very
minor changes. "Minor" is updated for new features and major non-breaking changes. "Major" is
updated with breaking changes. When you compile your kernel, the kernel version (as an ASCII
string) will be written to address 0x64 on page 0x00.

If you are working with a kernel that is not built from a major release, you will have a
sligthly different kernel version. Appended to the version will be "-nn-hhhhhhhh". "nn" is the
number of commits that have been made since your kernel release. "hhhhhhhh" is the git shorthash
of the commit you're currently on. A "+" will be appended to this if your working directory is
dirty.

## Getting Help

You're free to ask questions about the kernel (or the userspace) in
[#knightos](http://webchat.freenode.net/?channels=knightos&uio=d4) on irc.freenode.net. We're
not always listening, so stick around - it may be a while before your question is answered.

## Contributing

Contributions to the kernel should follow our
[contribution guidelines](https://github.com/KnightOS/kernel/blob/master/CONTRIBUTING.md).
All offers are welcome, but not all will be accepted. You might want to join #knightos (see previous
section) to join in on development discussion before you start writing code.

## Licensing

The kernel uses the permissive
[MIT license](https://github.com/KnightOS/kernel/blob/master/LICENSE). It permits use and
modification in most scenarios, including commercial.
