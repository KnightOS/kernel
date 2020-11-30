PLATFORM=TI73
TAG=0.6.12
AS=knightos/scas

OUTDIR=bin/$PLATFORM
ASFLAGS=-D$PLATFORM -DKERNEL_VERSION=$TAG -I include/ -I $OUTDIR/

LENGTH=`{switch($PLATFORM){case TI84pCSE; echo 400000; case *pSE; echo 200000; case TI84p; echo 100000; case * ; echo 80000}}
BOOT=`{echo 'ibase=16;obase=10;'^$LENGTH^'-4000'|bc}
PRIV=`{echo 'ibase=16;obase=10;'^$LENGTH^'-10000'|bc}

kernel:V: $OUTDIR/00.inc $OUTDIR/02.inc $OUTDIR/01.inc

$OUTDIR/kernel.rom: $OUTDIR/00.bin $OUTDIR/01.bin $OUTDIR/02.bin $OUTDIR/boot.bin $OUTDIR/privileged.bin
	knightos/mkrom $OUTDIR/kernel.rom 0x$LENGTH $OUTDIR/00.bin:0x00 $OUTDIR/01.bin:0x4000 $OUTDIR/02.bin:0x8000 $OUTDIR/boot.bin:0x$BOOT $OUTDIR/privileged.bin:0x$PRIV

$OUTDIR/01.bin: $OUTDIR/00.bin
$OUTDIR/02.bin: $OUTDIR/00.bin
$OUTDIR/boot.bin: $OUTDIR/00.bin
$OUTDIR/privileged.bin: $OUTDIR/00.bin

00_SOURCES:V:src/00/base.asm src/00/boot.asm src/00/concurrency.asm src/00/crc.asm src/00/display-color.asm src/00/display.asm src/00/filestreams.asm src/00/filesystem.asm src/00/flash.asm src/00/header.asm src/00/interrupt.asm src/00/jumptable.config src/00/keyboard.asm src/00/libraries.asm src/00/link.asm src/00/locks.asm src/00/math.asm src/00/memory.asm src/00/panic.asm src/00/random.asm src/00/restarts.asm src/00/signals.asm src/00/strings.asm src/00/thread.asm src/00/util.asm
	echo 'if (~ ' ^ $#PLATFORM ^ ' 0) { exit 1}' | rc
01_SOURCES:V:src/01/base.asm src/01/font.asm src/01/jumptable.config src/01/text.asm 
02_SOURCES:V:src/02/base.asm src/02/compression.asm src/02/crypto.asm src/02/fp-math.asm src/02/graphics.asm src/02/jumptable.config src/02/sort.asm src/02/strings.asm src/02/time.asm
boot_SOURCES:V:src/boot/base.asm
privileged_SOURCES:V:src/privileged/base.asm

$OUTDIR/%.inc: $OUTDIR/%.bin $OUTDIR/kernel.rom
	knightos/patchrom src/$stem/jumptable.config $OUTDIR/kernel.rom $stem < $OUTDIR/$stem.sym > $OUTDIR/$stem.inc

$OUTDIR/%.bin:D:%_SOURCES include/constants.asm
	mkdir -p $OUTDIR
	$AS $ASFLAGS -I src/$stem/ --symbols $OUTDIR/$stem.sym --listing $OUTDIR/$stem.list src/$stem/base.asm -o $OUTDIR/$stem.bin

clean:V:
	rm -r $OUTDIR
