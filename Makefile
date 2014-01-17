# Makefile for KnightSoft kernel
ifeq ($(OS),Windows_NT)
ASPREFIX=
EMPREFIX=
else
ASPREFIX=mono 
EMPREFIX=wine 
endif
AS=$(ASPREFIX)build/sass.exe
INCLUDE=inc/
ASFLAGS=--encoding "Windows-1252"
.DEFAULT_GOAL=TI84pSE

all:
	make TI73
	make TI83p
	make TI83pSE
	make TI84p
	make TI84pSE
	make TI84pCSE

# Platforms:
# Variables (all in hex):
#	PRIVILEGED: The address of the privileged page
#	BOOT: The address of the boot page
#	LENGTH: The length of the final ROM file
TI73: PLATFORM := TI73
TI73: PRIVILEGED := 70000
TI73: BOOT := 7C000
TI73: LENGTH := 80000
TI73: directories kernel

TI83p: PLATFORM := TI83p
TI83p: PRIVILEGED := 70000
TI83p: BOOT := 7C000
TI83p: LENGTH := 80000
TI83p: directories kernel

TI83pSE: PLATFORM := TI83pSE
TI83pSE: PRIVILEGED := 1F0000
TI83pSE: BOOT := 1FC000
TI83pSE: LENGTH := 200000
TI83pSE: directories kernel

TI84p: PLATFORM := TI84p
TI84p: PRIVILEGED := F0000
TI84p: BOOT := FC000
TI84p: LENGTH := 100000
TI84p: directories kernel

TI84pSE: PLATFORM := TI84pSE
TI84pSE: PRIVILEGED := 1F0000
TI84pSE: BOOT := 1FC000
TI84pSE: LENGTH := 200000
TI84pSE: directories kernel

TI84pCSE: PLATFORM := TI84pCSE
TI84pCSE: PRIVILEGED := 3F0000
TI84pCSE: BOOT := 3FC000
TI84pCSE: LENGTH := 400000
TI84pCSE: directories kernel

DEFINES=$(PLATFORM)

test: DEFINES=TI84pSE,TEST
test: TI84pSE
	$(ASPREFIX)build/BuildFS.exe 77 bin/kernel-TI84pSE.rom src/00/tests/testfs

runtest: test
	$(EMPREFIX)build/Wabbitemu.exe bin/kernel-TI84pSE.rom

# Build kernel
kernel: page00 pageBoot pagePrivledged
	$(ASPREFIX)build/MakeROM.exe bin/kernel-$(PLATFORM).rom $(LENGTH) bin/00.bin:0 bin/boot.bin:$(BOOT) bin/privileged.bin:$(PRIVILEGED)
	$(ASPREFIX)build/CreateJumpTable.exe src/jumptable.config bin/00.sym bin/kernel-$(PLATFORM).rom inc/kernel.inc bin/kernel.inc
	cat inc/kernelmem.inc >> bin/kernel.inc
	rm bin/00.bin
	rm bin/boot.bin
	rm bin/privileged.bin
	rm bin/00.sym

page00:
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/00/" --symbols bin/00.sym src/00/base.asm bin/00.bin --listing bin/00.list

pageBoot:
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/boot/" src/boot/base.asm bin/boot.bin --listing bin/boot.list

pagePrivledged:
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/privileged/" src/privileged/base.asm bin/privileged.bin --listing bin/priviledged.list

directories:
	mkdir -p bin

clean:
	rm -r bin
