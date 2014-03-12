# Makefile for KnightSoft kernel
ifeq ($(OS),Windows_NT)
ASPREFIX=
EMPREFIX=
else
ASPREFIX=mono 
EMPREFIX=wine 
endif
AS=$(ASPREFIX)build/sass.exe
INCLUDE=inc/;bin/
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
TI73: KEY := 02
TI73: UPGRADEEXT := 73u
TI73: BOOT := 7C000
TI73: LENGTH := 80000
TI73: directories kernel

TI83p: PLATFORM := TI83p
TI83p: PRIVILEGED := 70000
TI83p: KEY := 04
TI83p: UPGRADEEXT := 8xu
TI83p: BOOT := 7C000
TI83p: LENGTH := 80000
TI83p: directories kernel

TI83pSE: PLATFORM := TI83pSE
TI83pSE: PRIVILEGED := 1F0000
TI83pSE: KEY := 04
TI83pSE: UPGRADEEXT := 8xu
TI83pSE: BOOT := 1FC000
TI83pSE: LENGTH := 200000
TI83pSE: directories kernel

TI84p: PLATFORM := TI84p
TI84p: PRIVILEGED := F0000
TI84p: KEY := 0A
TI84p: UPGRADEEXT := 8xu
TI84p: BOOT := FC000
TI84p: LENGTH := 100000
TI84p: directories kernel

TI84pSE: PLATFORM := TI84pSE
TI84pSE: PRIVILEGED := 1F0000
TI84pSE: KEY := 0A
TI84pSE: UPGRADEEXT := 8xu
TI84pSE: BOOT := 1FC000
TI84pSE: LENGTH := 200000
TI84pSE: directories kernel

TI84pCSE: PLATFORM := TI84pCSE
TI84pCSE: PRIVILEGED := 3F0000
TI84pCSE: KEY := 0F
TI84pCSE: UPGRADEEXT := 8cu
TI84pCSE: BOOT := 3FC000
TI84pCSE: LENGTH := 400000
TI84pCSE: directories kernel

DEFINES=$(PLATFORM)

# Build kernel
kernel: page00 page01 page02 pageBoot pagePrivledged
	$(ASPREFIX)build/MakeROM.exe bin/kernel-$(PLATFORM).rom $(LENGTH) bin/00.bin:0 bin/01.bin:4000 bin/02.bin:8000 bin/boot.bin:$(BOOT) bin/privileged.bin:$(PRIVILEGED)
	cat inc/kernel.inc inc/kernelmem.inc bin/00.inc bin/01.inc bin/02.inc > bin/kernel.inc
	$(ASPREFIX)build/CreateJumpTable.exe 00 src/00/jumptable.config bin/00.sym bin/kernel-$(PLATFORM).rom
	$(ASPREFIX)build/CreateJumpTable.exe 01 src/01/jumptable.config bin/01.sym bin/kernel-$(PLATFORM).rom
	$(ASPREFIX)build/CreateJumpTable.exe 02 src/02/jumptable.config bin/02.sym bin/kernel-$(PLATFORM).rom
	$(ASPREFIX)build/CreateUpgrade.exe $(PLATFORM) bin/kernel-$(PLATFORM).rom build/$(KEY).key \
		bin/kernel-$(PLATFORM).$(UPGRADEEXT) 00 01 02 03

page00:
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/00/" --symbols bin/00.sym src/00/base.asm bin/00.bin --listing bin/00.list
	$(ASPREFIX)build/CreateJumpTable.exe --symbols 00 src/00/jumptable.config bin/00.sym bin/00.inc

page01: page00
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/01/" --symbols bin/01.sym src/01/base.asm bin/01.bin --listing bin/01.list
	$(ASPREFIX)build/CreateJumpTable.exe --symbols 01 src/01/jumptable.config bin/01.sym bin/01.inc

page02: page00
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/02/" --symbols bin/02.sym src/02/base.asm bin/02.bin --listing bin/02.list
	$(ASPREFIX)build/CreateJumpTable.exe --symbols 02 src/02/jumptable.config bin/02.sym bin/02.inc

pageBoot:
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/boot/" src/boot/base.asm bin/boot.bin --listing bin/boot.list

pagePrivledged:
	$(AS) $(ASFLAGS) --define "$(DEFINES)" --include "$(INCLUDE);src/privileged/" src/privileged/base.asm bin/privileged.bin --listing bin/priviledged.list

directories:
	mkdir -p bin

clean:
	rm -r bin
