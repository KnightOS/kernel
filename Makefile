# Makefile for KnightSoft kernel
AS=sass
ASFLAGS=--encoding "Windows-1252"
.DEFAULT_GOAL=TI84pSE
PLATFORM:=TI84pSE
TAG:=$(shell git describe --abbrev=0)
OUTDIR=bin/

# Platforms:
# Variables (all in hex):
#	PRIVILEGED: The address of the privileged page
#	BOOT: The address of the boot page
#	LENGTH: The length of the final ROM file
TI73: PLATFORM := TI73
TI73: DEVICE := TI-73
TI73: PRIVILEGED := 70000
TI73: KEY := 02
TI73: UPGRADEEXT := 73u
TI73: BOOT := 7C000
TI73: LENGTH := 0x80000
TI73: kernel

TI83p: PLATFORM := TI83p
TI83p: DEVICE := TI-83+
TI83p: PRIVILEGED := 70000
TI83p: KEY := 04
TI83p: UPGRADEEXT := 8xu
TI83p: BOOT := 7C000
TI83p: LENGTH := 0x80000
TI83p: kernel

TI83pSE: PLATFORM := TI83pSE
TI83pSE: DEVICE := TI-83+SE
TI83pSE: PRIVILEGED := 1F0000
TI83pSE: KEY := 04
TI83pSE: UPGRADEEXT := 8xu
TI83pSE: BOOT := 1FC000
TI83pSE: LENGTH := 0x200000
TI83pSE: kernel

TI84p: PLATFORM := TI84p
TI84p: DEVICE := TI-84+
TI84p: PRIVILEGED := F0000
TI84p: KEY := 0A
TI84p: UPGRADEEXT := 8xu
TI84p: BOOT := FC000
TI84p: LENGTH := 0x100000
TI84p: kernel

TI84pSE: PLATFORM := TI84pSE
TI84pSE: DEVICE := TI-84+SE
TI84pSE: PRIVILEGED := 1F0000
TI84pSE: KEY := 0A
TI84pSE: UPGRADEEXT := 8xu
TI84pSE: BOOT := 1FC000
TI84pSE: LENGTH := 0x200000
TI84pSE: kernel

TI84pCSE: PLATFORM := TI84pCSE
TI84pCSE: DEVICE := TI-84+CSE
TI84pCSE: PRIVILEGED := 3F0000
TI84pCSE: KEY := 0F
TI84pCSE: UPGRADEEXT := 8cu
TI84pCSE: BOOT := 3FC000
TI84pCSE: LENGTH := 0x400000
TI84pCSE: kernel $(OUTDIR)

DEFINES=--define $(PLATFORM)
BINDIR=$(OUTDIR)$(PLATFORM)/
INCLUDE=include/;$(BINDIR)

.PHONY: clean kernel baserom kernel-headers \
	TI73 TI83p TI83pSE TI84p TI84pSE TI84pCSE

run: TI84pSE
	$(EMU) $(BINDIR)kernel.rom

kernel: baserom $(OUTDIR)$(PLATFORM)/00.bin $(OUTDIR)$(PLATFORM)/01.bin $(OUTDIR)$(PLATFORM)/02.bin $(OUTDIR)$(PLATFORM)/privileged.bin $(OUTDIR)$(PLATFORM)/boot.bin
	mkrom $(BINDIR)kernel.rom $(LENGTH) \
		$(BINDIR)00.bin:0x00 $(BINDIR)01.bin:0x4000 \
		$(BINDIR)02.bin:0x8000 $(BINDIR)boot.bin:0x$(BOOT) \
		$(BINDIR)privileged.bin:0x$(PRIVILEGED)
	# Generate assembly headers
	mkdir -p $(BINDIR)../include/
	patchrom src/00/jumptable.config $(BINDIR)kernel.rom 00 < $(BINDIR)00.sym > $(BINDIR)00.inc
	patchrom src/01/jumptable.config $(BINDIR)kernel.rom 01 < $(BINDIR)01.sym > $(BINDIR)01.inc
	patchrom src/02/jumptable.config $(BINDIR)kernel.rom 02 < $(BINDIR)02.sym > $(BINDIR)02.inc
	cat include/kernel.inc include/defines.inc include/kernelmem.inc $(BINDIR)00.inc $(BINDIR)01.inc $(BINDIR)02.inc > $(BINDIR)../include/kernel.inc
	# Generate C headers
	patchrom -c src/00/jumptable.config $(BINDIR)kernel.rom 00 < $(BINDIR)00.sym > $(BINDIR)00.h
	patchrom -c src/01/jumptable.config $(BINDIR)kernel.rom 01 < $(BINDIR)01.sym > $(BINDIR)01.h
	patchrom -c src/02/jumptable.config $(BINDIR)kernel.rom 02 < $(BINDIR)02.sym > $(BINDIR)02.h
	cat headers/kernel.h.start $(BINDIR)00.h $(BINDIR)01.h $(BINDIR)02.h headers/kernel.h.end > $(BINDIR)../include/kernel.h
	# Generate kernel upgrade file
	mktiupgrade -p -k keys/$(KEY).key -d $(DEVICE) $(BINDIR)kernel.rom $(BINDIR)kernel.$(UPGRADEEXT) 00 01 02 03

kernel-headers-$(TAG).pkg: headers/* include/*
	rm -rf temp
	mkdir -p temp/root/include
	cp headers/package.config temp/
	cp -r bin/include temp/root/
	echo "version=$$(git describe --abbrev=0)" >> temp/package.config
	kpack -c temp/package.config kernel-headers-$(TAG).pkg temp/root/
	rm -rf temp

kernel-headers: kernel-headers-$(TAG).pkg

baserom:
	mkdir -p $(BINDIR)
	mkrom $(BINDIR)kernel.rom $(LENGTH) /dev/null:0x00

$(OUTDIR)$(PLATFORM)/00.bin: src/00/*.asm include/constants.asm src/00/jumptable.config
	@mkdir -p $(BINDIR)
	$(AS) $(ASFLAGS) $(DEFINES) --include "$(INCLUDE);src/00/" --symbols $(BINDIR)00.sym --listing $(BINDIR)00.list src/00/base.asm $(BINDIR)00.bin
	patchrom src/00/jumptable.config $(BINDIR)kernel.rom 00 < $(BINDIR)00.sym > $(BINDIR)00.inc

$(OUTDIR)$(PLATFORM)/01.bin: $(OUTDIR)$(PLATFORM)/00.bin src/01/*.asm include/constants.asm src/01/jumptable.config
	@mkdir -p $(BINDIR)
	$(AS) $(ASFLAGS) $(DEFINES) --include "$(INCLUDE);src/01/" --symbols $(BINDIR)01.sym --listing $(BINDIR)01.list src/01/base.asm $(BINDIR)01.bin
	patchrom src/01/jumptable.config $(BINDIR)kernel.rom 01 < $(BINDIR)01.sym > $(BINDIR)01.inc

$(OUTDIR)$(PLATFORM)/02.bin: $(OUTDIR)$(PLATFORM)/00.bin src/02/*.asm include/constants.asm src/02/jumptable.config
	@mkdir -p $(BINDIR)
	$(AS) $(ASFLAGS) $(DEFINES) --include "$(INCLUDE);src/02/" --symbols $(BINDIR)02.sym --listing $(BINDIR)02.list src/02/base.asm $(BINDIR)02.bin
	patchrom src/02/jumptable.config $(BINDIR)kernel.rom 02 < $(BINDIR)02.sym > $(BINDIR)02.inc

$(OUTDIR)$(PLATFORM)/privileged.bin: src/privileged/*.asm include/constants.asm $(OUTDIR)$(PLATFORM)/00.bin
	@mkdir -p $(BINDIR)
	$(AS) $(ASFLAGS) $(DEFINES) --include "$(INCLUDE);src/privileged/" --listing $(BINDIR)priviledged.list src/privileged/base.asm $(BINDIR)privileged.bin

$(OUTDIR)$(PLATFORM)/boot.bin: src/boot/*.asm include/constants.asm
	@mkdir -p $(BINDIR)
	$(AS) $(ASFLAGS) $(DEFINES) --include "$(INCLUDE);src/boot/" --listing $(BINDIR)boot.list src/boot/base.asm $(BINDIR)boot.bin

clean:
	rm -rf $(OUTDIR)
