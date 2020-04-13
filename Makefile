.PHONY: air2air.o air2air
AS=vasmm68k_mot
ASFLAGS=-Fhunk -m68010

all: air2air
air2air: air2air.o
	vlink air2air.o -o air2air
air2air.o:
	$(AS) $(ASFLAGS) air2air.s -o air2air.o
clean:
	rm -f *~ *.o a.out air2air
