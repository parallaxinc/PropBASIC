all:

linux:
	cd src; fpc -Tlinux -O3 PropBasic.lpr

mac:
	cd src; fpc -Tdarwin -O3 PropBasic.lpr

win:
	cd src; fpc -O3 PropBasic.lpr

clean:
	cd src; rm -f *.o *.ppu PropBasic propbasic
