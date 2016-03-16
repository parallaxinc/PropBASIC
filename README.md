# PropBASIC

PropBasic is a BASIC compiler for the Parallax(c) Propeller microcontroller. It translates program code written in the
BASIC computer language into Propeller assembly language instructions.


## Build Instructions

Down a source tarball or archive, or check out with git.

  git clone https://github.com/parallaxinc/PropBASIC

### Mac

1. Download and install [Homebrew](http://brew.sh/).
2. Open a terminal and run `$ brew install fpc`
3. `cd` to the directory where you unpacked PropBASIC.
4. Run `fpc -Tdarwin -opropbasic PropBasic.lpr` to build. An executable named `propbasic` will be created.

### Linux

1. Open a terminal and run `$ apt-get install fpc` on Debian, or `$ yum install fpc` on RedHat.
3. `cd` to the directory where you unpacked PropBASIC.
4. Run `fpc -Tlinux -opropbasic PropBasic.lpr` to build. An executable named `propbasic` will be created.

### Windows

1. Go to [freepascal.org](http://www.freepascal.org/) and download/install the latest version.
2. Add `"C:\FPC\3.0.0\bin\i386-win32"` to the system path.
3. Open PowerShell and `cd to the directory where you unpacked PropBASIC.
4. Run `fpc -Twin32 -opropbasic PropBasic.lpr` to build. An executable named `propbasic.exe` will be created.


