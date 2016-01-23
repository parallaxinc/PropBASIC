Program PropBasic; // Compile with Lazarus (Free Pascal)

{$MODE Delphi}

{$APPTYPE CONSOLE}

{
 =========================================================================

   File...... PropBasic
   Purpose... BASIC to Propeller Assembly Compiler
   Author.... Terry Hitt (Esterline Research & Design)
   Started... Aug 27, 2009
   Updated... Daily

 =========================================================================

  PropBasic.EXE "C:\SOURCECODE\TEST.pbas"

Switches:
  /Q = Quiet (No screen output)
  /P = Pause on warning or error (used to debug compiler)
  /E = Pause on exit
  /B = Brief output (does not show source code)
  /O = "Output_Directory" Specifies a diffrent directory for output files
  /V = Returns Version number as exit code (exit immediately)
  /NS = No Code (Does NOT include the BASIC code in the output file)
  /VP = Compiling for ViewPort

----------------------------------------------------------------------------
Version 00.01.00
  Fixed: Compiler crash on COGSTART if task name has not been defined
  Fixed: Calculated constant using a long constant
    mSec = _Freq / 1000

    value1 CON 10_000
    value2 CON value1 / 10
----------------------------------------------------------------------------
Version 00.01.01
  Fixed: COGSTART doesn't add "par" to __temp1
  Fixed: / doesn't error if first parameter is invalid
  Fixed: minWaitCnt in tasks was 128 instead of 80
  Fixed: str = str + longvar
  Fixed: str = "" ' comment
----------------------------------------------------------------------------
Version 00.01.02
  Fixed: test $,#1 WC ' Set Carry (doesn't work in LMM) I2CWrite
  Fixed: COGINIT if taskname doesn't exist crashes
  Fixed: Glitch on PinGroup = PinGroup
  Fixed: IF pinName THEN
  Changed: In LMM use "LONG label * 4" instead of "LONG @label - @__Init"
  Changed: Use relative jumps for LMM
    SUB __PC,#(($-label)*4)+4 ' Jump backward
    ADD __PC,#((label-$)*4)-4 ' Jump forward
----------------------------------------------------------------------------
Version 00.01.03
  Fixed: RDWORD buffer($1C0), temp
  Fixed: name HUB STRING(10) = "Hello"
  Fixed: trailing "," in data causes offset in next data label (counts it as element).
           Now causes a syntax error.
  Fixed: PULSIN hangs with RCSLOW
----------------------------------------------------------------------------
Version 00.01.04
  Fixed: INC, DEC where second parameter is an array with a variable index
  Added: I2CSpeed multiplier. "I2CSpeed 2" is double speed, "I2CSpeed 0.5" is half speed.
----------------------------------------------------------------------------
Version 00.01.05
  Added: Allows embedded escape sequences in strings.
    \r = 13
    \n = 10
    \\ = 92
    \" = 34
    \123 = 123 [must use 3 digits, so 16 would be \016 ]
    \x20 = $20 [must use 2 characters, so $A would be \x0A ]
----------------------------------------------------------------------------
Version 00.01.06
  Added:
    "CLKSET mode,freq" command
  Fixed:
    Allow more nested IF...ELSEIF loops, increased c_iMaxLoops from 127 to 1024
    LET pingroup= doesn't do "shl __temp1,#0" if pingroup starts at 0
    HIGH,LOW,TOGGLE,INPUT,OUTPUT,REVERSE if pin # is a constant < 9 then do shift as literal
    Allow I2C pins to be variables
  Removed:
    Copyright notice
----------------------------------------------------------------------------
Version 00.01.07
  Changed LMM to use @@@ (still need offset for start address)
----------------------------------------------------------------------------
Version 00.01.08
  Fixed data offsets (all messed up) from allowing embedded escape sequences
  Fixed @@@ issues in _LETLong and _LetStr
----------------------------------------------------------------------------
Version 00.01.09
  Fixed: I2CSTART with pins as variables
----------------------------------------------------------------------------
Version 00.01.10
  Fixed: SHIFTIN - REV was using bits instead of (32-bits)
  Fixed: SHIFTIN and SHIFTOUT when using a variable for 2nd parameter
----------------------------------------------------------------------------
Version 00.01.11
  Fixed: ON var GOSUB label0, label1, label2
  Fixed: Allow constant with COGSTOP
  Fixed: Embedded escape sequences "\"
----------------------------------------------------------------------------
Version 00.01.12
  Changed: Create a return stack for LMM
             this will save 1 instruction for every subroutine call
             and 1 long from every subroutine
  Optimized: SERIN use "waitpne" and "waitpeq" to wait for state  (saves 2)
                   use "muxz" or "muxnz" to set/clear data bits   (saves 1)
                   use data variable to do bit count (1 shifts out) (saves 1)
----------------------------------------------------------------------------
Version 00.01.13
  Optimized: WRLONG, WRWORD, WRBYTE (don't reload short constants)
  Added: "STACK value" to change stack size (defaults to 16) (4 to 255 allowed)
    NOTE: STACK only affects LMM code
  Fixed: pin = var (didn't set zero flag)
  Fixed: array(pos) = array(pos) + array(pos2)
----------------------------------------------------------------------------
Version 00.01.14
  Fixed: Using long constant in LDATA Jul 26, 2011
----------------------------------------------------------------------------
Version 00.01.15 - 00.01.18
  Fixed: multiply gets hung-up Sept 21, 2011

  Add support for ViewPort
    Added: WATCH VarName
    Added: UNWATCH LongVarName turns off watching
----------------------------------------------------------------------------
Version 00.01.19
  Fixed WATCH order for arrays
  Allow multiple parameters for WATCH and UNWATCH
  Allow WATCH and UNWATCH without any parameters for ALL
  Allow Viewport modifiers as WATCH varname "modifier"
    Example: WATCH time "unit=msec"
    * Modifier must be used at first WATCH
----------------------------------------------------------------------------
Version 00.01.20
  Fixed: remove WATCH modifier strings from HUB DATA
  Fixed: Multiply (2 bits)
  Added: Viewport Video support
  Fixed: Fix pinvar1 = ~pinvar2
    
----------------------------------------------------------------------------
Version 00.01.26
  FIXED: WDATA and LDATA (no data generated)
----------------------------------------------------------------------------
Version 00.01.27
  ADDED: __txtermAdr to support viewport terminal
----------------------------------------------------------------------------
Version 00.01.28
  Added: Support floating point values with # prefix
  Fixed: FREQ command caused abort if DEVICE is misspelled
  Fixed: Allow constant with COGSTOP (was NOT fixed in 1.11)
  Fixed: tempStr = STR temp (needs ",len" but causes abort) [Oct 10,2012]
  ----------------------------------------------------------------------------
Version 00.01.29
  Fixed: PAUSE XXXX does not cause an error
----------------------------------------------------------------------------
Version 00.01.30
  Added: Directive VIEWPORT defined if using viewport
----------------------------------------------------------------------------
Version 00.01.31
  FIXED: DO...LOOP x => y ' should be >= but no error
----------------------------------------------------------------------------
Version 00.01.40
  Changed from Delphi to Lazarus
----------------------------------------------------------------------------
Version 00.01.41
  Changed path character "\" to "/" for compatiblity with Mac and Linux
  Fixed PulsOut that hangs at lower clock speeds Aug 19, 2013
----------------------------------------------------------------------------
Version 00.01.42
  Changed to add "/" to end of g_sInputDir because Mac version seems to be lacking it
  Added code to avoid Lazarus warnings
----------------------------------------------------------------------------
Version 00.01.43
  Changed SERIN so it doesn't wait for stop bit
  Optimize Divide and remainder operation [June 27,2014]
----------------------------------------------------------------------------
Version 00.01.44
  Made PAUSE a subroutine
  Made *, */, **, /, and // operator subroutine

****************************************************************************
TODO:
  COUNTERA allow 2nd pin to be omitted saving an instruction
  task_COG_adr should not be outputted
  Need fixup code in TASKS
  #PinName doesn't error if PinName doesn't exist
  Add WZ,WC to ADD and SUB instructions and allows IF C THEN etc.
  Make subroutine for STR, SEROUT, SERIN
  A TASK cannot start another TASK

----------------------------------------------------
FUTURE:
  PRINT "Value=";value:digits:mode
  PRINTTO subroutineName
  USES "TV_LIB.pbas" as TV
    TV.Plot x,y
  A way to SEROUT several things
    SEROUT "Value=", value

  Allow input/output character streams:
    PRINT SendChar, "Value="; value
    PRINT SendChar, "Line 1", "Line 2" ' Comma starts new line
    PRINT SendChar, "Enter a value:";
    INPUT GetChar, value

  Need to change:
    Allow LMM subroutines to be used by multiple tasks. Return address kept by cog.

  Need to add: Allow SUBs and FUNCs to be native inside a LMM program.
    PROGRAM Start LMM
    MySub SUB 0,0,NATIVE

  Need to fix: Strings are nil in g_apoCmdVars

  Need to add: Support for 64 I/O device P8X64A
    DEVICE P8X64A
    INB, OUTB, DIRB for pins 32 to 63
    Changed: WAITPEQ, WAITPNE to have an optional third parameter for porta = 0, portb = 1
    Changed: HIGH, LOW, TOGGLE, INPUT, OUTPUT, REVERSE
    Changed: WAITPEQ, WAITPNE (for portb second parameter MUST be a defined pin, or append ,1 to command)
    Need to change: PULSIN, PULSOUT, SERIN, SEROUT, WAITPEQ, WAITPNE, I2C, SHIFTIN, SHIFTOUT, COUNTERAB

----------------------------------------------------------------------------

****************************************************************************
  NEEDS FIXED: When assigning a PIN group, the pins all go low momentarily.
  TODO: Fix PULSOUT for slow clocks RCSLOW LMM, check PULSIN, etc
****************************************************************************
Undocumented features:
  LMM support        - Create Large Memory Model code for PROGRAM and TASKs
  RDSBYTE, RDSWORD   - Reads a signed value from hub memory
  name TASK AUTO     - Task starts automatically BEFORE main code
  Multiline comments - Curly brackets can be used to create multi-line comments
  __RAM              - Virtual array to access COG ram by address
  FOR..NEXT          - Uses step -1 if start is greater than limit
  STR                - Destination can be HUB or DATA string

****************************************************************************

Version 2.0
  Todo: Make pin groups it's own variable type
  Need to add: REV instruction
  Need to add: HEXSTR
  Need to add: I2C command DON'T DRIVE SCL

----------------------------------------------------------------------------
set_z         test      $,#0  wz         'Instr = %01100010011111$$$$$$$$$000000000 z = result (instr & 0) == 0
set_nz        test      $,#1  wz         'Instr = %01100010011111$$$$$$$$$000000001 z = result (instr & 1) == 1
set_c         test      $,#1  wc         'Instr = %01100001011111$$$$$$$$$000000001 c = parity (instr & 1) == 1
set_nc        test      $,#0  wc         'Instr = %01100001011111$$$$$$$$$000000000 c = parity (instr & 0) == 0
set_z_c       shr       $,#31 wz,wc,nr   'Instr = %00101011011111$$$$$$$$$000011111 z = result (instr >> 31) == 0, c = instr.0  == 1
set_z_nc      shr       $,#30 wz,wc,nr   'Instr = %00101011011111$$$$$$$$$000011110 z = result (instr >> 30) == 0, c = instr.0 == 0
set_nz_c      shr       $,#1  wz,wc,nr   'Instr = %00101011011111$$$$$$$$$000000001 z = result (instr >> 1) <> 0, c = instr.0  == 1
set_nz_nc     shr       $,#2  wz,wc,nr   'Instr = %00101011011111$$$$$$$$$000000010 z = result (instr >> 2) <> 0, c = instr.0  == 0
save_z        muxnz     restore_z,#1
restore_z     test      $,#1 wz          'Instr = %01100010011111$$$$$$$$$00000000z z = result (instr & nz) == z
save_c        muxc      restore_c,#1
restore_c     test      $,#1 wc          'Instr = %01100001011111$$$$$$$$$00000000c c = parity (instr & c) == c
mov_z_c                 muxnc   $, $ wz,nr
mov_z_nc                muxc    $, $ wz,nr
mov_c_z                 muxz    $, $ wc,nr
mov_c_nz    if_z_or_nc  muxnz   $, $ wc,nr    ' condition flips parity to odd (required)
                                              ' this nop's for z = 0 AND c = 1 (c == nz)
not_z                   muxz    $, $ wz,nr
not_c                   muxnc   $, $ wc,nr
swap_z_c    if_z_ne_c   sumc    $, $ wc,wz,nr
----------------------------------------------------------------------------

}

uses
  SysUtils,
  DEVICES in 'DEVICES.PAS',
  GLOBAL in 'GLOBAL.PAS',
  _BRANCH in '_BRANCH.PAS',
  _FOR in '_FOR.PAS',
  _HIGH in '_HIGH.PAS',
  _IF in '_IF.PAS',
  _DO in '_DO.PAS',
  _INC in '_INC.PAS',
  _LET in '_LET.PAS',
  _LOOKUP in '_LOOKUP.PAS',
  _PAUSE in '_PAUSE.PAS',
  _PULSE in '_PULSE.PAS',
  _I2C in '_I2C.PAS',
  _ONEWIRE in '_ONEWIRE.PAS',
  _RANDOM in '_RANDOM.PAS',
  _RCTIME in '_RCTIME.PAS',
  _READ in '_READ.PAS',
  _SERIAL in '_SERIAL.PAS',
  _SHIFT in '_SHIFT.PAS',
  _VAR in '_VAR.PAS',
  _INPUT in '_INPUT.PAS',
  _MISC in '_MISC.PAS',
  _PROP in '_PROP.PAS',
  _TASK in '_TASK.PAS',
  _DEBUG in '_DEBUG.PAS';

Const
  c_sVersion = '00.01.44';
  c_iExitCodeVersion = 000144;
  c_sVerDate = 'Jul 13, 2014';

Var
  sLine: String;

Procedure CompileFile(pv_sGiven: String); Forward;

Procedure ProcessAsm;
Begin
  OutStr('  '' ASM');
  g_bInAsm:=True;
  g_bHandled:=True;
End;

Procedure ProcessEndAsm;
Begin
  OutStr('  '' ENDASM');
  g_bInAsm:=False;
  g_bHandled:=True;
End;

Procedure ProcessVerbatim;
Var
  sTemp: String;
  bInQuotes: Boolean;
  bSlash: Boolean;
  I: Integer;
Begin
  bSlash:=False;
  sTemp:='';
  sLine:=g_sOrigLine;
  While (Length(sLine) > 0) and ((Copy(sLine, 1, 1) = #9) or (Copy(sLine, 1, 1) = ' ')) Do Delete(sLine, 1, 1);
  I:=1;
  While (I < Length(sLine)) and (Not (sLine[I] In [' ',#9,'=','>','<','-','+','.',',','&','|','^','(',')','~','@','/','*','\'])) Do Inc(I);
  If I < Length(sLine) Then
  Begin
    sTemp:=Copy(sLine, I, Length(sLine)-I+1);
    sLine:=Copy(sLine, 1, I-1);
  End;
  If sLine = ReplaceLiteral(sLine) Then sLine:=g_sOrigLine
   Else sLine:=ReplaceLiteral(sLine) + sTemp;
  I:=Pos('\', sLine);
  If (Not (g_bInAsm or g_bMultiLineComment)) And (I > 0) Then //  WAS "If I = 1 Then"
  Begin
    sLine:='  '+Copy(sLine, I+1, Length(sLine)-I);
    bSlash:=True;
  End Else g_sOrigLine:=''; // Don't repeat commands in ASM...ENDASM blocks
  sTemp:='';
  bInQuotes:=False;
  I:=1;
  While I <= Length(sLine) Do
  Begin
    If sLine[I]='"' Then
    Begin
      bInQuotes:=Not bInQuotes;
      If (I+1 < Length(sLine)) and (sLine[I+1] = '"') Then
      Begin
        sTemp:=sTemp+sLine[I];
        Inc(I);
      End;
    End;
    sTemp:=sTemp+sLine[I];
    Inc(I);
  End;
  If bSlash and (Pos(';', sTemp) > 0) Then sTemp:=Copy(sTemp, 1, Pos(';', sTemp)-1);
  // Remove trailing spaces and tabs
  While (Length(sTemp) > 0) and (sTemp[Length(sTemp)] In [' ', #9]) Do Delete(sTemp, Length(sTemp), 1);
  OutSpin(sTemp);
  g_bHandled:=True;
  sLine:='';
End;


Procedure ProcessLabel;
Begin
  If Pos(' ', sLine) > 0 THEN Error(c_iErrorSyntaxError, 0);
  SetLength(sLine, Length(sLine)-1); // Remove colon from end of label
  OutStr(sLine);
  g_bHandled:=True;
End;

Procedure ProcessDevice;
Var
  I: Integer;
  sTemp: String;
Begin
  If (Upper(g_asCmdLine[2]) = 'P8X32A') or
     (Upper(g_asCmdLine[2]) = 'P2') Then
  Begin
    If Upper(g_asCmdLine[2]) = 'P8X32A' Then DeviceP8X32A
     Else DeviceP2;
    g_bIsProgram:=True;
    g_iPLL:=1;
    OutSpin('CON');
    If g_iCmdCnt = 2 Then
    Begin
      OutSpin('  _ClkMode = RCFAST'); // Assume RCFAST
      // Create a long constant _Freq
      g_oDevice.pAddLongConst('_FREQ', g_lFreq, IntStr(g_lFreq));
    End;
    If Odd(g_iCmdCnt) Then Error(c_iErrorInvalidNumberofParameters, 0);
    If g_iCmdCnt > 3 Then
    Begin
      g_bFreqSpecified:=False;
      If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
      sTemp:='  _ClkMode = '+g_asCmdLine[4];
      If Upper(g_asCmdLine[4]) = 'RCSLOW' Then
      Begin
        g_lFreq:=20000; // 20 KHz
        g_bFreqSpecified:=True;
        g_bInternalClock:=True;
        // Create a long constant _Freq
        g_oDevice.pAddLongConst('_FREQ', g_lFreq, IntStr(g_lFreq));
      End;
      If Upper(g_asCmdLine[4]) = 'RCFAST' Then
      Begin
        g_lFreq:=12000000; // 12 MHz
        g_bFreqSpecified:=True;
        g_bInternalClock:=True;
        // Create a long constant _Freq
        g_oDevice.pAddLongConst('_FREQ', g_lFreq, IntStr(g_lFreq));
      End;
      If (Copy(Upper(g_asCmdLine[4]), 1, 6) = 'XINPUT') or (Copy(Upper(g_asCmdLine[4]), 1, 4) = 'XTAL') Then
      Begin
        g_lFreq:=0;
        g_bFreqSpecified:=False;
        g_bInternalClock:=False;
      End;
      For I:=5 to g_iCmdCnt Do
      Begin
        If g_asCmdLine[I] <> ',' Then
        Begin
          sTemp:=sTemp + ' + '+g_asCmdLine[I];
          If Upper(g_asCmdLine[I]) = 'PLL16X' Then g_iPLL:=16;
          If Upper(g_asCmdLine[I]) = 'PLL8X' Then g_iPLL:=8;
          If Upper(g_asCmdLine[I]) = 'PLL4X' Then g_iPLL:=4;
          If Upper(g_asCmdLine[I]) = 'PLL2X' Then g_iPLL:=2;
        End;
      End;
      OutSpin(sTemp);
    End;
  End
  Else Error(c_iErrorInvalidParameter, 2);
End;

Procedure ProcessProgram; // PROGRAM label [LMM|NOSTARTUP|PASD|VIEWPORT]
Var
  i: Integer;
  iPlace: Integer;
  bStartup: Boolean;
  poLongVar: PLongVarObj;
Begin
  bStartup:=True;
  If Not g_bFreqSpecified Then Error(c_iErrorNoFreqSpecified, 0);
  If g_bInSub Then Error(c_iErrorNotValidInsideSub, 1);
  g_bUsedProgram:=True;

  iPlace:=3;
  If g_iCmdCnt > 1 Then
  Begin
    If g_iCmdCnt > 2 Then
    Begin
      Repeat
        If Upper(g_asCmdLine[iPlace]) = 'NOSTARTUP' Then bStartup:=False
        Else If Upper(g_asCmdLine[iPlace]) = 'LMM' Then
        Begin
          g_bLMM:=True;
          g_iMinWaitCnt:=80; // 69 for tightest loop
        End
        Else If Upper(g_asCmdLine[iPlace]) = 'PASD' Then
        Begin
          If g_lFreq >= 10000000 Then
           g_eDebugger:=e_PASD
          Else Error(c_iErrorPASDFreqError, iPlace);
        End
        Else If Upper(g_asCmdLine[iPlace]) = 'VIEWPORT' Then
        Begin
          If g_lFreq >= 10000000 Then
          Begin
            g_eDebugger:=e_ViewPort;
            AddDirective('VIEWPORT');
          End
          Else Error(c_iErrorViewPortFreqError, iPlace);
        End
        Else Error(c_iErrorSyntaxError, iPlace);
        Inc(iPlace);
      Until iPlace > g_iCmdCnt;
    End;

    If g_eDebugger = e_PASD Then
    Begin
      OutSpin('OBJ dbg: "PASDebug"');
      OutStr('');
    End
    Else If g_eDebugger = e_Viewport Then
    Begin
      OutSpin('OBJ vp: "Conduit"');
      OutSpin('');
    End;
    OutSpin('PUB __Program | __VarsPtr');
    If g_eDebugger = e_ViewPort Then
    Begin
      OutSpin('  __VarsPtr := vp_config');

      poLongVar:=g_oDevice.pAddLongVar('__txtermAdr', False, '0');
      poLongVar^.m_lValue:=0;
      poLongVar^.m_bHasInitValue:=True;
      poLongVar^.m_sFormat:='0';

      OutSpin('  __txtermAdr := vp.txTermAdr');
    End
    Else OutSpin('  __VarsPtr := 0');
    For i:=1 to g_iTaskCount Do
    Begin
      If g_eDebugger = e_Viewport Then
       OutSpin('  '+g_asTaskNames[i]+'_COG:='+g_asTaskNames[i]+'.Init(@__DataStart, @VP_Watch)')
      Else
       OutSpin('  '+g_asTaskNames[i]+'_COG:='+g_asTaskNames[i]+'.Init(@__DataStart, 0)');
      If g_bTaskAuto[i] Then OutSpin('  CogNew('+g_asTaskNames[i]+'_COG >> 2, __VarsPtr)');
    End;
    If g_eDebugger = e_PASD Then OutSpin('  dbg.start(31,30,@__Init)');
    If g_bLMM Then
    Begin
      OutSpin('  __OFFSET := @__Init');
      OutSpin('  CogInit(0, @_LMM_Entry, __VarsPtr)');
    End
    Else OutSpin('  CogInit(0, @__Init, __VarsPtr)');
    OutSpin('');
    OutSpin('DAT');
    OutStr('  org 0');
    OutSpin('__Init');
    OutSpin('__RAM');
    If g_eDebugger = e_PASD Then
    Begin
      OutSpin(' '' --------- Debugger Kernel add this at Entry (Addr 0) ---------');
      OutSpin('   long $34FC1202,$6CE81201,$83C120B,$8BC0E0A,$E87C0E03,$8BC0E0A');
      OutSpin('   long $EC7C0E05,$A0BC1207,$5C7C0003,$5C7C0003,$7FFC,$7FF8');
      OutSpin(' ''  --------------------------------------------------------------');
    End;
    If bStartup Then
    Begin
      If g_bIsP1 Then
      Begin
        OutStr('  mov dira,__InitDirA');
        OutStr('  mov outa,__InitOutA');
      End
      Else
      Begin
        OutStr('  mov dira,__InitDirA');
        OutStr('  mov pina,__InitOutA');
        OutStr('  mov dirb,__InitDirB');
        OutStr('  mov pinb,__InitOutB');
        OutStr('  mov dirc,__InitDirC');
        OutStr('  mov pinc,__InitOutC');
        OutStr('  mov dird,__InitDirD');
        OutStr('  mov pind,__InitOutD');
      End;
    End;
    If g_bLMM Then
    Begin
      OutStr('  rdlong __PC,__PC');
      OutStr('  long @@@'+g_asCmdLine[2]);
    End
    Else OutStr('  jmp #'+g_asCmdLine[2]);
    g_bHandled:=True;
  End
  Else Error(c_iErrorSyntaxError, 0);
End;


Procedure ProcessLoad;
Var
  sFileName: String;
  sDirName: String;
  tfIn: Text;
  sCallingCurrentFile: String;
  iCallingInLineCnt: Integer;
  bCompile: Boolean;
Begin
  sDirName:=''; // avoid lazarus warning
  If g_iCmdCnt = 2 Then
  Begin
    sFileName:=Copy(g_asCmdLine[2], 2, Length(g_asCmdLine[2])-2);
    If ExtractFilePath(sFileName) = '' Then
    Begin
      GetDir(0, sDirName);
      sFileName:=sDirName+'/'+sFileName;
    End;
    Assign(tfIn, sFileName);
    {$I-}
    Reset(tfIn);
    {$I+}
    If IOResult = 0 Then
    Begin
      Close(tfIn);
      For iCallingInLineCnt:=1 to g_iCmdCnt Do If g_abDeleteTemp[iCallingInLineCnt] Then Dispose(g_apoCmdVars[iCallingInLineCnt], Done);
      OutStr('');
      Inc(g_iLoadFilesCode);
      Inc(g_iLoadFilesTasks);
      g_asLoadFiles[g_iLoadFilesCode]:=sFileName;
      sCallingCurrentFile:=g_sCurrentFile;
      iCallingInLineCnt:=g_iInLineCnt;
      g_sCurrentFile:=sFileName;
      g_bInLoadCmd:=True;

      bCompile:=g_bCompile;
      g_bCompile:=True;
      CompileFile(sFileName);
      g_bCompile:=bCompile;

      g_bInLoadCmd:=False;
      g_sCurrentFile:=sCallingCurrentFile;
      g_iInLineCnt:=iCallingInLineCnt;
      sLine:='';
      For iCallingInLineCnt:=1 to g_iCmdCnt Do g_abDeleteTemp[iCallingInLineCnt]:=False;
      g_bHandled:=True;
    End
    Else ErrorStr(c_iErrorCouldNotReadSourceFile, sFileName);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessInclude;
Var
  tfTemp: Text;
  sTemp: String;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    OutStr('');
    Assign(tfTemp, Copy(g_asCmdLine[2], 2, Length(g_asCmdLine[2])-2));
    {$I-}
    Reset(tfTemp);
    If IOResult = 0 Then
    Begin
      While Not EOF(tfTemp) Do
      Begin
        ReadLn(tfTemp, sTemp);
        WriteLn(g_tfSrc, sTemp);
        Inc (g_iOutLineCnt);
      End;
      Close(tfTemp);
      sLine:='';
      g_bHandled:=True;
    End
    Else
    Begin
      ErrorStr(c_iErrorCouldNotReadSourceFile, g_asCmdLine[2]);
    End;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessRem;
Begin
  OutStr('  ');
  g_bHandled:=True;
End;

Procedure ProcessGoto;
Begin
  If g_bLMM Then
  Begin
    OutStr('  rdlong __PC,__PC');
    OutStr('  long @@@'+g_asCmdLine[2]);
  End
  Else OutStr('  jmp #'+g_asCmdLine[2]);
  g_bHandled:=True;
End;

Procedure ProcessExit;
Begin
  If g_iExitLabelCnt > 0 Then
  Begin
    g_asCmdLine[2]:=g_arExitLabels[g_iExitLabelCnt].sLabel;
    g_arExitLabels[g_iExitLabelCnt].bUsed:=TRUE;
    If g_bLMM Then
    Begin
      OutStr('  rdlong __PC,__PC');
      OutStr('  long @@@'+g_asCmdLine[2]);
    End
    Else OutStr('  jmp #'+g_asCmdLine[2]);
  End
  Else Error(c_iErrorExitNotInLoop, 0);
  g_bHandled:=True;
End;


Procedure ProcessGosub;
Var
  iSubroutinePlace: Integer;
  iParamCnt: Integer;
Begin
  iSubroutinePlace:=SubIndex(g_asCmdLine[2]);
  iParamCnt:=0;
  If g_iCmdCnt > 3 Then
  Begin
    iParamCnt:=SetupParameters(4); // Start parsing at g_asCmdLine[4]
  End;
  If iSubroutinePlace > 0 Then
  Begin
    g_arSubroutines[iSubroutinePlace].bUsed:=True;
    If (iParamCnt < g_arSubroutines[iSubroutinePlace].iMinParams) or
       (iParamCnt > g_arSubroutines[iSubroutinePlace].iMaxParams) Then
    Begin
      Error(c_iErrorInvalidNumberOfParameters, 0);
    End
    Else
    Begin
      If g_arSubroutines[iSubroutinePlace].iMinParams <> g_arSubroutines[iSubroutinePlace].iMaxParams Then
      Begin
        OutStr('  mov __paramcnt,#'+IntStr(iParamCnt));
      End;
      If g_bLMM and (Not (g_arSubroutines[iSubroutinePlace].bIsNativeInLMM)) Then
      Begin
        OutStr('  jmp #_LMM_CALL');
        OutStr('  long @@@'+g_asCmdLine[2]);
      End
      Else OutStr('  call #'+g_asCmdLine[2]);
      g_bHandled:=True;
    End;
  End
  Else
  Begin // Not a declared sub
    Error(c_iErrorInvalidParameter, 2);
  End;
  g_bHandled:=True;
End;


Procedure ProcessReturn; // Return inside SUB/FUNC
Var
  Place: Integer;
Begin
  If g_iCmdCnt > 1 Then
  Begin
    If Not g_bInSub Then Error(c_iErrorNotInSuborFunc, 0);
    If (Not Odd(g_iCmdCnt)) and (g_iCmdCnt < 9) Then
    Begin
      Place:=2;
      Repeat
        If (Place > 2) and (g_asCmdLine[Place-1] <> ',') Then Error(c_iErrorCommaExpected, Place-1);
        Case g_apoCmdVars[Place]^.eGetType of
          e_LongVar: If Upper(g_apoCmdVars[Place]^.sGetName) <> '__PARAM'+IntStr(Place Div 2) Then LongVarGet(Place, Place Div 2);
          e_LongConst: LongConstGet(Place, Place Div 2);
          e_ShortConst: ShortConstGet(Place, Place Div 2);
          Else Error(c_iErrorInvalidParameter, Place);
        End; // Case
        Inc(Place, 2);
      Until Place > g_iCmdCnt;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End;
  If g_bLMM Then
  Begin
    OutStr('  jmp #_LMM_RET');
  End
  Else
  Begin
    OutStr(g_sSubName+'_ret');
    OutStr('  ret');
  End;
  g_bReturnUsed:=True;
  g_bHandled:=True;
End;

Procedure ProcessFreq;
Var
  lTemp: LongInt;
  sTemp: String[12];
  poVar: PVarObj;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_bInternalClock Then Warning(c_iWarningInternalClock,  1);
    If g_apoCmdVars[2]^.eGetType = e_LongConst Then
    Begin
      g_lFreq:=g_apoCmdVars[2]^.lGetValue;
      lTemp:=Trunc(g_lFreq / g_iPLL);
      Str(lTemp:9, sTemp);
      If Not g_bInternalClock Then OutSpin('  _XInFreq = '+ sTemp);
      g_bFreqSpecified:=True;
      Dispose(g_oDevice.m_oVars.m_apoVars[g_oDevice.m_oVars.m_iVarCnt], Done);
      Dec(g_oDevice.m_oVars.m_iVarCnt);
      // Create or change long constant _Freq
      poVar:=g_oDevice.pGetVarPtr('_FREQ');
      If poVar = Nil Then g_oDevice.pAddLongConst('_FREQ', g_lFreq, IntStr(g_lFreq))
      Else
      Begin
        poVar^.m_lValue:=g_lFreq;
        poVar^.m_sFormat:=IntStr(g_lFreq);
      End;
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessXIn;
Var
  iTemp: Integer;
  sTemp: String;
  poVar: PVarObj;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_bInternalClock Then Warning(c_iWarningInternalClock,  1);
    If g_apoCmdVars[2]^.eGetType = e_LongConst Then
    Begin
      // Remove "_" from freq
      sTemp:=g_asCmdLine[2];
      While Pos('_', sTemp) > 0 Do Delete(sTemp, Pos('_', sTemp), 1);
      Val(sTemp, g_lFreq, iTemp);
      If iTemp <> 0 Then Error(c_iErrorInvalidParameter, 2);
      Str(g_lFreq, sTemp);
      g_lFreq:=g_lFreq * g_iPLL;
      If Not g_bInternalClock Then OutSpin('  _XInFreq = '+ sTemp);
      g_bFreqSpecified:=True;
      // Delete frequecny value from variable list
      Dispose(g_oDevice.m_oVars.m_apoVars[g_oDevice.m_oVars.m_iVarCnt], Done);
      Dec(g_oDevice.m_oVars.m_iVarCnt);
      // Create or change long constant _Freq
      poVar:=g_oDevice.pGetVarPtr('_FREQ');
      If poVar = Nil Then g_oDevice.pAddLongConst('_FREQ', g_lFreq, IntStr(g_lFreq))
      Else
      Begin
        poVar^.m_lValue:=g_lFreq;
        poVar^.m_sFormat:=IntStr(g_lFreq);
      End;
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessEnd;
Begin
  If g_iCmdCnt > 1 Then Error(c_iErrorSyntaxError, 0);
  OutStr('  mov __temp1,#0');
  OutStr('  waitpne __temp1,__temp1');
  g_bHandled:=True;
End;

Procedure ProcessAddress;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
    Begin
      OutStr('org '+g_apoCmdVars[2]^.sGetName);
      g_bHandled:=True;
    End
    Else Error(c_iErrorConstantExpected, 2);
  End
  Else
  Begin
    Error(c_iErrorInvalidNumberOfParameters, 0);
  End;
End;

Procedure ProcessNOP;
Begin
  OutStr('  nop');
  g_bHandled:=True;
End;

Procedure ProcessOpenBrace;
Begin
  g_bMultiLineComment:=True;
  ProcessVerbatim;
  g_bHandled:=True;
End;


Procedure ProcessStack;
Begin
  If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
  Begin
    If g_apoCmdVars[2]^.lGetValue In [4..255] Then
    Begin
      g_iStackSize:=g_apoCmdVars[2]^.lGetValue;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorInvalidParameter, 2);
End;

// ====================================================================

Procedure CompileFile(pv_sGiven: String);
// Maybe called recusively by include
Var
  I, J: Integer;
  iTemp: Integer;
  lTemp: LongInt;
  tfIn: Text;
  sConst, sTemp: String[64];
  poTempVar: PVarObj;
  bInQuotes: Boolean;
  bReplaceInQuotes: Boolean;
  poNoVar: PVarObj;
  siTemp: Single;
  dTemp: Double;
  bHubAbs: Boolean;
  lHubOffset: LongInt;
  iCmdCnt: Integer;
  sCmdLine2, sCmdLine3: String[64];
  iLoadFiles: Integer;
  iHubBytes: Integer;
Begin
  iHubBytes:=0; // avoid lazarus warning
  g_bInAsm:=False;
  New(poNoVar, Init('TEMP', 0));
  If ExtractFilePath(pv_sGiven) = '' Then
  Begin
    GetDir(0, g_sCurrentFile);
    pv_sGiven:=g_sCurrentFile+''+pv_sGiven;
  End;
  g_sCurrentFile:=pv_sGiven;
  Assign(tfIn, pv_sGiven);
  {$I-}
  Reset(tfIn);
  {$I+}
  If IOResult = 0 Then
  Begin
    g_iInLineCnt:=0;
    While Not EOF(tfIn) Do
    Begin
      ReadLn(tfIn, sLine);
      Inc(g_iInLineCnt);
      Inc(g_iTotalLineCnt);
      g_oDevice.m_oVars.m_iLastVarCnt:=g_oDevice.m_oVars.m_iVarCnt;
      g_sOrigLine:=sLine;
      If g_eOutput = Full Then WriteLn(g_iTotalLineCnt:4, '  ',sLine);
      g_bHandled:=False;
      // Check for compiler directives
      I:=1;
      While ((I <= Length(sLine)) and ((sLine[I] = ' ') or (sLine[I] = #9))) Do Inc(I);
      If Upper(Copy(sLine, I, 3)) = '''{$' Then
      Begin
        If Pos('}', sLine) > 0 Then
        Begin
          sLine:=Copy(sLine, 1, Pos('}', sLine));
          If Upper(Copy(sLine, I, 8)) = '''{$ELSE}' Then ProcessElseDirective;
          If Upper(Copy(sLine, I, 9)) = '''{$ENDIF}' Then ProcessEndIfDirective;
          If Upper(Copy(sLine, I, 10)) = '''{$DEFINE ' Then
          Begin
            If g_bCompile Then ProcessDefineDirective(Copy(sLine, I+10, Length(sLine)-(I+10))) Else g_bHandled:=True;
          End;
          If Upper(Copy(sLine, I, 12)) = '''{$UNDEFINE ' Then
          Begin
            If g_bCompile Then ProcessUnDefineDirective(Copy(sLine, I+12, Length(sLine)-(I+12))) Else g_bHandled:=True;
          End;
          If Upper(Copy(sLine, I, 11)) = '''{$WARNING ' Then
          Begin
            If g_bCompile Then ProcessWarningDirective(Copy(sLine, I+11, Length(sLine) - (I+11))) Else g_bHandled:=True;
          End;
          If Upper(Copy(sLine, I, 9)) = '''{$ERROR ' Then
          Begin
            If g_bCompile Then ProcessErrorDirective(Copy(sLine, I+9, Length(sLine) - (I+9))) Else g_bHandled:=True;
          End;
          If Upper(Copy(sLine, I, 8)) = '''{$USES ' Then
          Begin
            If g_bCompile Then ProcessUsesDirective(Copy(sLine, I+8, Length(sLine) - (I+8))) Else g_bHandled:=True; // _MISC
          End;
          If Upper(Copy(sLine, I, 9)) = '''{$IFDEF ' Then
          Begin
            {If g_bCompile Then} ProcessIfDefDirective(Copy(sLine, I+9, Length(sLine)-(I+9))); // _MISC
          End;
          If Upper(Copy(sLine, I, 10)) = '''{$IFNDEF ' Then
          Begin
            {If g_bCompile Then} ProcessIfNDefDirective(Copy(sLine, I+10, Length(sLine)-(I+10))); // _MISC
          End;
          If Upper(Copy(sLine, I, 10)) = '''{$IFFREQ ' Then
          Begin
            {If g_bCompile Then} ProcessIfFreqDirective(Copy(sLine, I+10, Length(sLine)-(I+10))); // _MISC
          End;
          If Upper(Copy(sLine, I, 10)) = '''{$IFUSED ' Then
          Begin
            {If g_bCompile Then} ProcessIfUsedDirective(Copy(sLine, I+10, Length(sLine)-(I+10))); // _MISC
          End;
          If Upper(Copy(sLine, I, 11)) = '''{$IFNUSED ' Then
          Begin
            {If g_bCompile Then} ProcessIfNUsedDirective(Copy(sLine, I+11, Length(sLine)-(I+11))); // _MISC
          End;
          If Upper(Copy(sLine, I, 7)) = '''{$BST ' Then ErrorStr(c_iErrorDirective, 'Not Allowed');
          If Upper(Copy(sLine, I, 7)) = '''{$CODE' Then ProcessCodeDirective; // _MISC
          If Upper(Copy(sLine, I, 8)) = '''{$TASKS' Then ProcessTasksDirective; // _MISC
        End
        Else Error(c_iErrorSyntaxError, 0);
      End;
      If Not g_bCompile Then
      Begin
        g_bHandled:=True;
        sLine:='';
      End;
      If g_bInAsm or g_bMultiLineComment Then
      Begin
        // Check for '}' for end of multi-line comment
        If (Copy(sLine, 1, 1) = '}') or (Pos(' }', sLine) > 0) Then
        Begin
          ProcessVerbatim;
          g_bMultiLineComment:=False;
        End;
        // Check for ENDASM command
        I:=1;
        While ((I <= Length(sLine)) and ((sLine[I] = ' ') or (sLine[I] = #9))) Do Inc(I);
        If Copy(sLine, I, 3) = '''{$' Then
        Begin
          OutStr('  ');
          g_bHandled:=True;
        End
        Else If Upper(Copy(sLine, I, 6)) <> 'ENDASM' Then ProcessVerbatim Else g_bInAsm:=False;
      End;
      g_iCmdCnt:=0;
      For I:=0 to c_iMaxCmdCnt+1 Do
      Begin
        g_asCmdLine[I]:='';
        g_abDeleteTemp[I]:=False;
        g_asUnaryOperator[I]:='';
      End;
      // *** Process line ***
      // Anything after a ' put in g_asCmdLine[c_iMaxCmdCnt+1]
      bInQuotes:=False;
      I:=1;
      While (I <= Length(sLine)) and ((sLine[I] <> '''') or bInQuotes)  Do
      Begin
        If sLine[I] = '"' Then
        Begin
          bInQuotes:=Not bInQuotes;
        End;
        Inc(I);
      End;
      If (I <= Length(sLine)) and (sLine[I] = '''') Then
      Begin
        g_asCmdLine[c_iMaxCmdCnt+1]:=Copy(sLine, I, (Length(sLine)-I)+1);
        If sLine[1] = '''' Then
         sLine:=''
        Else
         sLine:=Copy(sLine, 1, I-1);
      End;
      // Remove trailing spaces
      If sLine <> '' Then
       While (Length(sLine) > 0) and ((sLine[Length(sLine)] = ' ') or (sLine[Length(sLine)] = #9)) Do Delete(sLine, Length(sLine), 1);
      // Put leading spaces in g_asCmdLine[0]
      While (Length(sLine) > 0) and ((sLine[1]=' ') or (sLine[1] = #9)) Do
      Begin
        g_asCmdLine[0]:=g_asCmdLine[0]+sLine[1];
        Delete(sLine, 1, 1);
      End;
      If (sLine <> '') and (sLine[Length(sLine)]=':') Then ProcessLabel;
      // Parse Line
      If sLine <> '' Then
      Begin
        g_iCmdCnt:=1;
        bInQuotes:=False;
        Repeat
          If bInQuotes Then
          Begin
            g_asCmdLine[g_iCmdCnt]:=g_asCmdLine[g_iCmdCnt]+sLine[1];
            If sLine[1] = '"' Then
            Begin
              If (Length(sLine) > 1) and (sLine[2] = '"') Then
              Begin
                Delete(sLine, 1, 1);
                g_asCmdLine[g_iCmdCnt]:=g_asCmdLine[g_iCmdCnt]+sLine[1];
              End;
              bInQuotes:=False;
            End;
          End
          Else
          Begin
            If sLine[1] In [' ',#9,'=','>','<','-','+','.',',','&','|','^','(',')','~','@','/','*','\'] Then
            Begin
              // Single parameter literal code was "If g_asCmdLine[g_iCmdCnt] <> '' Then Inc(g_iCmdCnt);"
              // Replacement code for multi parameter literals
              If g_asCmdLine[g_iCmdCnt] <> '' Then
              Begin
                If g_asCmdLine[g_iCmdCnt] <> ReplaceLiteral(g_asCmdLine[g_iCmdCnt]) Then
                Begin
                  g_asCmdLine[g_iCmdCnt]:=ReplaceLiteral(g_asCmdLine[g_iCmdCnt]);
                  I:=1;
                  bReplaceInQuotes:=False;
                  While I <= Length(g_asCmdLine[g_iCmdCnt]) Do
                  Begin
                    If g_asCmdLine[g_iCmdCnt, I] = '"' Then bReplaceInQuotes:=Not bReplaceInQuotes;
                    If Not bReplaceInQuotes Then
                    Begin
                      If g_asCmdLine[g_iCmdCnt,I] In [' ',#9,'=','>','<','-','+','.',',','&','|','^','(',')','~','@','/','*','\'] Then
                      Begin
                        sLine:=Copy(g_asCmdLine[g_iCmdCnt], I, Length(g_asCmdLine[g_iCmdCnt])-I+1)+sLine;
                        g_asCmdLine[g_iCmdCnt]:=Copy(g_asCmdLine[g_iCmdCnt], 1, I-1);
                        I:=Length(g_asCmdLine[g_iCmdCnt]);
                      End
                    End;
                    Inc(I);
                  End;
                End;
                If g_asCmdLine[g_iCmdCnt] <> '' Then Inc(g_iCmdCnt);
                If g_iCmdCnt > c_iMaxCmdCnt Then g_iCmdCnt:=c_iMaxCmdCnt;
              End;
              // End of Replacement code for multi parameter literals
              If (sLine[1] <> ' ') and (sLine[1] <> #9) Then
              Begin
                g_asCmdLine[g_iCmdCnt]:=sLine[1];
                // Handle two character operators //, **, */, &~
                If ((sLine[1] In ['>','=','<']) and (sLine[2] In ['>','=','<'])) or
                   ((sLine[1] = '/') and (sLine[2]='/')) or
                   ((sLine[1] = '*') and ((sLine[2] = '*') or (sLine[2] = '/'))) or
                   ((sLine[1] = '&') and (sLine[2] = '~')) Then
                Begin
                  g_asCmdLine[g_iCmdCnt]:=sLine[1]+sLine[2];
                  Delete(sLine, 1, 1);
                End;
                Inc(g_iCmdCnt);
                If g_iCmdCnt > c_iMaxCmdCnt Then g_iCmdCnt:=c_iMaxCmdCnt;
              End;
            End
            Else
            Begin
              Begin
                g_asCmdLine[g_iCmdCnt]:=g_asCmdLine[g_iCmdCnt]+sLine[1];
                If sLine[1] = '"' Then
                Begin
                  If (Length(sLine) > 1) and (sLine[2] = '"') Then
                  Begin
                    Delete(sLine, 1, 1);
                    g_asCmdLine[g_iCmdCnt]:=g_asCmdLine[g_iCmdCnt]+sLine[1];
                  End;
                  bInQuotes:=True;
                End;
              End;
            End;
          End;
          Delete(sLine, 1, 1);
          If sLine = '' Then
          Begin
            If g_asCmdLine[g_iCmdCnt] = '' Then Dec(g_iCmdCnt);
            // Replacement code for multi parameter literals
            If g_asCmdLine[g_iCmdCnt] <> '' Then
            Begin
              If g_asCmdLine[g_iCmdCnt] <> ReplaceLiteral(g_asCmdLine[g_iCmdCnt]) Then
              Begin
                g_asCmdLine[g_iCmdCnt]:=ReplaceLiteral(g_asCmdLine[g_iCmdCnt]);
                I:=1;
                bInQuotes:=False;
                While I <= Length(g_asCmdLine[g_iCmdCnt]) Do
                Begin
                  If g_asCmdLine[g_iCmdCnt, I] = '"' Then
                  Begin
                    If (I = 1) or ((I > 1) and (g_asCmdLine[g_iCmdCnt, I-1] <> '/')) Then // Check for /" escape sequence
                    Begin
                      bInQuotes:=Not bInQuotes;
                      If (I < Length(g_asCmdLine[g_iCmdCnt])) and (g_asCmdLine[g_iCmdCnt, I+1] = '"') Then Inc(I);
                    End;
                  End;
                  If Not bInQuotes Then
                  Begin
                    If g_asCmdLine[g_iCmdCnt, I] In [' ',#9,'=','>','<','-','+','.',',','&','|','^','(',')','~','@','/','*','\'] Then
                    Begin
                      sLine:=Copy(g_asCmdLine[g_iCmdCnt], I, Length(g_asCmdLine[g_iCmdCnt])-I+1)+sLine;
                      g_asCmdLine[g_iCmdCnt]:=Copy(g_asCmdLine[g_iCmdCnt], 1, I-1);
                      I:=Length(g_asCmdLine[g_iCmdCnt]);
                    End
                  End;
                  Inc(I);
                End;
              End;
            End;
          End;
          // End of Replacement code for multi parameter literals
        Until sLine='';
        If g_asCmdLine[g_iCmdCnt] = '' Then Dec(g_iCmdCnt);
        If g_iCmdCnt > c_iMaxCmdCnt Then g_iCmdCnt:=c_iMaxCmdCnt;

        // Convert number formats
        // Convert #Pin to pin number
        I:=0;
        While I < g_iCmdCnt Do
        Begin
          Inc(I);
          // Replace literals
          If g_asCmdLine[I,1] In ['0'..'9', '$', '%', '"', '#'] Then
          Begin // Literal
            sConst:=g_asCmdLine[I];
            If g_asCmdLine[I,1] <> '"' Then While Pos('_', sConst) > 0 Do Delete(sConst, Pos('_', sConst), 1);
            Case sConst[1] of
              '$','0'..'9':
                 Begin
                   If sConst[1] = '$' Then
                   Begin
                     Val(sConst, lTemp, iTemp);
                     dTemp:=lTemp;
                   End
                   Else Val(sConst, dTemp, iTemp);
                   If (dTemp > 2147483647.0) and (g_asCmdLine[I-1] <> '-')Then
                   Begin
                     dTemp:= dTemp - 4294967296.0;
                     If dTemp > -0.99999 Then Error(c_iErrorInvalidParameter, I);
                   End;
                   If ((g_asCmdLine[I-1] <> '-') and (dTemp > 2147483647.0)) or
                      ((g_ascmdLine[I-1] = '-') and (dTemp > 2147483648.0)) Then
                   Begin
                     Error(c_iErrorInvalidParameter, I);
                     dTemp:=0.0;
                   End;
                   lTemp:=Round(dTemp);
                   If (g_asCmdLine[I-1] <> '-') and (lTemp >= 0) and (lTemp < 512) Then
                   Begin
                     g_apoCmdVars[I]:=New(PShortConstObj, Init(sConst, lTemp, g_asCmdLine[I]));
                     g_abDeleteTemp[I]:=True;
                   End
                   Else
                   Begin
                     If sConst[1] = '$' Then
                     Begin
                       Delete(sConst, 1, 1); // Remove $
                       sConst:='hex'+sConst; // prepend "hex"
                     End;

                     If g_asCmdLine[I-1] = '-' Then
                     Begin // Handle negative constant
                       g_apoCmdVars[I]:=g_oDevice.pGetVarPtr('_minus'+sConst);
                       g_asCmdLine[I]:='-'+g_asCmdLine[I];
                       If g_apoCmdVars[I] = Nil Then
                       Begin
                         If lTemp < 0 Then g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_minus'+sConst, lTemp, g_asCmdLine[I])
                          Else g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_minus'+sConst, -lTemp, g_asCmdLine[I]);
                       End;
                       g_abDeleteTemp[I]:=False;
                     End
                     Else
                     Begin // Handle Positive constant
                       g_apoCmdVars[I]:=g_oDevice.pGetVarPtr('_'+sConst);
                       If g_apoCmdVars[I] = Nil Then g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_'+sConst, lTemp, g_asCmdLine[I]);
                       g_abDeleteTemp[I]:=False;
                     End;
                   End;
                End;
              '"':
                Begin
                  If (g_asCmdLine[I] = '""') or (g_asCmdLine[I, 3] = '"') or (Upper(g_asCmdLine[2]) = 'CON') Then
                  Begin // Single character
                    If g_asCmdLine[I] = '""' Then g_apoCmdVars[I]:=New(PShortConstObj, Init('0', 0, '0'))
                    Else g_apoCmdVars[I]:=New(PShortConstObj, Init(sConst, Ord(sConst[2]), g_asCmdLine[I]));
                    g_abDeleteTemp[I]:=True;
                  End
                  Else
                  Begin // Literal String (do not create for LOAD, INCLUDE, DATA or WATCH command)
                    If ((I <> 2) or ((Upper(g_asCmdLine[1]) <> 'LOAD') and (Upper(g_asCmdLine[1]) <> 'INCLUDE'))) and
                       ((Upper(g_asCmdLine[1]) <> 'DATA') and (Upper(g_asCmdLine[2]) <> 'DATA') and (Upper(g_asCmdLine[1]) <> 'WATCH')) Then
                    Begin
                      Inc(g_iStringCnt);
                      g_apoCmdVars[I]:=g_oDevice.pAddDataLabel('InLineStr'+IntStr(g_iStringCnt), 1);
                      g_oDevice.pAddByteData(g_asCmdLine[I]+',0', iHubBytes);
                      Inc(g_lHubOffset, iHubBytes);
                      g_abDeleteTemp[I]:=False;
                    End
                    Else
                    Begin
                      g_apoCmdVars[I]:=Nil;
                    End;
                  End;
                End;
              '%':
                Begin
                  lTemp:=0;
                  sTemp:=sConst;
                  Delete(sTemp, 1, 1); // Remove 1st %
                  If sTemp[1] = '%' Then
                  Begin // Quadernary value
                    Delete(sTemp, 1, 1);
                    lTemp:=Ord(sTemp[1]) - Ord('0');
                    If Length(sTemp) > 1 Then
                    Repeat
                      lTemp:=lTemp SHL 2;
                      Delete(sTemp, 1, 1);
                      lTemp:=lTemp + Ord(sTemp[1]) - Ord('0');
                    Until Length(sTemp) = 1;
                    Str(lTemp:16, sConst);
                    While sConst[1] = ' ' Do Delete(sConst, 1, 1);
                    If (lTemp >= 0) and (lTemp < 512) Then
                    Begin
                      g_apoCmdVars[I]:=New(PShortConstObj, Init(sConst, lTemp, g_asCmdLine[I]));
                      g_abDeleteTemp[I]:=True;
                    End
                    Else
                    Begin
                      If sConst[1] = '-' Then sConst[1]:='N';
                      g_apoCmdVars[I]:=g_oDevice.pGetVarPtr('_quad'+sConst);
                      If g_apoCmdVars[I] = Nil Then g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_quad'+sConst, lTemp, g_asCmdLine[I]);
                      g_abDeleteTemp[I]:=False;
                    End;
                  End
                  Else
                  Begin // Binary value
                    If sTemp[1] <> '0' Then Inc(lTemp);
                    If Length(sTemp) > 1 Then
                    Repeat
                      lTemp:=lTemp SHL 1;
                      Delete(sTemp, 1, 1);
                      If sTemp[1] <> '0' Then Inc(lTemp);
                    Until Length(sTemp) = 1;
                    Str(lTemp:16, sConst);
                    While sConst[1] = ' ' Do Delete(sConst, 1, 1);
                    If (lTemp >= 0) and (lTemp < 512) Then
                    Begin
                      g_apoCmdVars[I]:=New(PShortConstObj, Init(sConst, lTemp, g_asCmdLine[I]));
                      g_abDeleteTemp[I]:=True;
                    End
                    Else
                    Begin
                      If sConst[1] = '-' Then sConst[1]:='N';
                      g_apoCmdVars[I]:=g_oDevice.pGetVarPtr('_bin'+sConst);
                      If g_apoCmdVars[I] = Nil Then g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_bin'+sConst, lTemp, g_asCmdLine[I]);
                      g_abDeleteTemp[I]:=False;
                    End;
                  End;
                End;
              '#':
                Begin
                  If g_ascmdLine[1] <> '\' Then // Only convert if NOT an assembly line
                  Begin
                    Delete(sConst, 1, 1);
                    If (sConst = '') and (g_asCmdLine[I+1] = '-') Then
                    Begin
                      If g_asCmdLine[I+2] = '.' Then
                      Begin
                        sConst:=g_asCmdLine[I+1]+'0';
                        g_asCmdLine[I]:=g_asCmdLine[I] + g_asCmdLine[I+1] + '0';
                        For J:=I+1 TO g_iCmdCnt-1 Do g_asCmdLine[J]:=g_asCmdLine[J+1];
                        Dec(g_iCmdCnt, 1);
                      End
                      Else
                      Begin
                        sConst:=g_asCmdLine[I+1]+g_asCmdLine[I+2];
                        g_asCmdLine[I]:=g_asCmdLine[I] + g_asCmdLine[I+1]+g_asCmdLine[I+2];
                        For J:=I+1 TO g_iCmdCnt-2 Do g_asCmdLine[J]:=g_asCmdLine[J+2];
                        Dec(g_iCmdCnt, 2);
                      End;
                    End;
                    If (Length(sConst) = 0) or (sConst[1] In ['-', '0'..'9']) Then
                    Begin
                      If sConst = '' Then
                      Begin
                        sConst:='0';
                        g_asCmdLine[I]:=g_asCmdLine[I]+'0';
                      End;  
                      If g_asCmdLine[I+1] = '.' Then
                      Begin
                        If sConst[1] = '-' Then sTemp:='N'+Copy(sConst, 2, Length(sConst)-1)+'_'+g_ascmdLine[I+2]
                         Else sTemp:=sConst+'_'+g_asCmdLine[I+2];
                        sConst:=sConst +'.'+g_asCmdLine[I+2];
                        Val(sConst, siTemp, iTemp);
                        If iTemp > 0 Then Error(c_iErrorInvalidParameter, I);
                        Move(siTemp, lTemp, SizeOf(lTemp));
                        g_apoCmdVars[I]:=g_oDevice.pGetVarPtr('_f'+sTemp);
                        If g_apoCmdVars[I] = Nil Then g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_f'+sTemp, lTemp, sConst);
                        g_abDeleteTemp[I]:=False;

                        g_asCmdLine[I]:=g_asCmdLine[I]+'.'+g_asCmdLine[I+2];

                        For J:=I+1 TO g_iCmdCnt-2 Do g_asCmdLine[J]:=g_asCmdLine[J+2];
                        Dec(g_iCmdCnt, 2);
                      End
                      Else
                      Begin
                        If sConst[1] = '-' Then sTemp:='N'+Copy(sConst, 2, Length(sConst)-1)
                         Else sTemp:=sConst;
                        Val(sConst, siTemp, iTemp);
                        If iTemp > 0 Then Error(c_iErrorInvalidParameter, I);
                        Move(siTemp, lTemp, SizeOf(lTemp));
                        g_apoCmdVars[I]:=g_oDevice.pGetVarPtr('_f'+sTemp);
                        If g_apoCmdVars[I] = Nil Then g_apoCmdVars[I]:=g_oDevice.pAddLongConst('_f'+sTemp, lTemp, sConst+'.0');
                        g_abDeleteTemp[I]:=False;
                      End;
                    End
                    Else
                    Begin // Check for #Pin
                      poTempVar:=g_oDevice.pGetVarPtr(sConst);
                      If poTempVar <> Nil Then
                      Begin
                        If poTempVar^.eGetType = e_Pin Then
                        Begin
                          g_apoCmdVars[I]:=New(PShortConstObj, Init(poTempVar^.sGetPinNumber,
                           poTempVar^.m_byPinNumber, poTempVar^.sGetPinNumber));
                          g_abDeleteTemp[I]:=True;
                        End;
                      End;
                    End;
                  End;
                End;
            End; // Case
          End
          Else
          // Get Var names
          Begin
            g_apoCmdVars[I]:=g_oDevice.pGetVarPtr(g_asCmdLine[I]);
            If g_apoCmdVars[I] = Nil Then g_apoCmdVars[I]:=poNoVar;
          End;
        End; // While I < g_iCmdCnt
        // Check for Unary Operators
        For I:=2 to g_iCmdCnt Do
        Begin
          If Upper(g_asCmdLine[I]) = 'NOT' Then g_asCmdLine[I]:='~';
          If (g_ascmdLine[I] = '~') or
             ((g_asCmdLine[I] = '-') and (g_apoCmdVars[I-1]^.eGetType = e_Ukn) and (g_asCmdLine[I-1] <> ')')) Then
          Begin
            g_asUnaryOperator[I+1]:=g_asCmdLine[I];
            // Shift the rest down
            For J:=I to g_iCmdCnt-1 Do
            Begin
              g_apoCmdVars[J]:=g_apoCmdVars[J+1];
              g_asCmdLine[J]:=g_asCmdLine[J+1];
              g_asUnaryOperator[J]:=g_asUnaryOperator[J+1];
              g_abDeleteTemp[J]:=g_abDeleteTemp[J+1];
            End;
            Dec(g_iCmdCnt);
          End;
        End; // Check for Unary Operators
        // Check for Assumed LET
        If ((Upper(g_asCmdLine[2]) <> 'VAR') and (Upper(g_asCmdLine[2]) <> 'PIN') and (Upper(g_asCmdLine[2]) <> 'CON')) and
           (g_apoCmdVars[1]^.eGetType In [e_LongVar, e_Pin, e_ByteData, e_HubByte, e_DataLabel]) Then
        Begin // assume LET
          For I:=g_iCmdCnt downto 1 Do
          Begin
            g_apoCmdVars[I+1]:=g_apoCmdVars[I];
            g_asCmdLine[I+1]:=g_asCmdLine[I];
            g_asUnaryOperator[I+1]:=g_asUnaryOperator[I];
            g_abDeleteTemp[I+1]:=g_abDeleteTemp[I];
          End;
          g_asCmdLine[1]:='LET';
          g_apoCmdVars[1]:=Nil;
          g_asUnaryOperator[1]:='';
          g_abDeleteTemp[1]:=False;
          Inc(g_iCmdCnt);
        End;
        // Check for Assumed GOSUB
        i:=1;
        While (I < g_iSubroutineCnt) and (g_arSubroutines[I].sLabel <> Upper(g_asCmdLine[1])) Do Inc(I);
        If (I <= g_iSubroutineCnt) and (g_arSubroutines[I].sLabel = Upper(g_asCmdLine[1])) Then
        Begin // assume GOSUB
          If g_iCmdCnt = 1 Then
          Begin
            For I:=g_iCmdCnt downto 1 Do
            Begin
              g_apoCmdVars[I+1]:=g_apoCmdVars[I];
              g_asCmdLine[I+1]:=g_asCmdLine[I];
              g_asUnaryOperator[I+1]:=g_asUnaryOperator[I];
              g_abDeleteTemp[I+1]:=g_abDeleteTemp[I];
            End;
            g_asCmdLine[1]:='GOSUB';
            g_apoCmdVars[1]:=Nil;
            Inc(g_iCmdCnt);
          End
          Else
          Begin
            For I:=g_iCmdCnt downto 2 Do
            Begin
              g_apoCmdVars[I+2]:=g_apoCmdVars[I];
              g_asCmdLine[I+2]:=g_asCmdLine[I];
              g_asUnaryOperator[I+2]:=g_asUnaryOperator[I];
              g_abDeleteTemp[I+2]:=g_abDeleteTemp[I];
            End;
            g_asCmdLine[2]:=g_asCmdLine[1];
            g_abDeleteTemp[2]:=False;
            g_apoCmdVars[2]:=Nil;
            g_asCmdLine[1]:='GOSUB';
            g_apoCmdVars[1]:=Nil;
            g_asCmdLine[3]:=',';
            g_abDeleteTemp[3]:=False;
            Inc(g_iCmdCnt, 2);
          End;
        End;

        // Translate @Var into Const
        // Translate @Pin into pin mask (long const)
        I:=2;
        While (Upper(g_asCmdLine[2]) <> 'VAR') and (I < g_iCmdCnt) Do
        Begin
          If g_asCmdLine[I]='@' Then
          Begin
            Case g_apoCmdVars[I+1]^.eGetType of
              e_LongVar:
                Begin
                  poTempVar:=New(PShortConstObj, Init(g_apoCmdVars[I+1]^.sGetName,
                   g_apoCmdVars[I+1]^.lGetValue, IntStr(g_apoCmdVars[I+1]^.lGetValue)));
                  g_abDeleteTemp[I]:=True;
                  g_apoCmdVars[I]:=poTempVar;
                  g_asCmdLine[I]:=poTempVar^.sGetName;
                  For J:=I+2 to g_iCmdCnt Do
                  Begin
                    g_apoCmdVars[J-1]:=g_apoCmdVars[J];
                    g_asCmdLine[J-1]:=g_asCmdLine[J];
                    g_asUnaryOperator[J-1]:=g_asUnaryOperator[J];
                    g_abDeleteTemp[J-1]:=g_abDeleteTemp[J];
                  End;
                  Dec(g_iCmdCnt, 1);
                  Inc(I);
                End;
              e_Pin:
                Begin
                  poTempVar:=New(PLongConstObj, Init(g_apoCmdVars[I+1]^.sGetName,
                   g_apoCmdVars[I+1]^.lGetValue, IntStr(g_apoCmdVars[I+1]^.lGetValue)));
                  g_abDeleteTemp[I]:=True;
                  g_apoCmdVars[I]:=poTempVar;
                  g_asCmdLine[I]:=poTempVar^.sGetName;
                  For J:=I+2 to g_iCmdCnt Do
                  Begin
                    g_apoCmdVars[J-1]:=g_apoCmdVars[J];
                    g_asCmdLine[J-1]:=g_asCmdLine[J];
                    g_asUnaryOperator[J-1]:=g_asUnaryOperator[J];
                    g_abDeleteTemp[J-1]:=g_abDeleteTemp[J];
                  End;
                  Dec(g_iCmdCnt, 1);
                  Inc(I);
                End;
              End; // Case
          End;
          Inc(I);
        End; // While I < g_iCmdCnt

        // Translate "ArrayName ( ByteVar|ConstVar )" into arrayvar
        I:=2;
        While I < g_iCmdCnt Do
        Begin
          If (g_asCmdLine[I]='(') and (g_asCmdLine[I+2]=')') Then
          Begin
            If (g_apoCmdVars[I-1]^.eGetType = e_LongVar) and
               (g_apoCmdVars[I+1]^.eGetType In [e_ShortConst, e_LongVar]) Then
            Begin // array(const|var)
              If g_apoCmdVars[I+1]^.eGetType = e_ShortConst Then
              Begin // Constant index
                g_apoCmdVars[I-1]:=New(PLongVarObj, Init(g_apoCmdVars[I-1]^.sGetName, False, g_apoCmdVars[I+1]^.sGetName));
//                g_apoCmdVars[I-1]^.m_sAlias:=g_apoCmdVars[I-1]^.m_sAlias + '('+IntStr(g_apoCmdVars[I+1]^.lGetValue)+')';
                g_abDeleteTemp[I-1]:=True;
              End
              Else
              Begin // LongVar index
                g_apoCmdVars[I-1]:=New(PLongVarObj, Init(g_apoCmdVars[I-1]^.sGetName, True, g_apoCmdVars[I+1]^.sGetName));
//                g_apoCmdVars[I-1]^.m_sAlias:=g_apoCmdVars[I-1]^.m_sAlias + '('+g_apoCmdVars[I+1]^.sGetName+')';
                g_abDeleteTemp[I-1]:=True;
              End;
              // Delete parens and index from list
              For J:=I+3 to g_iCmdCnt Do
              Begin
                g_apoCmdVars[J-3]:=g_apoCmdVars[J];
                g_asCmdLine[J-3]:=g_asCmdLine[J];
                g_asUnaryOperator[J-3]:=g_asUnaryOperator[J];
                g_abDeleteTemp[J-3]:=g_abDeleteTemp[J];
              End;
              Dec(g_iCmdCnt, 3);
              Inc(I);
            End // If
            Else If (g_apoCmdVars[I-1]^.eGetType In [e_HubByte, e_HubWord, e_HubLong, e_DataLabel, e_ByteData, e_WordData, e_LongData]) and
               (g_apoCmdVars[I+1]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar]) Then
            Begin // hubarray(const|var)
              lHubOffset:=g_apoCmdVars[I-1]^.m_lHubOffset;
              bHubAbs:=g_apoCmdVars[I-1]^.m_bHubAbs;
              If g_apoCmdVars[I+1]^.eGetType In [e_ShortConst, e_LongConst] Then
              Begin // Constant index
                Case g_apoCmdVars[I-1]^.eGetType of
                  e_HubByte, e_ByteData, e_DataLabel:
                  Begin
                    Inc(lHubOffset,g_apoCmdVars[I+1]^.lGetValue);
                    g_apoCmdVars[I-1]:=New(PHubByteObj, Init(g_apoCmdVars[I-1]^.sGetName, lHubOffset, 1, 0, False, g_apoCmdVars[I+1]^.sGetName, g_apoCmdVars[I+1]^.lGetValue));
                    g_abDeleteTemp[I-1]:=True;
                  End;
                  e_HubWord, e_WordData:
                  Begin
                    Inc(lHubOffset,g_apoCmdVars[I+1]^.lGetValue * 2);
                    g_apoCmdVars[I-1]:=New(PHubWordObj, Init(g_apoCmdVars[I-1]^.sGetName, lHubOffset, 1, 0, False, g_apoCmdVars[I+1]^.sGetName, g_apoCmdVars[I+1]^.lGetValue));
                    g_abDeleteTemp[I-1]:=True;
                  End;
                  e_HubLong, e_LongData:
                  Begin
                    Inc(lHubOffset,g_apoCmdVars[I+1]^.lGetValue * 4);
                    g_apoCmdVars[I-1]:=New(PHubLongObj, Init(g_apoCmdVars[I-1]^.sGetName, lHubOffset, 1, 0, False, g_apoCmdVars[I+1]^.sGetName, g_apoCmdVars[I+1]^.lGetValue));
                    g_abDeleteTemp[I-1]:=True;
                  End;
                End; // Case
              End
              Else
              Begin // LongVar index
                Case g_apoCmdVars[I-1]^.eGetType of
                  e_HubByte, e_ByteData, e_DataLabel:
                  Begin
                    g_apoCmdVars[I-1]:=New(PHubByteObj, Init(g_apoCmdVars[I-1]^.sGetName, lHubOffset, 1, 0, True, g_apoCmdVars[I+1]^.sGetName, 0));
                    g_abDeleteTemp[I-1]:=True;
                  End;
                  e_HubWord, e_WordData:
                  Begin
                    g_apoCmdVars[I-1]:=New(PHubWordObj, Init(g_apoCmdVars[I-1]^.sGetName, lHubOffset, 1, 0, True, g_apoCmdVars[I+1]^.sGetName, 0));
                    g_abDeleteTemp[I-1]:=True;
                  End;
                  e_HubLong, e_LongData:
                  Begin
                    g_apoCmdVars[I-1]:=New(PHubLongObj, Init(g_apoCmdVars[I-1]^.sGetName, lHubOffset, 1, 0, True, g_apoCmdVars[I+1]^.sGetName, 0));
                    g_abDeleteTemp[I-1]:=True;
                  End;
                End; // Case
              End;
              g_apoCmdVars[I-1]^.m_bHubAbs:=bHubAbs;

              // Delete parens and index from list
              For J:=I+3 to g_iCmdCnt Do
              Begin
                g_apoCmdVars[J-3]:=g_apoCmdVars[J];
                g_asCmdLine[J-3]:=g_asCmdLine[J];
                g_asUnaryOperator[J-3]:=g_asUnaryOperator[J];
                g_abDeleteTemp[J-3]:=g_abDeleteTemp[J];
              End;
              Dec(g_iCmdCnt, 3);
              Inc(I);
            End;
          End;
          Inc(I);
        End; // While I < g_iCmdCnt


// ----------------------------------------------------------------------------------------------

        If (Not g_bHandled) and (g_bCompoundIfPending) Then
        Begin
          //Insert IF as first parameter (LET will be there since a var was the first parameter)
          ProcessIf{(g_iCmdCnt, 2, g_iCmdCnt)};
        End;

        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'DEVICE') Then ProcessDevice;
        If (Not g_bHandled) and ((Upper(g_asCmdLine[2]) = 'DATA') or (Upper(g_asCmdlIne[2]) = 'WDATA') or
            (Upper(g_asCmdLine[2]) = 'LDATA')) Then ProcessData_Declare; // _READ
        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'HUB') Then ProcessHub; // _var
        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'CON') Then ProcessCon; // _var
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'DATA') Then ProcessData; // _Read
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WDATA') Then ProcessWData; // _Read
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LDATA') Then ProcessLData; // _Read

        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'VAR') Then ProcessVar; // _VAR
        If (Not g_bHandled) and (g_asCmdLine[1] = '\') Then ProcessVerbatim;

        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'PIN') Then ProcessPin; // _VAR
        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'FUNC') Then ProcessFunc_Declare;
        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'SUB') Then ProcessSub_Declare; // _VAR
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'SUB') Then ProcessSub_Define;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ASM') Then ProcessAsm;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ENDASM') Then ProcessEndAsm;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'REM') Then ProcessRem;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'FUNC') Then ProcessFunc_Define; // _VAR
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ENDSUB') Then ProcessEndSub; // _VAR
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ENDFUNC') Then ProcessEndFunc; // _VAR
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'BRANCH') Then ProcessBranch; // _BRANCH
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'PROGRAM') Then ProcessProgram;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'INCLUDE') Then ProcessInclude;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOAD') Then ProcessLoad;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'PAUSE') Then ProcessPause;     // _PAUSE
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'PAUSEUS') Then ProcessPauseUS; // _PAUSE
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'END') Then ProcessEnd;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'INC') Then ProcessInc;   // _INC
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'DEC') Then ProcessDec;   // _INC
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'HIGH') Then ProcessHigh; // _HIGH
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOW') Then ProcessLow;   // _HIGH
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'TOGGLE') Then ProcessToggle; // _HIGH
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'GOTO') Then ProcessGoto; // PROP
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'GOSUB') Then ProcessGosub;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RETURN') Then ProcessReturn;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LET') Then ProcessLet;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'PRINT') Then ProcessPrint; // _Input
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'IF') Then ProcessIf;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ELSE') Then ProcessElse;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ELSEIF') Then ProcessElseIf;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ENDIF') Then ProcessEndIf;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'DO') Then ProcessDo;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOOP') Then ProcessLoop; // _DO
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'FOR') Then ProcessFor;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'NEXT') Then ProcessNext;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ADDRESS') Then ProcessAddress;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RANDOM') Then ProcessRandom;     // _Random
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'FREQ') Then ProcessFreq;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'XIN') Then ProcessXIn;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'SEROUT') Then ProcessSerOut;     // _Serial
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'SERIN') Then ProcessSerIn;       // _Serial
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'SHIFTOUT') Then ProcessShiftOut; // _Shift
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'SHIFTIN') Then ProcessShiftIn;   // _Shift
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RCTIME') Then ProcessRCTime;     // _RCTime
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'PULSIN') Then ProcessPulsIn;     // _Pulse
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'PULSOUT') Then ProcessPulsOut;   // _Pulse
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'INPUT') Then ProcessInput;       // _Input
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'OUTPUT') Then ProcessOutput;     // _Input
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'REVERSE') Then ProcessReverse;   // _Input
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'DJNZ') Then ProcessDJNZ;         // _MISC
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'I2CSTART') Then ProcessI2CStart; // _I2C
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'I2CSTOP') Then ProcessI2CStop;   // _I2C
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'I2CWRITE') Then ProcessI2CWrite; // _I2C
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'I2CREAD') Then ProcessI2CRead;   // _I2C
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'I2CSPEED') Then ProcessI2CSpeed; // _I2C
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'OWRESET') Then ProcessOWReset;   // _OneWire
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'OWREAD') Then ProcessOWRead;     // _OneWire
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'OWWRITE') Then ProcessOWWrite;   // _OneWire
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'EXIT') Then ProcessExit;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ON') Then ProcessOn; // _BRANCH
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'NOP') Then ProcessNOP;

        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RDLONG') Then ProcessRdLong;    // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RDWORD') Then ProcessRdWord;    // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RDSWORD') Then ProcessRdSWord;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RDBYTE') Then ProcessRdByte;    // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'RDSBYTE') Then ProcessRdSByte;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WRLONG') Then ProcessWrLong;    // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WRWORD') Then ProcessWrWord;    // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WRBYTE') Then ProcessWrByte;    // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WAITCNT') Then ProcessWaitCnt;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WAITVID') Then ProcessWaitVid;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WAITPEQ') Then ProcessWaitPeq;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WAITPNE') Then ProcessWaitPne;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'COUNTERA') Then ProcessCounterAB(False); // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'COUNTERB') Then ProcessCounterAB(True);  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOCKNEW') Then ProcessLockNew;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOCKRET') Then ProcessLockRet;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOCKSET') Then ProcessLockSet;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'LOCKCLR') Then ProcessLockClr;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'CLKSET') Then ProcessClkSet;  // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'STACK') Then ProcessStack;
        If (Not g_bHandled) and (Upper(g_asCmdLine[2]) = 'TASK') Then ProcessTask_Define; // _Prop
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'TASK') Then
        Begin
          iCmdCnt:=g_iCmdCnt;
          sCmdLine2:=g_asCmdLine[2];
          sCmdLine3:=g_asCmdLine[3];

          If g_iLoadFilesCode > 0 Then
          Begin
            iLoadFiles:=g_iLoadFilesCode;
            g_iLoadFilesCode:=0;
            For I:=1 to iLoadFiles Do
            Begin
              g_bCompile:=False;
              CompileFile(g_asLoadFiles[I]);
            End;

            g_bLoadTasks:=True;
            If g_iLoadFilesTasks > 0 Then
            Begin
              iLoadFiles:=g_iLoadFilesTasks;
              For I:=1 to iLoadFiles Do
              Begin
                g_bCompile:=False;
                CompileFile(g_asLoadFiles[I]);
              End;
              g_iLoadFilesTasks:=0;
            End;

            g_bCompile:=True;
            g_iCmdCnt:=iCmdCnt;
            g_asCmdLine[2]:=sCmdLine2;
            g_asCmdLine[3]:=sCmdLine3;
          End;
          ProcessTask;        // _Task
        End;

        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'ENDTASK') Then
        Begin
          OutStr('  mov __temp1,#0');
          OutStr('  waitpne __temp1,__temp1');
          ProcessEndTask;  // _Task
        End;
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'COGSTART') Then ProcessCogStart;// _Task
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'COGINIT') Then ProcessCogInit;  // _Task
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'COGSTOP') Then ProcessCogStop;  // _Task
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'COGID') Then ProcessCogID;      // _Task

        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'BREAK') Then ProcessBreak;       // _Debug
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'WATCH') Then ProcessWatch;       // _Debug
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'UNWATCH') Then ProcessUnwatch;   // _Debug
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'UPDATE') Then ProcessUpdate;     // _Debug

        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'VPCONFIG') Then ProcessVPConfig; // _Debug
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'VPVIDEO') Then ProcessVPVideo;   // _Debug
        If (Not g_bHandled) and (Upper(g_asCmdLine[1]) = 'VPWAIT') Then ProcessVPWait;     // _Debug

        If (Not g_bHandled) and ((Upper(g_asCmdLine[1]) = 'FILE') or (Upper(g_asCmdLine[2]) = 'FILE')) Then ProcessFile; // _READ

        If (Not g_bHandled) and (g_asCmdLine[1] = '{') Then ProcessOpenBrace;

        If Not g_bHandled Then
        Begin
          If g_iLastErrorLine <> g_iInLineCnt Then Error(c_iErrorUnknownCommand, 1);
        End;
        If Not g_bNoSource Then WriteLn(g_tfSrc); // Create blank line between commands
      End // If sLine <> ''
      Else
      Begin
        If g_asCmdLine[c_iMaxCmdCnt+1] <> '' Then OutStr('  ')
         Else If Not g_bHandled Then WriteLn(g_tfSrc);
      End;
      // Dispose of any ConstVarObj's created
      For I:=1 to g_iCmdCnt Do If g_abDeleteTemp[I] Then Dispose(g_apoCmdVars[I], Done);
    End; // While Not EOF(fIn)
    // Generate error for any FOR without NEXT loops
    If g_iForCnt <> 0 Then
    Begin
      g_asCmdLine[1]:=g_arForVars[1].poValue^.sGetName;
      Error(c_iErrorForWithoutNext, 1);
    End;
    Close(tfIn);
  End // If IOResult = 0
  Else
  Begin
    ErrorStr(c_iErrorCouldNotReadSourceFile, pv_sGiven);
  End;
  Dispose(poNoVar, Done);
End;

Var
  I, I2: Integer;
  sTemp: String;
  iLoadFiles: Integer;
  iWatchLongCnt: Integer;
  iWatchSize: Integer;
Begin
  g_lFreq:=12000000; // Default to  RCFAST 12MHz
  g_iPLL:=1; // Default
  g_bFreqSpecified:=True;
  g_bInternalClock:=True;
  If ParamCount > 0 Then
  Begin
    g_iInLineCnt:=0;
    g_iOutLineCnt:=0;
    g_iErrorCnt:=0;
    g_iWarningCnt:=0;
    g_iExitLabelCnt:=0;
    g_iForCnt:=0;
    g_iForAccum:=0;
    g_iIfCnt:=0;
    g_iIfAccum:=0;
    g_iDoCnt:=0;
    g_iDoAccum:=0;
    g_bUsedProgram:=False;
    g_eOutput:=Full;
    g_sOutputDir:='';
    g_bPause:=False;
    g_bPauseExit:=False;
    g_bNoSource:=False;
    g_bReturnUsed:=False;
    g_iDefinesCnt:=0;
    g_bCompile:=True;
    g_iCondCnt:=0;

    If (ParamStr(1) = '/v') or (ParamStr(1) = '/V') Then Halt(c_iExitCodeVersion);
    If ParamCount > 1 Then
    Begin
      For I:=2 to ParamCount Do
      Begin
        If Uppercase(ParamStr(I)) = '/Q' Then g_eOutput:=Quiet;
        If Uppercase(ParamStr(I)) = '/B' Then g_eOutput:=Brief;
        If Uppercase(ParamStr(I)) = '/P' Then g_bPause:=True;
        If Uppercase(ParamStr(I)) = '/E' Then g_bPauseExit:=True;
        If Uppercase(ParamStr(I)) = '/O' Then g_sOutputDir:=ParamStr(I+1);
        If Uppercase(ParamStr(I)) = '/NS' Then g_bNoSource:=True;
        If Uppercase(ParamStr(I)) = '/VP' Then
        Begin
          g_eDebugger:=e_ViewPort;
          AddDirective('VIEWPORT');
        End;
      End;
    End;
    If g_eOutput <> Quiet Then WriteLn;

    // Parse filename
    g_sProjectName:=ExtractFileName(ParamStr(1));
    // Remove extension from filename
    If Pos('.', g_sProjectName) > 1 Then
    Begin
      I:=Length(g_sProjectName);
      While g_sProjectName[I] <> '.' Do Dec(I);
      g_sProjectName:=Copy(g_sProjectName, 1, I-1);
    End;
    g_sInputDir:=ExtractFilePath(ParamStr(1));
    If (Length(g_sInputDir) <> 0) and (Copy(g_sInputDir, Length(g_sInputDir), 1) <> '/') Then
     g_sInputDir:=g_sInputDir+'/';
    If g_sOutputDir = '' Then g_sOutputDir:=g_sInputDir;
    {$I-}
    ChDir(g_sInputDir);
    {$I+}
    If IOResult = 0 Then
    Begin
        Assign(g_tfSrc, g_sOutputDir+g_sProjectName+'.spin');
        {$I-}
        ReWrite(g_tfSrc);
        {$I+}
        If IOResult = 0 Then
        Begin
          Assign(g_tfErr, g_sOutputDir+g_sProjectName+'.err');
          {$I-}
          Erase(g_tfErr);
          {$I+}
          If IOResult <> 0 Then
          Begin
            // Could not delete error file
          End;
          Begin
            If Not g_bNoSource Then WriteLn(g_tfSrc, '''''  *** COMPILED WITH PropBasic VERSION ', c_sVersion, '  ', c_sVerDate, ' ***');
            CompileFile(ParamStr(1));
            If Not g_bUsedProgram Then Error(c_iErrorNoProgramCommandUsed, 0);

            iLoadFiles:=g_iLoadFilesCode;
            g_iLoadFilesCode:=0;
            For I2:=1 to iLoadFiles Do
            Begin
              g_bCompile:=False;
              CompileFile(g_asLoadFiles[I2]);
            End;
            g_iLoadFilesCode:=0;

            g_bLoadTasks:=True;
            iLoadFiles:=g_iLoadFilesTasks;
            For I:=1 to iLoadFiles Do
            Begin
              g_bCompile:=False;
              CompileFile(g_asLoadFiles[I]);
            End;
            g_iLoadFilesTasks:=0;

            OutputVarsAndDelete(False, g_lProgInitDirA, g_lProgInitOutA, g_lProgInitDirB, g_lProgInitPinB, g_lProgInitDirC, g_lProgInitPinC, g_lProgInitDirD, g_lProgInitPinD);

            // Output HUB variables and DATA
            WriteLn(g_tfSrc);
            WriteLn(g_tfSrc, 'DAT');
            WriteLn(g_tfSrc, '__DATASTART');

            sTemp:='';
            For I2:=1 to g_oDevice.m_oVars.m_iVarCnt Do
            Begin
              With g_oDevice.m_oVars.m_apoVars[I2]^ Do
              Begin

                // Output HUB variables (except byte arrays being watched with viewport)
                If (eGetType In [e_HubLong, e_HubWord, e_HubByte]) and (m_bHubAbs = False) Then
                Begin
                  If (eGetType <> e_HubByte) or (m_iWatchPos = 0) or (g_eDebugger <> e_Viewport) Then
                  Begin
                    Write(g_tfSrc, PadStr(sGetName, 16));
                    If m_lElements > 0 Then
                    Begin
                      Case eGetType of
                        e_HubLong: Write(g_tfSrc, ' LONG ');
                        e_HubWord: Write(g_tfSrc, ' WORD ');
                        e_HubByte: Write(g_tfSrc, ' BYTE ');
                      End;
                      If m_sValue <> '' Then  Write(g_tfSrc, m_sValue)
                      Else
                      Begin
                        Write(g_tfSrc, sGetValue);
                        If m_lElements > 1 Then Write(g_tfSrc, '['+IntStr(m_lElements)+']');
                      End; // If m_sValue <> ''
                    End; // If m_lElements > 0
                  End;
                  WriteLn(g_tfSrc);
                End; // If eGetType

                // Output DATA lines
                If eGetType =e_DataLabel Then WriteLn(g_tfSrc, sGetName); // output data label
                If eGetType =e_DataFile Then WriteLn(g_tfSrc, '  FILE '+sGetName); // output data file
                If (eGetType In [e_LongData, e_WordData, e_ByteData]) Then
                Begin
                  If sGetName <> sTemp Then
                  Begin
                    Write(g_tfSrc, PadStr(sGetName, 16));
                    sTemp:=sGetName;
                  End
                  Else Write(g_tfSrc, PadStr('', 16));
                  Case eGetType of
                    e_LongData: Write(g_tfSrc, ' LONG ');
                    e_WordData: Write(g_tfSrc, ' WORD ');
                    e_ByteData: Write(g_tfSrc, ' BYTE ');
                  End;
                  WriteLn(g_tfSrc, m_sFormat);
                End; // IF
              End; // With
            End; // For With
            WriteLn(g_tfSrc);

            If g_eDebugger = e_Viewport Then
            Begin
              iWatchLongCnt:=g_iWatchPos;
              iWatchSize:=g_iWatchPos * 4 - 4;
              // Adjust iWatchLongCnt and iWatchSize for any strings being watched
              For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
              Begin
                With g_oDevice.m_oVars.m_apoVars[I]^ Do
                Begin
                  If (eGetType = e_HubByte) and (m_iWatchPos <> 0) Then
                  Begin
                    Dec(iWatchLongCnt);
                    iWatchSize:=iWatchSize - 4 + ((m_lElements+3) and $FFFC); // Keep LONG aligned
                  End;
                End;
              End; // For I

              // Write HUB space for viewport WATCH variables
              If iWatchLongCnt = 0 Then WriteLn(g_tfSrc, '                 LONG 0 '' LONG Align'); // Need at least 1 element to LONG align strings
              WriteLn(g_tfSrc, 'VP_Watch         LONG 0['+IntStr(iWatchLongCnt)+']');

              // Write any strings that are being watched (in watched order)
              For I2:=1 to g_iWatchPos Do
              Begin
                For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
                Begin
                  With g_oDevice.m_oVars.m_apoVars[I]^ Do
                  Begin
                    If (eGetType = e_HubByte) and (m_iWatchPos <> 0) and (Abs(m_iWatchPos) = I2) Then
                    Begin
                      Write(g_tfSrc, PadStr(sGetName, 16), ' BYTE ');
                      If m_sValue <> '' Then  Write(g_tfSrc, m_sValue)
                      Else
                      Begin
                        Write(g_tfSrc, sGetValue);
                        If m_lElements > 1 Then Write(g_tfSrc, '['+IntStr(m_lElements)+']');
                      End;
                      WriteLn(g_tfSrc);
                      If m_lElements <> ((m_lElements + 3) and $FFFC) Then
                      Begin
                        // Pad with bytes to make LONG aligned
                        WriteLn(g_tfSrc, '                 BYTE 0['+IntStr(((m_lElements + 3) and $FFFC) - m_lElements)+']');
                      End;
                    End; // If
                  End; // With
                End; // For I
              End; // For I2

              // Write Viewport Configuration
              OutSpin('');
              OutSpin('PUB vp_config');
              If Length(g_sWatchStrings) > 0 Then Delete(g_sWatchStrings, Length(g_sWatchStrings), 1); // Remove trailing ","
              If g_sWatchPins <> '' Then
              Begin
                Delete(g_sWatchPins, Length(g_sWatchPins), 1);
                g_sWatchPins:='bits=['+g_sWatchPins+'],';
              End;
              If g_sVPVideoBuffer <> '' Then
              Begin
                OutSpin('  '+g_sVPVideoBuffer+'[0]:=7');
                OutSpin('  '+g_sVPVideoBuffer+'[1]:=0');
                OutSpin('  '+g_sVPVideoBuffer+'[2]:=0');
                OutSpin('  '+g_sVPVideoBuffer+'[3]:=0');
                OutSpin('  vp.register(@'+g_sVPVideoBuffer+')');
                OutSpin('  vp.config(string("video:source=prop"))');// view video from prop
                OutSpin('  vp.config(string("start:video"))');      //start in video view
              End;
              OutSpin('  vp.config(STRING("var:'+g_sWatchArrays+'IO('+g_sWatchPins+'base=2),'+g_sWatchLongs+g_sWatchStrings+'"))'); // Tell viewport the hub variable names
              OutSpin('  vp.monitorINA(@VP_watch)');
              For i:=1 to g_iVPConfigCnt Do OutSpin('  vp.config(@'+g_asVPConfig[i]+')');
              For i:=1 to g_iVPArrayCnt Do OutSpin('  vp.array(@'+g_asVPArrays[i]+','+IntStr(i+4)+')');
              OutSpin('  return vp.share(@VP_Watch,@VP_Watch+'+IntStr(iWatchSize)+')');           // Range of HUB vars to show in viewport, returns pointer to packetn,oktosend
              OutSpin('');
            End; // If g_eDebugger = e_Viewport

            Close(g_tfSrc);
            If g_eOutput <> Quiet Then
            Begin
              WriteLn;
              WriteLn('PropBasic Version ', c_sVersion, ' ', c_sVerDate);
              WriteLn('Finished Compile. ', g_iTotalLineCnt, ' Lines Read, ', g_iOutLineCnt, ' Lines Generated,  ', g_iWarningCnt, ' Warnings, ', g_iErrorCnt, ' Errors.');
            End;
            If g_bPauseExit Then ReadLn;
            If g_iErrorCnt = 0 Then Halt(c_iRetErrorCompileNoErrors) Else Halt(c_iRetErrorCompileErrors);
          End;
        End
        Else
        Begin
          WriteLn('Could not create output file ', ParamStr(1), '.spin');
          If g_bPause Then ReadLn;
          Halt(c_iRetErrorCouldNotCreateSrcFile);
        End;
    End
    Else Halt(c_iRetErrorInvalidDirectory);
  End
  Else
  Begin
    WriteLn;
    WriteLn('PropBasic Version ', c_sVersion, ' ', c_sVerDate);
    WriteLn('You must specify the complete path to the file to compile in quotes.');
    WriteLn;
    WriteLn('Switches:');
    WriteLn('  /Q = Quiet (No screen output)');
    WriteLn('  /P = Pause on warning or error (used to debug compiler)');
    WriteLn('  /E = Pause on exit');
    WriteLn('  /B = Brief output (does not show source code)');
    WriteLn('  /O = "Output_Directory" Specifies a diffrent directory for output files');
    WriteLn('  /V = Returns Version number as exit code (exit immediately)');
    WriteLn('  /NS = No Code (Does NOT include the BASIC code in the output file)');
    WriteLn('  /VP = Compiling for ViewPort');
    WriteLn;
    WriteLn('Example:');
    WriteLn('PropBasic "c:/myfiles/myprog.pbas" /p');
    WriteLn;
    WriteLn('Press ENTER to Quit:');
    ReadLn;
    Halt(c_iRetErrorNoFileSpecified);
  End

(*
----------------------------------------------------------------------------
Version 00.00.07
  Fixed: FUNC "RETURN const"
  Added: WAITPEQ, WAITPNE, WAITVID, WAITCNT
  Added: RCSLOW (20KHz), RCFAST (12MHz), XIN (instead of FREQ)
  Fixed: Literal constants "Baud CON "T19200"
  Added: SEROUT
----------------------------------------------------------------------------
Version 00.00.08
  Fixed: When a PIN variable is used as a SUB or FUNC parameter, it gives the pin number.
  Added: PIN INPUT, PIN OUTPUT, PIN HIGH, PIN LOW
  Fixed: WaitCnt, WaitVid, WaitPeq, WaitPne
----------------------------------------------------------------------------
Version 00.00.09
  FIXED: ASM...ENDASM do NOT change ' to ;
  Allow use of CLKFREQ (RDLONG CLKFREQ,temp)
  FIXED: Allow use of PROGRAM label NOSTARTUP
  Change LONG constants to binary for _InitDIRA & _InitOUTA
----------------------------------------------------------------------------
Version 00.00.10
  Added: SERIN (timeout not implemented yet)
----------------------------------------------------------------------------
Version 00.00.11
  Fixed: WRLONG longVar,const
  Fixed: String constant formating in spin file "Baud CON "T115200"
  Changed: Spacing in spin file. Needed more room for IF_C_AND_Z condition code
  Fixed: RDWORD, RDBYTE 1st parameter should be a HUB WORD or a HUB BYTE parameter
  Fixed: SERIN with timeout. Timeout is in milliseconds
----------------------------------------------------------------------------
Version 00.00.12
  Optimized: SEROUT T,N modes
  Added Abs, *, */, ** operators  (still need divide)
  Fixed: DO WHILE longVar cond LongVar
----------------------------------------------------------------------------
Version 00.00.13
  Change the way HUB variables and data works
  Implemented: DATA, WDATA, LDATA, GetAddr
    READ does not work, you need to use RDBYTE, RDWORD or RDLONG depending on
      what the data type you are reading
----------------------------------------------------------------------------
Version 00.00.14
  Fixed: Handle negative constants
  Added: I2C commands
  Added: ShiftIn, ShiftOut
  Added: COUNTERA, COUNTERB commands. COUNTERA mode [, apin [, bpin [, frqx [,phsx]]]]
----------------------------------------------------------------------------
Version 00.00.15
  Added: "/" and "//" operators and __REMAINDER system variable (same as __temp1)
----------------------------------------------------------------------------
Version 00.00.16
  Fixed: var = -value
  Fixed: "/" and "//" now support negative numbers
  Added: BRANCH, and ON...GOTO|GOSUB
  Changed: Programs that run in other cogs are now programed as TASKS
  Changed: DATA must be defined before it is used. I suggest this sequence:
    VARs, HUB, DATA
    When defining HUB variables, define any large arrays last, this will
      generate more effiect code. variables and data within the first 511 bytes
      use 1 less instruction.
----------------------------------------------------------------------------
Version 00.00.17
  Fixed: Allow PAUSEUS 4.7 (PAUSE does not support decimals, use PAUSEUS instead)
  Enhanced: Allow Long vars to be initialed "temp VAR LONG = 0"
  Fixed: __param1 = 1 << __param1
  Changed: Constants retain the format (binary, hex, dec)
  Fixed: Hex values over $7FFF_FFFF caused errors in DAT section
----------------------------------------------------------------------------
Version 00.00.18
  Fixed: temp = temp - shortcon ' Subtracts a negative value
----------------------------------------------------------------------------
Version 00.00.19
  Fixed: A single quote after comment ' is flagged as syntax error in DATA
  Fixed: LDATA and WDATA puts values in CON section
  Fixed: Short constants to keep their format
  Fixed: Long constants keep format but are duplicated "value CON $AABBCCDD"
----------------------------------------------------------------------------
Version 00.00.20
  Added: PULSIN pin,state,var
  Added: RCTIME pin,state,var
  Added: RANDOM var[,var]
----------------------------------------------------------------------------
Version 00.00.21
  Changed: I2CRECV to I2CREAD
  Changed: I2CSEND to I2CWRITE
---------------------------------------------------------------------------
Version 00.00.22
  Added: Arrays
----------------------------------------------------------------------------
Version 00.00.23
  Added: STR array, value, digits
  Added: SEROUT open modes
  Added: 1-Wire support (not tested)
----------------------------------------------------------------------------
Version 00.00.24
  Fixed: RDBYTE,RDWORD,RDLONG,WRBYTE,WRWORD,WRLONG when hub offset > 511
----------------------------------------------------------------------------
Version 00.00.25
  Fixed: "var = -pin" and "var = ~pin"
  Added: #pin = Pin Number, @pin = Pin Mask
----------------------------------------------------------------------------
Version 00.00.26
  Fixed: CON lines get spaced way over (use OutSpin instead of outStr)
  Fixed: Problem with *, / where 2nd parameter was loaded as first
  Fixed: Problem will step -longconst
----------------------------------------------------------------------------
Version 00.00.27
  Fixed: EXIT inside a FOR...NEXT
  Fixed: IF pin ' forgot NR on end of instruction
  Fixed: DATA offsets not calculated correctly
  Revised: INVALID.TXT and RESERVED.TXT
----------------------------------------------------------------------------
Version 00.00.28
  Fixed: Strings in DATA
  Added: PAUSE may have fractional value "PAUSE 10.5"
  Fixed: TASKS should NOT set dira or outa (holds output)
  Fixed: Pin = var tests pin instead of var (use MUXNZ too)
----------------------------------------------------------------------------
Version 00.00.29
  Fixed: Long constants were not available in TASKs
  Fixed: First variable name was not allowed in TASKs
  Fixed: ENDTASK generates "jmp #$" (same as END)
----------------------------------------------------------------------------
Version 00.00.30
  Added: pin groups "LEDS PIN 16..23 LOW"
  Changed: Make system variable names lower case (ina, outa, dira, par, etc)
  Changed: Using extension .pbas instead of .pb (some other programs use .pb)
----------------------------------------------------------------------------
Version 00.00.31
  Added: LOCKs (LOCKNEW, LOCKRET, LOCKSET, LOCKCLR)
    LOCKCLR id [,var] sets var to previous lock state
    LOCKSET id [,var] sets var to previous lock state
    LOCKNEW var Checks out a lock. -1 if no lock available (carry set)
    LOCKRET var Returns a previous checked out lock
  Added: Compound IF statements
    "IF var cond value OR"  and "IF var cond value AND"
  Changed: "/p" switch doesn't pause at completion, only at errors
----------------------------------------------------------------------------
Version 00.00.32
  Changed: Compound IF doesn't use "IF" on successive lines (One "IF", one "EndIF")
----------------------------------------------------------------------------
Version 00.00.33
  Fixed: RANDOM
  Fixed: IF ... OR ... LABEL (or EXIT)
----------------------------------------------------------------------------
Version 00.00.34
  Optimized: RANDOM per Chip's suggestion
  Changed: TASK init so that COGs and be started and stopped.
    TASKS now do NOT start automatically, but must be started with the CogStart command.
    COGSTART taskname
  Added: CogStart, CogStop, CogID commands
  Change: Allow empty data lines "FONT DATA" to let include files work easier
----------------------------------------------------------------------------
Version 00.00.35
  Added: COGINIT taskname, cogid
  Changed: Main code is always run in COG7 leaving cog0 - cog6 available
----------------------------------------------------------------------------
Version 00.00.36
  Fixed: tastname_COG_ofs was > 511 caused error
  Changed: Main program runs in cog 0
  Added: LOOP var ' Translates to optimal DJNZ
  Fixed: IF..AND inside another IF..THEN
----------------------------------------------------------------------------
Version 00.00.37
  Fixed: IF..OR invalid label "_IF_0"
----------------------------------------------------------------------------
Version 00.00.38
  Fixed: IF ... OR ... THEN EXIT
  Added: SERIN timeout and label
  Fixed: _bitDelay needs to be unique for each baud rate
----------------------------------------------------------------------------
Version 00.00.39
  Added: FILE command
    [label] FILE "filename.ext"
----------------------------------------------------------------------------
Version 00.00.40
  Fixed: PAUSEUS __PARAM1
----------------------------------------------------------------------------
Version 00.00.41
  Fixed: OWWRBYTE pin, const
----------------------------------------------------------------------------
Version 00.00.42
  Added: COGSTOP without parameter stop the current cog
  Changed: GETADDR, RDxxxx, WRxxxx
    now allows subscripts to hub var & better code generation
----------------------------------------------------------------------------
Version 00.00.43
  Fixed: GETADDR, RDxxxx, WRxxxx offsets match size
----------------------------------------------------------------------------
Version 00.00.44
  Fixed: Negative values not working properly
  Fixed: array element = array element + shortconst
  Fixed: array element = array element - shortconst
  Fixed: Hub label used as a SUB parameter, now passed it's address
  Fixed: Tabs used in DATA line
  Fixed: var = var - const
----------------------------------------------------------------------------
Version 00.00.45
  Fixed: 1-Wire commands
----------------------------------------------------------------------------
Version 00.00.46
  Changed: END puts COG in low-power mode
  Changed: Removed OWRDBIT and OWWRBIT, renamed OWWRBYTE and OWRDBYTE to OWREAD and OWWRITE
  Changed: Allow OWREAD and OWWRITE accept "\bits" parameter like SHIFTIN,SHIFTOUT
  Changed: SERIN and SEROUT long constants are named by baudrate
  Added:   Can alias variables "myVar VAR __param1"
----------------------------------------------------------------------------
Version 00.00.47
  Fixed: Error for FILE is source file doesn't exist
  Fixed: Locks up at ENDTASK command
----------------------------------------------------------------------------
Version 00.00.48
  Added: Add support for quaternary number prefix %%
  Added: INC var,value   DEC var,value
  Changed: STR now supresses leading zeros
----------------------------------------------------------------------------
Version 00.00.49
  Added: Long constant _Freq hold clock frequency
  Added: Allow computed constants
----------------------------------------------------------------------------
Version 00.00.50
  Fixed: % for binary numbers not working right video.pbas
  Fixed: STR is blank for value zero when leading spaces is used.
----------------------------------------------------------------------------
Version 00.00.51
  Fixed: Only output used variables and constants in TASKS
  Fixed: '{$USES pin|const}     {
  Changed: Use shifts for * and / by powers of two
  Fixed: IF ... OR ... THEN EXIT
----------------------------------------------------------------------------
Version 00.00.52
  Fixed: RDxxxx compiler loops if invalid parameter is used.
  Fixed: Allow PAUSE and PAUSEUS to allow parameter of zero or negative (no pause)
  Fixed: Negative "FOR" limit value generate invalid symbol name
  Fixed: On var GOSUB
  Fixed: Allow NEXT to support negative step variables (bDirPositive, bAddStep)
  Fixed: % and %% prefixes always made first digit zero
----------------------------------------------------------------------------
Version 00.00.53
  Fixed: _FREQ doesn't exist if XIN is used instead of FREQ
  Fixed: $WARNING in spin code prefixed with a ; should be a '
  Added: Support inline strings as SUB parameters
----------------------------------------------------------------------------
Version 00.00.54
  Fixed: Mult and Div by power of 2 when other value was a short constant
----------------------------------------------------------------------------
Version 00.00.55
  Fixed: Open modes for SEROUT and SERIN
----------------------------------------------------------------------------
Version 00.00.56
  Fixed: var1 = -var1 ' Didn't generate any code
----------------------------------------------------------------------------
Version 00.00.57
  Fixed: Variables disappearing. HUB was deleting variables
  Fixed: var = var / badvar ' doesn't give an error
----------------------------------------------------------------------------
Version 00.00.58
  Fixed: Variables disappearing. WDATA, LDATA was deleting variables
----------------------------------------------------------------------------
Version 00.00.59
  Changed: SUBs and FUNCs can have up to 20 parameters.
  Changed: Create .err file if any errors or warnings were encountered
    ERROR,filename,Line#,Error#,Error text
    WARNING,filename,Line#,Warning#,Warning text
----------------------------------------------------------------------------
Version 00.00.60
  Fixed: name CON constant - 100
  Fixed: PULSOUT, PULSOUT, RCTIME in TASK _1uSec not defined
----------------------------------------------------------------------------
Version 00.00.61
  Fixed: name CON constant - constantname
----------------------------------------------------------------------------
Version 00.00.62
  Fixed: Errors and Warnings
----------------------------------------------------------------------------
Version 00.00.63
  Fixed: SHIFTOUT \bits Gave "Comma expected" error
  Fixed: ENDTASK to put cog in low-power mode (like END)
----------------------------------------------------------------------------
Version 00.00.64
  Fixed: SHIFTIN MSBPOST has an extra XOR dira,__temp2 (toggle clock) instruction
----------------------------------------------------------------------------
Version 00.00.65
  Fixed: Allow RETURN to pass more than one value.
    Value get placed into __paramx variables
----------------------------------------------------------------------------
Version 00.00.66
  Fixed: "IF phsa" ' phsa & phsb are shadow registers and cannot be used
                     in the dest field of a read/write instruction like cmps
----------------------------------------------------------------------------
Version 00.00.67
  Fixed: DO..LOOP missing WZ, WC on cmps
  Need to fix: LOAD create an inline string with filename (INCLUDE)
----------------------------------------------------------------------------
Version 00.00.68
  Fixed: RCFAST (12MHz internal) clock mode assumed
----------------------------------------------------------------------------
Version 00.00.69
  Fixed: Internal clock warning when using XTAL clock modes
  Fixed: RCFAST, RCSLOW create _FREQ value
  Fixed: XIN gives warning if RCFAST or RCSLOW clock mode used
----------------------------------------------------------------------------
Version 00.00.70
  Changed: FREQ with RCSLOW or RCFAST gives warning
  Fixed: Do not emit _XInFreq line for RCSLOW or RCFAST
  Fixed: PAUSEUS with RCSLOW and short constants now emits NOPs
----------------------------------------------------------------------------
Version 00.00.71
  Fixed: Allow pin groups to be assigned a value > 511
  Changed: Pin groups are now MSBpin..LSBpin to match spin format
  Changed: Support for reverse pin groups
----------------------------------------------------------------------------
Version 00.00.72
  No changes
----------------------------------------------------------------------------
Version 00.00.73
  Fixed: eliminate "mov __temp1,__temp1" type instructions
          __temp1 = PinGroup
          PinGroup = __temp1
  Fixed: var = var / 2 ' Doesnt' work for negative numbers. Change SHR to SAR.
  Fixed: STR using 10 places (overflows)
     If STR option is 2,3 (signed) first char is sign "-" or " "
     Allows 11 places for option 2 or 3 (negative values)
----------------------------------------------------------------------------
Version 00.00.74
  Fixed: Allow variable for \bit with OWREAD, OWWRITE, SHIFTIN, SHIFTOUT
  Fixed: Allow OWWRITE to send more than 8 bits at once
----------------------------------------------------------------------------
Version 00.00.75
  Added: BST directive not allowed (special use)
----------------------------------------------------------------------------
Version 00.00.76
  Fixed: OWRESET with index array
  Fixed: WRBYTE, WRWORD with index array (used WRLONG)
  Fixed: DEVICE XINPUT thinks it is a set frequency like RCSLOW and RCFAST
----------------------------------------------------------------------------
Version 00.00.77
  Fixed: Inline strings don't work unless they are the first parameter
----------------------------------------------------------------------------
Version 00.00.78
  Fixed: SGN function
  Fixed: temp = temp * 8 ' Does "mov temp,temp"
----------------------------------------------------------------------------
Version 00.00.79
  Fixed: OWREAD (returned value is *2)
  Added: LMM support (disabled until further testing)
----------------------------------------------------------------------------
Version 00.00.80
  Fixed: WaitCnt required two parameters
----------------------------------------------------------------------------
Version 00.00.81
  Added: Multiline comments, curly brackets
  Fixed: IF ... OR
----------------------------------------------------------------------------
Version 00.00.82
  Fixed: Blank line within ASM...ENDASM crashes compiler
----------------------------------------------------------------------------
Version 00.00.83
  Fixed: TASK spin code could contain a mangled variable declaration
----------------------------------------------------------------------------
Version 00.00.84
  Fixed: Compiler crash if duplicate variable is an alias or an array
  Added: RDSWORD and RDSBYTE
----------------------------------------------------------------------------
Version 00.00.85
  Fixed: Strings in DATA create In-line strings too.
  Fixed: PAUSE and PAUSEUS for RCSLOW
  Fixed: Make FOR...NEXT using -1 for step if start is greater than limit
  Fixed: Make STR work with HUB arrays
  ADDED: name TASK AUTO ' Automatically starts task BEFORE main code
  ADDED: Make a __RAM array to work with @vararray (__RAM at __Init)
----------------------------------------------------------------------------
Version 00.00.86
  FIXED: Error if invalid device type in DEVICE command
  FIXED: I2CWRITE, I2CREAD
----------------------------------------------------------------------------
Version 00.00.87
  Fixed: Curly brackets inside and outside ASM...ENDASM block
  Fixed: % and %% data where MSB is set
----------------------------------------------------------------------------
Version 00.00.88
  FIXED: "DO WHILE|UNTIL pin =|<> value" performed opposite condition
  FIXED: "DO WHILE|UNTIL pin =|<> var" caused error
  FIXED: If pin group starts at 0, omit "shr xxx,#0" instruction
  FIXED: "IF P0 = P1 THEN"
  FIXED: Added INIT and __INIT to INVALID.TXT
  FIXED: subtracting a defined long constant performed an add instead of a subtraction
  FIXED: Allow pin group as right side expression for DO and LOOP
  FIXED: Allow pin group as right side expression for IF...THEN
----------------------------------------------------------------------------
Version 00.00.89
  FIXED: PULSIN now works in LMM and with slow clocks
    Value returned is always in 1uSec resolution. But granularity is higher
      for slower clocks or LMM (80MHZ LMM granularity is 2uSec)
----------------------------------------------------------------------------
Version 00.00.90
  Fixed:  value CON 512
                temp  VAR LONG = value ' This sets temp to @value instead of 512
  Added: SEROUT pin, baud, "STRING"
         SEROUT pin, baud, datalabel
         SEROUT pin, baud, hubarray
----------------------------------------------------------------------------
Version 00.00.91
  Fixed: pin = longvar ' Used "test", should have been "cmp"
  Added: SEROUT pin,baud,longvar\STR
  Added: SHIFTOUT and SHIFTIN speed multiplier
  Fixed: ON...GOSUB for LMM
----------------------------------------------------------------------------
Version 00.00.92
  Fixed: ON...GOSUB for LMM
  Changed: GOSUB only works with declared SUBs and FUNCs
  Fixed: Problem with value over 31-bits with bst
  Fixed: Divide by zero locks-up. New returns zero.
  Fixed: SEROUT string error if HUB offset is > $1FF
  Fixed: SEROUT string cannot have an index
----------------------------------------------------------------------------
Version 00.00.93
  Fixed: Hex values
----------------------------------------------------------------------------
Version 00.00.94
  Fixed: Constants used in DATA inside a TASK
    If a short constant replace with taskname#constname
    If a long constant replace with value
  Added: Support for PASD debugger "PROGRAM Start PASD" BREAK, WATCH
  Fixed: underscore removed from string in DATA  "_"
----------------------------------------------------------------------------
Version 00.00.95
  Fixed: PASD works from 10MHz and up
  Fixed: Hex, binary, char DATA within a TASK (MyTASK#$00)
----------------------------------------------------------------------------
Version 00.00.96
  WARNING: CANNOT DO strVar = anything + strVar
  Fixed: PINS = Array(const)
  Fixed: Allow indexes for SUB and FUNC parameters
    myStr HUB BYTE(81)
    ASSIGN myStr(5), "Hello"
  Fixed: "" evaluates to zero, "A" evaluates to 65, "AB" evaluates to a string
  Added:
    name HUB STRING (60)
    SERIN pin, baud, string
    string = string
    string = RIGHT string, count
    string = LEFT string, count
    string = MID string, start, count
    longVar = LEN string
    longVar = VAL string
    string = STR longVar, digit[, option]
      Option:
        0 - Unsigned leading zeros, z-string
        1 - (default) Unsigned leading spaces, z-string
        2 - Signed leading zeros, z-string
        3 - Signed leading spaces, z-string
        4 - Unsigned leading zeros, no terminating zero
        5 - Unsigned leading spaces, no terminating zero
        6 - Signed leading zeros, no terminating zero
        7 - Signed leading spaces, no terminating zero
----------------------------------------------------------------------------
Version 00.00.97
  Changed: Made GetAddr a function longVar = GetAddr hubvar
  Add: __STRING() virtual array
----------------------------------------------------------------------------
Version 00.00.97a
  Fixed: Duplicate DATA label causes abort
  Fixed: constants define in main, but used in TASK data get TASK# prefix when they should not
----------------------------------------------------------------------------
Version 00.00.98
  Added: Library compiler directive '{$CODE }
  Fixed: Can alias an array element
  Fixed: If an aliased variable is delared last, the label is not correct.
  Fixed: If an aliased variable is used, it could overwrite the original value
----------------------------------------------------------------------------
Version 00.00.98a
  Fixed bug with WDATA and LDATA
----------------------------------------------------------------------------
Version 00.00.99
  Fixed: If you alias system vars, TASK will purge system vars
  Fixed: I2CWRITE "rcl" instruction was missing parameter
  Fixed: LOAD does not fail after first TASK. MUST USE '{$TASKS compiler directive in libaries. }
----------------------------------------------------------------------------
*)

End.

