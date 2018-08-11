Unit GLOBAL;

{$MODE Delphi}

INTERFACE

Const
  c_lComputedConstantValue = -99999;

  c_iRetErrorCompileNoErrors = 0;
  c_iRetErrorCompileWarnings = 1;
  c_iRetErrorCompileErrors = 2;
  c_iRetErrorCouldNotCreateErrFile = 3;
  c_iRetErrorCouldNotCreateSrcFile = 4;
  c_iRetErrorNoFileSpecified = 5;
  c_iRetErrorCouldNotOpenInvalidtxtFile = 6;
  c_iRetErrorCouldNotReadSourceFile = 7;
  c_iRetErrorInvalidDirectory = 8;

  c_iMaxSRCLineLen = 512;
  c_iMaxCmdCnt = 127;
  c_iMaxVars = 1024;
  c_iMaxRegs = 32;
  c_iMaxSubroutines = 127;
  c_iMaxLoops = 1024; // Used for Exit Label queue
  c_iMaxLabels = 1024; // Used for label parameters
  c_iMaxDefines = 512;
  c_iMaxUses = 127; // When $USES is before SUB define

  c_iErrorInvalidVariableName = 1;
  c_iErrorDuplicateVariableName = c_iErrorInvalidVariableName + 1;
  c_iErrorConstantExpected = c_iErrorDuplicateVariableName + 1;
  c_iErrorInvalidUnaryOperator = c_iErrorConstantExpected + 1;
  c_iErrorInvalidParameter = c_iErrorInvalidUnaryOperator + 1;
  c_iErrorSyntaxError = c_iErrorInvalidParameter +1;
  c_iErrorInvalidNumberOfParameters = c_iErrorSyntaxError + 1;
  c_iErrorNotAForControlVariable = c_iErrorInvalidNumberOfParameters +1;
  c_iErrorBaudRateTooHigh = c_iErrorNotAForControlVariable +1;
  c_iErrorUnknownCommand = c_iErrorBaudRateTooHigh +1;
  c_iErrorCommaExpected = c_iErrorUnknownCommand +1;
  c_iErrorForWithoutNext = c_iErrorCommaExpected +1;
  c_iErrorNextWithoutFor = c_iErrorForWithoutNext +1;
  c_iErrorTooManySubs = c_iErrorNextWithoutFor +1;
  c_iErrorElseorEndIfwithoutIf = c_iErrorTooManySubs +1;
  c_iErrorLoopWithoutDo = c_iErrorElseorEndIfwithoutIf + 1;
  c_iErrorExitNotInLoop = c_iErrorLoopWithoutDo + 1;
  c_iErrorNoProgramCommandUsed = c_iErrorExitNotInLoop +1;
  c_iErrorTooManyDefines = c_iErrorNoProgramCommandUsed + 1;
  c_iErrorNotInSubOrFunc = c_iErrorTooManyDefines + 1;
  c_iErrorSubOrFuncCannotBeNested = c_iErrorNotInSubOrFunc + 1;
  c_iErrorNotValidInsideSub = c_iErrorSubOrFuncCannotBeNested + 1;
  c_iErrorCouldNotReadSourceFile = c_iErrorNotValidInsideSub + 1;
  c_iErrorDirective = c_iErrorCouldNotReadSourceFile + 1;
  c_iErrorNoFreqSpecified = c_iErrorDirective +1;
  c_iErrorLongVariableExpected = c_iErrorNoFreqSpecified + 1;
  c_iErrorPASDFreqError = c_iErrorLongVariableExpected + 1;
  c_iErrorViewportFreqError = c_iErrorPASDFreqError + 1;

  c_asErrorText: Array[1..28] of String[50] = (
  'INVALID VARIABLE NAME',
  'DUPLICATE VARIABLE NAME',
  'CONSTANT EXPECTED',
  'INVALID UNARY OPERATOR',
  'INVALID PARAMETER',
  'SYNTAX ERROR',
  'INVALID NUMBER OF PARAMETERS',
  'NOT A "FOR" CONTROL VARIABLE',
  'BAUDRATE IS TOO HIGH',
  'UNKNOWN COMMAND',
  'COMMA EXPECTED',
  'FOR WITHOUT NEXT',
  'NEXT WITHOUT FOR',
  'TOO MANY SUBS DEFINED',
  'ELSE OR ENDIF WITHOUT IF',
  'LOOP WITHOUT DO',
  'EXIT NOT IN FOR-NEXT OR DO-LOOP',
  'NO "PROGRAM" COMMAND USED',
  'TOO MANY DEFINES',
  'NOT IN A SUB OR FUNC',
  'SUB OR FUNC CANNOT BE NESTED',
  'NOT VALID INSIDE SUB',
  'COULD NOT READ SOURCE FILE ',
  'DIRECTIVE ERROR:',
  'NO FREQ SPECIFIED',
  'LONG VAR EXPECTED',
  'FREQ >= 10MHZ FOR PASD',
  'FREQ >= 10MHZ FOR VIEWPORT'
  );

  c_iWarningInternalClock = 1;
  c_iWarningEndFuncWithoutReturn = c_iWarningInternalClock + 1;
  c_iWarningDirective = c_iWarningEndFuncWithoutReturn + 1;

  c_asWarningText: Array[1..3] of String[60] = (
  'NOT RECOMMENDED WITH INTERNAL CLOCK',
  'ENDFUNC USED WITHOUT RETURN',
  'DIRECTIVE WARNING:'
);

Type
  TLiteralRecd = Record
    sName: String[32];
    sValue: String[128];
  End;

  // EVarType for in-line strings is e_ByteData
  EVarType = (e_LongVar, e_LongConst, e_Pin, e_ShortConst,
   e_HubByte, e_HubWord, e_HubLong,
   e_ByteData, e_WordData, e_LongData,
   e_DataLabel, e_DataFile, e_LongVarHidden, e_Ukn);

  EDebuggerType = (e_None, e_Viewport, e_PASD);

  PVarObj = ^TVarObj;
  TVarObj = Object
    m_eType: EVarType; // Variable type
    m_sName: String[128]; // Variable name
    m_bHasInitValue: Boolean; // True for LongVar with an inital value "temp VAR LONG = 123"
    m_lValue: LongInt; // Variable value (value of constant, initial value of variable, offset of HUB vars, mask for pins)
    m_sValue: String; // Variable string value or null string (HUBBYTE can be a string [name HUB STRING(10) = "Terry"]
    m_lHubOffset: LongInt; // If a hub variable, this is the hub offset
    m_lElements: LongInt; // If an array, this is the number of elements in the array, if a pin, this is the number of pins in the group
    m_byPinNumber: Byte; // If a pin, this is the pin number
    m_bHubAbs: Boolean; // True if hub variable is NOT an offset from PAR
    m_sFormat: String; // If a long constant, this is the format (_HEX8000)
    m_bVarIndex: Boolean; // True if index is a variable
    m_sIndex: String[128]; // Index name or literal constant ("0" if no index)
    m_lIndex: LongInt; // If index is a constant, this is the indexes value
    m_sAlias: String[128]; // If this var is an alias, this is the name it is an alias of, otherwise this is the same as it's own name
    m_bUsed: Boolean; // Set to true when used
    m_bShadowReg: Boolean; // Set to tru for phsa and phsb these cannot be used in dest field of cmps instructions
    m_bRevPins: Boolean;
    m_sTaskPrefix: String[128]; // Task name that variable was create in (used for constants in DATA declared in TASKs)
    m_bIOA: Boolean; // Is I/O PortA (True) or I/O portB (False)
    m_bPermanent: Boolean; // True if this is a system var (outa, etc)
    m_iWatchPos: Integer; // 0=not watched, positive if this variable is being WATCH with the debugger, negative if was watched but currently is not (UNWATCH)
    m_iWatchIndex: Integer; // Index into VP_Watch array of this variable
    Constructor Init(pv_sName: String; pv_lValue: LongInt);
    Destructor Done; Virtual;
    Function eGetType: EVarType;
    Function sGetName: String; Virtual;
    Function sGetTrueName: String; Virtual; // Returns actual variable name
    Function sGetIndexName: String; Virtual; // Returns name + index
    Function lGetValue: LongInt; Virtual;
    Function sGetValue: String; Virtual; // Const includes '#'
    Function sGetPinNumber: String; Virtual;
    Function bIsHubAbs: Boolean; Virtual;
  End;

  PLongVarObj = ^TLongVarObj;
  TLongVarObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_bVarIndex: Boolean; pv_sIndex: String);
  End;

  PLongConstObj = ^TLongConstObj;
  TLongConstObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String);
  End;

  PHubByteObj = ^THubByteObj;
  THubByteObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lHubOffset, pv_lElements, pv_lValue: LongInt; pv_bVarIndex: Boolean; pv_sIndex: String; pv_lIndex: LongInt);
  End;

  PHubWordObj = ^THubWordObj;
  THubWordObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lHubOffset, pv_lElements, pv_lValue: LongInt; pv_bVarIndex: Boolean; pv_sIndex: String; pv_lIndex: LongInt);
  End;

  PHubLongObj = ^THubLongObj;
  THubLongObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lHubOffset, pv_lElements, pv_lValue: LongInt; pv_bVarIndex: Boolean; pv_sIndex: String; pv_lIndex: LongInt);
  End;

  PByteDataObj = ^TByteDataObj;
  TByteDataObj = Object(TVarObj)
    Constructor Init(pv_sData: String; pv_lHubOffset: LongInt);
  End;

  PWordDataObj = ^TWordDataObj;
  TWordDataObj = Object(TVarObj)
    Constructor Init(pv_sData: String; pv_lHubOffset: LongInt);
  End;

  PLongDataObj = ^TLongDataObj;
  TLongDataObj = Object(TVarObj)
    Constructor Init(pv_sData: String; pv_lHubOffset: LongInt);
  End;

  PPinObj = ^TPinObj;
  TPinObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lValue: LongInt);
  End;

  PShortConstObj = ^TShortConstObj;
  TShortConstObj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String);
  End;

  PDataLabelObj = ^TDataLabelObj;
  TDataLabelobj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lHubOffset: LongInt; pv_iSize: Integer);
  End;

  PDataFileObj = ^TDataFileObj;
  TDataFileobj = Object(TVarObj)
    Constructor Init(pv_sName: String; pv_lHubOffset: LongInt);
  End;

  PVars = ^TVars;
  TVars = Object
    m_iVarCnt: Integer;
    m_iLastVarCnt: Integer;
    m_iKeep: Integer;
    m_apoVars: Array[1..c_iMaxVars] of PVarObj;
    Constructor Init;
    Function GetVarPtr(pv_sName: String): PVarObj;
  End;

  TForRec = Record
    poValue: PVarObj;
    poLimit: PVarObj;
    poStep: PVarObj;
    bDirPositive: Boolean;
    bAddStep: Boolean;
    bDeleteValue: Boolean;
    bDeleteLimit: Boolean;
    bDeleteStep: Boolean;
    sLabel: String[32]; //_AA_(iForCntAccm)
  End;

  TSubroutineRec = Record
    sLabel: String[32];
    bIsFunc: Boolean;
    iMinParams: Integer;
    iMaxParams: Integer;
    bUsed: Boolean;
    bIsNativeInLMM: Boolean;
  End;

  TIfRec = Record
    iAccumID: Integer;
    bElseUsed: Boolean;
    bIsElseIf: Boolean;
    bIsAND: Boolean;
    bIsOR: Boolean;
  End;

  TDoRec = Record
    iAccumID: Integer;
    bDoCondition: Boolean; // True for DO UNTIL | WHILE
  End;

  TExitRec = Record
    sLabel: String[32];
    bUsed: Boolean;
  End;

  TDeviceObj = Object
    m_eType: (e_P8X32A, e_P2);
    m_oVars: TVars;
    m_sDeviceName: String[15];
    Constructor Init;

    Function pGetVarPtr(pv_sName: String): PVarObj; // Returns Nil if name is not found

    Function pAddLongVar(pv_sName: String; pv_bVarIndex: Boolean; pv_sIndex: String): PLongVarObj;
    Function pAddLongConst(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String): PLongConstObj;

    Function pAddHubByte(pv_sName: String; pv_lHubOffset, pv_lHubElements, pv_lValue: LongInt): PHubByteObj;
    Function pAddHubWord(pv_sName: String; pv_lHubOffset, pv_lHubElements, pv_lValue: LongInt): PHubWordObj;
    Function pAddHubLong(pv_sName: String; pv_lHubOffset, pv_lHubElements, pv_lValue: LongInt): PHubLongObj;

//    Function pAddByteData(pv_sData: String): PByteDataObj;
    Function pAddByteData(pv_sData: String; Var pr_iHubBytes: Integer): PByteDataObj;
    Function pAddWordData(pv_sData: String): PWordDataObj;
    Function pAddLongData(pv_sData: String): PLongDataObj;

    Function pAddPin(pv_sName: String; pv_lValue: LongInt): PPinObj;
    Function pAddPinGroup(pv_sName: String; pv_lFromValue, pv_lToValue: LongInt): PPinObj;
    Function pAddShortConst(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String): PShortConstObj;

    Function pAddDataLabel(pv_sName: String; pv_iSize: Integer): pDataLabelObj;
    Function pAddDataFile(pv_sName: String): pDataFileObj;
  End;

Var
  // Command line parameters
  g_iTotalLineCnt: Integer;
  g_sCurrentFile: String;
  g_sOutputDir: String;
  g_sInputDir: String;
  g_eOutput: (Full, Brief, Quiet);
  g_bFBErrorFormat: Boolean;
  g_bPause: Boolean;
  g_bPauseExit: Boolean;
  g_bNoSource: Boolean;
  g_bInAsm: Boolean;
  g_bCompile: Boolean;
  g_bDisableWarnings: Boolean;
  g_tfSrc: Text;
  g_tfErr: Text;
  g_iLastErrorLine: Integer;
  g_iErrorCnt: Integer;
  g_iWarningCnt: Integer;
  g_sOrigLine: String;
  g_oDevice: TDeviceObj;
  g_bHandled: Boolean;
  g_iInLineCnt: Integer;
  g_iOutLineCnt: Integer;
  g_iCmdCnt: Integer;
  g_asCmdLine: Array[0..c_iMaxCmdCnt+1] of String[128];
  g_apoCmdVars: Array[1..c_iMaxCmdCnt] of PVarObj;
  g_asUnaryOperator: Array[0..c_iMaxCmdCnt+1] of String[1];
  g_abDeleteTemp: Array[0..c_iMaxCmdCnt+1] of Boolean;
  g_lFreq: LongInt;
  g_iPLL: Integer;
  g_bFreqSpecified: Boolean;
  g_bInternalClock: Boolean;

  g_bUsedProgram: Boolean;
  g_iForCnt: Integer;
  g_iForAccum: Integer;
  g_arForVars: Array[1..32] of TForRec;
  g_iIfCnt: Integer;
  g_iIfAccum: Integer;
  g_arIf: Array[1..32] of TIfRec;
  g_iDoCnt: Integer;
  g_iDoAccum: Integer;
  g_arDo: Array[1..32] of TDoRec;
  g_iLiteralCnt: Integer;
  g_arLiterals: Array[1..100] of TLiteralRecd;
  g_iSubroutineCnt: Integer;
  g_arSubroutines: Array[1..c_iMaxSubroutines] of TSubroutineRec;
  g_iExitLabelCnt: Integer;
  g_arExitLabels: Array[1..c_iMaxLoops] of TExitRec;
  g_iParamStringCnt: Integer;
  g_bInSub: Boolean;
  g_bIsFunc: Boolean;
  g_bReturnUsed: Boolean;
  g_byReturnLongs: Byte;
  g_sSubName: String[32];

  g_iDefinesCnt: Integer;
  g_asDefines: Array[1..c_iMaxDefines, 0..1] of String[32];
  g_iSerInTimeOutCnt: Integer; // Used to generate unique labels

  g_dI2CSpeed: Double;
  g_iLabelCnt: Integer;
  g_bAutoPage: Boolean;

  g_asUses: Array[1..c_iMaxUses] of String[32];
  g_iUsesCnt: Integer;

  g_iCondCnt: Integer;
  g_bCondState: Array[1..32] of Boolean;
  g_bElseState: Array[1..32] of Boolean;

  g_lHubOffset: LongInt;
  g_bIsTask: Boolean;
  g_sTaskName: String;
  g_bIsProgram: Boolean;
  g_iTaskCount: Integer;
  g_asTaskNames: Array[1..64] of String;
  g_bTaskAuto: Array[1..64] of Boolean;
  g_lTaskInitDirA, g_lTaskInitOutA: LongInt;
  g_lProgInitDirA, g_lProgInitOutA: LongInt;
  g_lTaskInitDirB, g_lTaskInitPinB: LongInt;
  g_lProgInitDirB, g_lProgInitPinB: LongInt;
  g_lTaskInitDirC, g_lTaskInitPinC: LongInt;
  g_lProgInitDirC, g_lProgInitPinC: LongInt;
  g_lTaskInitDirD, g_lTaskInitPinD: LongInt;
  g_lProgInitDirD, g_lProgInitPinD: LongInt;
  g_sProjectName: String;
  g_bCompoundIfPending: Boolean;
  g_iStringCnt: Integer;
  g_iMaxParams: Integer; // Subroutine parameters, initially set to 4
  g_bLMM: Boolean;
  g_bMultiLineComment: Boolean;
  g_iMinWaitCnt: Integer;
  g_eDebugger: EDebuggerType;

  g_bInLoadCmd: Boolean;
  g_iLoadFilesCode: Integer;
  g_iLoadFilesTasks: Integer;
  g_asLoadFiles: Array[1..32] of String;
  g_bLoadTasks: Boolean;

  g_iStackSize: Integer;

  g_iWatchPos: Integer;
  g_iWatchIndex: Integer;
  g_sWatchArrays: String;
  g_sWatchLongs: String;
  g_sWatchStrings: String;
  g_sWatchPins: String;

  g_iVPConfigCnt: Integer;
  g_asVPConfig: Array[1..20] of String[128];
  g_iVPArrayCnt: Integer;
  g_asVPArrays: Array[1..20] of String[128];
  g_sVPVideoBuffer: String;

  g_bIsP1: Boolean;
  g_bIsP2: Boolean;
  g_iMaxPin: Integer; // Maximum pin number (31 for P1, 91 for P2)

  g_bUsesPause: Boolean = False;
  g_bUsesMult: Boolean = False;
  g_bUsesDiv: Boolean = False;
  g_bUsesStr: Boolean = False;

Function PadStr(pv_sGiven: String; pv_iLen: Integer): String;
Function HexStr(pv_iGiven: Integer): String;
Function IntStr(pv_lGiven: LongInt): String;
Function NewLabelStr: String;
Function LabelStr: String;
Function Upper(pv_sGiven: String): String;
Procedure Error(pv_iError, pv_iParam: Integer);
Procedure Warning(pv_iWarning, pv_iParam: Integer);
Procedure ErrorStr(pv_iError: Integer; pv_sParam: String);
Procedure WarningStr(pv_iWarning: Integer; pv_sParam: String);

Procedure OutStr(pv_sInstr: String);
Procedure OutSpin(pv_sInstr: String);

Procedure AddLiteral(pv_sName, pv_sValue: String);
Function ReplaceLiteral(pv_sGiven: String): String;

Function PinGet(pv_iCmdPos, pv_iParam: Integer): Integer;
Procedure LongVarGet(pv_iCmdPos, pv_iParam: Integer);
Function LongConstGet(pv_iCmdPos, pv_iParam: Integer): Integer;
Function ShortConstGet(pv_iCmdPos, pv_iParam: Integer): Integer;

Procedure AddDirective(sName: String);
Procedure RemoveDirective(sName: String);
Function SubIndex(pv_sParam: String): Integer;
Function UsesIndex(pv_sParam: String): Integer;
Function SubNameUsed(sName: String): Boolean;

Function SetupParameters(pv_iParamPlace: Integer): Integer;

Procedure ProcessHubAddr(pv_iPlace: Integer; Var pr_sDest: String; pv_iSize: Integer);
Procedure ProcessHubAddrLit(pv_iPlace: Integer; pv_sDest: String; pv_iSize: Integer);

Procedure VP_WriteVar(pv_iGiven: Integer);
Procedure VP_WriteForVar(pv_iGiven: Integer);

Procedure OutputVarsAndDelete(pv_bTask: Boolean; pv_lDirA, pv_lOutA, pv_lDirB, pv_lPinB, pv_lDirC, pv_lPinC, pv_lDirD, pv_lPinD: LongInt);

{
Function IsLabel(pv_sParam: String): Boolean;
}

IMPLEMENTATION

Function PadStr(pv_sGiven: String; pv_iLen: Integer): String;
Begin
  PadStr:=pv_sGiven+
   Copy('                                                   ', 1, pv_iLen-Length(pv_sGiven));
End;

Function HexStr(pv_iGiven: Integer): String;
Const
  HexChars: String[16] = '0123456789ABCDEF';
Begin
  If pv_iGiven < 256 Then
   HexStr:='$'+HexChars[(pv_iGiven Div 16)+1]+HexChars[(pv_iGiven AND 15)+1]
  Else
   HexStr:=HexStr(pv_iGiven Div 256)+Copy(HexStr(pv_iGiven AND 255), 3, 2);
End;

Function IntStr(pv_lGiven: LongInt): String;
Var
  sTemp: String[12];
Begin
  Str(pv_lGiven, sTemp);
  IntStr:=sTemp;
End;

Function NewLabelStr: String;
Var
  sTemp: String[4];
Begin
  Inc(g_iLabelCnt);
  Str(g_iLabelCnt:4, sTemp);
  If sTemp[1] = ' ' Then sTemp[1]:='0';
  If sTemp[2] = ' ' Then sTemp[2]:='0';
  If sTemp[3] = ' ' Then sTemp[3]:='0';
  NewLabelStr:='__L'+sTemp;
End;

Function LabelStr: String;
Var
  sTemp: String[4];
Begin
  Str(g_iLabelCnt:4, sTemp);
  If sTemp[1] = ' ' Then sTemp[1]:='0';
  If sTemp[2] = ' ' Then sTemp[2]:='0';
  If sTemp[3] = ' ' Then sTemp[3]:='0';
  LabelStr:='__L'+sTemp;
End;

Function Upper(pv_sGiven: String): String;
Var
  I: Integer;
Begin
  For I:=1 to Length(pv_sGiven) Do pv_sGiven[I]:=UpCase(pv_sGiven[I]);
  Upper:=pv_sGiven;
End;

Procedure Error(pv_iError, pv_iParam: Integer);
Var
  bWriteError: Boolean;
Begin
  g_iLastErrorLine:=g_iInLineCnt;
  Inc(g_iErrorCnt);
  Write(g_tfSrc, ' ERROR ', pv_iError:2, '  ', c_asErrorText[pv_iError]);
  If pv_iParam <> 0 Then Write(g_tfSrc, ' "', g_asCmdLine[pv_iParam], '" ');
  WriteLn(g_tfSrc, ' ; ', g_sOrigLine);

  // Create error file if it doesn't exist
  bWriteError:=True;
  {$I-}
  Append(g_tfErr);
  {$I+}
  If IOResult <> 0 Then
  Begin
    {$I-}
    ReWrite(g_tfErr);
    {$I+}
    If IOResult <> 0 Then bWriteError:=False;
  End;
  If bWriteError Then
  Begin
    If g_bFBErrorFormat Then
      Write(g_tfErr, g_sCurrentFile, '(', g_iInLineCnt, ') error ', pv_iError, ': ', c_asErrorText[pv_iError]) // "Free BASIC" error report
    Else
      Write(g_tfErr, 'ERROR,', g_sCurrentFile, ',', g_iInLineCnt, ',', pv_iError, ',', c_asErrorText[pv_iError]);
    If pv_iParam <> 0 Then WriteLn(g_tfErr, ' "', g_asCmdLine[pv_iParam], '"') Else WriteLn(g_tfErr);
    Close(g_tfErr);
  End;

  If g_eOutput = Full Then
  Begin
    Write(' ERROR ', pv_iError:2, '  ', c_asErrorText[pv_iError]);
    If pv_iParam <> 0 Then Write(' "', g_asCmdLine[pv_iParam], '" ');
    WriteLn(' ; ', g_sOrigLine);
    If g_bPause Then ReadLn;
  End;
End;

Procedure Warning(pv_iWarning, pv_iParam: Integer);
Var
  bWriteError: Boolean;
Begin
  If g_bDisableWarnings = False Then
  Begin
    g_iLastErrorLine:=g_iInLineCnt;
    Inc(g_iWarningCnt);
    Write(g_tfSrc, ''' WARNING ', pv_iWarning:2, '  ', c_asWarningText[pv_iWarning]);
    If pv_iParam <> 0 Then Write(g_tfSrc, ' "', g_asCmdLine[pv_iParam], '" ');
    WriteLn(g_tfSrc, ' ; ', g_sOrigLine);

    // Create error file if it doesn't exist
    bWriteError:=True;
    {$I-}
    Append(g_tfErr);
    {$I+}
    If IOResult <> 0 Then
    Begin
      {$I-}
      ReWrite(g_tfErr);
      {$I+}
      If IOResult <> 0 Then bWriteError:=False;
    End;
    If bWriteError Then
    Begin
      Write(g_tfErr, 'WARNING,', g_sCurrentFile, ',', g_iInLineCnt, ',', pv_iWarning, ',', c_asWarningText[pv_iWarning]);
      If pv_iParam <> 0 Then WriteLn(g_tfErr, ' "', g_asCmdLine[pv_iParam], '"') Else WriteLn(g_tfErr);
      Close(g_tfErr);
    End;

    If g_eOutput = Full Then
    Begin
      Write(' WARNING ', pv_iWarning:2, '  ', c_asWarningText[pv_iWarning]);
      If pv_iParam <> 0 Then Write(' "', g_asCmdLine[pv_iParam], '" ');
      WriteLn(' ; ', g_sOrigLine);
      If g_bPause Then ReadLn;
    End;
  End; { If g_bDisableWarnings = false }
End;

Procedure ErrorStr(pv_iError: Integer; pv_sParam: String);
Var
  bWriteError: Boolean;
Begin
  g_iLastErrorLine:=g_iInLineCnt;
  Inc(g_iErrorCnt);
  WriteLn(g_tfSrc, ' ERROR ', pv_iError:2, '  ', c_asErrorText[pv_iError],
   ' "', pv_sParam, '" ; ', g_sOrigLine);

  // Create error file if it doesn't exist
  bWriteError:=True;
  {$I-}
  Append(g_tfErr);
  {$I+}
  If IOResult <> 0 Then
  Begin
    {$I-}
    ReWrite(g_tfErr);
    {$I+}
    If IOResult <> 0 Then bWriteError:=False;
  End;
  If bWriteError Then
  Begin
    If g_bFBErrorFormat Then
      WriteLn(g_tfErr, g_sCurrentFile, '(', g_iInLineCnt, ') error ', pv_iError, ': ', c_asErrorText[pv_iError])
    Else
      WriteLn(g_tfErr, 'ERROR,', g_sCurrentFile, ',', g_iInLineCnt, ',', pv_iError, ',', c_asErrorText[pv_iError], ' "', pv_sParam, '"');
    Close(g_tfErr);
  End;

  If g_eOutput = Full Then
  Begin
    WriteLn(' ERROR ', pv_iError:2, '  ', c_asErrorText[pv_iError],
     ' "', pv_sParam, '" ; ', g_sOrigLine);
    If g_bPause Then ReadLn;
  End;
End;

Procedure WarningStr(pv_iWarning: Integer; pv_sParam: String);
Var
  bWriteError: Boolean;
Begin
  g_iLastErrorLine:=g_iInLineCnt;
  Inc(g_iWarningCnt);
  Write(g_tfSrc, ''' WARNING ', pv_iWarning:2, '  ', c_asWarningText[pv_iWarning]);
  Write(g_tfSrc, ' "', pv_sParam, '" ');
  WriteLn(g_tfSrc, ' ; ', g_sOrigLine);

  // Create error file if it doesn't exist
  bWriteError:=True;
  {$I-}
  Append(g_tfErr);
  {$I+}
  If IOResult <> 0 Then
  Begin
    {$I-}
    ReWrite(g_tfErr);
    {$I+}
    If IOResult <> 0 Then bWriteError:=False;
  End;
  If bWriteError Then
  Begin
    WriteLn(g_tfErr, 'WARNING,', g_sCurrentFile, ',', g_iInLineCnt, ',', pv_iWarning, ',', c_asWarningText[pv_iWarning], ' "', pv_sParam, '"');
    Close(g_tfErr);
  End;

  If g_eOutput = Full Then
  Begin
    Write(' WARNING ', pv_iWarning:2, '  ', c_asWarningText[pv_iWarning]);
    Write(' "', pv_sParam, '" ');
    WriteLn(' ; ', g_sOrigLine);
    If g_bPause Then ReadLn;
  End;
End;

Procedure OutStr(pv_sInstr: String);
Const
  c_sSpaces = '                                                                      ';
Var
  sInstr: String;
  sOutStr: String;
  sLabel: String[32];
  sFirst: String[14];
Begin
  Inc(g_iOutLineCnt);
  sInstr:=pv_sInstr;
  sLabel:='';
  sFirst:='';
  If Length(sInstr) > 0 Then
  Begin
    Repeat
      sLabel:=sLabel + sInstr[1];
      Delete(sInstr, 1, 1);
    Until (Length(sInstr) = 0) or (sInstr[1] = ' ');
    sLabel:=sLabel + c_sSpaces;
    If sLabel[1] = ' ' Then sLabel:=Copy(sLabel, 1, 18); // If not a label, limit to 18 characters

    If Length(sInstr) > 0 Then
    Repeat
      Delete(sInstr, 1, 1);
    Until (Length(sInstr) = 0) or (sInstr[1] <> ' ');

    If Length(sInstr) > 0 Then
    Repeat
      sFirst:=sFirst + sInstr[1];
      Delete(sInstr, 1, 1);
    Until (Length(sInstr) = 0) or (sInstr[1] = ' ');
    sFirst:=sFirst + c_sSpaces;
    If Copy(sFirst, 1, 2) = 'IF' Then
    Begin
      sLabel:='    '+sFirst;
      sFirst:='';
      If Length(sInstr) > 0 Then
      Repeat
        Delete(sInstr, 1, 1);
      Until (Length(sInstr) = 0) or (sInstr[1] <> ' ');

      If Length(sInstr) > 0 Then
      Repeat
        sFirst:=sFirst + sInstr[1];
        Delete(sInstr, 1, 1);
      Until (Length(sInstr) = 0) or (sInstr[1] = ' ');
      sFirst:=sFirst + c_sSpaces;
    End;
    If Length(sInstr) > 0 Then
    Repeat
      Delete(sInstr, 1, 1);
    Until (Length(sInstr) = 0) or (sInstr[1] <> ' ');
  End;
  sInstr:=sLabel+sFirst+sInstr;
  If Length(sInstr) < 60 Then sInstr:=sInstr+Copy(c_sSpaces, 1, 60-Length(sInstr));
  If g_sOrigLine <> '' Then
  Begin
    If g_bNoSource Then
    Begin
      If Pos('''', g_sOrigLine) > 0 Then
      Begin
        g_sOrigLine:=Copy(g_sOrigLine, Pos('''', g_sOrigLine)+1, Length(g_sOrigLine)-Pos('''', g_sOrigLine));
        If sInstr = '                                ' Then sOutStr:=' '' ' + g_sOrigLine
         Else sOutStr:=sInstr + ' '' ' + g_sOrigLine;
      End
      Else sOutStr:=sInstr;
    End
    Else sOutStr:=sInstr + ' ''' + g_sOrigLine;
  End
  Else sOutStr:=sInstr;
  If Length(sOutStr) > c_iMaxSRCLineLen Then sOutStr:=Copy(sOutStr, 1, c_iMaxSRCLineLen);
  WriteLn(g_tfSrc, sOutStr);
  g_sOrigLine:='';
End;

Procedure OutSpin(pv_sInstr: String);
Const
  c_sSpaces = '                                                                         ';
Var
  sInstr: String;
  sOutStr: String;
Begin
  Inc(g_iOutLineCnt);
  sInstr:=pv_sInstr;
  If Length(sInstr) < 60 Then sInstr:=sInstr+Copy(c_sSpaces, 1, 60-Length(sInstr));
  If g_sOrigLine <> '' Then
  Begin
    If g_bNoSource Then
    Begin
      If Pos('''', g_sOrigLine) > 0 Then
      Begin
        g_sOrigLine:=Copy(g_sOrigLine, Pos('''', g_sOrigLine)+1, Length(g_sOrigLine)-Pos('''', g_sOrigLine));
        If sInstr = '                                ' Then sOutStr:=' '' ' + g_sOrigLine
         Else sOutStr:=sInstr + ' '' ' + g_sOrigLine;
      End
      Else sOutStr:=sInstr;
    End
    Else sOutStr:=sInstr + ' ''' + g_sOrigLine;
  End
  Else sOutStr:=sInstr;
  If Length(sOutStr) > c_iMaxSRCLineLen Then sOutStr:=Copy(sOutStr, 1, c_iMaxSRCLineLen);
  WriteLn(g_tfSrc, sOutStr);
  g_sOrigLine:='';
End;

//            ************* OBJECT TVarObj ******************

Constructor TVarObj.Init(pv_sName: String; pv_lValue: LongInt);
Begin
  m_eType:=e_Ukn;
  m_sName:=pv_sName;
  m_sAlias:=pv_sName;
  m_lValue:=pv_lValue;
  m_sValue:='';
  m_lElements:=1;
  m_byPinNumber:=0;
  m_bHubAbs:=False;
  m_bHasInitValue:=False;
  m_sIndex:='0';
  m_lIndex:=0;
  m_bUsed:=True;
  m_bShadowReg:=False;
  m_bRevPins:=False;
  m_sTaskPrefix:='';
  m_bIOA:=True; // Assume I/O Bank A
  m_bPermanent:=False; // Assume not a permanent variable
  m_iWatchPos:=0; // Assume not being watched in debugger
  m_iWatchIndex:=0;
End;

Destructor TVarObj.Done;
Begin
End;

Function TVarObj.eGetType: EVarType;
Begin
  eGetType:=m_eType;
End;

Function TVarObj.sGetName: String;
Begin
  sGetName:=m_sName;
End;

Function TVarObj.sGetTrueName: String;
Begin
  sGetTrueName:=m_sName;
End;

Function TVarObj.sGetIndexName: String;
Begin
  If (m_sIndex = '0') or (m_sIndex = '') Then sGetIndexName:=m_sName
   Else sGetIndexName:=m_sName + '+' + m_sIndex;
End;

Function TVarObj.lGetValue: LongInt;
Begin
  lGetValue:=m_lValue;
End;

Function TVarObj.sGetValue: String;
Begin
  sGetValue:=IntStr(m_lValue);
End;

Function TVarObj.sGetPinNumber: String;
Begin
  sGetPinNumber:=IntStr(m_byPinNumber);
End;

Function TVarObj.bIsHubAbs: Boolean;
Begin
  bIsHubAbs:=m_bHubAbs;
End;

//            ************* OBJECT TLongVarObj ******************

Constructor TLongVarObj.Init(pv_sName: String; pv_bVarIndex: Boolean; pv_sIndex: String);
Begin
  Inherited Init(pv_sName, 0);
  m_eType:=e_LongVar;
  m_bVarIndex:=pv_bVarIndex;
  m_sIndex:=pv_sIndex;
End;

//            ************* OBJECT TLongConstObj ******************

Constructor TLongConstObj.Init(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String);
Begin
  Inherited Init(pv_sName, pv_lValue);
  m_eType:=e_LongConst;
  m_sFormat:=pv_sFormat;
End;

//            ************* OBJECT THubByteObj ******************

Constructor THubByteObj.Init(pv_sName: String; pv_lHubOffset, pv_lElements, pv_lValue: LongInt; pv_bVarIndex: Boolean; pv_sIndex: String; pv_lIndex: LongInt);
Begin
  Inherited Init(pv_sName, pv_lValue);
  m_eType:=e_HubByte;
  m_lHubOffset:=pv_lHubOffset;
  m_lElements:=pv_lElements;
  m_bVarIndex:=pv_bVarIndex;
  m_sIndex:=pv_sIndex;
  m_lIndex:=pv_lIndex;
End;

//            ************* OBJECT THubWordObj ******************

Constructor THubWordObj.Init(pv_sName: String; pv_lHubOffset, pv_lElements, pv_lValue: LongInt; pv_bVarIndex: Boolean; pv_sIndex: String; pv_lIndex: LongInt);
Begin
  Inherited Init(pv_sName, pv_lValue);
  m_eType:=e_HubWord;
  m_lHubOffset:=pv_lHubOffset;
  m_lElements:=pv_lElements;
  m_bVarIndex:=pv_bVarIndex;
  m_sIndex:=pv_sIndex;
  m_lIndex:=pv_lIndex;
End;

//            ************* OBJECT THubLongObj ******************

Constructor THubLongObj.Init(pv_sName: String; pv_lHubOffset, pv_lElements, pv_lValue: LongInt; pv_bVarIndex: Boolean; pv_sIndex: String; pv_lIndex: LongInt);
Begin
  Inherited Init(pv_sName, pv_lValue);
  m_eType:=e_HubLong;
  m_lHubOffset:=pv_lHubOffset;
  m_lElements:=pv_lElements;
  m_bVarIndex:=pv_bVarIndex;
  m_sIndex:=pv_sIndex;
  m_lIndex:=pv_lIndex;
End;

//            ************* OBJECT TByteDataObj ******************

Constructor TByteDataObj.Init(pv_sData: String; pv_lHubOffset: LongInt);
Begin
  Inherited Init('', m_lHubOffset);
  m_lHubOffset:=pv_lHubOffset;
  m_sFormat:=pv_sData;
  m_eType:=e_ByteData;
End;

//            ************* OBJECT TWordDataObj ******************

Constructor TWordDataObj.Init(pv_sData: String; pv_lHubOffset: LongInt);
Begin
  Inherited Init('', m_lHubOffset);
  m_lHubOffset:=pv_lHubOffset;
  m_sFormat:=pv_sData;
  m_eType:=e_WordData;
End;

//            ************* OBJECT TLongDataObj ******************

Constructor TLongDataObj.Init(pv_sData: String; pv_lHubOffset: LongInt);
Begin
  Inherited Init('', m_lHubOffset);
  m_lHubOffset:=pv_lHubOffset;
  m_sFormat:=pv_sData;
  m_eType:=e_LongData;
End;


//            ************* OBJECT TPinObj ******************

Constructor TPinObj.Init(pv_sName: String; pv_lValue: LongInt);
Var
  lTemp: LongInt;
Begin
  lTemp:=1 SHL (pv_lValue and 31);
  Inherited Init(pv_sName, lTemp);
  m_eType:=e_Pin;
  m_byPinNumber:=pv_lValue AND 31;
  If pv_lValue > 31 Then m_bIOA:=False;
End;

//            ************* OBJECT TShortConstObj ******************

Constructor TShortConstObj.Init(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String);
Begin
  Inherited Init(pv_sName, pv_lValue);
  m_eType:=e_ShortConst;
  m_sFormat:=pv_sFormat;
End;

//            ************* OBJECT TDataLabelObj ******************

Constructor TDataLabelObj.Init(pv_sName: String; pv_lHubOffset: LongInt; pv_iSize: Integer);
Begin
  Inherited Init(pv_sName, g_lHubOffset);
  m_lValue:=pv_iSize;
  m_lHubOffset:=pv_lHubOffset;
  m_eType:=e_DataLabel;
End;

//            ************* OBJECT TDataFileObj ******************

Constructor TDataFileObj.Init(pv_sName: String; pv_lHubOffset: LongInt);
Begin
  Inherited Init(pv_sName, g_lHubOffset);
  m_lHubOffset:=pv_lHubOffset;
  m_eType:=e_DataFile;
End;

//            ************* OBJECT TVars ******************

Constructor TVars.Init;
Begin
  m_iVarCnt:=0;
End;

Function TVars.GetVarPtr(pv_sName: String): PVarObj; // Returns Nil if not found
Var
  sGiven: String[32];
  I: Integer;
Begin
  GetVarPtr:=Nil;
  sGiven:=Upper(pv_sName);
  I:=m_iVarCnt+1;
  Repeat
    Dec(I);
  Until (I = 0) or (Upper(m_apoVars[I]^.sGetTrueName) = sGiven);
  If I > 0 Then GetVarPtr:=m_apoVars[I];
End;

//            ************* OBJECT TDeviceObj ******************

Constructor TDeviceObj.Init;
Begin
  m_oVars.Init;
End;

Function TDeviceObj.pAddLongVar(pv_sName: String; pv_bVarIndex: Boolean; pv_sIndex: String): PLongVarObj;
Var
  poLongVar: PLongVarObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poLongVar, Init(pv_sName, pv_bVarIndex, pv_sIndex));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poLongVar;
    pAddLongVar:=poLongVar;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddLongVar:=Nil;
  End;
End;

Function TDeviceObj.pAddPin(pv_sName: String; pv_lValue: LongInt): PPinObj;
Var
  poPin: PPinObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poPin, Init(pv_sName, pv_lValue));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poPin;
    pAddPin:=poPin;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddPin:=Nil;
  End;
End;

Function TDeviceObj.pAddPinGroup(pv_sName: String; pv_lFromValue, pv_lToValue: LongInt): PPinObj;
Var
  poPin: PPinObj;
  bIOA: Boolean;
Begin
  bIOA:=True; // Assume I/O Bank A
  If pv_lFromValue > 31 Then
  Begin
    bIOA:=False;
    Dec(pv_lFromValue, 32);
    Dec(pv_lToValue, 32);
  End;
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    If pv_lFromValue < pv_lToValue Then
    Begin // Reversed pins
      New(poPin, Init(pv_sName, pv_lFromValue));
      With poPin^ Do
      Begin
        m_lElements:=(pv_lToValue - pv_lFromValue) + 1;
        m_lValue:=(1 SHL (m_lElements)- 1) SHL pv_lFromValue;
        m_bRevPins:=True;
        m_bIOA:=bIOA;
      End; // With
    End
    Else
    Begin // Not reversed pins
      New(poPin, Init(pv_sName, pv_lToValue));
      With poPin^ Do
      Begin
        m_lElements:=(pv_lFromValue - pv_lToValue) + 1;
        m_lValue:=(1 SHL (m_lElements)- 1) SHL pv_lToValue;
        m_bRevPins:=False;
        m_bIOA:=bIOA;
      End; // With
    End;
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poPin;
    pAddPinGroup:=poPin;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddPinGroup:=Nil;
  End;
End;

Function TDeviceObj.pAddLongConst(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String): PLongConstObj;
Var
  poLongConst: PLongConstObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poLongConst, Init(pv_sName, pv_lValue, pv_sFormat));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poLongConst;
    pAddLongConst:=poLongConst;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddLongConst:=Nil;
  End;
End;

Function TDeviceObj.pAddHubByte(pv_sName: String; pv_lHubOffset, pv_lHubElements, pv_lValue: LongInt): PHubByteObj;
Var
  poHubByte: PHubByteObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poHubByte, Init(pv_sName, pv_lHubOffset, pv_lHubElements, pv_lValue, False, '', 0));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poHubByte;
    pAddHubByte:=poHubByte;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddHubByte:=Nil;
  End;
End;

Function TDeviceObj.pAddHubWord(pv_sName: String; pv_lHubOffset, pv_lHubElements, pv_lValue: LongInt): PHubWordObj;
Var
  poHubWord: PHubWordObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poHubWord, Init(pv_sName, pv_lHubOffset, pv_lHubElements, pv_lValue, False, '', 0));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poHubWord;
    pAddHubWord:=poHubWord;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddHubWord:=Nil;
  End;
End;

Function TDeviceObj.pAddHubLong(pv_sName: String; pv_lHubOffset, pv_lHubElements, pv_lValue: LongInt): PHubLongObj;
Var
  poHubLong: PHubLongObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poHubLong, Init(pv_sName, pv_lHubOffset, pv_lHubElements, pv_lValue, False, '', 0));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poHubLong;
    pAddHubLong:=poHubLong;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddHubLong:=Nil;
  End;
End;

Function TDeviceObj.pAddByteData(pv_sData: String; Var pr_iHubBytes: Integer): PByteDataObj;
Var
  poByteData: PByteDataObj;
  I: Integer;
  iDataSize: Integer;
  bInQuotes: Boolean;
Begin
  // Handle escape sequences in pv_sData
  iDataSize:=0;
  I:=1;
  bInQuotes:=False;

  While I < Length(pv_sData) Do
  Begin
    If bInQuotes Then
    Begin
      If pv_sData[I] = '\' Then
      Begin
        If I+1 < Length(pv_sData) Then
        Begin
          Case pv_sData[I+1] of
            '0'..'9': Begin // decimal Copy(pv_sData, I+1, 3)
                        If pv_sData[I-1] = '"' Then
                        Begin
                          If pv_sData[I+4] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-2)+Copy(pv_sData, I+1, 3)+Copy(pv_sData, I+5, Length(pv_sData) - (I+4))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-2)+Copy(pv_sData, I+1, 3)+',"'+Copy(pv_sData, I+4, Length(pv_sData) - (I+3));
                          Inc(I, 3);
                        End
                        Else
                        Begin
                          If pv_sData[I+4] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",'+Copy(pv_sData, I+1, 3)+Copy(pv_sData, I+5, Length(pv_sData) - (I+4))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",'+Copy(pv_sData, I+1, 3)+',"'+Copy(pv_sData, I+4, Length(pv_sData) - (I+3));
                          Inc(I, 6);
                        End;
                        Inc(iDataSize);
                        bInQuotes:=False;
                      End;
            'x', 'X': Begin // hex '$'+Copy(pv_sData, I+2, 2)
                        If pv_sData[I-1] = '"' Then
                        Begin
                          If pv_sData[I+4] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-2)+'$'+Copy(pv_sData, I+2, 2)+Copy(pv_sData, I+5, Length(pv_sData) - (I+4))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-2)+'$'+Copy(pv_sData, I+2, 2)+',"'+Copy(pv_sData, I+4, Length(pv_sData) - (I+3));
                          Inc(I, 3);
                        End
                        Else
                        Begin
                          If pv_sData[I+4] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",'+'$'+Copy(pv_sData, I+2, 2)+Copy(pv_sData, I+5, Length(pv_sData) - (I+4))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",'+'$'+Copy(pv_sData, I+2, 2)+',"'+Copy(pv_sData, I+4, Length(pv_sData) - (I+3));
                          Inc(I, 6);
                        End;
                        Inc(iDataSize);
                        bInQuotes:=False;
                      End;
            'r', 'R': Begin // 13
                        If pv_sData[I-1] = '"' Then
                        Begin
                          If pv_sData[I+2] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-2)+'13'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-2)+'13,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                          Inc(I, 2);
                        End
                        Else
                        Begin
                          If pv_sData[I+2] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",13'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",13,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                          Inc(I, 5);
                        End;
                        Inc(iDataSize);
                        bInQuotes:=False;
                      End;
            'n', 'N': Begin // 10
                        If pv_sData[I-1] = '"' Then
                        Begin
                          If pv_sData[I+2] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-2)+'10'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-2)+'10,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                          Inc(I, 2);
                        End
                        Else
                        Begin
                          If pv_sData[I+2] = '"' Then
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",10'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                          Else
                           pv_sData:=Copy(pv_sData, 1, I-1)+'",10,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                          Inc(I, 5);
                        End;
                        Inc(iDataSize);
                        bInQuotes:=False;
                      End;
            '\': Begin // 92
                   If pv_sData[I-1] = '"' Then
                   Begin
                     If pv_sData[I+2] = '"' Then
                      pv_sData:=Copy(pv_sData, 1, I-2)+'92'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                     Else
                      pv_sData:=Copy(pv_sData, 1, I-2)+'92,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                     Inc(I, 2);
                   End
                   Else
                   Begin
                     If pv_sData[I+2] = '"' Then
                      pv_sData:=Copy(pv_sData, 1, I-1)+'",92'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                     Else
                      pv_sData:=Copy(pv_sData, 1, I-1)+'",92,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                     Inc(I, 5);
                   End;
                   Inc(iDataSize);
                   bInQuotes:=False;
                 End;
            '"': Begin // 34
                   If pv_sData[I-1] = '"' Then
                   Begin
                     If pv_sData[I+2] = '"' Then
                      pv_sData:=Copy(pv_sData, 1, I-2)+'34'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                     Else
                      pv_sData:=Copy(pv_sData, 1, I-2)+'34,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                     Inc(I, 2);
                   End
                   Else
                   Begin
                     If pv_sData[I+2] = '"' Then
                      pv_sData:=Copy(pv_sData, 1, I-1)+'",34'+Copy(pv_sData, I+3, Length(pv_sData) - (I+2))
                     Else
                      pv_sData:=Copy(pv_sData, 1, I-1)+'",34,"'+Copy(pv_sData, I+2, Length(pv_sData) - (I+1));
                     Inc(I, 5);
                   End;
                   Inc(iDataSize);
                   bInQuotes:=False;
                 End;
          End; // Case
        End; // If I+1 < Length(pv_sData)
      End; // If pv_sData[I] = '\
    End; // If bInQuotes
    If bInQuotes Then
    Begin
      If pv_sData[I] <> '"' Then Inc(iDataSize) Else bInQuotes:=False;
    End
    Else
    Begin
      If (pv_sData[I] = ',') and (pv_sData[I-1] <> '"') Then Inc(iDataSize);
      If pv_sData[I] = '"' Then bInQuotes:=True;
    End;
    Inc(I);
  End; //   While I < Length(pv_sData) Do
  // Feb 13,2016 If pv_sData[I] <> '"' Then Inc(iDataSize);
  If (I <= Length(pv_sData)) and (pv_sData[I] <> '"') Then Inc(iDataSize);
  New(poByteData, Init(pv_sData, g_lHubOffset));
  Inc(m_oVars.m_iVarCnt);
  m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poByteData;
  pAddByteData:=poByteData;
  pr_iHubBytes:=iDataSize;
End;

Function TDeviceObj.pAddWordData(pv_sData: String): PWordDataObj;
Var
  poWordData: PWordDataObj;
Begin
  g_lHubOffset:=(g_lHubOffset + 1) and $FFFFFFFE;
  New(poWordData, Init(pv_sData, g_lHubOffset));
  Inc(m_oVars.m_iVarCnt);
  m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poWordData;
  pAddWordData:=poWordData;
End;


Function TDeviceObj.pAddLongData(pv_sData: String): PLongDataObj;
Var
  poLongData: PLongDataObj;
Begin
  g_lHubOffset:=(g_lHubOffset + 3) and $FFFFFFFC;
  New(poLongData, Init(pv_sData, g_lHubOffset));
  Inc(m_oVars.m_iVarCnt);
  m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poLongData;
  pAddLongData:=poLongData;
End;

Function TDeviceObj.pAddShortConst(pv_sName: String; pv_lValue: LongInt; pv_sFormat: String): PShortConstObj;
Var
  poShortConst: PShortConstObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poShortConst, Init(pv_sName, pv_lValue, pv_sFormat));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poShortConst;
    pAddShortConst:=poShortConst;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddShortConst:=Nil;
  End;
End;

Function TDeviceObj.pAddDataLabel(pv_sName: String; pv_iSize: Integer): PDataLabelObj;
Var
  poDataLabel: PDataLabelObj;
Begin
  If pGetVarPtr(pv_sName) = Nil Then
  Begin
    New(poDataLabel, Init(pv_sName, g_lHubOffset, pv_iSize));
    Inc(m_oVars.m_iVarCnt);
    m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poDataLabel;
    pAddDataLabel:=poDataLabel;
  End
  Else
  Begin
    ErrorStr(c_iErrorDuplicateVariableName, pv_sName);
    pAddDataLabel:=Nil;
  End;
End;

Function TDeviceObj.pAddDataFile(pv_sName: String): PDataFileObj;
Var
  poDataFile: PDataFileObj;
Begin
  New(poDataFile, Init(pv_sName, g_lHubOffset));
  Inc(m_oVars.m_iVarCnt);
  m_oVars.m_apoVars[m_oVars.m_iVarCnt]:=poDataFile;
  pAddDataFile:=poDataFile;
End;

Function TDeviceObj.pGetVarPtr(pv_sName: String): PVarObj;
Var
  poVar: PVarObj;
Begin
  poVar:=m_oVars.GetVarPtr(pv_sName);
  If poVar <> Nil Then poVar^.m_bUsed:=True;
  pGetVarPtr:=poVar;
End;

// ===========================================================

Procedure AddLiteral(pv_sName, pv_sValue: String);
Begin
  Inc(g_iLiteralCnt);
  g_arLiterals[g_iLiteralCnt].sName:=pv_sName;
  g_arLiterals[g_iLiteralCnt].sValue:=pv_sValue;
End;

Function ReplaceLiteral(pv_sGiven: String): String;
Var
  I: Integer;
  iPlace: Integer;
Begin
  iPlace:=0;
  For I:=1 to g_iLiteralCnt Do
  Begin
    If Upper(pv_sGiven) = Upper(g_arLiterals[I].sName) Then iPlace:=I;
  End;
  If iPlace = 0 Then ReplaceLiteral:=pv_sGiven
   Else
   Begin
     ReplaceLiteral:=g_arLiterals[iPlace].sValue;
   End;
End;

Function PinGet(pv_iCmdPos, pv_iParam: Integer): Integer;
Begin
  PinGet:=0; // Assume zero
  If g_asUnaryOperator[pv_iCmdPos] = '' Then
  Begin
    If g_apoCmdVars[pv_iCmdPos]^.sGetName <> '__param'+IntStr(pv_iParam) Then
    Begin
      OutStr('  mov __param'+IntStr(pv_iParam)+',#'+g_apoCmdVars[pv_iCmdPos]^.sGetPinNumber);
      PinGet:=1;
    End;
  End
  Else Error(c_iErrorInvalidParameter, pv_iParam);
End;

Procedure LongVarGet(pv_iCmdPos, pv_iParam: Integer);
Var
  sOp: String[6];
Begin
  sOp:='  mov ';
  If (g_asUnaryOperator[pv_iCmdPos] = '') or (g_asUnaryOperator[pv_iCmdPos] = '-') or
     (g_asUnaryOperator[pv_iCmdPos] = '~') Then
  Begin
    If (g_asUnaryOperator[pv_iCmdPos] = '~') or (g_asUnaryOperator[pv_iCmdPos] = '-') Then sOp:='  neg ';
    If g_apoCmdVars[pv_iCmdPos]^.sGetIndexName <> '__param'+IntStr(pv_iParam) Then
    Begin
      If g_apoCmdVars[pv_iCmdPos]^.m_bVarIndex = False Then
       OutStr(sOp+'__param'+IntStr(pv_iParam)+','+g_apoCmdVars[pv_iCmdPos]^.sGetIndexName)
      Else
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[pv_iCmdPos]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[pv_iCmdPos]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVS');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[pv_iCmdPos]^.m_sIndex+',#'+g_apoCmdVars[pv_iCmdPos]^.sGetName);
          OutStr('  movs $+2,'+g_apoCmdVars[pv_iCmdPos]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[pv_iCmdPos]^.m_sIndex+',#'+g_apoCmdVars[pv_iCmdPos]^.sGetName);
        End;
        OutStr(sOp+'__param'+IntStr(pv_iParam)+',0-0');
      End;
      If g_asUnaryOperator[pv_iCmdPos] = '~' Then OutStr('  subs __param'+IntStr(pv_iParam)+',#1');
    End;
  End
  Else Error(c_iErrorInvalidUnaryOperator, pv_iCmdPos);
End;

Function LongConstGet(pv_iCmdPos, pv_iParam: Integer): Integer;
Begin
  If g_asUnaryOperator[pv_iCmdPos] = '' Then
  Begin
    If g_apoCmdVars[pv_iCmdPos]^.sGetName <> '__param'+IntStr(pv_iParam) Then
    Begin
      OutStr('  mov __param'+IntStr(pv_iParam)+','+g_apoCmdVars[pv_iCmdPos]^.sGetName);
      LongConstGet:=1;
    End
    Else LongConstGet:=0;
  End
  Else If g_asUnaryOperator[pv_iCmdPos] = '~' Then
  Begin
    OutStr('  mov __param'+IntStr(pv_iParam)+','+g_apoCmdVars[pv_iCmdPos]^.sGetName);
    LongConstGet:=1;
  End
  Else If g_asUnaryOperator[pv_iCmdPos] = '-' Then
  Begin
    OutStr('  mov __param'+IntStr(pv_iParam)+','+g_apoCmdVars[pv_iCmdPos]^.sGetName);
    LongConstGet:=1;
  End
  Else
  Begin
    Error(c_iErrorInvalidUnaryOperator, pv_iCmdPos);
    LongConstGet:=0;
  End;
End;

Function ShortConstGet(pv_iCmdPos, pv_iParam: Integer): Integer;
Begin
  If g_asUnaryOperator[pv_iCmdPos] = '' Then
  Begin
    If g_apoCmdVars[pv_iCmdPos]^.sGetName <> '__param'+IntStr(pv_iParam) Then
    Begin
      OutStr('  mov __param'+IntStr(pv_iParam)+',#'+g_apoCmdVars[pv_iCmdPos]^.sGetName);
      ShortConstGet:=1;
    End
    Else ShortConstGet:=0;
  End
  Else If g_asUnaryOperator[pv_iCmdPos] = '~' Then
  Begin
    OutStr('  mov __param'+IntStr(pv_iParam)+',#'+g_apoCmdVars[pv_iCmdPos]^.sGetName);
    ShortConstGet:=1;
  End
  Else If g_asUnaryOperator[pv_iCmdPos] = '-' Then
  Begin
    OutStr('  mov __param'+IntStr(pv_iParam)+',#'+g_apoCmdVars[pv_iCmdPos]^.sGetName);
    ShortConstGet:=1;
  End
  Else
  Begin
    Error(c_iErrorInvalidUnaryOperator, pv_iCmdPos);
    ShortConstGet:=0;
  End;
End;


Function ConstVarName(pv_iCmdPos: Integer): String;
Begin
  ConstVarName:=g_apoCmdVars[pv_iCmdPos]^.sGetName;
End;

Function ConstVarNamePtr(pv_poVarObj: PVarObj): String;
Begin
  ConstVarNamePtr:=pv_poVarObj^.sGetName;
End;


Function SubIndex(pv_sParam: String): Integer;
Var
  iSubroutinePlace: Integer;
Begin
  iSubroutinePlace:=1;
  While (iSubroutinePlace < g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel <> Upper(pv_sParam)) Do Inc(iSubroutinePlace);
  If (iSubroutinePlace <= g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel = Upper(pv_sParam)) Then SubIndex:=iSubroutinePlace
   Else SubIndex:=0;
End;

Function UsesIndex(pv_sParam: String): Integer;
Var
  iUsesPlace: Integer;
Begin
  iUsesPlace:=1;
  While (iUsesPlace < g_iUsesCnt) and (g_asUses[iUsesPlace] <> Upper(pv_sParam)) Do Inc(iUsesPlace);
  If (iUsesPlace <= g_iUsesCnt) and (g_asUses[iUsesPlace] = Upper(pv_sParam)) Then UsesIndex:=iUsesPlace
   Else UsesIndex:=0;
End;


Procedure AddDirective(sName: String);
Var
  iFound: Integer;
  I: Integer;
Begin
  iFound:=0;
  For I:=1 to g_iDefinesCnt Do If Upper(g_asDefines[I, 0]) = Upper(sName) Then iFound:=I;
  If iFound = 0 Then
  Begin
    Inc(g_iDefinesCnt);
    iFound:=g_iDefinesCnt;
  End;
  g_asDefines[iFound, 0]:=sName;
  g_asDefines[iFound, 1]:='';
End;

Procedure RemoveDirective(sName: String);
Var
  iFound: Integer;
  I: Integer;
Begin
  iFound:=0;
  For I:=1 to g_iDefinesCnt Do If Upper(g_asDefines[I, 0]) = Upper(sName) Then iFound:=I;
  If iFound > 0 Then
  Begin
    For I:=iFound to g_iDefinesCnt-1 Do g_asDefines[I]:=g_asDefines[I+1];
    Dec(g_iDefinesCnt);
  End;
End;

Function SubNameUsed(sName: String): Boolean;
Var
  bFound: Boolean;
  i: Integer;
Begin
  sName:=Upper(sName);
  bFound:=False; // Assume not found
  For i:=0 to g_iSubroutineCnt -1 Do
  Begin
    If g_arSubroutines[i].sLabel = sName Then bFound:=g_arSubroutines[i].bUsed;
  End;
  SubNameUsed:=bFound;
End;


Function SetupParameters(pv_iParamPlace: Integer): Integer; // Returns iParamCnt
Var
  iParamPlace: Integer;
  iParamCnt: Integer;
Begin
  iParamPlace:=pv_iParamPlace;
  iParamCnt:=1;
  While iParamPlace <= g_iCmdCnt Do
  Begin
    Case g_apoCmdVars[iParamPlace]^.eGetType of
      e_LongVar: LongVarGet(iParamPlace, iParamCnt);
      e_LongConst: LongConstGet(iParamPlace, iParamCnt);
      e_ShortConst: ShortConstGet(iParamPlace, iParamCnt);
      e_Pin: PinGet(iParamPlace, iParamCnt);
      Else
      Begin
        // Assume parameter is a Hub varible or Data Label
        OutStr('  mov __param'+IntStr(iParamCnt)+',__'+g_apoCmdVars[iParamPlace]^.sGetName+'_adr');
        If g_asCmdLine[iParamPlace+1] = '(' Then
        Begin
          // Indexed hub variable "myStr(5)"
          Case g_apoCmdVars[iParamPlace+2]^.eGetType of
            e_ShortConst: OutStr('  add __param'+IntStr(iParamCnt)+',#'+g_apoCmdVars[iParamPlace+2]^.sGetName);
            e_LongConst, e_LongVar: OutStr('  add __param'+IntStr(iParamCnt)+','+g_apoCmdVars[iParamPlace+2]^.sGetIndexName);
          End; // Case
          Inc(iParamPlace, 3); // Skip ( value ) parameters
        End;
      End;
    End; // Case
    Inc(iParamCnt);
    Inc(iParamPlace);
    If (iParamPlace < g_iCmdCnt) and (g_asCmdLine[iParamPlace] <> ',') Then Error(c_iErrorSyntaxError, iParamPlace);
    If iParamPlace = g_iCmdCnt Then Error(c_iErrorSyntaxError, g_iCmdCnt); // Trailing comma
    Inc(iParamPlace);
  End; // While
  Dec(iParamCnt);
  SetupParameters:=iParamCnt;
End;


Procedure ProcessHubAddr(pv_iPlace: Integer; Var pr_sDest: String; pv_iSize: Integer); // Returns name of hub address variable
Var
  poVar: PVarObj;
  bInit: Boolean;
  sTemp: String;
Begin
  bInit:=False; // Assume dest is not initialized
  If g_apoCmdVars[pv_iPlace]^.eGetType In [e_HubByte, e_HubWord, e_HubLong, e_DataLabel] Then
  Begin
    // Handle index
    If g_apoCmdVars[pv_iPlace]^.m_bVarIndex Then
    Begin // longvar index
      OutStr('  mov '+pr_sDest+','+g_apoCmdVars[pv_iPlace]^.m_sIndex);
      If pv_iSize = 2 Then OutStr('  shl '+pr_sDest+',#1');
      If pv_iSize = 4 Then OutStr('  shl '+pr_sDest+',#2');
      bInit:=True;
    End
    Else
    Begin // constant index
      If (g_apoCmdVars[pv_iPlace]^.m_sIndex <> '') and (g_apoCmdVars[pv_iPlace]^.m_sIndex <> '0') Then
      Begin
        If (g_apoCmdVars[pv_iPlace]^.m_lIndex * pv_iSize) < 512 Then
        Begin
          If Not bInit Then
          Begin
            If pv_iSize = 1 Then OutStr('  mov '+pr_sDest+',#'+g_apoCmdVars[pv_iPlace]^.m_sIndex)
             Else OutStr('  mov '+pr_sDest+',#'+g_apoCmdVars[pv_iPlace]^.m_sIndex + ' * '+IntStr(pv_iSize));
             bInit:=True;
          End
          Else
          Begin
            If pv_iSize = 1 Then OutStr('  add '+pr_sDest+',#'+g_apoCmdVars[pv_iPlace]^.m_sIndex)
             Else OutStr('  add '+pr_sDest+',#'+g_apoCmdVars[pv_iPlace]^.m_sIndex + ' * '+IntStr(pv_iSize));
          End;
        End
        Else
        Begin // Long const index
          If pv_iSize = 1 Then
          Begin
            poVar:=g_oDevice.pGetVarPtr(g_apoCmdVars[pv_iPlace]^.m_sIndex);
            If poVar = Nil Then
            Begin
              poVar:=g_oDevice.pGetVarPtr('_'+g_apoCmdVars[pv_iPlace]^.m_sIndex);
              If poVar = Nil Then poVar:=g_oDevice.pAddLongConst(g_apoCmdVars[pv_iPlace]^.m_sIndex, g_apoCmdVars[pv_iPlace]^.m_lIndex, g_apoCmdVars[pv_iPlace]^.m_sIndex);
            End;
          End
          Else
          Begin // index size is 2 or 4
            sTemp:=g_apoCmdVars[pv_iPlace]^.m_sIndex + '_x'+IntStr(pv_iSize);
            If sTemp[1] = '$' Then
            Begin
              Delete(sTemp, 1, 1); // Remove $
              sTemp:='hex'+sTemp; // prepend "hex"
            End;
            If sTemp[1] <> '_' Then sTemp:='_'+sTemp;
            poVar:=g_oDevice.pGetVarPtr(sTemp);
            If poVar = Nil Then poVar:=g_oDevice.pAddLongConst(sTemp, g_apoCmdVars[pv_iPlace]^.m_lIndex * pv_iSize, IntStr(g_apoCmdVars[pv_iPlace]^.m_lIndex * pv_iSize));
          End;

          If Not bInit Then
          Begin
            OutStr('  mov '+pr_sDest+','+poVar^.sGetName);
            bInit:=True;
          End
          Else
          Begin
            OutStr('  add '+pr_sDest+','+poVar^.sGetName);
          End;
        End;
      End;
    End; // Handle index

    If Not bInit Then pr_sDest:='__'+g_apoCmdVars[pv_iPlace]^.sGetName+'_adr'
     Else OutStr('  add '+pr_sDest+',__'+g_apoCmdVars[pv_iPlace]^.sGetName+'_adr');

    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidParameter, pv_iPlace);
End;


Procedure ProcessHubAddrLit(pv_iPlace: Integer; pv_sDest: String; pv_iSize: Integer);
Var
  sTemp: String;
Begin
  sTemp:=pv_sDest;
  ProcessHubAddr(pv_iPlace, sTemp, pv_iSize);
  If sTemp <> pv_sDest Then OutStr('  mov '+pv_sDest+','+sTemp);
End;


Procedure VP_WriteVar(pv_iGiven: Integer);
Begin
  If (g_eDebugger = e_Viewport) and (g_apoCmdVars[pv_iGiven]^.m_iWatchPos > 0) Then
  Begin
    OutStr('  wrlong '+g_apoCmdVars[pv_iGiven]^.sGetName+',__'+g_apoCmdVars[pv_iGiven]^.sGetName+'_adr');
  End;
End;

Procedure VP_WriteForVar(pv_iGiven: Integer);
Begin
  If (g_eDebugger = e_Viewport) and (g_arForVars[pv_iGiven].poValue^.m_iWatchPos > 0) Then
  Begin
    OutStr('  wrlong '+g_arForVars[pv_iGiven].poValue^.sGetName+',__'+g_arForVars[pv_iGiven].poValue^.sGetName+'_adr');
  End;
End;

Procedure OutputVarsAndDelete(pv_bTask: Boolean; pv_lDirA, pv_lOutA, pv_lDirB, pv_lPinB, pv_lDirC, pv_lPinC, pv_lDirD, pv_lPinD: LongInt);
Var
  I, I2, J, K: Integer;
  iLongConstCnt: Integer;
  asConstNames: Array[1..512] of String[32];
  alConstValues: Array[1..512] of LongInt;
  asConstFormats: Array[1..512] of String[64];
  sTemp: String[64];
  lTemp: LongInt;
  poVar: PVarObj;
  poLongConst: PVarObj;
  iWaitCnt, iShift: Integer;
Begin
  If g_bIsTask = pv_bTask Then
  Begin
    If g_bUsesPause Then
    Begin
      iWaitCnt:=g_lFreq Div 1000;
      iShift:=0;
      While iWaitCnt < g_iMinWaitCnt Do
      Begin
        iWaitCnt:=iWaitCnt * 2;
        Inc(iShift);
      End;
      poLongConst:=g_oDevice.pGetVarPtr('_'+IntStr(1 SHL iShift)+'mSec');
      If poLongConst = Nil Then poLongConst:=g_oDevice.pAddLongConst('_'+IntStr(1 SHL iShift)+'mSec', iWaitCnt, IntStr(iWaitCnt));
      OutSpin('''-------------------------------------------------------------------------------------------');
      OutSpin('''PAUSE SUBROUTINE (pass mSec in _temp1)');
      OutSpin(''' ------------------------------------------------------------------------------------------');
      OutStr('_PAUSE');
      OutStr('  shr __temp1,#'+IntStr(iShift)+' WZ'); // Set Zero flag even if we are not shifting
      OutStr('  mov __temp2,'+poLongConst^.sGetName);
      OutStr('  add __temp2,cnt');
      OutStr('_PAUSE_1');
      OutStr('  IF_NZ  waitcnt __temp2,'+poLongConst^.sGetName);
      If g_bLMM Then
      Begin
        OutStr('  IF_NZ djnz __temp1,#_LMM_JUMP');
        OutStr('  long @@@_PAUSE_1');
        OutStr('  jmp #_LMM_RET');
      End
      Else
      Begin
        OutStr('  IF_NZ  djnz __temp1,#_PAUSE_1');
        OutStr('_PAUSE_ret');
        OutStr('  ret');
      End;
    End; // If g_bUsesPause

    If g_bUsesMult Then
    Begin
      OutSpin('''-------------------------------------------------------------------------------------------');
      OutSpin('''MULTIPLY SUBROUTINE (__temp1 = __temp1 * __temp2), (__temp2 = __temp1 */ __temp2), (__temp4 = __temp1 ** __temp2)');
      OutSpin(''' ------------------------------------------------------------------------------------------');
      OutStr('_MULTIPLY');
      // Set Zero flag to sign
      OutStr('  abs __temp1,__temp1 WC');
      OutStr('  muxc __temp3,#1');
      OutStr('  abs __temp2,__temp2 WC');
      OutStr('  IF_C xor __temp3,#1');
      OutStr('  mov __temp4,#0');
      OutStr('  mov __temp5,#32');
      OutStr('  shr __temp1,#1 WC');
      OutStr('_MULTIPLY_1');
      OutStr('  IF_C add __temp4,__temp2 WC');
      OutStr('  rcr __temp4,#1 WC');
      OutStr('  rcr __temp1,#1 WC');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp5,#_LMM_JUMP');
        OutStr('  long @@@_MULTIPLY_1');
      End
      Else OutStr('  djnz __temp5,#_MULTIPLY_1');

      OutStr('  mov __temp2,__temp1');
      OutStr('  shr __temp2,#16');
      OutStr('  mov __temp5,__temp4');
      OutStr('  shl __temp5,#16');
      OutStr('  or __temp2,__temp5');



      OutStr('  test __temp3,#1 WZ');
      OutStr('  IF_NZ neg __temp4,__temp4');
      OutStr('  IF_NZ neg __temp1,__temp1');
      OutStr('  IF_NZ neg __temp2,__temp2');
      //OutStr('  IF_NZ neg __temp1,__temp1 WZ');
      //OutStr('  IF_NZ sub __temp4,#1');
      If g_bLMM Then OutStr('  jmp #_LMM_RET')
      Else
      Begin
        OutStr('_MULTIPLY_ret');
        OutStr('  ret');
      End;
    End; // If g_bUsesMult

    If g_bUsesDiv Then
    Begin
      OutSpin('''-------------------------------------------------------------------------------------------');
      OutSpin('''DIV SUBROUTINE (__temp1 = __temp1 / __temp2; __temp4 = remainder)');
      OutSpin(''' ------------------------------------------------------------------------------------------');
      OutStr('_DIV');


      // OutStr('  mov __temp4,__temp1');
      OutStr('  mov __temp4,#0'); // Result
      OutStr('  abs __temp1,__temp1 WC');
      OutStr('  muxc __temp5,#1');
      OutStr('  abs __temp2,__temp2 WC, WZ');
      OutStr('  IF_Z mov __temp1,#0');
      If g_bLMM Then
      Begin
        OutStr('  IF_Z jmp #_LMM_JUMP');
        OutStr('  long @@@_DIV_ZERO');
      End
      Else OutStr('  IF_Z jmp #_DIV_ZERO');
      OutStr('  IF_C xor __temp5,#1');
      OutStr('  mov __temp3,#33'); // Bit Counter
      OutStr('_DIV_1');
      OutStr('  cmpsub __temp4,__temp2 WC');
      OutStr('  rcl __temp1,#1 WC');
      OutStr('  rcl __temp4,#1');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp3,#_LMM_JUMP');
        OutStr('  long @@@_DIV_1');
      End
      Else OutStr('  djnz __temp3,#_DIV_1');
      OutStr('  shr __temp4,#1'); // fix remainder (new method)

      OutStr('  test __temp5,#1 WZ');
      OutStr('  IF_NZ neg __temp1,__temp1');
      OutStr('  IF_NZ neg __temp4,__temp4');
   //   If (g_apoCmdVars[6]^.eGetType = e_LongVar) or (g_apoCmdVars[6]^.lGetValue = c_lComputedConstantValue) Then OutStr(sDivZeroLabel);
   //   If g_apoCmdVars[2]^.sGetIndexName <> '__temp4' Then
   //   OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp4');

      OutStr('_DIV_ZERO');

      If g_bLMM Then OutStr('  jmp #_LMM_RET')
      Else
      Begin
        OutStr('_DIV_ret');
        OutStr('  ret');
      End;
    End; // If g_bUsesDiv


    If g_bUsesStr Then
    Begin
      OutSpin('''-------------------------------------------------------------------------------------------');
       OutSpin('''STR SUBROUTINE (__temp1 = value; __temp2 = digits; __temp3 = mode; __temp4 = hub address)');
       OutSpin(''' ------------------------------------------------------------------------------------------');
       OutStr('_STR');
{
       Do a STR for 10 digits with sign and zero terminator byte
       Always use spaces for leading zeros
       Move minus sign to just before first non-zero digit

       ascii = STR 123,3
       __temp1 = @ascii + 1 if signed and positive + $4000_0000 if zero terminator, + $8000_0000 if minus sign needed
       __temp2 = 123
       __temp3 = %0000000111 (skip 7 digits and store 3 digits. When __temp3 = 0 we are finished)
       __temp4 = " " (default first character)

       MOV __temp5,__temp4 ' Begin with starting character (" " or "0")
Dig10:
       CMPSUB __temp2,_1_000_000_000 WC WR
       IF_C OR __temp5,#16   ' Convert space to zero
       IF_C MOV __temp4,#"0" ' Next digit defaults to zero
       IF_C ADD __temp5,#1   ' Advance digit
       IF_C JMP #Dig10
       JMP #StoreDigit

NextDigit:
       MOV __temp5,__temp4 ' Begin with starting character (" " or "0")
       CMPSUB __temp2,_100_000_000 WC WR
       IF_C OR __temp5,#16   ' Convert space to zero
       IF_C MOV __temp4,#"0" ' Next digit defaults to zero
       IF_C ADD __temp5,#1   ' Advance digit
       IF_C JMP #NextDigit

       ' __temp2 = __temp2 * 10
       MOV __temp6,__temp2
       SHL __temp6,#3
       ADD __temp2,__temp2
       ADD __temp2,__temp6

StoreDigit:
       ' Do we need to store a "-" ?
       ' Is _temp5 <> " "
       CMP __temp5,#" " WZ   ' If char is a space, don't store "-" yet
       SHL __temp1,#1 WC NR  ' Do we even need a "-" stored ?
       IF_ZANDC WRBYTE __temp1,#"-" ' Store a "-"
       IF_ZANDC AND __temp1,_7FFF_FFFF ' Clear flag that we need a "-" stored
       IF_ZANDC ADD __temp1,#1 ' Move pointer to next char in string

DelaySign:
       WRBYTE __temp1,__temp5
       SHL __temp3,#1 WC WZ
       IF_C ADD __temp1,#1
       IF_NZ JMP #NextDigit


}
      If g_bLMM Then OutStr('  jmp #_LMM_RET')
      Else
      Begin
        OutStr('_STR_ret');
        OutStr('  ret');
      End;
    End; // If g_bUsesStr

    If g_bLMM Then
    Begin
      OutSpin('DAT');
      OutSpin('  ''-------------------------------------------------------------------------------------------');
      OutSpin('  ''LMM Execution code');
      OutSpin('  '' ------------------------------------------------------------------------------------------');
      OutSpin('');
      OutSpin('  ORG 0');
      OutSpin('');
      OutSpin('_LMM_Entry');
      OutStr('    mov __PC,__OFFSET');
      OutSpin('');
      OutSpin('_LMM_LOOP');
      OutStr('    rdlong __INSTR1,__PC');
      OutStr('    add __PC,#4');
      OutSpin('__INSTR1');
      OutStr('    nop   '' Placeholder for LMM instruction');
      OutSpin('');
      OutStr('    rdlong __INSTR2,__PC');
      OutStr('    add __PC,#4');
      OutSpin('__INSTR2');
      OutStr('    nop   '' Placeholder for LMM instruction');
      OutSpin('');
      OutStr('    rdlong __INSTR3,__PC');
      OutStr('    add __PC,#4');
      OutSpin('__INSTR3');
      OutStr('    nop   '' Placeholder for LMM instruction');
      OutSpin('');
      OutStr('    rdlong __INSTR4,__PC');
      OutStr('    add __PC,#4');
      OutSpin('__INSTR4');
      OutStr('    nop   '' Placeholder for LMM instruction');
      OutSpin('');
      OutStr('    jmp #_LMM_LOOP');
      OutSpin('');
      OutSpin('_LMM_JUMP');
      OutStr('    rdlong __PC,__PC');
      OutStr('    jmp #_LMM_LOOP');
      OutSpin('');
      OutSpin('_LMM_CALL');
      OutStr('    movd $+2,__STACKPTR'); // Prepare to save return address
      OutStr('    add __STACKPTR,#1'); // Adjust stack pointer
      OutStr('    mov 0-0,__PC'); // Save return address
      OutStr('    rdlong __PC,__PC'); // Get calling address
      OutStr('    jmp #_LMM_LOOP');
      OutSpin('');
      OutSpin('_LMM_RET');
      OutStr('    sub __STACKPTR,#1'); // Adjust stack pointer
      OutStr('    movs $+2,__STACKPTR'); // Prepare to get return address
      OutStr('    mov __PC,#4'); // skip call address instruction
      OutStr('    add __PC,0-0'); // Get return address
      OutStr('    jmp #_LMM_LOOP');
      OutSpin('');
      OutSpin('_LMM_MOVS');
      OutStr('    rdlong __MOVS,__PC');
      OutStr('    movs __MOVS,__INDEX');
      OutStr('    add __PC,#4');
      OutSpin('__MOVS');
      OutStr('    nop '' Placeholder for LMM instruction');
      OutStr('    jmp #_LMM_LOOP');
      OutSpin('');
      OutSpin('_LMM_MOVD');
      OutStr('    rdlong __MOVD,__PC');
      OutStr('    movd __MOVD,__INDEX');
      OutStr('    add __PC,#4');
      OutSpin('__MOVD');
      OutStr('    nop '' Placeholder for LMM instruction');
      OutStr('    jmp #_LMM_LOOP');
      OutSpin('');
      OutStr('    '' Variables for LMM execution code');
      OutSpin('__INDEX          LONG 0  '' Used by MOVS and MOVD');
      OutSpin('__OFFSET         LONG 0-0'' Set by spin');
      OutSpin('__PC             LONG 0  '' Program counter');
      OutSpin('__STACKPTR       LONG __STACK  '' Stack pointer');
      OutSpin('__STACK          LONG 0['+IntStr(g_iStackSize)+'] '' Return stack');
    End;

    // Output e_LongVar and constants

    // Output __InitDirA and __InitOutA
    WriteLn(g_tfSrc, '''**********************************************************************');
    Write(g_tfSrc, PadStr('__InitDirA', 16), ' LONG %');
    For I:= 31 downto 0 Do
    Begin
      If (pv_lDirA AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
      If I In [24, 16, 8] Then Write(g_tfSrc, '_');
    End;
    If g_bIsP2 Then
    Begin
      Write(g_tfSrc, PadStr('__InitDirB', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lDirB AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
      Write(g_tfSrc, PadStr('__InitDirC', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lDirC AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
      Write(g_tfSrc, PadStr('__InitDirD', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lDirD AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
    End; // g_bIsP2
    WriteLn(g_tfSrc);
    If g_bIsP1 Then
    Begin
      Write(g_tfSrc, PadStr('__InitOutA', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lOutA AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
    End
    Else
    Begin // P2
      Write(g_tfSrc, PadStr('__InitPinA', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lOutA AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
      Write(g_tfSrc, PadStr('__InitPinB', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lPinB AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
      Write(g_tfSrc, PadStr('__InitPinC', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lPinC AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
      Write(g_tfSrc, PadStr('__InitPinD', 16), ' LONG %');
      For I:= 31 downto 0 Do
      Begin
        If (pv_lPinD AND (1 SHL I)) <> 0 Then Write(g_tfSrc, '1') Else Write(g_tfSrc, '0');
        If I In [24, 16, 8] Then Write(g_tfSrc, '_');
      End;
    End;

    WriteLn(g_tfSrc);

    // Reorder vars so that aliased vars are right before the variable they are aliasing
    With g_oDevice.m_oVars Do
    Begin
      I:=1;
      While I <= m_iVarCnt Do
      Begin
        If Upper(m_apoVars[I]^.m_sAlias) <> Upper(m_apoVars[I]^.m_sName) Then
        Begin
          // Find aliased variable place
          J:=1;
          While Upper(m_apoVars[J]^.m_sName) <> Upper(m_apoVars[I]^.m_sAlias) Do Inc(J);
          If J < I Then
          Begin
            // Move vars from J to I-1 down 1, put var[I] in var[J] place
            poVar:=m_apoVars[I];
            For K:=I-1 downto J Do m_apoVars[K+1]:=m_apoVars[K];
            m_apoVars[J]:=poVar;
          End;
        End;
        Inc(I);
      End;
    End; // With


    // Get a sorted array of constant values used so we don't duplicate any
    iLongConstCnt:=0;
    For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      If g_oDevice.m_oVars.m_apoVars[I]^.m_bUsed and (g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_LongConst) Then
      Begin
        Inc(iLongConstCnt);
        asConstNames[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.sGetName;
        alConstValues[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.lGetValue;
//        asConstFormats[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.m_sFormat; removed July 26, 2011 bug when using long constants in LDATA
        If g_oDevice.m_oVars.m_apoVars[I]^.m_sFormat[1] In ['$', '%', '-', '0'..'9'] Then
         asConstFormats[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.m_sFormat
        Else
         asConstFormats[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.sGetValue;
      End;

      If g_oDevice.m_oVars.m_apoVars[I]^.m_bUsed and (g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_Pin) Then
      Begin
        Inc(iLongConstCnt);
        asConstNames[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.sGetName;
        alConstValues[iLongConstCnt]:=g_oDevice.m_oVars.m_apoVars[I]^.lGetValue;
        If g_oDevice.m_oVars.m_apoVars[I]^.m_lElements = 1 Then
         asConstFormats[iLongConstCnt]:='1 << '+IntStr(g_oDevice.m_oVars.m_apoVars[I]^.m_byPinNumber)
        Else
         asConstFormats[iLongConstCnt]:=IntStr((1 SHL g_oDevice.m_oVars.m_apoVars[I]^.m_lElements) - 1)+' << '+IntStr(g_oDevice.m_oVars.m_apoVars[I]^.m_byPinNumber);
      End;
    End;

    For I:=1 to iLongConstCnt-1 Do
    Begin
      For I2:=I+1 to iLongConstCnt Do
      Begin
        If alConstValues[I2] < alConstValues[I] Then
        Begin
          // Swap elements
          sTemp:=asConstNames[I];
          asConstNames[I]:=asConstNames[I2];
          asConstNames[I2]:=sTemp;
          sTemp:=asConstFormats[I];
          asConstFormats[I]:=asConstFormats[I2];
          asConstFormats[I2]:=sTemp;
          lTemp:=alConstValues[I];
          alConstValues[I]:=alConstValues[I2];
          alConstValues[I2]:=lTemp;
        End;
      End;
    End;
    For I:=1 to iLongConstCnt-1 Do
    Begin
      If asConstFormats[I] = asConstFormats[I+1] Then WriteLn(g_tfSrc, PadStr(asConstNames[I], 16))
      Else WriteLn(g_tfSrc, PadStr(asConstNames[I], 16), ' LONG ', asConstFormats[I]);
    End;
    If iLongConstCnt > 0 Then WriteLn(g_tfSrc, PadStr(asConstNames[iLongConstCnt], 16), ' LONG ', asConstFormats[iLongConstCnt]);
    WriteLn(g_tfSrc);

    // Output LongVars with initial values
    For I2:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      With g_oDevice.m_oVars.m_apoVars[I2]^ Do
      Begin
        If (eGetType = e_LongVar) and (m_bHasInitValue) Then
        Begin
          If m_sAlias = m_sName Then WriteLn(g_tfSrc, PadStr(sGetName, 16), ' LONG ', m_sFormat)
           Else WriteLn(g_tfSrc, PadStr(sGetName, 16));
        End;
      End;
    End;

    // Output HUB variables addresses
    For I2:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      With g_oDevice.m_oVars.m_apoVars[I2]^ Do
      Begin
        If eGetType In [e_HubByte, e_HubWord, e_HubLong, e_DataLabel] Then
        Begin
          If m_bHubAbs Then WriteLn(g_tfSrc, PadStr('__'+sGetName+'_adr', 16), ' LONG '+sGetValue)
           Else
           Begin
             If pv_bTask Then WriteLn(g_tfSrc, PadStr('__'+sGetName+'_adr', 16), ' LONG '+IntStr(m_lHubOffset))
              Else WriteLn(g_tfSrc, PadStr('__'+sGetName+'_adr', 16), ' LONG @@@'+sGetName);
           End;
        End;
      End;
    End;

    If g_eDebugger = e_Viewport Then
    Begin
      // Output WATCH variable addresses
      For I2:=1 to g_oDevice.m_oVars.m_iVarCnt Do
      Begin
        With g_oDevice.m_oVars.m_apoVars[I2]^ Do
        Begin
          If (eGetType = e_LongVar) and (m_iWatchPos <> 0) Then
          Begin
            If pv_bTask Then WriteLn(g_tfSrc, PadStr('__'+sGetName+'_adr', 16), ' LONG '+IntStr(m_lHubOffset+((Abs(m_iWatchPos)-1)*4)))
             Else WriteLn(g_tfSrc, PadStr('__'+sGetName+'_adr', 16), ' LONG @@@VP_Watch+'+IntStr((Abs(m_iWatchIndex)-1)*4));
          End;
        End;  // With
      End; // For I2
    End; // If g_eDebugger = e_Viewport

    // Output TASK addresses if for main program
    If Not pv_bTask Then
    Begin
      For I2:=1 to g_iTaskCount Do
      Begin
        WriteLn(g_tfSrc, PadStr(g_asTaskNames[I2]+'_COG', 16), ' LONG 0-0');
      End;
    End; // If Not pv_bTask

    WriteLn(g_tfSrc);

    For I2:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      With g_oDevice.m_oVars.m_apoVars[I2]^ Do
      Begin
        If (Upper(sGetName) <> 'DIRA') and (Upper(sGetName) <> 'INA') and (Upper(sGetName) <> 'OUTA') and (Upper(sGetName) <> 'CNT') and
           (Upper(sGetName) <> 'CTRA') and (Upper(sGetName) <> 'CTRB') and (Upper(sGetName) <> 'FRQA') and (Upper(sGetName) <> 'FRQB') and
           (Upper(sGetName) <> 'PHSA') and (Upper(sGetName) <> 'PHSB') and (Upper(sGetName) <> 'VCFG') and (Upper(sGetName) <> 'VSCL') and
           (Upper(sGetName) <> 'PAR')  and (Upper(sGetName) <> '__RAM') Then
        Begin
          Case eGetType of
            e_LongVar:
              Begin
                If Not m_bHasInitValue Then
                Begin
                  If m_sAlias <> m_sName Then WriteLn(g_tfSrc, sGetName)
                   Else WriteLn(g_tfSrc, PadStr(sGetName, 16), ' RES '+IntStr(m_lElements));
                End;
              End;
          End;
        End;
      End; // With
    End; // FOR I2

    WriteLn(g_tfSrc);

    WriteLn(g_tfSrc, 'FIT 492');
    WriteLn(g_tfSrc);

    WriteLn(g_tfSrc, 'CON');
    For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      With g_oDevice.m_oVars.m_apoVars[I]^ Do
      Begin
        Case eGetType of
          e_ShortConst: WriteLn(g_tfSrc, '  ', PadStr(sGetName, 32), ' = ', m_sFormat);
//          e_HubByte, e_HubWord, e_HubLong, e_DataLabel: If m_bHubAbs = False Then WriteLn(g_tfSrc, '  ', PadStr(sGetName+'_ofs', 32), ' = ', m_lHubOffset);
        End;
      End;  // With
    End; // For I
  End; // If Not g_bIsTask

  // Clear SUB and FUNC list
  g_iSubroutineCnt:=0;


  If pv_bTask Then
  Begin
    // Create subroutine to "fix-up" HUB address values
    OutSpin('');
    OutSpin('PUB _Fixup(pv_DataStart, pv_VP_Watch)');
    // Output HUB variables addresses
    For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      With g_oDevice.m_oVars.m_apoVars[I]^ Do
      Begin
        If eGetType In [e_HubByte, e_HubWord, e_HubLong, e_DataLabel] Then
        Begin
          If (Not m_bHubAbs) Then OutSpin('  __'+sGetName+'_adr += pv_DataStart')
        End;
      End;
    End;

    // Output WATCH variable addresses
    For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      With g_oDevice.m_oVars.m_apoVars[I]^ Do
      Begin
        If (eGetType = e_LongVar) and (m_iWatchPos <> 0) Then OutSpin('  __'+sGetName+'_adr += pv_VP_Watch')
      End;
    End;
    OutSpin('');
  End; // If pv_bTask

  // Delete all e_LongVar variables (keep pins & constants and hub variables; Mark pins & constants as not used)
  For I:=1 to g_oDevice.m_oVars.m_iVarCnt Do
  Begin
    If Not g_oDevice.m_oVars.m_apoVars[I]^.m_bPermanent Then
    Begin
      Case g_oDevice.m_oVars.m_apoVars[I]^.eGetType of
        e_Pin, e_LongConst: g_oDevice.m_oVars.m_apoVars[I]^.m_bUsed:=False;
        e_LongVar:
          Begin
            g_oDevice.m_oVars.m_apoVars[I]^.m_sName:='...';
            g_oDevice.m_oVars.m_apoVars[I]^.m_sAlias:='...';
            g_oDevice.m_oVars.m_apoVars[I]^.m_eType:=e_Ukn;
          End;
      End; // Case
    End; // If
  End; // For

End;

Begin  // Main code
  g_iTotalLineCnt:=0;
  g_iLiteralCnt:=0;
  g_iExitLabelCnt:=0;
  g_iLastErrorLine:=-1;
  g_iParamStringCnt:=0;
  g_iSerInTimeOutCnt:=0;
  g_iSubroutineCnt:=0;
  g_dI2CSpeed:=1.0;
  g_iErrorCnt:=0;
  g_iWarningCnt:=0;
  g_iLabelCnt:=0;
  g_bAutoPage:=True;
  g_iUsesCnt:=0;
  g_iIfCnt:=0;
  g_iCondCnt:=0;
  g_byReturnLongs:=0;
  g_sSubName:='';
  g_lHubOffset:=0;
  g_bIsTask:=False;
  g_bIsProgram:=False;
  g_iTaskCount:=0;
  g_iMaxPin:=31; // Assume P1

  g_lProgInitDirA:=0;
  g_lProgInitOutA:=0;
  g_lTaskInitDirA:=0;
  g_lTaskInitOutA:=0;

  g_lProgInitDirB:=0;
  g_lProgInitPinB:=0;
  g_lTaskInitDirB:=0;
  g_lTaskInitPinB:=0;

  g_lProgInitDirC:=0;
  g_lProgInitPinC:=0;
  g_lTaskInitDirC:=0;
  g_lTaskInitPinC:=0;

  g_lProgInitDirD:=0;
  g_lProgInitPinD:=0;
  g_lTaskInitDirD:=0;
  g_lTaskInitPinD:=0;

  g_sProjectName:='';
  g_bCompoundIfPending:=False;
  g_iStringCnt:=0;
  g_iMaxParams:=4;
  g_bLMM:=False;
  g_bMultiLineComment:=False;
  g_iMinWaitCnt:=16; // 10 for tightest loop native code, 69 for tightest loop LMM
  g_sTaskName:='';
  g_eDebugger:=e_None;
  g_bInLoadCmd:=False;
  g_iLoadFilesCode:=0;
  g_iLoadFilesTasks:=0;
  g_bLoadTasks:=False;

  g_iStackSize:=16;

  g_iWatchPos:=1; // IO is always watched
  g_iWatchIndex:=1; // IO is always watched
  g_sWatchArrays:='';
  g_sWatchLongs:='';
  g_sWatchStrings:='';
  g_sWatchPins:='';
  g_iVPConfigCnt:=0;
  g_iVPArrayCnt:=0;
  g_sVPVideoBuffer:='';
  g_bUsesPause:=False;
  g_bUsesMult:=False;
  g_bUsesDiv:=False;
  g_bUsesStr:=False;
End.

