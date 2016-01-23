Unit _SERIAL;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessSerOut;
Procedure ProcessSerIn;

Implementation

Procedure ProcessSerOut; { SerOut Pin,Baud,Data }
{ OT = Open drain, driven high }
{ ON = Open source, driven low }
Var
  poDelayVar: PVarObj;
  lBaudDelay: LongInt;
  lBaudRate: Longint;
  sBaudRate, sBaudDelay: String[15];
  sMode: Char;
  bOpenMode: Boolean;
  sNextCharLabel, sZeroCharLabel: String;
  iValError: Integer;
Begin
  sNextCharLabel:='';
  If g_bInternalClock Then Warning(c_iWarningInternalClock, 1);
  If g_iCmdCnt = 6 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    // temp1 = pin mask
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
        End;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
        End;
      e_LongConst, e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVS');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[2]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
            End;
            OutStr('  shl __temp1,0-0');
          End
          Else
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    // temp2 = character
    If g_apoCmdVars[6]^.eGetType =  e_ShortConst Then
    Begin
      OutStr('  mov __temp2,#'+g_apoCmdVars[6]^.sGetName);
    End
    Else If g_apoCmdVars[6]^.eGetType In [e_LongConst, e_LongVar] Then
    Begin
      If g_apoCmdVars[6]^.m_bVarIndex Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[6]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[6]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVS');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[6]^.m_sIndex+',#'+g_apoCmdVars[6]^.sGetName);
          OutStr('  movs $+2,'+g_apoCmdVars[6]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[6]^.m_sIndex+',#'+g_apoCmdVars[6]^.sGetName);
        End;
        OutStr('  mov __temp2,0-0');
      End
      Else OutStr('  mov __temp2,'+g_apoCmdVars[6]^.sGetIndexName);
    End
    Else If g_apoCmdVars[6]^.eGetType In [e_HubByte, e_DataLabel, e_ByteData]  Then
    Begin
      ProcessHubAddrLit(6, '__temp5', 1);
      sNextCharLabel:=NewLabelStr;
      OutStr(sNextCharLabel);
      sZeroCharLabel:=NewLabelStr;
      OutStr('  rdbyte __temp2,__temp5 WZ');
      OutStr('  adds __temp5,#1');
      If g_bLMM Then OutStr('  IF_Z add __PC,#(('+sZeroCharLabel+'-$)*4)-4')
      Else OutStr('  IF_Z  jmp #'+sZeroCharLabel);
    End
    Else Error(c_iErrorInvalidParameter, 6);
    OutStr('  or __temp2,#256'); // Stop bit is 1
    OutStr('  shl __temp2,#1'); // Make room for start bit

    sBaudRate:=Upper(g_asCmdLine[4]);
    sMode:=UpCase(sBaudRate[1]);
    If sBaudRate <> '' Then Delete(sBaudRate, 1, 1);
    bOpenMode:=False;
    If sMode = 'O' Then
    Begin
      bOpenMode:=True;
      sMode:=UpCase(sBaudRate[1]);
      If sBaudRate <> '' Then Delete(sBaudRate, 1, 1);
    End;
    Val(sBaudRate, lBaudRate, iValError);

    Inc(iValError); // avoid lazarus warning

    lBaudDelay:=1000;
    If lBaudRate < 1 Then Error(c_iErrorInvalidParameter, 4)
    Else
    Begin
      lBaudDelay:=g_lFreq Div lBaudRate;
      If lBaudDelay < 10 Then Error(c_iErrorBaudRateTooHigh, 4);
    End;

    sBaudDelay:='_'+sBaudRate+'baud';
    poDelayVar:=g_oDevice.pGetVarPtr(sBaudDelay);
    If poDelayVar = Nil Then poDelayVar:=g_oDevice.pAddLongConst(sBaudDelay, lBaudDelay, IntStr(lBaudDelay));

    OutStr('  mov __temp3,#10'); // Send 10 bits
    If sMode In ['T', 'N'] Then
    Begin // Normal serial output
      // Start Bit is a "0" bit. High for True, Low for iNverted
      OutStr('  mov __temp4,'+sBaudDelay);
      OutStr('  add __temp4,cnt');

      If bOpenMode Then
      Begin // Setup dira and outa
        OutStr('  andn dira,__temp1'); // Make pin an input
        If sMode = 'N' Then OutStr('  or outa,__temp1') // Setup to drive high "ON"
        Else OutStr('  andn outa,__temp1'); // Setup to drive low "OT"
      End
      Else OutStr('  or dira,__temp1'); // Make pin output

      OutStr(NewLabelStr);
      OutStr('  shr __temp2,#1 WC');

      If bOpenMode = False Then
      Begin
        If sMode = 'T' Then OutStr('  muxc outa,__temp1')
        Else OutStr('  muxnc outa,__temp1');
      End
      Else
      Begin
        If sMode = 'N' Then OutStr('  muxc dira,__temp1')
         Else OutStr('  muxnc dira,__temp1');
      End;

      OutStr('  waitcnt __temp4,'+poDelayVar^.sGetName);
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp3,#_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  djnz __temp3,#'+LabelStr);
      If sNextCharLabel <> '' Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  sub __PC,#(($-'+sNextCharLabel+')*4)+4');
//          OutStr('  jmp #_LMM_JUMP');
//          OutStr('  long '+sNextCharLabel+' * 4');
        End
        Else OutStr('  jmp #'+sNextCharLabel);
        OutStr(sZeroCharLabel);
      End;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessSerIn; { SerIn Pin,Baud,LongVar,[timeout[,to_label]] or SERIN Pin,Baud,String[,terminator]}
Var
  lBaudDelay: LongInt;
  lBaudRate: Longint;
  sBaudRate, sBaudDelay: String[15];
  sMode: Char;
  sTOLabelStr: String;
  sNextCharLabel: String;
  iValError: Integer;
Begin
  If g_bInternalClock Then Warning(c_iWarningInternalClock, 1);
  If g_iCmdCnt In [6, 8, 9, 10] Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    // temp1 = pin mask
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
        End;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
        End;
      e_LongConst, e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVS');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[2]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
            End;
            OutStr('  shl __temp1,0-0');
          End
          Else
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    OutStr('  andn dira,__temp1'); // Make pin input
    sBaudRate:=Upper(g_asCmdLine[4]);
    sMode:=UpCase(sBaudRate[1]);
    If sBaudRate <> '' Then Delete(sBaudRate, 1, 1);
    If sMode = 'O' Then
    Begin // Ignore if open mode is specified
      sMode:=UpCase(sBaudRate[1]);
      If sBaudRate <> '' Then Delete(sBaudRate, 1, 1);
    End;
    Val(sBaudRate, lBaudRate, iValError);

    Inc(iValError); // avoid lazarus warning

    lBaudDelay:=1000;
    If lBaudRate < 1 Then Error(c_iErrorInvalidParameter, 4)
    Else
    Begin
      lBaudDelay:=g_lFreq Div lBaudRate;
      If lBaudDelay < 10 Then Error(c_iErrorBaudRateTooHigh, 4);
    End;

    sBaudDelay:='_'+sBaudRate+'baud';
    If g_oDevice.pGetVarPtr(sBaudDelay) = Nil Then g_oDevice.pAddLongConst(sBaudDelay, lBaudDelay, IntStr(lBaudDelay));

    If g_iCmdCnt In [8, 10] Then
    Begin
      If g_asCmdLine[7] <> ',' Then Error(c_iErrorCommaExpected, 7);
      If (g_iCmdCnt = 10) and (g_asCmdLine[9] <> ',') Then Error(c_iErrorCommaExpected, 9);
      Case g_apoCmdVars[8]^.eGetType of
        e_LongVar, e_LongConst:
          Begin
            If g_apoCmdVars[8]^.m_bVarIndex Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_apoCmdVars[8]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[8]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[8]^.m_sIndex+',#'+g_apoCmdVars[8]^.sGetName);
                OutStr('  movs $+2,'+g_apoCmdVars[8]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[8]^.m_sIndex+',#'+g_apoCmdVars[8]^.sGetName);
              End;
              OutStr('  mov __temp3,0-0');
            End
            Else OutStr('  mov __temp3,'+g_apoCmdVars[8]^.sGetIndexName);
          End;
        e_ShortConst: OutStr('  mov __temp3,#'+g_apoCmdVars[8]^.sGetName);
      End;
      If g_oDevice.pGetVarPtr('_1mSec') = Nil Then g_oDevice.pAddLongConst('_1mSec', g_lFreq Div 1000, IntStr(g_lFreq Div 1000));
      OutStr('  mov __temp4,_1mSec');
      OutStr('  add __temp4,cnt');
    End;


    If g_apoCmdVars[6]^.eGetType In [e_ByteData, e_HubByte, e_DataLabel] Then
    Begin
      ProcessHubAddrLit(6, '__temp5', 1);
      sNextCharLabel:=NewLabelStr;
      OutStr(sNextCharLabel);
      g_bHandled:=True;
    End;


    // Wait for idle state
    If g_iCmdCnt = 8 Then sTOLabelStr:=NewLabelStr;

    If g_iCmdCnt In [8, 10] Then
    Begin
      OutStr(NewLabelStr);
      OutStr('  cmp __temp4,cnt WC');
      OutStr('  IF_C add __temp4,_1mSec');
      OutStr('  IF_C sub __temp3,#1 WZ');
      If g_iCmdCnt = 8 Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  IF_C_AND_Z rdlong __PC,__PC');
          OutStr('  long @@@'+sTOLabelStr);
        End
        Else OutStr('  IF_C_AND_Z jmp #'+sTOLabelStr);
      End
      Else
      Begin
        If g_bLMM Then
        Begin
          OutStr('  IF_C_AND_Z rdlong __PC,__PC');
          OutStr('  long @@@'+g_asCmdLine[10]);
        End
        Else OutStr('  IF_C_AND_Z jmp #'+g_asCmdLine[10]);
      End;
      OutStr('  and __temp1,ina WZ, NR');
    End;
    If sMode = 'T' Then
    Begin
      If g_iCmdCnt In [8, 10] Then
      Begin
        If g_bLMM Then OutStr('  IF_Z sub __PC,#(($-'+LabelStr+')*4)+4')
         Else OutStr('  IF_Z jmp #'+LabelStr)
      End
      Else OutStr('  waitpeq __temp1,__temp1');
    End
    Else
    Begin
      If g_iCmdCnt In [8, 10] Then
      Begin
        If g_bLMM Then OutStr('  IF_NZ sub __PC,#(($-'+LabelStr+')*4)+4')
         Else OutStr('  IF_NZ jmp #'+LabelStr);
      End
      Else OutStr('  waitpne __temp1,__temp1');
    End;

    // Code to wait for start bit
    If g_iCmdCnt In [8, 10] Then
    Begin
      OutStr(NewLabelStr);
      OutStr('  cmp __temp4,cnt WC');
      OutStr('  IF_C add __temp4,_1mSec');
      OutStr('  IF_C sub __temp3,#1 WZ');
      If g_iCmdCnt = 8 Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  IF_C_AND_Z rdlong __PC,__PC');
          OutStr('  long @@@'+sTOLabelStr);
        End
        Else OutStr('  IF_C_AND_Z jmp #'+sTOLabelStr)
      End
      Else
      Begin
        If g_bLMM Then
        Begin
          OutStr('  IF_C_AND_Z rdlong __PC,__PC');
          OutStr('  long @@@'+g_asCmdLine[10]);
        End
        Else OutStr('  IF_C_AND_Z jmp #'+g_asCmdLine[10]);
      End;
      OutStr('  and __temp1,ina WZ, NR');
    End;
    If Not (sMode In ['T', 'N']) Then Error(c_iErrorInvalidParameter, 2);
    If sMode = 'T' Then
    Begin
      If g_iCmdCnt In [8, 10] Then
      Begin
        If g_bLMM Then OutStr('  IF_NZ sub __PC,#(($-'+LabelStr+')*4)+4')
         Else OutStr('  IF_NZ jmp #'+LabelStr);
      End
      Else OutStr('  waitpne __temp1,__temp1');
    End
    Else
    Begin
      If g_iCmdCnt In [8, 10] Then
      Begin
        If g_bLMM Then OutStr('  IF_Z sub __PC,#(($-'+LabelStr+')*4)+4')
         Else OutStr('  IF_Z jmp #'+LabelStr);
      End
      Else OutStr('  waitpeq __temp1,__temp1');
    End;
    
    // Wait 1.5 bit periods
    OutStr('  mov __temp3,'+sBaudDelay);
    OutStr('  shr __temp3,#1');
    OutStr('  add __temp3,'+sBaudDelay);
    OutStr('  add __temp3,cnt');
    OutStr('  mov __temp2,#128'); // Clear result
    // Get data bits
    OutStr(NewLabelStr); // Get data bits
    OutStr('  waitcnt __temp3,'+sBaudDelay);
    OutStr('  and __temp1,ina WZ, NR');
    If sMode = 'T' Then OutStr('  muxnz __temp2,#256')
     Else OutStr('  muxz __temp2,#256');
    OutStr('  shr __temp2,#1 WC');
    If g_bLMM Then
    Begin
      OutStr('  IF_NC jmp #_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  IF_NC jmp #'+LabelStr);
    // temp2 = character
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar:
        Begin
          If g_apoCmdVars[6]^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[6]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[6]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVD');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[6]^.m_sIndex+',#'+g_apoCmdVars[6]^.sGetName);
              OutStr('  movd $+2,'+g_apoCmdVars[6]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[6]^.m_sIndex+',#'+g_apoCmdVars[6]^.sGetName);
            End;
            OutStr('  mov 0-0,__temp2');
          End
          Else OutStr('  mov '+g_apoCmdVars[6]^.sGetName+',__temp2');
          VP_WriteVar(6);
          g_bHandled:=True;
        End;
      e_ByteData, e_HubByte, e_DataLabel:
        Begin
          OutStr('  cmp __temp2,#13 WZ');
          OutStr('  IF_E mov __temp2,#0');
          OutStr('  wrbyte __temp2,__temp5');
          OutStr('  IF_NE add __temp5,#1');
          If g_bLMM Then OutStr('  IF_NE sub __PC,#(($-'+sNextCharLabel+')*4)+4')
           Else OutStr('  IF_NE jmp #'+sNextCharLabel);
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; // Case
    If g_iCmdCnt = 8 Then OutSpin(sTOLabelStr);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

End.
