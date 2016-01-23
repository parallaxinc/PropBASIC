Unit _SHIFT;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessShiftOut;
Procedure ProcessShiftIn;

Implementation

Procedure ProcessShiftOut; { ShiftOut DPin,CPin,Mode,var[\BitsConst=8][,SpeedMult=1] }
{ Clock=5uSec, Data=13uSec }
{ Mode: 0=LSBFIRST, 1=MSBFIRST }
Var
  lDelay: LongInt;
  iMode: Integer;
  iSendBits: Integer;
Begin
// __temp1 = Data Pin BitMask
// __temp2 = Clock Pin BitMask
// __temp3 = Value to send
// __temp4 = Bit count (counts down)
// __temp5 = WaitCnt value
  If (g_iCmdCnt = 8) or (g_iCmdCnt = 10) or (g_iCmdCnt = 12) Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    If g_asCmdLine[7] <> ',' Then Error(c_iErrorCommaExpected, 7);
    // Set __temp1 to Data pin bitmask
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
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin
            OutStr('  mov __temp1,#1');
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
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End;
    // Make data pin (__temp1) an output
    OutStr('  or dira,__temp1'); // Make pin output
    // Set __temp2 to Clock pin bitmask
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin:
        Begin
          OutStr('  mov __temp2,'+g_apoCmdVars[4]^.sGetName);
        End;
      e_ShortConst:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,#'+g_apoCmdVars[4]^.sGetName);
        End;
      e_LongConst, e_LongVar:
        Begin
          If g_apoCmdVars[4]^.m_bVarIndex Then
          Begin
            OutStr('  mov __temp2,#1');
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVS');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
            End;
            OutStr('  shl __temp2,0-0');
          End
          Else
          Begin
            OutStr('  mov __temp2,#1');
            OutStr('  shl __temp2,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End;
    // Make clock pin (__temp2) an output
    OutStr('  or dira,__temp2'); // Make pin output
    // Get mode constant
    If g_apoCmdVars[6]^.eGetType <> e_ShortConst Then Error(c_iErrorInvalidParameter, 6);
    iMode:=g_apoCmdVars[6]^.lGetValue;
    // Set __temp3 to value to shift out
    Case g_apoCmdVars[8]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp3,#'+g_apoCmdVars[8]^.sGetName);
        End;
      e_LongConst, e_LongVar:
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
          Else OutStr('  mov __temp3,'+g_apoCmdVars[8]^.sGetName);
        End;
      Else Error(c_iErrorInvalidParameter, 8);
    End; // Case
    // Set __temp4 to bit count
    iSendBits:=8;
    If (g_iCmdCnt >= 10) and (g_asCmdLine[9] = '\') Then
    Begin
      Case g_apoCmdVars[10]^.eGetType of
        e_ShortConst:
          Begin
            If g_apoCmdVars[10]^.lGetValue In [1..32] Then
            Begin
              OutStr('  mov __temp4,#'+g_apoCmdVars[10]^.sGetName);
              iSendBits:=g_apoCmdVars[10]^.lGetValue;
            End
            Else Error(c_iErrorInvalidParameter, 10);
          End;
        e_LongVar:
          Begin
            OutStr('  mov __temp4,#32');
            OutStr('  sub __temp4,'+g_apoCmdVars[10]^.sGetIndexName);
            iSendBits:=-1;
          End;
        Else Error(c_iErrorInvalidParameter, 10);
      End; // Case
    End
    Else
    Begin
      iSendBits:=8;
      OutStr('  mov __temp4,#8');
    End;
    // Shift value if iMode is MSBFIRST (1)
    If (iMode = 1) and (iSendBits < 31) Then
    Begin
      If iSendBits > 0 Then OutStr('  shl __temp3,#'+IntStr(32-iSendBits))
      Else
      Begin // Bit count is a variable
        OutStr('  shl __temp3,__temp4');
        OutStr('  sub __temp4,#32');
        OutStr('  neg __temp4,__temp4');
      End;
    End;
    // Get delay value
    lDelay:=g_lFreq Div 150000;
    If (g_iCmdCnt = 12) or ((g_iCmdCnt = 10) and (g_asCmdLine[9] = ',')) Then
    Begin
      If g_apoCmdVars[g_iCmdCnt]^.eGetType = e_ShortConst Then
      Begin
        If g_apoCmdVars[g_iCmdCnt]^.lGetValue > 0 Then
        Begin
          lDelay:=lDelay Div g_apoCmdVars[g_iCmdCnt]^.lGetValue;
        End
        Else Error(c_iErrorInvalidParameter, g_iCmdCnt);
      End
      Else Error(c_iErrorConstantExpected, g_iCmdCnt);
    End;
    If lDelay < 20 Then lDelay:=20;
    // Setup initial WaitCnt value
    If lDelay > 511 Then OutStr('  mov __temp5,#511')
     Else OutStr('  mov __temp5,#'+IntStr(lDelay));
    OutStr('  add __temp5,cnt');
    // Main shiftout loop
    //   Put data value bit into carry then into data pin
    OutStr(NewLabelStr);
    If iMode = 1 Then OutStr('  shl __temp3,#1 WC') Else OutStr('  shr __temp3,#1 WC');
    OutStr('  muxc outa,__temp1');
    // Delay
    If lDelay > 511 Then OutStr('  waitcnt __temp5,#511')
     Else OutStr('  waitcnt __temp5,#'+IntStr(lDelay));
    // Toggle clock pin
    OutStr('  xor outa,__temp2');
    // Delay
    If lDelay > 511 Then OutStr('  waitcnt __temp5,#511')
     Else OutStr('  waitcnt __temp5,#'+IntStr(lDelay));
    // Toggle clock pin
    OutStr('  xor outa,__temp2');
    // Delay
    If lDelay > 511 Then OutStr('  waitcnt __temp5,#511')
     Else OutStr('  waitcnt __temp5,#'+IntStr(lDelay));
    If g_bLMM Then
    Begin
      OutStr('  djnz __temp4,#_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  djnz __temp4,#'+LabelStr);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessShiftIn; // ShiftIn DPin,CPin,Mode,ByteVar[\Bits=8][,SpeedMult=1]
// Mode: 0=MSBPRE, 1=LSBPRE, 2=MSBPOST, 3=LSBPOST
Var
  lDelay: LongInt;
  iMode: Integer;
  iRecvBits: Integer;
Begin
// __temp1 = Data Pin BitMask
// __temp2 = Clock Pin BitMask
// __temp3 = Value received
// __temp4 = Bit count (counts down)
// __temp5 = WaitCnt value
  iRecvBits:=8; // assume 8 bits
  If (g_iCmdCnt = 8) or (g_iCmdCnt = 10) or (g_iCmdCnt = 12) Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    If g_asCmdLine[7] <> ',' Then Error(c_iErrorCommaExpected, 7);
    // Set __temp1 to Data pin bitmask
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
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin
            OutStr('  mov __temp1,#1');
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
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End;
    // Make data pin (__temp1) an input
    OutStr('  andn dira,__temp1'); // Make pin input
    // Set __temp2 to Clock pin bitmask
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin:
        Begin
          OutStr('  mov __temp2,'+g_apoCmdVars[4]^.sGetName);
        End;
      e_ShortConst:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,#'+g_apoCmdVars[4]^.sGetName);
        End;
      e_LongConst, e_LongVar:
        Begin
          If g_apoCmdVars[4]^.m_bVarIndex Then
          Begin
            OutStr('  mov __temp2,#1');
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVS');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
            End;
            OutStr('  shl __temp2,0-0');
          End
          Else
          Begin
            OutStr('  mov __temp2,#1');
            OutStr('  shl __temp2,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End;
    // Make clock pin (__temp2) an output
    OutStr('  or dira,__temp2'); // Make pin output
    // Set data value to zero
    OutStr('  mov __temp3,#0');
    // Get mode constant
    If g_apoCmdVars[6]^.eGetType <> e_ShortConst Then Error(c_iErrorInvalidParameter, 6);
    iMode:=g_apoCmdVars[6]^.lGetValue;
    // Set __temp4 to bit count
    If (g_iCmdCnt >= 10) and (g_asCmdLine[9] = '\') Then
    Begin
      Case g_apoCmdVars[10]^.eGetType of
        e_ShortConst:
          Begin
            If g_apoCmdVars[10]^.lGetValue In [1..32] Then
            Begin
              OutStr('  mov __temp4,#'+g_apoCmdVars[10]^.sGetName);
              iRecvBits:=g_apoCmdVars[10]^.lGetValue;
            End
            Else Error(c_iErrorInvalidParameter, 10);
          End;
        e_LongVar:
          Begin
            OutStr('  mov __temp4,'+g_apoCmdVars[10]^.sGetIndexName);
          End;
        Else Error(c_iErrorInvalidParameter, 10);
      End; // Case
    End
    Else
    Begin
      iRecvBits:=8;
      OutStr('  mov __temp4,#8');
    End;
    // Get delay value
    lDelay:=Round(g_lFreq / 100000);
    If (g_iCmdCnt = 12) or ((g_iCmdCnt = 10) and (g_asCmdLine[9] = ',')) Then
    Begin
      If g_apoCmdVars[g_iCmdCnt]^.eGetType = e_ShortConst Then
      Begin
        If g_apoCmdVars[g_iCmdCnt]^.lGetValue > 0 Then
        Begin
          lDelay:=lDelay Div g_apoCmdVars[g_iCmdCnt]^.lGetValue;
        End
        Else Error(c_iErrorInvalidParameter, g_iCmdCnt);
      End
      Else Error(c_iErrorConstantExpected, g_iCmdCnt);
    End;
    // Setup initial WaitCnt value
    OutStr('  mov __temp5,cnt');
    If lDelay > 511 Then OutStr('  add __temp5,#511')
     Else OutStr('  add __temp5,#'+IntStr(lDelay));
    // Main shiftout loop
    //   Put data value bit into carry then into data pin
    OutStr(NewLabelStr);
    If iMode In [0, 1] Then
    Begin // Pre modes
      // Put data bit into Carry
      OutStr('  and __temp1,ina NR, WC');
      // Rotate carry into result (__temp3)
      OutStr('  rcl __temp3,#1');
    End;
    // Toggle clock pin
    OutStr('  xor outa,__temp2');
    // Delay
    If lDelay > 511 Then OutStr('  waitcnt __temp5,#511')
     Else OutStr('  waitcnt __temp5,#'+IntStr(lDelay));
    // Toggle clock pin
    OutStr('  xor outa,__temp2');
    // Delay
    If lDelay > 511 Then OutStr('  waitcnt __temp5,#511')
     Else OutStr('  waitcnt __temp5,#'+IntStr(lDelay));
    If iMode In [2, 3] Then
    Begin // Post modes
      // Put data bit into Carry
      OutStr('  and __temp1,ina NR, WC');
      // Rotate carry into result (__temp3)
      OutStr('  rcl __temp3,#1');
    End;
    If g_bLMM Then
    Begin
      OutStr('  djnz __temp4,#_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  djnz __temp4,#'+LabelStr);
    // Reverse bits if iMode is LSBFIRST (1,3)
    If iMode In [1, 3] Then
    Begin
      If (g_iCmdCnt < 10) or (g_apoCmdVars[10]^.eGetType = e_ShortConst) Then OutStr('  rev __temp3,#'+IntStr(32-iRecvBits))
       Else
      Begin
        OutStr('  mov __temp1,#32');
        OutStr('  sub __temp1,'+g_apoCmdVars[10]^.sGetIndexName);
        OutStr('  rev __temp3,__temp1');
      End;
    End;
    If g_apoCmdVars[8]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[8]^.m_bVarIndex Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[8]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[8]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[8]^.m_sIndex+',#'+g_apoCmdVars[8]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[8]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[8]^.m_sIndex+',#'+g_apoCmdVars[8]^.sGetName);
        End;
        OutStr('  mov 0-0,__temp3');
      End
      Else OutStr('  mov '+g_apoCmdVars[8]^.sGetName+',__temp3');
      VP_WriteVar(8);
    End
    Else Error(c_iErrorInvalidParameter, 8);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

End.
