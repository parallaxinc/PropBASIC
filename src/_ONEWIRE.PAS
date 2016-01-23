Unit _ONEWIRE;

{$MODE Delphi}

 // LMM Index


Interface

Uses GLOBAL;

Procedure ProcessOWReset;  // OWRESET DQPin[,var]
Procedure ProcessOWRead; // OWRDBYTE DQPin,bytevar | arrayelement
Procedure ProcessOWWrite; // OWWRBYTE DQPin, byteexpr

Implementation

Procedure ProcessOWReset; // OWRESET Pin[,var]
// If sense: low 500, high 15, readbit1, high 70, readbit0, high 500
// Non sensing: low500, high500
Var
  poVar: PVarObj;
  l_15uSec, l_70uSec, l_500uSec: LongInt;
Begin
  If g_iCmdCnt In [2, 4] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin: OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    // Make pin low
    OutStr('  andn outa,__temp1');
    OutStr('  or dira,__temp1');
    // Pause 500uS
    l_15uSec:=g_lFreq Div 66666;
    If l_15uSec > 511 Then
    Begin
      poVar:=g_oDevice.pGetVarPtr('_15uSec');
      If poVar = Nil Then g_oDevice.pAddLongConst('_15uSec', l_15uSec, IntStr(l_15uSec));
    End;
    l_70uSec:=g_lFreq Div 14285;
    If l_70uSec > 511 Then
    Begin
      poVar:=g_oDevice.pGetVarPtr('_70uSec');
      If poVar = Nil Then g_oDevice.pAddLongConst('_70uSec', l_70uSec, IntStr(l_70uSec));
    End;
    l_500uSec:=g_lFreq Div 2000;
    If l_500uSec > 511 Then
    Begin
      poVar:=g_oDevice.pGetVarPtr('_500uSec');
      If poVar = Nil Then g_oDevice.pAddLongConst('_500uSec', l_500uSec, IntStr(l_500uSec));
      OutStr('  mov __temp2,_500uSec');
    End
    Else
    Begin
      OutStr('  mov __temp2,#'+IntStr(l_500uSec));
    End;
    OutStr('  add __temp2,cnt');
    // Pause 500uSec, setup for 15uSec
    If l_15uSec > 511 Then OutStr('  waitcnt __temp2,_15uSec')
     Else OutStr('  waitcnt __temp2,#'+IntStr(l_15uSec));
    // Make pin input
    OutStr('  andn dira,__temp1');
    If g_iCmdCnt = 4 Then
    Begin
      If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
      OutStr('  mov __temp3,#0');
      If l_70uSec > 511 Then OutStr('  waitcnt __temp2,_70uSec')
       Else OutStr('  waitcnt __temp2,#'+IntStr(l_70uSec));
      // If pin is 1, set [4].1
      OutStr('  and __temp1,ina WZ, NR');
      OutStr('  muxnz __temp3,#2');
      // Pause 70uS
      OutStr('  waitcnt __temp2,#0');
      // If pin is 1 set [4].0
      OutStr('  and __temp1,ina WZ, NR');
      OutStr('  muxnz __temp3,#1');
      If  g_apoCmdVars[4]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[4]^.m_bVarIndex Then
        Begin
          If g_bLMM Then
          Begin
            OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
            OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
            OutStr('  jmp #_LMM_MOVD');
          End
          Else
          Begin
            OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[4]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
          End;
          OutStr('  mov 0-0,__temp3');
        End
        Else OutStr('  mov '+g_apoCmdVars[4]^.sGetIndexName+',__temp3');
      End
      Else Error(c_iErrorLongVariableExpected, 4);
    End;
    If l_500uSec > 511 Then OutStr('  add __temp2,_500uSec')
     Else OutStr('  add __temp2,#'+IntStr(l_500uSec));
    OutStr('  waitcnt __temp2,#0');
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessOWRead; // OWRead Pin,var{\bits}
Var
  poVar: PVarObj;
  l_60uSec, l_10uSec: LongInt;
  iBits: Integer;
Begin
  iBits:=8; // Assume eight bits
  If g_iCmdCnt In [4, 6] Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_apoCmdVars[4]^.eGetType = e_LongVar Then
    Begin
      Case g_apoCmdVars[2]^.eGetType of
        e_Pin: OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
        e_ShortConst:
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          End;
        e_LongVar:
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        Else Error(c_iErrorInvalidParameter, 2);
      End; // Case
      l_10uSec:=g_lFreq Div 100000;
      If l_10uSec > 511 Then
      Begin
        poVar:=g_oDevice.pGetVarPtr('_10uSec');
        If poVar = Nil Then g_oDevice.pAddLongConst('_10uSec', l_10uSec, IntStr(l_10uSec));
      End;
      l_60uSec:=g_lFreq Div 16666;
      If l_60uSec > 511 Then
      Begin
        poVar:=g_oDevice.pGetVarPtr('_60uSec');
        If poVar = Nil Then g_oDevice.pAddLongConst('_60uSec', l_60uSec, IntStr(l_60uSec));
      End;
      // clear [4]
      OutStr('  mov '+g_apoCmdVars[4]^.sGetIndexName+',#0');
      If g_iCmdCnt = 6 Then
      Begin
        If g_asCmdLine[5] = '\' Then
        Begin
          Case g_apoCmdVars[6]^.eGetType of
            e_ShortConst:
              Begin
                OutStr('  mov __temp3,#'+g_apoCmdVars[6]^.sGetName);
                iBits:=g_apoCmdVars[6]^.lGetValue;
              End;
            e_LongVar:
              Begin
                OutStr('  mov __temp3,'+g_apoCmdVars[6]^.sGetIndexName);
                iBits:=-1;
              End;
            Else Error(c_iErrorInvalidParameter, 6);
          End; // case
        End
        Else Error(c_iErrorSyntaxError, 5);
      End
      Else
      Begin
        OutStr('  mov __temp3,#8');
        iBits:=8;
      End;
      If (iBits < 0) or (iBits > 9) Then
      Begin
        OutStr('  mov __temp4,#1');
        OutStr('  shl __temp4,__temp3');
        OutStr('  shr __temp4,#1');
      End;
      OutStr(NewLabelStr);
      // make pin an output
      OutStr('  or dira,__temp1');
      OutStr('  shr '+g_apoCmdVars[4]^.sGetIndexName+',#1');
      // pause 5us
      OutStr('  mov __temp2,cnt');
      OutStr('  add __temp2,#'+IntStr(g_lFreq Div 200000));
      If l_10uSec > 511 Then OutStr('  waitcnt __temp2,_10uSec')
       Else OutStr('  waitcnt __temp2,#'+IntStr(l_10uSec));
      // make pin an input
      OutStr('  andn dira,__temp1');
      If l_60uSec > 511 Then OutStr('  waitcnt __temp2,_60uSec')
       Else OutStr('  waitcnt __temp2,#'+IntStr(l_60uSec));
      // put pin state into [4]
      OutStr('  and __temp1,ina WZ, NR');
      If (iBits > 0) and (iBits < 10) Then OutStr('  IF_NZ or '+g_apoCmdVars[4]^.sGetIndexName+',#'+IntStr(1 shl (iBits-1)))
       Else OutStr('  IF_NZ or '+g_apoCmdVars[4]^.sGetIndexName+',__temp4');
      // pause 55us
      OutStr('  waitcnt __temp2,#0');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp3,#_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  djnz __temp3,#'+LabelStr);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 4);
    VP_WriteVar(4);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessOWWrite; // OWWrite Pin, value{\bits}
Var
  poVar: PVarObj;
  l_5uSec, l_10uSec, l_60uSec, l_65uSec: LongInt;
Begin
  // 1 bit = low for 5us, open for 65us
  // 0 bit = low for 60us, open for 10us
  If g_iCmdCnt In [4, 6] Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_apoCmdVars[4]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
    Begin
      Case g_apoCmdVars[2]^.eGetType of
        e_Pin: OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
        e_ShortConst:
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          End;
        e_LongVar:
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        Else Error(c_iErrorInvalidParameter, 2);
      End; // Case

      l_5uSec:=g_lFreq Div 200000;
      l_10uSec:=g_lFreq Div 100000;
      l_60uSec:=g_lFreq Div 16666;
      l_65uSec:=g_lFreq Div 15384;

      If l_10uSec > 511 Then
      Begin
        poVar:=g_oDevice.pGetVarPtr('_10uSec');
        If poVar = Nil Then g_oDevice.pAddLongConst('_10uSec', l_10uSec, IntStr(l_10uSec));
      End;

      If l_60uSec > 511 Then
      Begin
        poVar:=g_oDevice.pGetVarPtr('_60uSec');
        If poVar = Nil Then g_oDevice.pAddLongConst('_60uSec', l_60uSec, IntStr(l_60uSec));
      End;

      If l_65uSec > 511 Then
      Begin
        poVar:=g_oDevice.pGetVarPtr('_65uSec');
        If poVar = Nil Then g_oDevice.pAddLongConst('_65uSec', l_65uSec, IntStr(l_65uSec));
      End;

      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst: OutStr('  mov __temp2,#'+g_apoCmdVars[4]^.sGetName);
        e_LongConst: OutStr('  mov __temp2,'+g_apoCmdVars[4]^.sGetName);
        e_LongVar:
          Begin
            If g_apoCmdVars[4]^.m_bVarIndex Then
            Begin
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
              OutStr('  mov __temp2,0-0');
            End
            Else OutStr('  mov __temp2,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End; // Case
      If g_iCmdCnt = 6 Then
      Begin
        If g_asCmdLine[5] = '\' Then
        Begin
          Case g_apoCmdVars[6]^.eGetType of
            e_ShortConst: OutStr('  mov __temp3,#'+g_apoCmdVars[6]^.sGetName);
            e_LongVar: OutStr('  mov __temp3,'+g_apoCmdVars[6]^.sGetIndexName);
            Else Error(c_iErrorInvalidParameter, 6);
          End; // case
        End
        Else Error(c_iErrorSyntaxError, 5);
      End
      Else OutStr('  mov __temp3,#8');
      OutStr(NewLabelStr);
      OutStr('  shr __temp2,#1 WC');
      OutStr('  or dira,__temp1');
      OutStr('  mov __temp4,cnt');
      If l_60uSec > 511 Then OutStr('  IF_NC add __temp4,_60uSec') Else OutStr('  IF_NC add __temp4,#'+IntStr(l_60uSec));
      If l_10uSec > 511 Then OutStr('  IF_NC waitcnt __temp4,_10uSec') Else OutStr('  IF_NC waitcnt __temp4,#'+IntStr(l_10uSec));
      OutStr('  IF_C add __temp4,#'+IntStr(l_5uSec));
      If l_65uSec > 511 Then OutStr('  IF_C waitcnt __temp4,_65uSec') Else OutStr('  IF_C waitcnt __temp4,#'+IntStr(l_65uSec));
      OutStr('  andn dira,__temp1');
      OutStr('  waitcnt __temp4,#0');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp3,#_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  djnz __temp3,#'+LabelStr);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


End.
