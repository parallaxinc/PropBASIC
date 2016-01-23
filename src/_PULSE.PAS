Unit _PULSE;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessPulsIn;
Procedure ProcessPulsOut;

Implementation

Procedure ProcessPulsIn; { PULSIN pin,state,var ' var = duration in 1uSecs }
Var
  iInc, iClocks: Integer;
Begin
  iInc:=1;
  iClocks:=g_lFreq div 1000000;
  If g_bLMM Then
  Begin // calculate for LMM
    While iClocks < 100 Do // 97 MIN
    Begin
      Inc(iInc);
      iClocks:=g_lFreq div (1000000 Div iInc);
    End;
  End
  Else
  Begin
    While iClocks < 20 Do // 18 MIN
    Begin
      Inc(iInc);
      iClocks:=g_lFreq div (1000000 Div iInc);
    End;
  End;

  If g_iCmdCnt = 6 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    If g_apoCmdVars[6]^.eGetType <> e_LongVar Then Error(c_iErrorInvalidParameter, 6);
    If g_apoCmdVars[4]^.eGetType <> e_ShortConst Then Error(c_iErrorInvalidParameter, 4)
     Else If (g_apoCmdVars[4]^.lGetValue < 0) or (g_apoCmdVars[4]^.lGetValue > 1) Then Error(c_iErrorInvalidParameter, 4);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
        End;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          g_bHandled:=True;
        End;
    e_LongVar:
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
        g_bHandled:=True;
      End
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    OutStr('  andn dira,__temp1'); // Make pin an input
    If g_apoCmdVars[6]^.m_bVarIndex Then OutStr('  mov __temp3,#0')
     Else OutStr('  mov '+g_apoCmdVars[6]^.sGetName+',#0'); // Zero count
    // Wait for pin <> state
    If g_apoCmdVars[4]^.lGetValue = 0 Then
     OutStr('  waitpeq __temp1,__temp1')
    Else
     OutStr('  waitpne __temp1,__temp1');
    // Wait for pin = state
    If g_apoCmdVars[4]^.lGetValue = 0 Then
     OutStr('  waitpne __temp1,__temp1')
    Else
     OutStr('  waitpeq __temp1,__temp1');
    // Measure until pin <> state again
    OutStr('  mov __temp2,cnt');
    OutStr('  adds __temp2,#'+IntStr(iClocks));
    OutStr(NewLabelStr);
    If g_apoCmdVars[6]^.m_bVarIndex Then OutStr('  adds __temp3,#'+IntStr(iInc))
     Else OutStr('  adds '+g_apoCmdVars[6]^.sGetName+',#'+IntStr(iInc));
    OutStr('  waitcnt __temp2,#'+IntStr(iClocks));
    OutStr('  and __temp1,ina NR, WZ');
    // If pin = state then jmp
    If g_apoCmdVars[4]^.lGetValue = 0 Then
    Begin
      If g_bLMM Then
      Begin
        OutStr('  IF_Z sub __PC,#(($-'+LabelStr+')*4)+4');
//        OutStr('  IF_Z jmp #_LMM_JUMP');
//        OutStr('  long '+LabelStr+' * 4');
      End
      Else OutStr('  IF_Z jmp #'+LabelStr)
    End
    Else
    Begin
      If g_bLMM Then
      Begin
        OutStr('  IF_NZ sub __PC,#(($-'+LabelStr+')*4)+4');
//        OutStr('  IF_NZ jmp #_LMM_JUMP');
//        OutStr('  long '+LabelStr+' * 4');
      End
      Else OutStr('  IF_NZ jmp #'+LabelStr);
    End;
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
      OutStr('  mov 0-0,__temp3');
    End;
    VP_WriteVar(6);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessPulsOut; { PULSOUT pin,duration duration in 1uSec }
Var
  iShift: Integer;
  iWait: Integer;
Begin
  // min wait is 10 for native code or 69 for LMM code
  iShift:=0;
  iWait:=g_lFreq Div 1000000;
  If g_bLMM Then
  Begin
    While iWait < 100 Do
    Begin
      Inc(iShift);
      iWait:=g_lFreq Div (1000000 SHR iShift);
    End;
  End
  Else
  Begin
    While iWait < 20 Do
    Begin
      Inc(iShift);
      iWait:=g_lFreq Div (1000000 SHR iShift);
    End;
  End;
  If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          OutStr('  or dira,'+g_apoCmdVars[2]^.sGetName);
          OutStr('  xor outa,'+g_apoCmdVars[2]^.sGetName);
        End;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  or dira,__temp1');
          OutStr('  xor outa,__temp1');
          g_bHandled:=True;
        End;
    e_LongVar:
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
          OutStr('  or dira,__temp1');
          OutStr('  xor outa,__temp1');
        End
        Else
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          OutStr('  or dira,__temp1');
          OutStr('  xor outa,__temp1');
        End;
        g_bHandled:=True;
      End
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    Case g_apoCmdVars[4]^.eGetType of
      e_ShortConst: OutStr('  mov __temp3,#'+g_apoCmdVars[4]^.sGetName);
      e_LongConst: OutStr('  mov __temp3,'+g_apoCmdVars[4]^.sGetName);
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
          OutStr('  mov __temp3,0-0');
        End
        Else OutStr('  mov __temp3,'+g_apoCmdVars[4]^.sGetIndexName);
      End;
    End; // Case
    If iShift > 0 Then OutStr('  shr __temp3,#'+IntStr(iShift));
    OutStr('  min __temp3,#1'); // Always do at least one loop
    OutStr('  mov __temp2,cnt');
    OutStr('  adds __temp2,#'+IntStr(iWait));
    OutStr(NewLabelStr);
    OutStr('  waitcnt __temp2,#'+IntStr(iWait));
    If g_bLMM Then
    Begin
      OutStr('  djnz __temp3,#_LMM_JUMP');
      OutStr('  long '+LabelStr+' * 4');
    End
    Else OutStr('  djnz __temp3,#'+LabelStr);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin: OutStr('  xor outa,'+g_apoCmdVars[2]^.sGetName);
      e_LongVar, e_LongConst, e_ShortConst: OutStr('  xor outa,__temp1');
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


End.
