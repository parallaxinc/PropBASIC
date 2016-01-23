Unit _RCTIME;

{$MODE Delphi}

 // LMM Index

Interface

Uses GLOBAL;

Procedure ProcessRCTime; { pin,state,result }

Implementation

Procedure ProcessRCTime;
Begin
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
      End
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    OutStr('  andn dira,__temp1'); // Make pin an input
    If g_apoCmdVars[6]^.m_bVarIndex Then OutStr('  mov __temp3,#0')
     Else OutStr('  mov '+g_apoCmdVars[6]^.sGetName+',#0'); // Zero count
    // Measure until pin <> state
    OutStr('  mov __temp2,cnt');
    OutStr('  adds __temp2,#'+IntStr(g_lFreq DIV 1000000));
    OutStr(NewLabelStr);
    If g_apoCmdVars[6]^.m_bVarIndex Then OutStr('  adds __temp3,#1')
     Else OutStr('  adds '+g_apoCmdVars[6]^.sGetName+',#1'); // Zero count
    OutStr('  waitcnt __temp2,#'+IntStr(g_lFreq DIV 1000000));
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
      Else OutStr('  IF_Z jmp #'+LabelStr);
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
    g_bHandled:=True;
    If g_apoCmdVars[6]^.m_bVarIndex Then OutStr('  mov '+g_apoCmdVars[6]^.sGetIndexName+',__temp3');
    VP_WriteVar(6);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

End.
