Unit _INC;

{$MODE Delphi}

 // LMM Index

Interface

Uses GLOBAL;

Procedure ProcessInc;
Procedure ProcessDec;

Implementation

Procedure ProcessInc;
Begin
  If g_iCmdCnt In [2, 4] Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin
        If (g_apoCmdVars[4]^.eGetType = e_LongVar) and (g_apoCmdVars[4]^.m_bVarIndex) Then
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
          OutStr('  mov __temp1,0-0');
        End;

        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
        End;
        If g_iCmdCnt = 4 Then
        Begin
          If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  adds 0-0,#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst: OutStr('  adds 0-0,'+g_apoCmdVars[4]^.sGetName);
            e_LongVar:
              Begin
                If g_apoCmdVars[4]^.m_bVarIndex Then OutStr('  adds 0-0,__temp1')
                 Else OutStr('  adds 0-0,'+g_apoCmdVars[4]^.sGetIndexName);
              End;
            Else Error(c_iErrorInvalidParameter, 4);
          End; // Case
        End
        Else OutStr('  adds 0-0,#1');
      End
      Else
      Begin
        If g_iCmdCnt = 4 Then
        Begin
          If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst: OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetName);
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
                  OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
                End
                Else OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              End;
            Else Error(c_iErrorInvalidParameter, 4);
          End; // Case
        End
        Else
        Begin
          OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+',#1');
        End;
      End;
      VP_WriteVar(2);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End;
End;

Procedure ProcessDec;
Begin
  If g_iCmdCnt In [2, 4] Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin
        If (g_apoCmdVars[4]^.eGetType = e_LongVar) and (g_apoCmdVars[4]^.m_bVarIndex) Then
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
          OutStr('  mov __temp1,0-0');
        End;

        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
        End;
        If g_iCmdCnt = 4 Then
        Begin
          If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  subs 0-0,#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst: OutStr('  subs 0-0,'+g_apoCmdVars[4]^.sGetName);
            e_LongVar:
              Begin
                If g_apoCmdVars[4]^.m_bVarIndex Then OutStr('  subs 0-0,__temp1')
                 Else OutStr('  subs 0-0,'+g_apoCmdVars[4]^.sGetIndexName);
              End;
            Else Error(c_iErrorInvalidParameter, 4);
          End; // Case
        End
        Else OutStr('  subs 0-0,#1');
      End
      Else
      Begin
        If g_iCmdCnt = 4 Then
        Begin
          If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst: OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetName);
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
                  OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
                End
                Else OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              End;
            Else Error(c_iErrorInvalidParameter, 4);
          End; // Case
        End
        Else OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',#1');
      End;
      VP_WriteVar(2);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End;
End;

End.
