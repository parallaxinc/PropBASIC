Unit _PROP;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessRdLong;
Procedure ProcessRdWord;
Procedure ProcessRdSWord;
Procedure ProcessRdByte;
Procedure ProcessRdSByte;

Procedure ProcessWrLong;
Procedure ProcessWrWord;
Procedure ProcessWrByte;

Procedure ProcessWaitCnt;
Procedure ProcessWaitVid;
Procedure ProcessWaitPeq;
Procedure ProcessWaitPne;

Procedure ProcessCounterAB(pv_bIsCounterB: Boolean);

Procedure ProcessLockNew;
Procedure ProcessLockRet;
Procedure ProcessLockSet;
Procedure ProcessLockClr;

Procedure ProcessClkSet;

Implementation

Procedure ProcessRdLong;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    // Hub write to a pointer
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 4)
     Else ProcessHubAddrLit(2, '__temp1', 4);
  End;
  iPlace:=3;
//  If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      If g_apoCmdVars[iPlace+1]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
        Begin
          If g_bLMM Then
          Begin
            OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  jmp #_LMM_MOVD');
          End
          Else
          Begin
            OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
          End;
          OutStr('  rdlong 0-0,'+sName);
        End
        Else
        Begin
          OutStr('  rdlong '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          VP_WriteVar(iPlace+1);
        End;
        Inc(iPlace, 2);
        If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#4');
      End
      Else
      Begin
        Error(c_iErrorInvalidParameter, iPlace+1);
        iPlace:=g_iCmdCnt;
      End;
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;

Procedure ProcessRdWord;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 2)
     Else ProcessHubAddrLit(2, '__temp1', 2);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      If g_apoCmdVars[iPlace+1]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
        Begin
          If g_bLMM Then
          Begin
            OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  jmp #_LMM_MOVD');
          End
          Else
          Begin
            OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
          End;
          OutStr('  rdword 0-0,'+sName);
        End
        Else
        Begin
          OutStr('  rdword '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          VP_WriteVar(iPlace+1);
        End;
        Inc(iPlace, 2);
        If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#2');
      End
      Else
      Begin
        Error(c_iErrorInvalidParameter, iPlace+1);
        iPlace:=g_iCmdCnt;
      End;
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;


Procedure ProcessRdSWord;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 2)
     Else ProcessHubAddrLit(2, '__temp1', 2);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      If g_apoCmdVars[iPlace+1]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
        Begin
          OutStr('  rdword __temp1,'+sName);
          OutStr('  shl __temp1,#16');
          OutStr('  sar __temp1,#16');
          If g_bLMM Then
          Begin
            OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  jmp #_LMM_MOVD');
          End
          Else
          Begin
            OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
          End;
          OutStr('  mov 0-0,__temp1');
        End
        Else
        Begin
          OutStr('  rdword '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          OutStr('  shl '+g_apoCmdVars[iPlace+1]^.sGetIndexName+',#16');
          OutStr('  sar '+g_apoCmdVars[iPlace+1]^.sGetIndexName+',#16');
          VP_WriteVar(iPlace+1);
        End;
        Inc(iPlace, 2);
        If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#2');
      End
      Else
      Begin
        Error(c_iErrorInvalidParameter, iPlace+1);
        iPlace:=g_iCmdCnt;
      End;
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;


Procedure ProcessRdByte;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 1)
     Else ProcessHubAddrLit(2, '__temp1', 1);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      If g_apoCmdVars[iPlace+1]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
        Begin
          If g_bLMM Then
          Begin
            OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  jmp #_LMM_MOVD');
          End
          Else
          Begin
            OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
          End;
          OutStr('  rdbyte 0-0,'+sName);
        End
        Else
        Begin
          OutStr('  rdbyte '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          VP_WriteVar(iPlace+1);
        End;
        Inc(iPlace, 2);
        If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#1');
      End
      Else
      Begin
        Error(c_iErrorInvalidParameter, iPlace+1);
        iPlace:=g_iCmdCnt;
      End;
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;

Procedure ProcessRdSByte;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 1)
     Else ProcessHubAddrLit(2, '__temp1', 1);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      If g_apoCmdVars[iPlace+1]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
        Begin
          OutStr('  rdbyte __temp1,'+sName);
          OutStr('  shl __temp1,#24');
          OutStr('  sar __temp1,#24');
          If g_bLMM Then
          Begin
            OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  jmp #_LMM_MOVD');
          End
          Else
          Begin
            OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
          End;
          OutStr('  mov 0-0,__temp1');
        End
        Else
        Begin
          OutStr('  rdbyte '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          OutStr('  shl '+g_apoCmdVars[iPlace+1]^.sGetIndexName+',#24');
          OutStr('  sar '+g_apoCmdVars[iPlace+1]^.sGetIndexName+',#24');
          VP_WriteVar(iPlace+1);
        End;
        Inc(iPlace, 2);
        If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#1');
      End
      Else
      Begin
        Error(c_iErrorInvalidParameter, iPlace+1);
        iPlace:=g_iCmdCnt;
      End;
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;


Procedure ProcessWrLong;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 4)
     Else ProcessHubAddrLit(2, '__temp1', 4);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      Case g_apoCmdVars[iPlace+1]^.eGetType of
        e_LongVar, e_LongConst:
          Begin
            If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
              End;
              OutStr('  wrlong 0-0,'+sName);
            End
            Else OutStr('  wrlong '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          End;
        e_ShortConst:
          Begin
            If (g_apoCmdVars[iPlace-1]^.eGetType = e_ShortConst) and (g_apoCmdVars[iPlace-1]^.lGetValue = g_apoCmdVars[iPlace+1]^.lGetValue) Then
            Begin
              // Don't need to reload same value
            End
            Else OutStr('  mov __temp2,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('   wrlong __temp2,'+sName);
          End;
        Else Error(c_iErrorInvalidParameter, iPlace+1);
      End; // Case
      Inc(iPlace, 2);
      If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#4');
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;

Procedure ProcessWrWord;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 2)
     Else ProcessHubAddrLit(2, '__temp1', 2);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      Case g_apoCmdVars[iPlace+1]^.eGetType of
        e_LongVar, e_LongConst:
          Begin
            If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
              End;
              OutStr('  wrword 0-0,'+sName);
            End
            Else OutStr('  wrword '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          End;
        e_ShortConst:
          Begin
            If (g_apoCmdVars[iPlace-1]^.eGetType = e_ShortConst) and (g_apoCmdVars[iPlace-1]^.lGetValue = g_apoCmdVars[iPlace+1]^.lGetValue) Then
            Begin
              // Don't need to reload same value
            End
            Else OutStr('  mov __temp2,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  wrword __temp2,'+sName);
          End;
        Else Error(c_iErrorInvalidParameter, iPlace+1);
      End; // Case
      Inc(iPlace, 2);
      If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#2');
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;

Procedure ProcessWrByte;
Var
  iPlace: Integer;
  sName: String;
Begin
  sName:='__temp1';
  If g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      e_LongConst, e_LongVar:
        Begin
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
            OutStr('  mov __temp1,0-0');
          End
          Else
          Begin
            If g_iCmdCnt = 4 Then sName:=g_apoCmdVars[2]^.sGetIndexName
             Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        End;
    End; // Case
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then ProcessHubAddr(2, sName, 1)
     Else ProcessHubAddrLit(2, '__temp1', 1);
  End;
  iPlace:=3;
  // 12-23-2017 If g_asCmdLine[3] = '(' Then iPlace:=6;
  Repeat
    If g_asCmdLine[iPlace] = ',' Then
    Begin
      Case g_apoCmdVars[iPlace+1]^.eGetType of
        e_LongVar, e_LongConst:
          Begin
            If g_apoCmdVars[iPlace+1]^.m_bVarIndex Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace+1]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[iPlace+1]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[iPlace+1]^.m_sIndex+',#'+g_apoCmdVars[iPlace+1]^.sGetName);
              End;
              OutStr('  wrbyte 0-0,'+sName);
            End
            Else OutStr('  wrbyte '+g_apoCmdVars[iPlace+1]^.sGetIndexName+','+sName);
          End;
        e_ShortConst:
          Begin
            If (g_apoCmdVars[iPlace-1]^.eGetType = e_ShortConst) and (g_apoCmdVars[iPlace-1]^.lGetValue = g_apoCmdVars[iPlace+1]^.lGetValue) Then
            Begin
              // Don't need to reload same value
            End
            Else OutStr('  mov __temp2,#'+g_apoCmdVars[iPlace+1]^.sGetName);
            OutStr('  wrbyte __temp2,'+sName);
          End;
        Else Error(c_iErrorInvalidParameter, iPlace+1);
      End; // Case
      Inc(iPlace, 2);
      If g_iCmdCnt > iPlace Then OutStr('  add '+sName+',#1');
    End
    Else
    Begin
      Error(c_iErrorCommaExpected, iPlace);
      iPlace:=g_iCmdCnt;
    End;
  Until iPlace >= g_iCmdCnt;
  g_bHandled:=True;
End;


Procedure ProcessWaitCnt; // WAITCNT value[,value]
Begin
  If g_iCmdCnt In [2,4] Then
  Begin
    If (g_iCmdCnt = 4) and (g_asCmdLine[3] <> ',') Then Error(c_iErrorCommaExpected, 3);
    If g_apoCmdVars[2]^.eGetType In [e_LongVar, e_LongConst] Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin // [2] is var index
        If g_iCmdCnt = 2 Then
        Begin
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
          Outstr('  waitcnt 0-0,#0');
        End
        Else
        Case g_apoCmdVars[4]^.eGetType of
          e_ShortConst:
            Begin
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
              Outstr('  waitcnt 0-0,#'+g_apoCmdVars[4]^.sGetName);
            End;
          e_LongConst, e_LongVar:
            Begin
              If g_apoCmdVars[4]^.m_bVarIndex Then
              Begin // [2] and [4] are var index
                If g_bLMM Then
                Begin
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  mov __temp1,0-0');
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  waitcnt __temp1,0-0');
                End
                Else
                Begin
                  OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  movd $+5,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  waitcnt 0-0,0-0');
                End;
              End
              Else
              Begin // [2] is var index, [4] is not
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
                Outstr('  waitcnt 0-0,'+g_apoCmdVars[4]^.sGetName);
              End;
            End;
          Else Error(c_iErrorInvalidParameter, 4);
        End;
      End
      Else
      Begin // [2] is not var index
        If g_iCmdCnt = 2 Then
        Begin
          OutStr('  waitcnt '+g_apoCmdVars[2]^.sGetIndexName+',#0');
        End
        Else
        Begin
          If g_apoCmdVars[4]^.m_bVarIndex Then
          Begin // [2] is not var index, [4] is var index
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
            OutStr('  waitcnt '+g_apoCmdVars[2]^.sGetName+',0-0');
          End
          Else
          Begin // [2] and [4] and not var index
            Case g_apoCmdVars[4]^.eGetType of
              e_ShortConst: OutStr('  waitcnt '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
              e_LongConst, e_LongVar: OutStr('  waitcnt '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              Else Error(c_iErrorInvalidParameter, 4);
            End;
          End;
        End;
      End;
    End
    Else If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
    Begin
      OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      If g_iCmdCnt = 2 Then
      Begin
        OutStr('  waitcnt __temp1,#0');
      End
      Else
      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst: OutStr('  waitcnt __temp1,#'+g_apoCmdVars[4]^.sGetName);
        e_LongConst, e_LongVar:
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
              OutStr('  waitcnt __temp1,0-0');
            End
            Else OutStr('  waitcnt __temp1,'+g_apoCmdVars[4]^.sGetName);
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End;
    End
    Else Error(c_iErrorInvalidParameter, 2);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessWaitVid; // WAITVID value,value
Begin
  If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_apoCmdVars[2]^.eGetType In [e_LongVar, e_LongConst] Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin // [2] is var index
        Case g_apoCmdVars[4]^.eGetType of
          e_ShortConst: OutStr('  waitvid '+g_apoCmdVars[2]^.sGetName+',#'+g_apoCmdVars[4]^.sGetName);
          e_LongConst, e_LongVar:
            Begin
              If g_apoCmdVars[4]^.m_bVarIndex Then
              Begin // [2] and [4] are var index
                If g_bLMM Then
                Begin
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  mov __temp1,0-0');
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  waitvid __temp1,0-0');
                End
                Else
                Begin
                  OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  movd $+5,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  waitvid 0-0,0-0');
                End;
              End
              Else
              Begin // [2] is var index, [4] is not
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
                Outstr('  waitvid 0-0,'+g_apoCmdVars[4]^.sGetName);
              End;
            End;
          Else Error(c_iErrorInvalidParameter, 4);
        End;
      End
      Else
      Begin // [2] is not var index
        If g_apoCmdVars[4]^.m_bVarIndex Then
        Begin // [2] is not var index, [4] is var index
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
          OutStr('  waitvid '+g_apoCmdVars[2]^.sGetName+',0-0');
        End
        Else
        Begin // [2] and [4] and not var index
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  waitvid '+g_apoCmdVars[2]^.sGetName+',#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst, e_LongVar: OutStr('  waitvid '+g_apoCmdVars[2]^.sGetName+','+g_apoCmdVars[4]^.sGetName);
            Else Error(c_iErrorInvalidParameter, 4);
          End;
        End;
      End;
    End
    Else If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
    Begin
      OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst: OutStr('  waitvid __temp1,#'+g_apoCmdVars[4]^.sGetName);
        e_LongConst, e_LongVar:
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
              OutStr('  waitvid __temp1,0-0');
            End
            Else OutStr('  waitvid __temp1,'+g_apoCmdVars[4]^.sGetName);
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End;
    End
    Else Error(c_iErrorInvalidParameter, 2);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessWaitPeq; // WAITPEQ state,pins[,1]
Begin
  If g_iCmdCnt In [4, 6] Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);

    If g_iCmdCnt = 6 Then
    Begin
      If g_apoCmdVars[6]^.eGetType = e_ShortConst Then
      Begin
        If g_apoCmdVars[6]^.lGetValue = 0 Then OutStr('  neg 0,#1 WC,NR  '' Clear carry')
         Else OutStr('  cmpsub 0,0 WC, NR '' Set carry');
      End
      Else Error(c_iErrorInvalidParameter, 6);
    End
    Else
    Begin
      If g_iMaxPin > 31 Then
      Begin // 64 pin version
        If g_apoCmdVars[4]^.eGetType = e_Pin Then
        Begin
          If g_apoCmdVars[4]^.m_bIOA Then OutStr('  neg 0,#1 WC,NR '' Clear carry')
           Else OutStr('  cmpsub 0,0 WC, NR '' Set carry')
        End
        Else OutStr('  neg 0,#1 WC,NR '' Clear carry');
      End;
    End;
    If g_apoCmdVars[2]^.eGetType In [e_LongVar, e_LongConst, e_Pin] Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin // [2] is var index
        Case g_apoCmdVars[4]^.eGetType of
          e_ShortConst: OutStr('  waitpeq '+g_apoCmdVars[2]^.sGetName+',#'+g_apoCmdVars[4]^.sGetName);
          e_LongConst, e_LongVar, e_Pin:
            Begin
              If g_apoCmdVars[4]^.m_bVarIndex Then
              Begin // [2] and [4] are var index
                If g_bLMM Then
                Begin
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  mov __temp1,0-0');
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  waitpeq __temp1,0-0');
                End
                Else
                Begin
                  OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  movd $+5,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  waitpeq 0-0,0-0');
                End;
              End
              Else
              Begin // [2] is var index, [4] is not
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
                Outstr('  waitpeq 0-0,'+g_apoCmdVars[4]^.sGetIndexName);
              End;
            End;
          Else Error(c_iErrorInvalidParameter, 4);
        End;
      End
      Else
      Begin // [2] is not var index
        If g_apoCmdVars[4]^.m_bVarIndex Then
        Begin // [2] is not var index, [4] is var index
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
          OutStr('  waitpeq '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
        End
        Else
        Begin // [2] and [4] and not var index
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  waitpeq '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst, e_LongVar, e_Pin: OutStr('  waitpeq '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
            Else Error(c_iErrorInvalidParameter, 4);
          End;
        End;
      End;
    End
    Else If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
    Begin
      OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst: OutStr('  waitpeq __temp1,#'+g_apoCmdVars[4]^.sGetName);
        e_LongConst, e_LongVar, e_Pin:
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
              OutStr('  waitpeq __temp1,0-0');
            End
            Else OutStr('  waitpeq __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End;
    End
    Else Error(c_iErrorInvalidParameter, 2);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessWaitPne; // WAITPNE state,pins[,1]
Begin
  If g_iCmdCnt In [4, 6] Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_iCmdCnt = 6 Then
    Begin
      If g_apoCmdVars[6]^.eGetType = e_ShortConst Then
      Begin
        If g_apoCmdVars[6]^.lGetValue = 0 Then OutStr('  neg 0,#1 WC,NR '' Clear carry')
         Else OutStr('  cmpsub 0,0 WC, NR '' Set carry');
      End
      Else Error(c_iErrorInvalidParameter, 6);
    End
    Else
    Begin
      If g_iMaxPin > 31 Then
      Begin // 64 pin version
        If g_apoCmdVars[4]^.eGetType = e_Pin Then
        Begin
          If g_apoCmdVars[4]^.m_bIOA Then OutStr('  neg 0,#1 WC,NR '' Clear carry')
           Else OutStr('  cmpsub 0,0 WC, NR '' Set carry')
        End
        Else OutStr('  neg 0,#1 WC,NR '' Clear carry');
      End;
    End;
    If g_apoCmdVars[2]^.eGetType In [e_LongVar, e_LongConst, e_Pin] Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin // [2] is var index
        Case g_apoCmdVars[4]^.eGetType of
          e_ShortConst: OutStr('  waitpeq '+g_apoCmdVars[2]^.sGetName+',#'+g_apoCmdVars[4]^.sGetName);
          e_LongConst, e_LongVar, e_Pin:
            Begin
              If g_apoCmdVars[4]^.m_bVarIndex Then
              Begin // [2] and [4] are var index
                If g_bLMM Then
                Begin
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  mov __temp1,0-0');
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                  OutStr('  waitpne __temp1,0-0');
                End
                Else
                Begin
                  OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  movd $+5,'+g_apoCmdVars[2]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                  OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                  OutStr('  waitpne 0-0,0-0');
                End;
              End
              Else
              Begin // [2] is var index, [4] is not
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
                Outstr('  waitpne 0-0,'+g_apoCmdVars[4]^.sGetIndexName);
              End;
            End;
          Else Error(c_iErrorInvalidParameter, 4);
        End;
      End
      Else
      Begin // [2] is not var index
        If g_apoCmdVars[4]^.m_bVarIndex Then
        Begin // [2] is not var index, [4] is var index
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
          OutStr('  waitpne '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
        End
        Else
        Begin // [2] and [4] and not var index
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  waitpne '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst, e_LongVar, e_Pin: OutStr('  waitpne '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
            Else Error(c_iErrorInvalidParameter, 4);
          End;
        End;
      End;
    End
    Else If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
    Begin
      OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst: OutStr('  waitpne __temp1,#'+g_apoCmdVars[4]^.sGetName);
        e_LongConst, e_LongVar, e_Pin:
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
              OutStr('  waitpne __temp1,0-0');
            End
            Else OutStr('  waitpne __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End;
    End
    Else Error(c_iErrorInvalidParameter, 2);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessCounterAB(pv_bIsCounterB: Boolean);
Var
  sCtr: String[1];
Begin
  If pv_bIsCounterB Then sCtr:='b' Else sCtr:='a';
  If g_iCmdCnt In [2,4,6,8,10] Then // COUNTERx mode, apin, bpin, frqx, phsx
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst: OutStr('  movi ctr'+sCtr+',#'+g_apoCmdVars[2]^.sGetName);
      e_LongVar:
        Begin
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
            OutStr('  movi ctr'+sCtr+',0-0');
          End
          Else OutStr('  movi ctr'+sCtr+','+g_apoCmdVars[2]^.sGetName);
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    If g_iCmdCnt > 3 Then
    Begin
      If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
      Case g_apoCmdVars[4]^.eGetType of
        e_Pin: OutStr('  movs ctr'+sCtr+',#'+g_apoCmdVars[4]^.sGetPinNumber);
        e_ShortConst: OutStr('  movs ctr'+sCtr+',#'+g_apoCmdVars[4]^.sGetName);
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
              OutStr('  movs ctr'+sCtr+',0-0');
            End
            Else OutStr('  movs ctr'+sCtr+','+g_apoCmdVars[4]^.sGetName);
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End; // Case
      If g_iCmdCnt > 5 Then
      Begin
        If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
        Case g_apoCmdVars[6]^.eGetType of
          e_Pin: OutStr('  movd ctr'+sCtr+',#'+g_apoCmdVars[6]^.sGetPinNumber);
          e_ShortConst: OutStr('  movd ctr'+sCtr+',#'+g_apoCmdVars[6]^.sGetName);
          e_LongVar:
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
                OutStr('  movd ctr'+sCtr+',0-0');
              End
              Else OutStr('  movd ctr'+sCtr+','+g_apoCmdVars[6]^.sGetName);
            End;
          Else Error(c_iErrorInvalidParameter, 6);
        End; // Case
        If g_iCmdCnt > 7 Then
        Begin
          If g_asCmdLine[7] <> ',' Then Error(c_iErrorCommaExpected, 7);
          Case g_apoCmdVars[8]^.eGetType of
            e_ShortConst: OutStr('  mov frq'+sCtr+',#'+g_apoCmdVars[8]^.sGetName);
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
                  OutStr('  mov frq'+sCtr+',0-0');
                End
                Else OutStr('  mov frq'+sCtr+','+g_apoCmdVars[8]^.sGetName);
              End;
            Else Error(c_iErrorInvalidParameter, 8);
          End; // Case

          If g_iCmdCnt > 9 Then
          Begin
            If g_asCmdLine[9] <> ',' Then Error(c_iErrorCommaExpected, 9);
            Case g_apoCmdVars[10]^.eGetType of
              e_ShortConst: OutStr('  mov phs'+sCtr+',#'+g_apoCmdVars[10]^.sGetName);
              e_LongConst, e_LongVar:
                Begin
                  If g_apoCmdVars[10]^.m_bVarIndex Then
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  mov __INDEX,#'+g_apoCmdVars[10]^.sGetName);
                      OutStr('  add __INDEX,'+g_apoCmdVars[10]^.m_sIndex);
                      OutStr('  jmp #_LMM_MOVS');
                    End
                    Else
                    Begin
                      OutStr('  add '+g_apoCmdVars[10]^.m_sIndex+',#'+g_apoCmdVars[10]^.sGetName);
                      OutStr('  movs $+2,'+g_apoCmdVars[10]^.m_sIndex);
                      OutStr('  sub '+g_apoCmdVars[10]^.m_sIndex+',#'+g_apoCmdVars[10]^.sGetName);
                    End;
                    OutStr('  mov phs'+sCtr+',0-0');
                  End
                  Else OutStr('  mov phs'+sCtr+','+g_apoCmdVars[10]^.sGetName);
                End;
              Else Error(c_iErrorInvalidParameter, 10);
            End; // Case
          End;
        End;
      End;
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessLockNew; // LOCKNEW LongVar
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      OutStr('  locknew '+g_apoCmdVars[2]^.sGetName);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorSyntaxError, 0);
End;

Procedure ProcessLockRet;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      OutStr('  lockret '+g_apoCmdVars[2]^.sGetName);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorSyntaxError, 0);
End;

Procedure ProcessLockSet;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  lockset __temp1');
          g_bHandled:=True;
        End;
      e_LongVar:
        Begin
          OutStr('  lockset '+g_apoCmdVars[2]^.sGetName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // case
  End
  Else If (g_iCmdCnt = 4) and (g_asCmdLine[3] = ',') Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  lockset __temp1 WC');
        End;
      e_LongVar:
        Begin
          OutStr('  lockset '+g_apoCmdVars[2]^.sGetName+' WC');
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    If g_apoCmdVars[4]^.eGetType = e_LongVar Then
    Begin
      OutStr('  subx '+g_apoCmdVars[4]^.sGetName+','+g_apoCmdVars[4]^.sGetName);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End
  Else Error(c_iErrorSyntaxError, 0);
End;

Procedure ProcessLockClr; // LOCKCLR id{,previousState}
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  lockclr __temp1');
          g_bHandled:=True;
        End;
      e_LongVar:
        Begin
          OutStr('  lockclr '+g_apoCmdVars[2]^.sGetName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // case
  End
  Else If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  lockclr __temp1 WC');
        End;
      e_LongVar:
        Begin
          OutStr('  lockclr '+g_apoCmdVars[2]^.sGetName+' WC');
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    If g_apoCmdVars[4]^.eGetType = e_LongVar Then
    Begin
      OutStr('  subx '+g_apoCmdVars[4]^.sGetName+','+g_apoCmdVars[4]^.sGetName);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End
  Else Error(c_iErrorSyntaxError, 0);
End;


Procedure ProcessClkSet;
Begin
  If g_iCmdCnt > 1 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  clkset __temp1');
        End;
      e_LongConst, e_LongVar:
        Begin
          OutStr('  clkset '+g_apoCmdVars[2]^.sGetIndexName);
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
  End
  Else Error(c_iErrorSyntaxError, 0);
  If g_iCmdCnt = 4 Then
  Begin
    Case g_apoCmdVars[4]^.eGetType of
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetName);
          OutStr('  wrlong __temp1,#0');
        End;
      e_LongConst, e_LongVar:
        Begin
          OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
          OutStr('  wrlong __temp1,#0');
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End; // Case
  End; // If
  g_bHandled:=True;
End;

End.
