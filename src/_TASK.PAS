Unit _TASK;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessTask_Define;
Procedure ProcessTask;
Procedure ProcessEndTask;
Procedure ProcessCogStart;
Procedure ProcessCogInit;
Procedure ProcessCogStop;
Procedure ProcessCogID;

Implementation

Procedure ProcessTask_Define;
Begin
  If g_iCmdCnt In [2,3] Then
  Begin
    OutSpin('OBJ '+g_asCmdLine[1]+': "'+g_asCmdLine[1]+'.spin"');
    Inc(g_iTaskCount);
    g_asTaskNames[g_iTaskCount]:=g_asCmdLine[1];
    g_bTaskAuto[g_iTaskCount]:=False;

    If g_iCmdCnt = 3 Then
    Begin
      If Upper(g_asCmdLine[3]) = 'AUTO' Then
      Begin
        g_bTaskAuto[g_iTaskCount]:=True;
      End
      Else Error(c_iErrorInvalidParameter, 3);
    End;

    // Create a HUB LONG variable to hold Task COG ID
//    g_lHubOffset:=(g_lHubOffset + 3) and $FFFFFFFC;
//    g_oDevice.pAddHubLong(g_asTaskNAmes[g_iTaskCount]+'_COG', g_lHubOffset, 1, 0);
//    Inc(g_lHubOffset, 4);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessTask;
Begin
  If g_iCmdCnt In [2,3] Then
  Begin
    If g_bIsTask Then OutputVarsAndDelete(False, g_lTaskInitDirA, g_lTaskInitOutA, g_lTaskInitDirB, g_lTaskInitPinB, g_lTaskInitDirC, g_lTaskInitPinC, g_lTaskInitDirD, g_lTaskInitPinD)
     Else OutputVarsAndDelete(False, g_lProgInitDirA, g_lProgInitOutA, g_lProgInitDirB, g_lProgInitPinB, g_lProgInitDirC, g_lProgInitPinC, g_lProgInitDirD, g_lProgInitPinD);

    g_bLMM:=False;
    g_iMinWaitCnt:=16;
    If g_iCmdCnt = 3 Then
    Begin
      If Upper(g_asCmdLine[3]) = 'LMM' Then
      Begin
        g_bLMM:=True;
        g_iMinWaitCnt:=80;
      End  
      Else Error(c_iErrorInvalidParameter, 3);
    End;

    g_lTaskInitDirA:=0;
    g_lTaskInitOutA:=0;

    WriteLn(g_tfSrc);
    OutSpin(''' '+g_sOrigLine);

    // Setup new spin file for TASK code
    Close(g_tfSrc);
    g_sTaskName:=g_asCmdLine[2];
    Assign(g_tfSrc, g_sOutputDir+g_sTaskName+'.spin');
    ReWrite(g_tfSrc);
    OutSpin('PUB Init(pv_DataStart, pv_VP_Watch)');
    OutSpin('  _Fixup(pv_DataStart, pv_VP_Watch)');
    If g_bLMM Then
    Begin
      OutSpin('  __OFFSET := @__Init');
      OutSpin('  Return ((pv_DataStart >> 2) << 18) + ((@_LMM_Entry >> 2) << 4)');
    End
    Else OutSpin('  Return ((pv_DataStart >> 2) << 18) + ((@__INIT >> 2) << 4)');
    OutSpin('');
    OutSpin('DAT');
    OutSpin('  ORG 0');
    OutSpin('__INIT');
    OutStr('  mov dira,__InitDirA');
    OutStr('  mov outa,__InitOutA');
    g_bIsTask:=True;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


Procedure ProcessEndTask;
Begin
//  OutStr('  mov __temp1,#0');
//  OutStr('  waitpne __temp1,__temp1');
  OutputVarsAndDelete(True, g_lTaskInitDirA, g_lTaskInitOutA, g_lTaskInitDirB, g_lTaskInitPinB, g_lTaskInitDirC, g_lTaskInitPinC, g_lTaskInitDirD, g_lTaskInitPinD);

  Close(g_tfSrc);
  Assign(g_tfSrc, g_sOutputDir+g_sProjectName+'.spin');
  Append(g_tfSrc);
  g_bHandled:=True;
End;


Procedure ProcessCogStart; // CogStart taskname{, var}
Begin
  OutStr('  mov __temp1,'+g_asCmdLine[2]+'_COG');
  OutStr('  add __temp1,#8');

  If g_iCmdCnt = 2 Then
  Begin
    OutStr('  coginit __temp1');
    g_bHandled:=True;
  End
  Else If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_apoCmdVars[4]^.eGetType = e_LongVar Then
    Begin
      OutStr('  coginit __temp1 WR, WC');
      OutStr('  IF_C  mov __temp1,#8');
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
        OutStr('  mov 0-0,__temp1');
      End
      Else OutStr('  mov '+g_apoCmdVars[4]^.sGetIndexName+',__temp1');
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End;
  g_bHandled:=True;
End;


Procedure ProcessCogInit; // CogInit taskname, cogID
Begin
  OutStr('  mov __temp1,'+g_asCmdLine[2]+'_COG');
  If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    Case g_apoCmdVars[4]^.eGetType of
      e_ShortConst: OutStr('  add __temp1,#'+g_apoCmdVars[4]^.sGetName);
      e_LongConst: OutStr('  add __temp1,'+g_apoCmdVars[4]^.sGetName);
      e_LongVar:
        Begin
          If g_apoCmdVars[4]^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVS');
              OutStr('  add __temp1,0-0');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  add __temp1,0-0');
            End;
          End
          Else OutStr('  add __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End; // Case
    OutStr('  coginit __temp1');
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
  g_bHandled:=True;
End;


Procedure ProcessCogStop; // CogStop ID
Begin
  If g_iCmdCnt = 1 Then
  Begin
    OutStr('  cogid __temp1');
    OutStr('  cogstop __temp1');
    g_bHandled:=True;
  End
  Else If g_iCmdCnt = 2 Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVS');
          OutStr('  mov __temp1,0-0');
          OutStr('  cogstop __temp1');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movs $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  mov __temp1,0-0');
          OutStr('  cogstop __temp1');
        End;
      End
      Else OutStr('  cogstop '+g_apoCmdVars[2]^.sGetIndexName);
      g_bHandled:=True;
    End
    Else
    Begin
      If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
      Begin
        OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
        OutStr('  cogstop __temp1');
        g_bHandled:=True;
      End
      Else Error(c_iErrorInvalidParameter, 2);
    End;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessCogID; // CogID var
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin
        OutStr('  cogid __temp1');
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
          OutStr('  mov 0-0,__temp1');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  mov 0-0,__temp1');
        End;
      End
      Else OutStr('  cogid '+g_apoCmdVars[2]^.sGetIndexName);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;


End.
