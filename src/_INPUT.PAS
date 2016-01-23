Unit _INPUT;

{$MODE Delphi}

Interface

Procedure ProcessInput;
Procedure ProcessOutput;
Procedure ProcessReverse;
Procedure ProcessPrint;

Implementation

Uses GLOBAL;


Procedure ProcessInput;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.m_bIOA Then
          Begin
            OutStr('  andn dira,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            OutStr('  andn dirb,'+g_apoCmdVars[2]^.sGetName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.lGetValue > g_iMaxPin Then Error(c_iErrorInvalidParameter, 2);
          If g_apoCmdVars[2]^.lGetValue < 9 Then
           OutStr('  mov __temp1,#1 << '+g_apoCmdVars[2]^.sGetName)
          Else
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          End;
          If g_apoCmdVars[2]^.lGetValue < 32 Then
          Begin
            OutStr('  andn dira,__temp1');
          End
          Else
          Begin
            OutStr('  andn dirb,__temp1');
          End;
          g_bHandled:=True;
        End;
    e_LongVar:
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
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,0-0');
            OutStr('  andn dira,__temp1');
          End
          Else
          Begin
            OutStr('  mov __temp2,0-0');
            OutStr('  cmp __temp2,#32 WC');
            OutStr('  shl __temp1,__temp2');
            OutStr('  IF_C andn dira,__temp1');
            OutStr('  IF_NC andn dirb,__temp1');
          End;
        End
        Else
        Begin
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  andn dira,__temp1');
          End
          Else
          Begin
            OutStr('  cmp '+g_apoCmdVars[2]^.sGetIndexName+',#32 WC');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  IF_C andn dira,__temp1');
            OutStr('  IF_NC andn dirb,__temp1');
          End;
        End;
        g_bHandled:=True;
      End
    Else Error(c_iErrorInvalidParameter, 2);
    End;
  End;
End;

Procedure ProcessOutput;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.m_bIOA Then
          Begin
            OutStr('  or dira,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            OutStr('  or dirb,'+g_apoCmdVars[2]^.sGetName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.lGetValue > g_iMaxPin Then Error(c_iErrorInvalidParameter, 2);
          If g_apoCmdVars[2]^.lGetValue < 9 Then
           OutStr('  mov __temp1,#1 << '+g_apoCmdVars[2]^.sGetName)
          Else
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          End;
          If g_apoCmdVars[2]^.lGetValue < 32 Then
          Begin
            OutStr('  or dira,__temp1');
          End
          Else
          Begin
            OutStr('  or dirb,__temp1');
          End;
          g_bHandled:=True;
        End;
    e_LongVar:
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
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,0-0');
            OutStr('  or dira,__temp1');
          End
          Else
          Begin
            OutStr('  mov __temp2,0-0');
            OutStr('  cmp __temp2,#32 WC');
            OutStr('  shl __temp1,__temp2');
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
          End;
        End
        Else
        Begin
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  or dira,__temp1');
          End
          Else
          Begin
            OutStr('  cmp '+g_apoCmdVars[2]^.sGetIndexName+',#32 WC');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
          End;
        End;
        g_bHandled:=True;
      End
    Else Error(c_iErrorInvalidParameter, 2);
    End;
  End;
End;

Procedure ProcessReverse;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.m_bIOA Then
          Begin
            OutStr('  xor dira,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            OutStr('  xor dirb,'+g_apoCmdVars[2]^.sGetName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.lGetValue > g_iMaxPin Then Error(c_iErrorInvalidParameter, 2);
          If g_apoCmdVars[2]^.lGetValue < 9 Then
           OutStr('  mov __temp1,#1 << '+g_apoCmdVars[2]^.sGetName)
          Else
          Begin
            OutStr('  mov __temp1,#1');
            OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          End;
          If g_apoCmdVars[2]^.lGetValue < 32 Then
          Begin
            OutStr('  xor dira,__temp1');
          End
          Else
          Begin
            OutStr('  xor dirb,__temp1');
          End;
          g_bHandled:=True;
        End;
    e_LongVar:
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
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,0-0');
            OutStr('  xor dira,__temp1');
          End
          Else
          Begin
            OutStr('  mov __temp2,0-0');
            OutStr('  cmp __temp2,#32 WC');
            OutStr('  shl __temp1,__temp2');
            OutStr('  IF_C xor dira,__temp1');
            OutStr('  IF_NC xor dirb,__temp1');
          End;
        End
        Else
        Begin
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  xor dira,__temp1');
          End
          Else
          Begin
            OutStr('  cmp '+g_apoCmdVars[2]^.sGetIndexName+',#32 WC');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  IF_C xor dira,__temp1');
            OutStr('  IF_NC xor dirb,__temp1');
          End;
        End;
        g_bHandled:=True;
      End
    Else Error(c_iErrorInvalidParameter, 2);
    End;
  End;
End;

Procedure ProcessPrint;  // PRINT subName, "literal"; longvar, stringvar
Var
  iPlace: Integer;
  sNextCharLabel: String;
Begin
  iPlace:=4;
  Case g_apoCmdVars[iPlace]^.eGetType of
    e_DataLabel:
      Begin
        ProcessHubAddrLit(iPlace, '__temp1', 1);
        sNextCharLabel:=NewLabelStr;
        OutStr(sNextCharLabel);
        OutStr('  rdbyte __param1,__temp1 WZ');
        OutStr('  adds __temp1,#1');
        OutStr('  IF_Z jmp #'+NewLabelStr); // FIX FOR LMM
        OutStr('  call '+g_asCmdLine[2]);
        OutStr('  jmp #'+sNextCharLabel); // FIX FOR LMM
        OutStr(LabelStr);
        g_bHandled:=True;
      End;
  End; // Case
End;

End.

