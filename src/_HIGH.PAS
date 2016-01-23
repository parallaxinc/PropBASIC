Unit _HIGH;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessHigh;
Procedure ProcessLow;
Procedure ProcessToggle;

Implementation

Procedure ProcessHigh;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.m_bIOA Then
          Begin
            OutStr('  or dira,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  or outa,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            OutStr('  or dirb,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  or outb,'+g_apoCmdVars[2]^.sGetName);
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
            OutStr('  or outa,__temp1');
          End
          Else
          Begin
            OutStr('  or dirb,__temp1');
            OutStr('  or outb,__temp1');
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
            OutStr('  or outa,__temp1');
          End
          Else
          Begin
            OutStr('  mov __temp2,0-0');
            OutStr('  cmp __temp2,#32 WC');
            OutStr('  shl __temp1,__temp2');
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_C or outa,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
            OutStr('  IF_NC or outb,__temp1');
          End;
        End
        Else
        Begin
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  or dira,__temp1');
            OutStr('  or outa,__temp1');
          End
          Else
          Begin
            OutStr('  cmp '+g_apoCmdVars[2]^.sGetIndexName+',#32 WC');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_C or outa,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
            OutStr('  IF_NC or outb,__temp1');
          End;
        End;
        g_bHandled:=True;
      End
    Else Error(c_iErrorInvalidParameter, 2);
    End;
  End;
End;


Procedure ProcessLow;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.m_bIOA Then
          Begin
            OutStr('  or dira,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  andn outa,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            OutStr('  or dirb,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  andn outb,'+g_apoCmdVars[2]^.sGetName);
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
            OutStr('  andn outa,__temp1');
          End
          Else
          Begin
            OutStr('  or dirb,__temp1');
            OutStr('  andn outb,__temp1');
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
            OutStr('  andn outa,__temp1');
          End
          Else
          Begin
            OutStr('  mov __temp2,0-0');
            OutStr('  cmp __temp2,#32 WC');
            OutStr('  shl __temp1,__temp2');
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_C andn outa,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
            OutStr('  IF_NC andn outb,__temp1');
          End;
        End
        Else
        Begin
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  or dira,__temp1');
            OutStr('  andn outa,__temp1');
          End
          Else
          Begin
            OutStr('  cmp '+g_apoCmdVars[2]^.sGetIndexName+',#32 WC');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_C andn outa,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
            OutStr('  IF_NC andn outb,__temp1');
          End;
        End;
        g_bHandled:=True;
      End
    Else Error(c_iErrorInvalidParameter, 2);
    End;
  End;
End; // ProcessLow


Procedure ProcessToggle;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.m_bIOA Then
          Begin
            OutStr('  or dira,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  xor outa,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            OutStr('  or dirb,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  xor outb,'+g_apoCmdVars[2]^.sGetName);
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
            OutStr('  xor outa,__temp1');
          End
          Else
          Begin
            OutStr('  or dirb,__temp1');
            OutStr('  xor outb,__temp1');
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
            OutStr('  xor outa,__temp1');
          End
          Else
          Begin
            OutStr('  mov __temp2,0-0');
            OutStr('  cmp __temp2,#32 WC');
            OutStr('  shl __temp1,__temp2');
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_C xor outa,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
            OutStr('  IF_NC xor outb,__temp1');
          End;
        End
        Else
        Begin
          If g_iMaxPin < 32 Then
          Begin
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  or dira,__temp1');
            OutStr('  xor outa,__temp1');
          End
          Else
          Begin
            OutStr('  cmp '+g_apoCmdVars[2]^.sGetIndexName+',#32 WC');
            OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
            OutStr('  IF_C or dira,__temp1');
            OutStr('  IF_C xor outa,__temp1');
            OutStr('  IF_NC or dirb,__temp1');
            OutStr('  IF_NC xor outb,__temp1');
          End;
        End;
        g_bHandled:=True;
      End
    Else Error(c_iErrorInvalidParameter, 2);
    End;
  End;
End;


End.
