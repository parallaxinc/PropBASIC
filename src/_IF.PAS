Unit _IF;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessIf;
Procedure ProcessElse;
Procedure ProcessElseIf;
Procedure ProcessEndIf;

Implementation

Procedure ProcessIf;
Var
  sOrig3: String[32];
  sNot3: String[2];
  sCond: String[5];

  Procedure ProcessIfLong;
  Begin
    If g_iCmdCnt In [3,4] Then
    Begin // IF var THEN
      If SOrig3 = '=' Then
      Begin
        If g_apoCmdVars[2]^.m_bVarIndex Then
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          If g_bLMM Then
          Begin
            OutStr('  tjz 0-0,#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjz 0-0,#'+g_asCmdLine[4]);
        End
        Else
        Begin
          If g_bLMM Then
          Begin
            OutStr('  tjz '+g_apoCmdVars[2]^.sGetIndexName+',#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjz '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_asCmdLine[4]);
        End;
      End
      Else
      Begin
        If g_apoCmdVars[2]^.m_bVarIndex Then
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          If g_bLMM Then
          Begin
            OutStr('  tjnz 0-0,#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjnz 0-0,#'+g_asCmdLine[4]);
        End
        Else
        Begin
          If g_bLMM Then
          Begin
            OutStr('  tjnz  '+g_apoCmdVars[2]^.sGetIndexName+',#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjnz '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_asCmdLine[4]);
        End;
      End;
      g_bHandled:=True;
    End
    Else
    Case g_apoCmdVars[4]^.eGetType of
      e_LongVar, e_LongConst:
        Begin

          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin // [2] is var index
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
                OutStr('  cmps __temp1,0-0 WZ, WC');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  movd $+5,'+g_apoCmdVars[2]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  cmps 0-0,0-0 WZ, WC');
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
              OutStr('  cmps 0-0,'+g_apoCmdVars[4]^.sGetIndexName+' WZ, WC');
            End;
          End
          Else
          Begin // [2] is not var index
            If g_apoCmdVars[4]^.m_bVarIndex Then
            Begin // [2] is not var index [4] is var index
              If g_apoCmdVars[2]^.m_bShadowReg Then OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
              OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
              If g_apoCmdVars[2]^.m_bShadowReg Then OutStr('  cmps __temp1,0-0 WZ, WC')
               Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+',0-0 WZ, WC');
            End
            Else
            Begin // [2] and [4] are not var index
              If g_apoCmdVars[2]^.m_bShadowReg Then
              Begin
                OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
                OutStr('  cmps __temp1,'+g_apoCmdVars[4]^.sGetIndexName+' WZ, WC');
              End
              Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName+' WZ, WC');
            End;
          End;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.m_bVarIndex Then
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
            OutStr('  cmps 0-0,#'+g_apoCmdVars[4]^.sGetName+' WZ, WC');
          End
          Else
          Begin
            If g_apoCmdVars[2]^.m_bShadowReg Then
            Begin
              OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
              OutStr('  cmps __temp1,#'+g_apoCmdVars[4]^.sGetName+' WZ, WC');
            End
            Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName+' WZ, WC');
          End;
        End;
      e_Pin:
        Begin // Pin group, use __temp2
          OutStr('  mov __temp2,ina');
          OutStr('  and __temp2,'+g_apoCmdVars[4]^.sGetName);
          If g_apoCmdVars[4]^.m_byPinNumber > 0 Then OutStr('  shr __temp2,#'+IntStr(g_apoCmdVars[4]^.m_byPinNumber));
          If g_apoCmdVars[4]^.m_bRevPins Then OutStr('  rev __temp2,#'+IntStr(32-g_apoCmdVars[4]^.m_lElements));

          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin // [2] is var index
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
            OutStr('  cmps 0-0,__temp2 WZ, WC');
          End
          Else
          Begin // [2] is not var index
            If g_apoCmdVars[2]^.m_bShadowReg Then
            Begin
              OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
              OutStr('  cmps __temp1,__temp2 WZ, WC');
            End
            Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+',__temp2 WZ, WC');
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End; // Case
    If Not g_bHandled Then
    Begin
      sCond:=''; // Assume invalid
      If sOrig3 = '=' Then sCond:='IF_E';
      If sOrig3 = '<>' Then sCond:='IF_NE';
      If sOrig3 = '<' Then sCond:='IF_B';
      If sOrig3 = '>' Then sCond:='IF_A';
      If sOrig3 = '<=' Then sCond:='IF_BE';
      If sOrig3 = '>=' Then sCond:='IF_AE';
      If sCond = '' Then Error(c_iErrorInvalidParameter, 3);
      If g_bLMM Then
      Begin
        OutStr('  '+sCond+' rdlong __PC,__PC');
        OutStr('  long @@@'+g_asCmdLine[6]);
      End
      Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
      g_bHandled:=True;
    End;
  End; // If LongVar


  Procedure ProcessIfPin;
  Begin
    If g_apoCmdVars[2]^.m_lElements = 1 Then
    Begin
      If g_iCmdCnt In [3,4] Then
      Begin // IF pin THEN
        If SOrig3 = '=' Then
        Begin
          OutStr('  and '+g_apoCmdVars[2]^.sGetName+',ina WZ, NR');
          If g_bLMM Then
          Begin
            OutStr('  IF_Z rdlong __PC,__PC');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  IF_Z jmp #'+g_asCmdLine[4]);
        End
        Else
        Begin
          If g_bLMM Then
          Begin
            OutStr('  IF_NZ rdlong __PC,__PC');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  IF_NZ jmp #'+g_asCmdLine[4]);
        End;
        g_bHandled:=True;
      End
      Else
      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst:
          Begin
            OutStr('  and '+g_apoCmdVars[2]^.sGetName+',ina WZ, NR');
            If (sOrig3 = '=') and (g_apoCmdVars[4]^.lGetValue = 0) or
               (sOrig3 = '<>') and (g_apoCmdVars[4]^.lGetValue <> 0) Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  IF_Z rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[6]);
              End
              Else OutStr('  IF_Z jmp #'+g_asCmdLine[6]);
            End;
            If (sOrig3 = '=') and (g_apoCmdVars[4]^.lGetValue <> 0) or
               (sOrig3 = '<>') and (g_apoCmdVars[4]^.lGetValue = 0) Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  IF_NZ rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[6]);
              End
              Else OutStr('  IF_NZ jmp #'+g_asCmdLine[6]);
            End;
            g_bHandled:=True;
          End;

        e_Pin:
          Begin // If PinVar ? PinVar
            OutStr('  and '+g_apoCmdVars[2]^.sGetName+',ina WZ, NR');
            OutStr('  and '+g_apoCmdVars[4]^.sGetName+',ina WC, NR');
            If g_asCmdLine[3] = '=' Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[6]);
              End
              Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
            End
            Else
            Begin
              If g_bLMM Then
              Begin
                OutStr('  IF_C_NE_Z rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[6]);
              End
              Else OutStr('  IF_C_NE_Z jmp #'+g_asCmdLine[6]);
            End;
            g_bHandled:=True;
          End;

        e_LongVar:
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
              OutStr('  subs 0-0,#1 WC, NR'); // Set carry if zero
              OutStr('  and '+g_apoCmdVars[2]^.sGetIndexName+',ina WZ, NR'); // Set zero flag to pin state
              If g_bLMM Then
              Begin
                OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[6]);
              End
              Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
            End
            Else
            Begin
              OutStr('  and '+g_apoCmdVars[2]^.sGetIndexName+',ina WZ, NR'); // Set zero flag to pin state
              OutStr('  subs '+g_apoCmdVars[4]^.sGetIndexName+',#1 WC, NR'); // Set carry if zero
              If g_asCmdLine[3] = '=' Then
              Begin
                If g_bLMM Then
                Begin
                  OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
              End
              Else
              Begin
                If g_bLMM Then
                Begin
                  OutStr('  IF_C_NE_Z rdlong __PC,__PC');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  IF_C_NE_Z jmp #'+g_asCmdLine[6]);
              End;
            End;
            g_bHandled:=True;
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End; // Case
    End
    Else
    Begin // If PinGroup
      OutStr('  mov __temp1,ina');
      OutStr('  and __temp1,'+g_apoCmdVars[2]^.sGetName);
      If g_apoCmdVars[2]^.m_byPinNumber > 0 Then OutStr('  shr __temp1,#'+IntStr(g_apoCmdVars[2]^.m_byPinNumber));
      If g_apoCmdVars[2]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[2]^.m_lElements));
      If g_abDeleteTemp[2] Then Dispose(g_apoCmdVars[2], Done);
      g_apoCmdVars[2]:=g_oDevice.pGetVarPtr('__TEMP1');
      g_abDeleteTemp[2]:=False;
      ProcessIfLong;
    End;
  End; // If Pin


  Procedure ProcessIfOrLong;
  Begin
    If g_iCmdCnt In [3,4] Then
    Begin // IF var THEN
      If SOrig3 = '=' Then
      Begin
        If g_apoCmdVars[2]^.m_bVarIndex Then
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          If g_bLMM Then
          Begin
            OutStr('  tjnz 0-0,#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjnz 0-0,#'+g_asCmdLine[4]);
        End
        Else
        Begin
          If g_bLMM Then
          Begin
            OutStr('  tjnz '+g_apoCmdVars[2]^.sGetIndexName+',#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjnz '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_asCmdLine[4]);
        End;
      End
      Else
      Begin
        If g_apoCmdVars[2]^.m_bVarIndex Then
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          If g_bLMM Then
          Begin
            OutStr('  tjz 0-0,#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjz 0-0,#'+g_asCmdLine[4]);
        End
        Else
        Begin
          If g_bLMM Then
          Begin
            OutStr('  tjz '+g_apoCmdVars[2]^.sGetIndexName+',#_LMM_JUMP');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  tjz'+g_apoCmdVars[2]^.sGetIndexName+',#'+g_asCmdLine[4]);
        End;
      End;
      g_bHandled:=True;
    End
    Else
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin:
        Begin
          OutStr('  mov __temp2,ina');
          OutStr('  and __temp2,'+g_apoCmdVars[4]^.sGetName);
          If g_apoCmdVars[4]^.m_byPinNumber > 0 Then OutStr('  shr __temp2,#'+IntStr(g_apoCmdVars[4]^.m_byPinNumber));
          If g_apoCmdVars[4]^.m_bRevPins Then OutStr('  rev __temp2,#'+IntStr(32-g_apoCmdVars[4]^.m_lElements));
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin // [2] is var index
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
            OutStr('  cmps 0-0,__temp2 WZ, WC');
          End
          Else
          Begin // [2] is not var index
            If g_apoCmdVars[2]^.m_bShadowReg Then
            Begin
              OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
              OutStr('  cmps __temp1,__temp2 WZ, WC');
            End
            Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+',__temp2 WZ, WC');
          End;
        End;
      e_LongVar, e_LongConst:
        Begin
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin // [2] is var index
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
                OutStr('  cmps __temp1,0-0 WZ, WC');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  movd $+5,'+g_apoCmdVars[2]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  cmps 0-0,0-0 WZ, WC');
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
              OutStr('  cmps 0-0,'+g_apoCmdVars[4]^.sGetIndexName+' WZ, WC');
            End;
          End
          Else
          Begin // [2] is not var index
            If g_apoCmdVars[4]^.m_bVarIndex Then
            Begin // [2] is not var index [4] is var index
              If g_apoCmdVars[2]^.m_bShadowReg Then OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
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
              If g_apoCmdVars[2]^.m_bShadowReg Then OutStr('  cmps __temp1,0-0 WZ, WC')
               Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+',0-0 WZ, WC');
            End
            Else
            Begin // [2] and [4] are not var index
              If g_apoCmdVars[2]^.m_bShadowReg Then
              Begin
                OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
                OutStr('  cmps __temp1,'+g_apoCmdVars[4]^.sGetIndexName+' WZ, WC');
              End
              Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName+' WZ, WC');
            End;
          End;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.m_bVarIndex Then
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
            OutStr('  cmps 0-0,#'+g_apoCmdVars[4]^.sGetName+' WZ, WC');
          End
          Else
          Begin
            If g_apoCmdVars[2]^.m_bShadowReg Then
            Begin
              OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
              OutStr('  cmps __temp1,#'+g_apoCmdVars[4]^.sGetName+' WZ, WC');
            End
            Else OutStr('  cmps '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName+' WZ, WC');
          End;
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End; // Case
    If Not g_bHandled Then
    Begin
      sCond:=''; // Assume invalid
      If sOrig3 = '<>' Then sCond:='IF_E';
      If sOrig3 = '=' Then sCond:='IF_NE';
      If sOrig3 = '>=' Then sCond:='IF_B';
      If sOrig3 = '<=' Then sCond:='IF_A';
      If sOrig3 = '>' Then sCond:='IF_BE';
      If sOrig3 = '<' Then sCond:='IF_AE';
      If sCond = '' Then Error(c_iErrorInvalidParameter, 3);
      If g_bLMM Then
      Begin
        OutStr('  '+sCond+' rdlong __PC,__PC');
        OutStr('  long @@@'+g_asCmdLine[4]);
      End
      Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[4]);
      g_bHandled:=True;
    End;
  End; // IfOr LongVar


  Procedure ProcessIfOrPin;
  Begin
    If g_apoCmdVars[2]^.m_lElements = 1 Then
    Begin
      OutStr('  and '+g_apoCmdVars[2]^.sGetName+',ina WZ, NR');
      If g_iCmdCnt In [3,4] Then
      Begin // IF pin THEN
        If SOrig3 = '=' Then
        Begin
          If g_bLMM Then
          Begin
            OutStr('  IF_NZ rdlong __PC,__PC');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  IF_NZ jmp #'+g_asCmdLine[4])
        End
        Else
        Begin
          If g_bLMM Then
          Begin
            OutStr('  IF_Z rdlong __PC,__PC');
            OutStr('  long @@@'+g_asCmdLine[4]);
          End
          Else OutStr('  IF_Z jmp #'+g_asCmdLine[4]);
        End;
        g_bHandled:=True;
      End
      Else
      Case g_apoCmdVars[4]^.eGetType of
        e_ShortConst:
          Begin
            If (sOrig3 = '<>') and (g_apoCmdVars[4]^.lGetValue = 0) or
               (sOrig3 = '=') and (g_apoCmdVars[4]^.lGetValue <> 0) Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  IF_Z rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[4]);
              End
              Else OutStr('  IF_Z jmp #'+g_asCmdLine[4]);
            End;
            If (sOrig3 = '<>') and (g_apoCmdVars[4]^.lGetValue <> 0) or
               (sOrig3 = '=') and (g_apoCmdVars[4]^.lGetValue = 0) Then
            Begin
              If g_bLMM Then
              Begin
                OutStr('  IF_NZ rdlong __PC,__PC');
                OutStr('  long @@@'+g_asCmdLine[4]);
              End
              Else OutStr('  IF_NZ jmp #'+g_asCmdLine[4]);
            End;
          End;
        Else Error(c_iErrorInvalidParameter, 4);
      End; // Case
      If sOrig3 = '<' Then Error(c_iErrorInvalidParameter, 3);
      If sOrig3 = '>' Then Error(c_iErrorInvalidParameter, 3);
      If sOrig3 = '<=' Then Error(c_iErrorInvalidParameter, 3);
      If sOrig3 = '>=' Then Error(c_iErrorInvalidParameter, 3);
      g_bHandled:=True;
    End
    Else
    Begin // If PinGroup
      OutStr('  mov __temp1,ina');
      OutStr('  and __temp1,'+g_apoCmdVars[2]^.sGetName);
      If g_apoCmdVars[2]^.m_byPinNumber > 0 Then OutStr('  shr __temp1,#'+IntStr(g_apoCmdVars[2]^.m_byPinNumber));
      If g_apoCmdVars[2]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[2]^.m_lElements));
      If g_abDeleteTemp[2] Then Dispose(g_apoCmdVars[2], Done);
      g_apoCmdVars[2]:=g_oDevice.pGetVarPtr('__TEMP1');
      g_abDeleteTemp[2]:=False;
      ProcessIfOrLong;
    End;
  End; // IfOr Pin


Begin // ProcessIf  "IF var cond var THEN {label}"
  // If program used "THEN GOTO" remove the "GOTO"
  If (Upper(g_asCmdLine[g_iCmdCnt-2]) = 'THEN') AND (Upper(g_asCmdLine[g_iCmdCnt-1]) = 'GOTO') Then
  Begin
    g_asCmdLine[g_iCmdCnt-1]:=g_asCmdLine[g_iCmdCnt];
    Dec(g_iCmdCnt);
  End;

  // IF program used "IF var THEN" or "IF var GOTO" change it to "IF var <> 0 THEN" or "IF var <> 0 GOTO"
 If g_iCmdCnt In [3,4,5,6] Then
  Begin
    If g_iCmdCnt > 4 Then sOrig3:=g_asCmdLine[3] Else sOrig3:='<>';
    If g_iCmdCnt In [3,5] Then
    Begin
      If (Upper(g_asCmdLine[1]) = 'IF') or (Upper(g_asCmdLine[1]) = 'ELSEIF') Then Inc(g_iIfAccum);
      Inc(g_iIfCnt);
      With g_arIf[g_iIfCnt] Do
      Begin
        iAccumID:=g_iIfAccum;
        bElseUsed:=False;
        bIsAnd:=False;
        bIsOr:=False;
        If Upper(g_asCmdLine[1]) = 'IF' Then bIsElseIf:=False
         Else bIsElseIf:=True;
      End; // With
      g_asCmdLine[g_iCmdCnt+1]:='__ELSE_'+IntStr(g_arIf[g_iIfCnt].iAccumID);
      If g_asUnaryOperator[2] <> '~' Then
      Begin
        sNot3:=''; // Assume invalid
        If sOrig3='=' Then sNot3:='<>';
        If sOrig3='<>' Then sNot3:='=';
        If sOrig3='>' Then sNot3:='<=';
        If sOrig3='<' Then sNot3:='>=';
        If sOrig3='>=' Then sNot3:='<';
        If sOrig3='<=' Then sNot3:='>';
        If sNot3 = '' THEN Error(c_iErrorInvalidParameter, 3);
        sOrig3:=sNot3;
      End;
    End
    Else If g_asUnaryOperator[2] = '~' Then
    Begin
      sNot3:=''; // Assume invalid
      If sOrig3='=' Then sNot3:='<>';
      If sOrig3='<>' Then sNot3:='=';
      If sOrig3='>' Then sNot3:='<=';
      If sOrig3='<' Then sNot3:='>=';
      If sOrig3='>=' Then sNot3:='<';
      If sOrig3='<=' Then sNot3:='>';
      If sNot3 = '' Then Error(c_iErrorInvalidParameter, 3);
      sOrig3:=sNot3;
    End;
    g_asUnaryOperator[2]:='';
    If Upper(g_asCmdLine[g_iCmdCnt]) = 'EXIT' Then
    Begin
      If g_iExitLabelCnt > 0 Then
      Begin
        g_asCmdLine[g_iCmdCnt]:=g_arExitLabels[g_iExitLabelCnt].sLabel;
        g_arExitLabels[g_iExitLabelCnt].bUsed:=TRUE;
      End
      Else Error(c_iErrorExitNotInLoop, 0);
    End;
    If ((g_iCmdCnt In [5,6]) and ((Upper(g_asCmdLine[5]) = 'THEN') or (Upper(g_asCmdLine[5]) = 'GOTO'))) or
       ((g_iCmdCnt In [3,4]) and ((Upper(g_asCmdLine[3]) = 'THEN') or (Upper(g_asCmdLine[3]) = 'GOTO'))) Then
    Begin
      Case g_apoCmdVars[2]^.eGetType of
        e_LongVar: ProcessIfLong;
        e_Pin: ProcessIfPin;
        Else Error(c_iErrorInvalidParameter, 2);
      End; // Case
      If (g_iIfCnt > 0) Then
      Begin
        // Feb 13,2016 If ((g_iCmdCnt In [4, 6]) and ((g_arIf[g_iIfCnt].bIsOR) or (g_arIf[g_iIfCnt].bIsAND))) or
        //   ((g_iCmdCnt In [3, 5]) and ((g_arIf[g_iIfCnt-1].bIsOr) or (g_arIf[g_iIfCnt-1].bIsAND))) Then
        If ((g_iCmdCnt In [4, 6]) and ((g_arIf[g_iIfCnt].bIsOR) or (g_arIf[g_iIfCnt].bIsAND))) or
           ((g_iIfCnt > 1) and
            ((g_iCmdCnt In [3, 5]) and ((g_arIf[g_iIfCnt-1].bIsOr) or (g_arIf[g_iIfCnt-1].bIsAND)))) Then
        Begin
          If g_iCmdCnt In [4, 6] Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  rdlong __PC,__PC');
              OutStr('  long @@@__ENDIF_'+IntStr(g_arIf[g_iIfCnt].iAccumID));
            End
            Else OutStr('  jmp #__ENDIF_'+IntStr(g_arIf[g_iIfCnt].iAccumID));
          End;
          OutStr('__IF_'+IntStr(g_arIf[g_iIfCnt].iAccumID));
          If g_iCmdCnt In [4, 6] Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  rdlong __PC,__PC');
              OutStr('  long @@@'+g_asCmdLine[g_iCmdCnt]);
            End
            Else OutStr('  jmp #'+g_asCmdLine[g_iCmdCnt]);
            ProcessEndIf;
          End;
        End;
      End;
      g_bCompoundIfPending:=False;
    End // [5] = 'THEN' or 'GOTO'

    Else If ((g_iCmdCnt = 5) and (Upper(g_asCmdLine[5]) = 'OR')) or
       ((g_iCmdCnt = 3) and (Upper(g_asCmdLine[3]) = 'OR')) Then
    Begin // Handle OR
      g_asCmdLine[4]:='__IF_'+IntStr(g_arIF[g_iIfCnt].iAccumID);
      g_arIf[g_iIfCnt].bIsOR:=True;
      g_bCompoundIfPending:=True;
      Case g_apoCmdVars[2]^.eGetType of
        e_LongVar: ProcessIfOrLong;
        e_Pin: ProcessIfOrPin;
        Else Error(c_iErrorInvalidParameter, 2);
      End; // Case
    End // 3 or 5 = 'OR'

    Else If ((g_iCmdCnt = 5) and (Upper(g_asCmdLine[5]) = 'AND')) or
       ((g_iCmdCnt = 3) and (Upper(g_asCmdLine[3]) = 'AND')) Then
    Begin // Handle AND
      g_arIf[g_iIfCnt].bIsAND:=True;
      g_bCompoundIfPending:=True;
      g_asCmdLine[4]:='__IF_'+IntStr(g_arIF[g_iIfCnt].iAccumID);
      Case g_apoCmdVars[2]^.eGetType of
        e_LongVar: ProcessIfLong;
        e_Pin: ProcessIfPin;
        Else Error(c_iErrorInvalidParameter, 2);
      End; // Case
    End // 3 or 5 = 'AND'
    Else Error(c_iErrorSyntaxError, 0);
  End // g_iCmdCnt In [5,6]
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessElse;
Var
  i: Integer;
Begin
  If g_iCmdCnt = 1 Then
  Begin
    If g_iIfCnt > 0 Then
    Begin
      i:=g_iIfCnt;
      While g_arIf[i].bIsElseIf Do Dec(i);
      If g_bLMM Then
      Begin
        OutStr('  rdlong __PC,__PC');
        OutStr('  long @@@__ENDIF_'+IntStr(g_arIf[i].iAccumID));
      End
      Else OutStr('  jmp #__ENDIF_'+IntStr(g_arIf[i].iAccumID));
      OutStr('__ELSE_'+IntStr(g_arIf[g_iIfCnt].iAccumID));
      g_arIf[g_iIfCnt].bElseUsed:=True;
      g_bHandled:=True;
    End
    Else Error(c_iErrorElseOrEndIfWithoutIf, 1);
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessElseIf;
Var
  i: Integer;
Begin
  If g_iIfCnt > 0 Then
  Begin
    i:=g_iIfCnt;
    While (i > 0) and g_arIf[i].bIsElseIf Do Dec(i);
    If g_bLMM Then
    Begin
      OutStr('  rdlong __PC,__PC');
      OutStr('  long @@@__ENDIF_'+IntStr(g_arIf[i].iAccumID));
    End
    Else OutStr('  jmp #__ENDIF_'+IntStr(g_arIf[i].iAccumID));
    OutStr('__ELSE_'+IntStr(g_arIf[g_iIfCnt].iAccumID));
    g_arIf[g_iIfCnt].bElseUsed:=True;
    ProcessIf{(g_iCmdCnt, 2, g_iCmdCnt)};
    g_bHandled:=True;
  End
  Else Error(c_iErrorElseOrEndIfWithoutIf, 1);
End;

Procedure ProcessEndIf;
Var
  i: Integer;
Begin
  If g_iIfCnt > 0 Then
  Begin
    If Not g_arIf[g_iIfCnt].bElseUsed Then OutStr('__ELSE_'+IntStr(g_arIf[g_iIfCnt].iAccumID));
    i:=g_iIfCnt-1;
    While (i > 0) and ((g_arIf[i].iAccumID = g_arIf[g_iIfCnt].iAccumID) or (g_arIf[i+1].bIsElseIf)) Do Dec(i);
    OutStr('__ENDIF_'+IntStr(g_arIf[i+1].iAccumID));
    g_iIfCnt:=i;
    g_bHandled:=True;
  End
  Else Error(c_iErrorElseOrEndIfWithoutIf, 1);
End;

End.
