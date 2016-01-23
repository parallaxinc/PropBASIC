Unit _DO;

{$MODE Delphi}

 // LMM Index

Interface

Uses GLOBAL;

Procedure ProcessDo;
Procedure ProcessLoop;

Implementation

Procedure ProcessDo;
Var
  sOrig4: String[32];
  sNot3: String[2];
  sName: String[32];
  sCond: String[5];
Begin
  Inc(g_iDoAccum);
  Inc(g_iDoCnt);
  g_arDo[g_iDoCnt].iAccumID:=g_iDoAccum;
  g_arDo[g_iDoCnt].bDoCondition:=False;
  Inc(g_iExitLabelCnt);
  g_arExitLabels[g_iExitLabelCnt].sLabel:='__LOOP_'+IntStr(g_arDo[g_iDoCnt].iAccumID);
  g_arExitLabels[g_iExitLabelCnt].bUsed:=FALSE;
  OutStr('__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
  g_asCmdLine[6]:='__LOOP_'+IntStr(g_arDo[g_iDoCnt].iAccumID);
  If g_iCmdCnt = 1 Then
  Begin
    g_bHandled:=True;
  End
  Else
  Begin
    If g_iCmdCnt = 3 Then
    Begin // DO (WHILE | UNTIL) var
      Case g_apoCmdVars[3]^.eGetType of
        e_LongVar:
          Begin // DO (WHILE | UNTIL) longVar
            If ((Upper(g_asCmdLine[2]) = 'UNTIL') and (g_asUnaryOperator[3] = '')) or
               ((Upper(g_asCmdLine[2]) = 'WHILE') and (g_asUnaryOperator[3] = '~')) Then
            Begin
              // IF LongVar[3] is non-zero, JUMP to LABEL[6]
              If g_apoCmdVars[3]^.m_bVarIndex Then
              Begin
                OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                If g_bLMM Then
                Begin
                  OutStr('  tjnz 0-0,#_LMM_JUMP');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  tjnz 0-0,#'+g_asCmdLine[6]);
              End
              Else
              Begin
                If g_bLMM Then
                Begin
                  OutStr('  tjnz '+g_apoCmdVars[3]^.sGetIndexName+',#_LMM_JUMP');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  tjnz '+g_apoCmdVars[3]^.sGetIndexName+',#'+g_asCmdLine[6]);
              End;  
              g_bHandled:=True;
            End
            Else
            Begin
              // IF LongVar[3] is zero, JUMP to LABEL[6]
              If g_apoCmdVars[3]^.m_bVarIndex Then
              Begin
                OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                If g_bLMM Then
                Begin
                  OutStr('  tjnz 0-0,#_LMM_JUMP');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  tjz 0-0,#'+g_asCmdLine[6]);
              End
              Else
              Begin
                If g_bLMM Then
                Begin
                  OutStr('  tjnz '+g_apoCmdVars[3]^.sGetIndexName+',#_LMM_JUMP');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  tjz '+g_apoCmdVars[3]^.sGetIndexName+',#'+g_asCmdLine[6]);
              End;
              g_bHandled:=True;
            End;
          End
        Else Error(c_iErrorInvalidParameter, 3);
      End;
    End
    Else If g_iCmdCnt = 5 Then
    Begin
      // Translate "DO UNTIL x[3] cond[4] y[5] label[6]" INTO "IF x[2] cond[3] y[4] THEN label[6]"
      // Translate "DO WHILE x[3] cond[4] y[5] label[6]" INTO "IF x[2] ~cond[3] y[4] THEN label[6]"
      sOrig4:=g_asCmdLine[4];
      If Upper(g_asCmdLine[2]) = 'WHILE' Then
      Begin
        sNot3:='';
        g_arDo[g_iDoCnt].bDoCondition:=True;
        If g_asCmdLine[4]='=' Then sNot3:='<>';
        If g_asCmdLine[4]='<>' Then sNot3:='=';
        If g_asCmdLine[4]='>' Then sNot3:='<=';
        If g_asCmdLine[4]='<' Then sNot3:='>=';
        If g_asCmdLine[4]='>=' Then sNot3:='<';
        If g_asCmdLine[4]='<=' Then sNot3:='>';
        If sNot3 <> '' Then g_asCmdLine[4]:=sNot3 Else Error(c_iErrorInvalidParameter, 4);
      End;
      If (Upper(g_asCmdLine[2]) = 'WHILE') or (Upper(g_asCmdLine[2]) = 'UNTIL') Then
      Begin
        If g_apoCmdVars[3]^.eGetType = e_Pin Then
        Begin // Pin var
          If g_apoCmdVars[3]^.m_lElements = 1 Then
          Begin // single Pin var
            If (g_asCmdLine[4] = '=') or (g_asCmdLine[4] = '<>') Then
            Begin
              Case g_apoCmdVars[5]^.eGetType of
                e_ShortConst:
                  Begin
                    OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR');
                    If ((g_apoCmdVars[5]^.lGetValue = 0) and (g_asCmdLine[4] = '=')) or
                       ((g_apoCmdVars[5]^.lGetValue = 1) and (g_asCmdLine[4] = '<>')) Then
                    Begin
                      If g_bLMM Then
                      Begin
                        OutStr('  IF_Z rdlong __PC,__PC');
                        OutStr('  long @@@'+g_asCmdLine[6]);
                      End
                      Else OutStr('  IF_Z jmp #'+g_asCmdLine[6])
                    End
                    Else
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
                    OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR');
                    OutStr('  and '+g_apoCmdVars[5]^.sGetIndexName+',ina WC, NR');
                    If g_asCmdLine[4] = '=' Then
                    Begin
                      If g_bLMM Then
                      Begin
                        OutStr('  IF_C_NE_Z rdlong __PC,__PC');
                        OutStr('  long @@@'+g_asCmdLine[6]);
                      End
                      Else OutStr('  IF_C_NE_Z jmp #'+g_asCmdLine[6]);
                    End
                    Else
                    Begin
                      If g_bLMM Then
                      Begin
                        OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                        OutStr('  long @@@'+g_asCmdLine[6]);
                      End
                      Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
                    End;
                    g_bHandled:=True;
                  End;
                e_LongVar:
                  Begin
                    If g_apoCmdVars[5]^.m_bVarIndex Then
                    Begin
                      If g_bLMM Then
                      Begin
                        OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                        OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                        OutStr('  jmp #_LMM_MOVS');
                      End
                      Else
                      Begin
                        OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                        OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                        OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                      End;
                      OutStr('  mov __temp1,0-0');
                      OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR'); // Set zero flag to pin state
                      OutStr('  subs __temp1,#1 WC, NR'); // Set carry if zero
                      If g_bLMM Then
                      Begin
                        OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                        OutStr('  long @@@'+g_asCmdLine[6]);
                      End
                      Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
                    End
                    Else
                    Begin
                      OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR'); // Set zero flag to pin state
                      OutStr('  subs '+g_apoCmdVars[5]^.sGetIndexName+',#1 WC, NR'); // Set carry if zero
                      If g_asCmdLine[4] = '=' Then
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
                Else Error(c_iErrorInvalidParameter, 5);
              End; // Case
            End
            Else
            Begin
              g_asCmdLine[4]:=sOrig4;
              Error(c_iErrorInvalidParameter, 4);
            End;
          End
          Else
          Begin // pin group
            OutStr('  mov __temp1,ina');
            OutStr('  and __temp1,'+g_apoCmdVars[3]^.sGetName);
            If g_apoCmdVars[3]^.m_byPinNumber > 0 Then OutStr('  shr __temp1,#'+IntStr(g_apoCmdVars[3]^.m_byPinNumber));
            If g_apoCmdVars[3]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[3]^.m_lElements));
            Case g_apoCmdVars[5]^.eGetType of
              e_LongVar, e_LongConst:
                Begin
                  If g_apoCmdVars[5]^.m_bVarIndex Then
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                      OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                      OutStr('  jmp #_LMM_MOVS');
                    End
                    Else
                    Begin
                      OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+','+g_apoCmdVars[5]^.sGetName);
                      OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                      OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+','+g_apoCmdVars[5]^.sGetName);
                    End;
                    OutStr('  cmps __temp1,0-0 WZ, WC');
                  End
                  Else OutStr('  cmps __temp1,'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                  sCond:='';
                  If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                  If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                  If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                  If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                  If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                  If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                  If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                  If g_bLMM Then
                  Begin
                    OutStr('  '+sCond+' rdlong __PC,__PC');
                    OutStr('  long @@@'+g_asCmdLine[6]);
                  End
                  Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                  g_bHandled:=True;
                End;
              e_ShortConst:
                Begin
                  OutStr('  cmps __temp1,#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                  sCond:=''; // Assume invlaid
                  If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                  If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                  If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                  If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                  If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                  If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                  If sCond='' Then Error(c_iErrorInvalidParameter, 4);
                  If g_bLMM Then
                  Begin
                    OutStr('  '+sCond+' rdlong __PC,__PC');
                    OutStr('  long @@@'+g_asCmdLine[6]);
                  End
                  Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                  g_bHandled:=True;
                End;
              e_Pin:
                Begin
                  OutStr('  mov __temp2,ina');
                  OutStr('  and __temp2,'+g_apoCmdVars[5]^.sGetName);
                  If g_apoCmdVars[5]^.m_byPinNumber > 0 Then OutStr('  shr __temp2,#'+IntStr(g_apoCmdVars[5]^.m_byPinNumber));
                  If g_apoCmdVars[5]^.m_bRevPins Then OutStr('  rev __temp2,#'+IntStr(32-g_apoCmdVars[5]^.m_lElements));
                  OutStr('  cmps __temp1,__temp2 WZ, WC');
                  sCond:=''; // Assume invalid
                  If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                  If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                  If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                  If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                  If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                  If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                  if sCond='' Then Error(c_iErrorInvalidParameter, 4);
                  If g_bLMM Then
                  Begin
                    OutStr('  '+sCond+' rdlong __PC,__PC');
                    OutStr('  long @@@'+g_asCmdLine[6]);
                  End
                  Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                  g_bHandled:=True;
                End;
              Else Error(c_iErrorInvalidParameter, 5);
            End; // Case
          End;
        End
        Else If g_apoCmdVars[3]^.eGetType = e_LongVar Then
        Begin // LongVar
          If g_apoCmdVars[3]^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_apoCmdVars[3]^.sGetName);
              OutStr('  add __INDEX,'+g_apoCmdVars[3]^.m_sIndex);
              OutStr('  jmp #_LMM_MOVS');
            End
            Else
            Begin
              OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
              OutStr('  movs $+2,'+g_apoCmdVars[3]^.m_sIndex);
              OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
            End;
            OutStr('  mov __temp1,0-0');
            sName:='__temp1';
          End
          Else
          Begin
            If g_apoCmdVars[3]^.m_bShadowReg Then
            Begin
              OutStr('  mov __temp1,'+g_apoCmdVars[3]^.sGetIndexName);
              sName:='__temp1';
            End
            Else sName:=g_apoCmdVars[3]^.sGetIndexName;
          End;
          Case g_apoCmdVars[5]^.eGetType of
            e_LongVar, e_LongConst:
              Begin
                If g_apoCmdVars[5]^.m_bVarIndex Then
                Begin
                  If g_bLMM Then
                  Begin
                    OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                    OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                    OutStr('  jmp #_LMM_MOVS');
                  End
                  Else
                  Begin
                    OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+','+g_apoCmdVars[5]^.sGetName);
                    OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                    OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+','+g_apoCmdVars[5]^.sGetName);
                  End;
                  OutStr('  cmps '+sName+',0-0 WZ, WC');
                End
                Else OutStr('  cmps '+sName+','+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                sCond:=''; // Assume invalid
                If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                If g_bLMM Then
                Begin
                  OutStr('  '+sCond+' rdlong __PC,__PC');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                g_bHandled:=True;
              End;
            e_ShortConst:
              Begin
                OutStr('  cmps '+sName+',#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                sCond:=''; // Assume invalid
                If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                If g_bLMM Then
                Begin
                  OutStr('  '+sCond+' rdlong __PC,__PC');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                g_bHandled:=True;
              End;
            e_Pin:
              Begin
                OutStr('  mov __temp2,ina');
                OutStr('  and __temp2,'+g_apoCmdVars[5]^.sGetName);
                If g_apoCmdVars[5]^.m_byPinNumber > 0 Then OutStr('  shr __temp2,#'+IntStr(g_apoCmdVars[5]^.m_byPinNumber));
                If g_apoCmdVars[5]^.m_bRevPins Then OutStr('  rev __temp2,#'+IntStr(32-g_apoCmdVars[5]^.m_lElements));
                OutStr('  cmps '+sName+',__temp2 WZ, WC');
                sCond:=''; // Assume invalid
                If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                If g_bLMM Then
                Begin
                  OutStr('  '+sCond+' rdlong __PC,__PC');
                  OutStr('  long @@@'+g_asCmdLine[6]);
                End
                Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                g_bHandled:=True;
              End;
            Else Error(c_iErrorInvalidParameter, 5);
          End; // Case
        End // If ByteVar
        Else Error(c_iErrorInvalidParameter, 3);
      End // [2] = 'WHILE' or 'UNTIL'
      Else Error(c_iErrorSyntaxError, 0);
    End // g_iCmdCnt = 5
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End;
End;


Procedure ProcessLoop;
Var
  sNot4: String[2];
  sCond: String[5];
Begin
  If g_iDoCnt > 0 Then
  Begin
    If g_iCmdCnt = 1 Then
    Begin
      If g_bLMM Then
      Begin
        OutStr('  rdlong __PC,__PC');
        OutStr('  long @@@__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
      End
      Else OutStr('  jmp #__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
      OutStr('__LOOP_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
      g_bHandled:=True;
    End
    Else
    Begin
      g_asCmdLine[6]:='__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID);
      If g_iCmdCnt = 2 Then
      Begin
        If Upper(g_asCmdLine[2]) = 'NEVER' Then
        Begin
          OutStr('__LOOP_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
          g_bHandled:=True;
        End
        Else If g_apoCmdVars[2]^.eGetType = e_LongVar Then
        Begin // LOOP var
          If g_apoCmdVars[2]^.m_bVarIndex Then
          Begin
            OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
            OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
            OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
            If g_bLMM Then
            Begin
              OutStr('  djnz 0-0,#_LMM_JUMP');
              OutStr('  long @@@__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
            End
            Else OutStr('  djnz 0-0,#__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
          End
          Else
          Begin
            If g_bLMM Then
            Begin
              OutStr('  djnz '+g_apoCmdVars[2]^.sGetIndexName+',#_LMM_JUMP');
              OutStr('  long @@@__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
            End
            Else OutStr('  djnz '+g_apoCmdVars[2]^.sGetIndexName+',#__DO_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
          End;
          OutStr('__LOOP_'+IntStr(g_arDo[g_iDoCnt].iAccumID));
          g_bHandled:=True;
        End
        Else Error(c_iErrorSyntaxError, 0);
      End
      Else
      Begin
        If g_iCmdCnt = 3 Then
        Begin // LOOP (UNTIL | WHILE) var
          Case g_apoCmdVars[3]^.eGetType of
            e_Pin:
              Begin
                If g_apoCmdVars[3]^.m_lElements = 1 Then
                Begin // Single Pin
                  OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR');
                  If ((Upper(g_asCmdLine[2]) = 'WHILE') and (g_asUnaryOperator[2] = '')) or
                     ((Upper(g_asCmdLIne[2]) = 'UNTIL') and (g_asUnaryOperator[2] = '~')) Then
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  IF_NZ rdlong __PC,__PC');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  IF_NZ jmp #'+g_asCmdLine[6]);
                  End
                  Else
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  IF_Z rdlong __PC,__PC');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  IF_Z jmp #'+g_asCmdLine[6]);
                  End;
                End
                Else
                Begin  // Pin Group
                  OutStr('  mov __temp1,ina');
                  OutStr('  and __temp1,'+g_apoCmdVars[3]^.sGetName);
                  If g_apoCmdVars[3]^.m_byPinNumber > 0 Then OutStr('  shr __temp1,#'+IntStr(g_apoCmdVars[3]^.m_byPinNumber));
                  If g_apoCmdVars[3]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[3]^.m_lElements));
                  Case g_apoCmdVars[5]^.eGetType of
                    e_ShortConst:
                      Begin
                        OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+',#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                      End;
                    e_LongConst, e_LongVar:
                      Begin
                        If g_apoCmdVars[5]^.m_bVarIndex Then
                        Begin // [3] is not var index, [5] is var index
                          If g_bLMM Then
                          Begin
                            OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                            OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                            OutStr('  jmp #_LMM_MOVS');
                          End
                          Else
                          Begin
                            OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                            OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                            OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                          End;
                          OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+',0-0 WZ, WC');
                        End
                        Else
                        Begin // [3] and [5] are not var index
                          OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+','+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                        End;
                      End;
                    Else Error(c_iErrorInvalidParameter, 5);
                  End; // Case
                  sCond:=''; // Assume invalid
                  If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                  If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                  If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                  If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                  If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                  If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                  If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                  If g_bLMM Then
                  Begin
                    OutStr('  '+sCond+' rdlong __PC,__PC');
                    OutStr('  long @@@'+g_asCmdLine[6]);
                  End
                  Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                  g_bHandled:=True;
                End;
              End;
            e_LongVar:
              Begin
                If ((Upper(g_asCmdLine[2]) = 'WHILE') and (g_asUnaryOperator[2] = '')) or
                   ((Upper(g_asCmdLIne[2]) = 'UNTIL') and (g_asUnaryOperator[2] = '~')) Then
                Begin
                  If g_apoCmdVars[3]^.m_bVarIndex Then
                  Begin
                    OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                    OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                    OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                    If g_bLMM Then
                    Begin
                      OutStr('  tjnz 0-0,#_LMM_JUMP');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  tjnz 0-0,#'+g_asCmdLine[6]);
                  End
                  Else
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  tjnz '+g_apoCmdVars[3]^.sGetIndexName+',#_LMM_JUMP');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  tjnz '+g_apoCmdVars[3]^.sGetIndexName+',#'+g_asCmdLine[6]);
                  End;
                  g_bHandled:=True;
                End
                Else
                Begin
                  // IF LongVar[3] is zero, JUMP to LABEL[6]
                  If g_apoCmdVars[3]^.m_bVarIndex Then
                  Begin
                    OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                    OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                    OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                    If g_bLMM Then
                    Begin
                      OutStr('  tjnz 0-0,#_LMM_JUMP');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  tjz 0-0,#'+g_asCmdLine[6]);
                  End
                  Else
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  tjnz '+g_apoCmdVars[3]^.sGetIndexName+',#_LMM_JUMP');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  tjz '+g_apoCmdVars[3]^.sGetIndexName+',#'+g_asCmdLine[6]);
                  End;
                  g_bHandled:=True;
                End;
              End;
          Else Error(c_iErrorInvalidParameter, 3);
          End;
          g_bHandled:=True;
        End
        Else If (g_arDo[g_iDoCnt].bDoCondition = FALSE) and (g_iCmdCnt = 5) Then
        Begin
          If Upper(g_asCmdLine[2]) = 'UNTIL' Then
          Begin
            sNot4:='';
            g_arDo[g_iDoCnt].bDoCondition:=True;
            sCond:=''; // Assume invalid
            If g_asCmdLine[4]='=' Then sNot4:='<>';
            If g_asCmdLine[4]='<>' Then sNot4:='=';
            If g_asCmdLine[4]='>' Then sNot4:='<=';
            If g_asCmdLine[4]='<' Then sNot4:='>=';
            If g_asCmdLine[4]='>=' Then sNot4:='<';
            If g_asCmdLine[4]='<=' Then sNot4:='>';
            If sNot4 = '' Then Error(c_iErrorInvalidParameter, 4);
            If sNot4 <> '' Then g_asCmdLine[4]:=sNot4;
          End;
          If (Upper(g_asCmdLine[2]) = 'WHILE') or (Upper(g_asCmdLine[2]) = 'UNTIL') Then
          Begin
            Case g_apoCmdVars[3]^.eGetType of
              e_Pin:
                Begin
                  If g_apoCmdVars[3]^.m_lElements = 1 Then
                  Begin // Single Pin var
                    If (g_asCmdLine[4] = '=') or (g_asCmdLine[4] = '<>') Then
                    Begin
                      Case g_apoCmdVars[5]^.eGetType of
                        e_ShortConst:
                          Begin // PinVar ? ShortConst
                            OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR');
                            If ((g_apoCmdVars[5]^.lGetValue = 0) and (g_asCmdLine[4] = '=')) or
                               ((g_apoCmdVars[5]^.lGetValue = 1) and (g_asCmdLine[4] = '<>')) Then
                            Begin
                              If g_bLMM Then
                              Begin
                                OutStr('  IF_Z rdlong __PC,__PC');
                                OutStr('  long @@@'+g_asCmdLine[6]);
                              End
                              Else OutStr('  IF_Z jmp #'+g_asCmdLine[6])
                            End
                            Else
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
                            OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR');
                            OutStr('  and '+g_apoCmdVars[5]^.sGetIndexName+',ina WC, NR');
                            If g_asCmdLine[4] = '=' Then
                            Begin
                              If g_bLMM Then
                              Begin
                                OutStr('  IF_C_NE_Z rdlong __PC,__PC');
                                OutStr('  long @@@'+g_asCmdLine[6]);
                              End
                              Else OutStr('  IF_C_NE_Z jmp #'+g_asCmdLine[6]);
                            End
                            Else
                            Begin
                              If g_bLMM Then
                              Begin
                                OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                                OutStr('  long @@@'+g_asCmdLine[6]);
                              End
                              Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
                            End;
                            g_bHandled:=True;
                          End;

                        e_LongVar:
                          Begin
                            If g_apoCmdVars[5]^.m_bVarIndex Then
                            Begin
                              If g_bLMM Then
                              Begin
                                OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                                OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                                OutStr('  jmp #_LMM_MOVS');
                              End
                              Else
                              Begin
                                OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                                OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                                OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                              End;
                              OutStr('  mov __temp1,0-0');
                              OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR'); // Set zero flag to pin state
                              OutStr('  subs __temp1,#1 WC, NR'); // Set carry if zero
                              If g_bLMM Then
                              Begin
                                OutStr('  IF_C_EQ_Z rdlong __PC,__PC');
                                OutStr('  long @@@'+g_asCmdLine[6]);
                              End
                              Else OutStr('  IF_C_EQ_Z jmp #'+g_asCmdLine[6]);
                            End
                            Else
                            Begin
                              OutStr('  and '+g_apoCmdVars[3]^.sGetIndexName+',ina WZ, NR'); // Set zero flag to pin state
                              OutStr('  subs '+g_apoCmdVars[5]^.sGetIndexName+',#1 WC, NR'); // Set carry if zero
                              If g_asCmdLine[4] = '=' Then
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
                        Else Error(c_iErrorInvalidParameter, 5);
                      End; // Case
                    End;
                  End
                  Else
                  Begin // Pin group
                    OutStr('  mov __temp1,ina');
                    OutStr('  and __temp1,'+g_apoCmdVars[3]^.sGetName);
                    If g_apoCmdVars[3]^.m_byPinNumber > 0 Then OutStr('  shr __temp1,#'+IntStr(g_apoCmdVars[3]^.m_byPinNumber));
                    If g_apoCmdVars[3]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[3]^.m_lElements));
                    Case g_apoCmdVars[5]^.eGetType of
                      e_ShortConst:
                        Begin
                          OutStr('  cmps __temp1,#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                        End;
                      e_LongConst, e_LongVar:
                        Begin
                          If g_apoCmdVars[5]^.m_bVarIndex Then
                          Begin // [3] is not var index, [5] is var index
                            If g_bLMM Then
                            Begin
                              OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                              OutStr('  jmp #_LMM_MOVS');
                            End
                            Else
                            Begin
                              OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                              OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                            End;
                            OutStr('  cmps __temp1,0-0 WZ, WC');
                          End
                          Else
                          Begin // [3] and [5] are not var index
                            OutStr('  cmps __temp1,'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                          End;
                        End;
                      e_Pin:
                        Begin
                          OutStr('  mov __temp2,ina');
                          OutStr('  and __temp2,'+g_apoCmdVars[5]^.sGetName);
                          If g_apoCmdVars[5]^.m_byPinNumber > 0 Then OutStr('  shr __temp2,#'+IntStr(g_apoCmdVars[5]^.m_byPinNumber));
                          If g_apoCmdVars[5]^.m_bRevPins Then OutStr('  rev __temp2,#'+IntStr(32-g_apoCmdVars[5]^.m_lElements));
                          OutStr('  cmps __temp1,__temp2 WZ, WC');
                        End;
                      Else Error(c_iErrorInvalidParameter, 5);
                    End; // Case
                    sCond:=''; // Assume invalid
                    If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                    If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                    If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                    If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                    If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                    If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                    If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                    If g_bLMM Then
                    Begin
                      OutStr('  '+sCond+' rdlong __PC,__PC');
                      OutStr('  long @@@'+g_asCmdLine[6]);
                    End
                    Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                    g_bHandled:=True;
                  End;
                End;
              e_LongVar:
                Begin // LongVar
                  Case g_apoCmdVars[5]^.eGetType of
                    e_ShortConst:
                      Begin
                        If g_apoCmdVars[3]^.m_bVarIndex Then
                        Begin
                          If g_bLMM Then
                          Begin
                            OutStr('  mov __INDEX,#'+g_apoCmdVars[3]^.sGetName);
                            OutStr('  add __INDEX,'+g_apoCmdVars[3]^.m_sIndex);
                            OutStr('  jmp #_LMM_MOVD');
                          End
                          Else
                          Begin
                            OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                            OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                            OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                          End;
                          OutStr('  cmps 0-0,#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                        End
                        Else
                        Begin
                          If g_apoCmdVars[3]^.m_bShadowReg Then
                          Begin
                            OutStr('  mov __temp1,'+g_apoCmdVars[3]^.sGetIndexName);
                            OutStr('  cmps __temp1,#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                          End
                          Else OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+',#'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                        End;
                      End;
                    e_LongConst, e_LongVar:
                      Begin
                        If g_apoCmdVars[3]^.m_bVarIndex Then
                        Begin // [3] is var index
                          If g_apoCmdVars[5]^.m_bVarIndex Then
                          Begin // [3] and [5] are var index
                            If g_bLMM Then
                            Begin
                              OutStr('  mov __INDEX,#'+g_apoCmdVars[3]^.sGetName);
                              OutStr('  add __INDEX,'+g_apoCmdVars[3]^.m_sIndex);
                              OutStr('  jmp #_LMM_MOVS');
                              OutStr('  mov __temp1,0-0');
                              OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                              OutStr('  jmp #_LMM_MOVS');
                              OutStr('  cmp __temp1,0-0');
                            End
                            Else
                            Begin
                              OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                              OutStr('  movd $+5,'+g_apoCmdVars[3]^.m_sIndex);
                              OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                              OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                              OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  cmps 0-0,0-0 WZ, WC');
                            End;
                          End
                          Else
                          Begin // [3] if var index, [5] is not
                            If g_bLMM Then
                            Begin
                              OutStr('  mov __INDEX,#'+g_apoCmdVars[3]^.sGetName);
                              OutStr('  add __INDEX,'+g_apoCmdVars[3]^.m_sIndex);
                              OutStr('  jmp #_LMM_MOVD');
                            End
                            Else
                            Begin
                              OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                              OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                              OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                            End;
                            OutStr('  cmps 0-0,'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                          End;
                        End
                        Else
                        Begin // [3] is not var index
                          If g_apoCmdVars[5]^.m_bVarIndex Then
                          Begin // [3] is not var index, [5] is var index
                            If g_apoCmdVars[3]^.m_bShadowReg Then OutStr('  mov __temp1,'+g_apoCmdVars[3]^.sGetIndexName);
                            If g_bLMM Then
                            Begin
                              OutStr('  mov __INDEX,#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  add __INDEX,'+g_apoCmdVars[5]^.m_sIndex);
                              OutStr('  jmp #_LMM_MOVS');
                            End
                            Else
                            Begin
                              OutStr('  add '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                              OutStr('  movs $+2,'+g_apoCmdVars[5]^.m_sIndex);
                              OutStr('  sub '+g_apoCmdVars[5]^.m_sIndex+',#'+g_apoCmdVars[5]^.sGetName);
                            End;
                            If g_apoCmdVars[3]^.m_bShadowReg Then OutStr('  cmps __temp1,0-0 WZ, WC')
                             Else OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+',0-0 WZ, WC');
                          End
                          Else
                          Begin // [3] and [5] are not var index
                            If g_apoCmdVars[3]^.m_bShadowReg Then
                            Begin
                              OutStr('  mov __temp1,'+g_apoCmdVars[3]^.sGetIndexName);
                              OutStr('  cmps __temp1,'+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                            End
                            Else OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+','+g_apoCmdVars[5]^.sGetIndexName+' WZ, WC');
                          End;
                        End;
                      End;
                    e_Pin:
                      Begin
                        OutStr('  mov __temp2,ina');
                        OutStr('  and __temp2,'+g_apoCmdVars[5]^.sGetName);
                        If g_apoCmdVars[5]^.m_byPinNumber > 0 Then OutStr('  shr __temp2,#'+IntStr(g_apoCmdVars[5]^.m_byPinNumber));
                        If g_apoCmdVars[5]^.m_bRevPins Then OutStr('  rev __temp2,#'+IntStr(32-g_apoCmdVars[5]^.m_lElements));
                        If g_apoCmdVars[3]^.m_bVarIndex Then
                        Begin // [3] is var index
                          If g_bLMM Then
                          Begin
                            OutStr('  mov __INDEX,#'+g_apoCmdVars[3]^.sGetName);
                            OutStr('  add __INDEX,'+g_apoCmdVars[3]^.m_sIndex);
                            OutStr('  jmp #_LMM_MOVD');
                          End
                          Else
                          Begin
                            OutStr('  add '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                            OutStr('  movd $+2,'+g_apoCmdVars[3]^.m_sIndex);
                            OutStr('  sub '+g_apoCmdVars[3]^.m_sIndex+',#'+g_apoCmdVars[3]^.sGetName);
                          End;
                          OutStr('  cmps 0-0,__temp2 WZ, WC');
                        End
                        Else
                        Begin // [3] is not var index
                          If g_apoCmdVars[3]^.m_bShadowReg Then
                          Begin
                            OutStr('  mov __temp1,'+g_apoCmdVars[3]^.sGetIndexName);
                            OutStr('  cmps __temp1,__temp2 WZ, WC');
                          End
                          Else OutStr('  cmps '+g_apoCmdVars[3]^.sGetIndexName+',__temp2 WZ, WC');
                        End;
                      End;
                    Else Error(c_iErrorInvalidParameter, 5);
                  End; // Case
                  sCond:=''; // Assume invalid
                  If g_asCmdLine[4] = '=' Then sCond:='IF_Z';
                  If g_asCmdLine[4] = '<>' Then sCond:='IF_NZ';
                  If g_asCmdLine[4] = '<' Then sCond:='IF_B';
                  If g_asCmdLine[4] = '>' Then sCond:='IF_A';
                  If g_asCmdLine[4] = '<=' Then sCond:='IF_BE';
                  If g_asCmdLine[4] = '>=' Then sCond:='IF_AE';
                  If sCond = '' Then Error(c_iErrorInvalidParameter, 4);
                  If g_bLMM Then
                  Begin
                    OutStr('  '+sCond+' rdlong __PC,__PC');
                    OutStr('  long @@@'+g_asCmdLine[6]);
                  End
                  Else OutStr('  '+sCond+' jmp #'+g_asCmdLine[6]);
                  g_bHandled:=True;
                End;

              Else Error(c_iErrorInvalidParameter, 3);
            End; // Case
            If g_bHandled and g_arExitLabels[g_iExitLabelCnt].bUsed Then OutStr('__LOOP_'+IntStr(g_arDo[g_iDoCnt].iAccumID)); // In case EXIT was used
          End // [2] = 'WHILE' or 'UNTIL'
          Else Error(c_iErrorSyntaxError, 0);
        End
        Else Error(c_iErrorInvalidNumberofParameters, 0);
      End;
    End;
  End
  Else Error(c_iErrorLoopWithoutDo, 1);
  Dec(g_iDoCnt);
  Dec(g_iExitLabelCnt);
End;


End.
