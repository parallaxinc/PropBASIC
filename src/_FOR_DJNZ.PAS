Unit _For;  

Interface

Uses Global;

Procedure ProcessFor;
Procedure ProcessNext;

Implementation

Procedure ProcessFor;
Var
  bCont: Boolean;
  sTemp: String;
Begin
  bCont:=True;
  If (g_iCmdCnt = 6) or (g_iCmdCnt = 8) Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      If (g_asCmdLine[3] = '=') and (Upper(g_asCmdLine[5]) = 'TO') Then
      Begin
        Inc(g_iForCnt);
        g_arForVars[g_iForCnt].poValue:=PLongVarObj(g_apoCmdVars[2]);
        g_arForVars[g_iForCnt].bDeleteValue:=g_abDeleteTemp[2];
        g_abDeleteTemp[2]:=False;
        If g_iCmdCnt = 8 Then
        Begin
          If g_apoCmdVars[8]^.eGetType In [e_ShortConst, e_LongConst, e_LongVar] Then
          Begin
            If Upper(g_asCmdLine[7]) = 'STEP' Then
            Begin
              If (g_apoCmdVars[6]^.eGetType = e_LongVar) or
                 (g_apoCmdVars[6]^.eGetType = e_ShortConst) or
                 (g_apoCmdVars[6]^.eGetType = e_LongConst) Then
              Begin
                g_arForVars[g_iForCnt].poStep:=g_apoCmdVars[8];
                g_arForVars[g_iForCnt].bDeleteStep:=g_abDeleteTemp[8];
                g_abDeleteTemp[8]:=False;
                If g_asUnaryOperator[8] = '-' Then
                Begin
                  g_arForVars[g_iForCnt].bDirPositive:=False;
                  If g_apoCmdVars[8]^.eGetType = e_LongVar Then g_arForVars[g_iForCnt].bAddStep:=False Else g_arForVars[g_iForCnt].bAddStep:=True;
                End
                Else
                Begin
                  g_arForVars[g_iForCnt].bAddStep:=True;
                  // If step if a var, and limit < start then assume negative step
                  If (g_apoCmdVars[4]^.eGetType In [e_ShortConst, e_LongConst]) and (g_apoCmdVars[6]^.eGetType In [e_ShortConst, e_LongConst]) Then
                  Begin
                    If g_apoCmdVars[4]^.lGetValue > g_apoCmdVars[6]^.lGetValue Then g_arForVars[g_iForCnt].bDirPositive:=False Else g_arForVars[g_iForCnt].bDirPositive:=True;
                  End
                  Else g_arForVars[g_iForCnt].bDirPositive:=True;
                End;
              End
              Else
              Begin
                Dec(g_iForCnt);
                Error(c_iErrorInvalidParameter, 6);
                bCont:=False;
              End;
            End
            Else
            Begin
              Dec(g_iForCnt);
              bCont:=False;
              Error(c_iErrorSyntaxError, 0);
            End;
          End
          Else Error(c_iErrorInvalidParameter, 8);  
        End
        Else
        Begin // Implied Step 1 or -1
          If (g_apoCmdVars[4]^.eGetType In [e_ShortConst, e_LongConst]) and
             (g_apoCmdVars[6]^.eGetType In [e_ShortConst, e_LongConst]) and
             (g_apoCmdVars[4]^.lGetValue > g_apoCmdVars[6]^.lGetValue) Then
          Begin
            g_arForVars[g_iForCnt].poStep:=New(PShortConstObj, Init('1', 1, '1'));
            g_arForVars[g_iForCnt].bDirPositive:=False;
            g_arForVars[g_iForCnt].bAddStep:=False;
            g_arForVars[g_iForCnt].bDeleteStep:=True;
          End
          Else
          Begin
            g_arForVars[g_iForCnt].poStep:=New(PShortConstObj, Init('1', 1, '1'));
            g_arForVars[g_iForCnt].bDirPositive:=True;
            g_arForVars[g_iForCnt].bAddStep:=True;
            g_arForVars[g_iForCnt].bDeleteStep:=True;
          End;
        End;
        If bCont Then
        Begin
          If (g_apoCmdVars[6]^.eGetType In [e_LongVar, e_ShortConst, e_LongConst]) Then
          Begin
            If (g_apoCmdVars[6]^.eGetType In [e_ShortConst, e_LongConst]) and (g_asUnaryOperator[6] = '-') Then
            Begin
              g_apoCmdVars[6]^.m_lValue:=-g_apoCmdVars[6]^.m_lValue;
              g_apoCmdVars[6]^.m_sName:=g_apoCmdVars[6]^.m_sName;
            End;
            g_arForVars[g_iForCnt].poLimit:=g_apoCmdVars[6];
            g_arForVars[g_iForCnt].bDeleteLimit:=g_abDeleteTemp[6];
            g_abDeleteTemp[6]:=False;
          End
          Else
          Begin
            Dec(g_iForCnt);
            Error(c_iErrorInvalidParameter, 6);
            bCont:=False;
          End;
        End;
        If bCont and (g_apoCmdVars[4]^.eGetType In [e_LongVar, e_ShortConst, e_LongConst]) Then
        Begin
          Inc(g_iForAccum);
          Inc(g_iExitLabelCnt);
          sTemp:=g_arForvars[g_iForCnt].poValue^.sGetIndexName;
          If sTemp[1] = '(' Then Delete(sTemp, 1, 1);
          If Pos('+', sTemp) > 0 Then Delete(sTemp, Pos('+', sTemp), (Length(sTemp)-Pos('+', sTemp))+1);
          g_arExitLabels[g_iExitLabelCnt].sLabel:='__NEXT_'+sTemp+'_'+IntStr(g_iForAccum);
          g_arExitLabels[g_iExitLabelCnt].bUsed:=FALSE;
          g_arForVars[g_iForCnt].sLabel:='_'+sTemp+'_'+IntStr(g_iForAccum);
          If g_apoCmdVars[2]^.sGetIndexName <> g_apoCmdVars[4]^.sGetIndexName Then
          Case g_apoCmdVars[4]^.eGetType of
            e_LongVar, e_LongConst:
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
                  If g_arForvars[g_iForCnt].poValue^.m_bVarIndex Then
                  Begin
                    OutStr('  mov __temp1,0-0');
                    If g_bLMM Then
                    Begin
                      OutStr('  mov __INDEX,#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                      OutStr('  add __INDEX,'+g_arForVars[g_iForCnt].poValue^.m_sIndex);
                      OutStr('  jmp #_LMM_MOVD');
                    End
                    Else
                    Begin
                      OutStr('  add '+g_arForVars[g_iForCnt].poValue^.m_sIndex+',#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                      OutStr('  movd $+2,'+g_arForVars[g_iForCnt].poValue^.m_sIndex);
                      OutStr('  sub '+g_arForVars[g_iForCnt].poValue^.m_sIndex+',#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                    End;
                    OutStr('  mov 0-0,__temp1');
                  End
                  Else OutStr('  mov '+g_arForVars[g_iForCnt].poValue^.sGetIndexName+',0-0');
                End
                Else
                Begin
                  If g_arForVars[g_iForCnt].poValue^.m_bVarIndex Then
                  Begin
                    If g_bLMM Then
                    Begin
                      OutStr('  mov __INDEX,#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                      OutStr('  add __INDEX,'+g_arForVars[g_iForCnt].poValue^.m_sIndex);
                      OutStr('  jmp #_LMM_MOVD');
                    End
                    Else
                    Begin
                      OutStr('  add '+g_arForVars[g_iForCnt].poValue^.m_sIndex+',#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                      OutStr('  movd $+2,'+g_arForVars[g_iForCnt].poValue^.m_sIndex);
                      OutStr('  sub '+g_arForVars[g_iForCnt].poValue^.m_sIndex+',#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                    End;
                    OutStr('  mov 0-0,'+g_apoCmdVars[4]^.sGetIndexName);
                  End
                  Else
                  Begin
                    OutStr('  mov '+g_arForVars[g_iForCnt].poValue^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
                  End;  
                End;
              End;
            e_ShortConst:
              Begin
                If g_arForVars[g_iForCnt].poValue^.m_bVarIndex Then
                Begin
                  If g_bLMM Then
                  Begin
                    OutStr('  mov __INDEX,#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                    OutStr('  add __INDEX,'+g_arForVars[g_iForCnt].poValue^.m_sIndex);
                    OutStr('  jmp #_LMM_MOVD');
                  End
                  Else
                  Begin
                    OutStr('  add '+g_arForVars[g_iForCnt].poValue^.m_sIndex+',#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                    OutStr('  movd $+2,'+g_arForVars[g_iForCnt].poValue^.m_sIndex);
                    OutStr('  sub '+g_arForVars[g_iForCnt].poValue^.m_sIndex+',#'+g_arForVars[g_iForCnt].poValue^.sGetName);
                  End;
                  OutStr('  mov 0-0,#'+g_apoCmdVars[4]^.sGetName);
                End
                Else
                Begin
                  OutStr('  mov '+g_arForVars[g_iForCnt].poValue^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
                End;
              End;
          End;
          OutStr('__FOR'+g_arForVars[g_iForCnt].sLabel);
          VP_WriteForVar(g_iForCnt);
          g_bHandled:=True;
        End
        Else
        Begin
          If bCont Then Dec(g_iForCnt);
          If bCont Then Error(c_iErrorInvalidParameter, 4);
//          bCont:=False;
        End;
      End
      Else
      Begin
        Error(c_iErrorSyntaxError, 0);
      End;
    End
    Else
    Begin
      Error(c_iErrorInvalidParameter, 2);
    End;
  End
  Else
  Begin
    Error(c_iErrorInvalidNumberOfParameters, 0);
  End;
End;


Procedure ProcessNext;
Var
  iFor: Integer;
  I: Integer;
  bUseDJNZ: Boolean;
Begin
  bUseDJNZ:=False;
  iFor:=0;
  If g_iCmdCnt = 1 Then
  Begin
    If g_iForCnt = 0 Then Error(c_iErrorNextWithOutFor, 0);
    iFor:=g_iForCnt;
  End
  Else
  Begin
    If g_apoCmdVars[2]^.eGetType In [e_LongVar] Then
    Begin
      iFor:=1;
      While (iFor <= g_iForCnt) and (g_arForVars[iFor].poValue^.sGetIndexName <> g_apoCmdVars[2]^.sGetIndexName) Do Inc(iFor);
      If iFor > g_iForCnt Then iFor:=g_iForCnt;
      If (iFor = 0) or (g_apoCmdVars[2]^.sGetIndexName <> g_arForVars[iFor].poValue^.sGetName) Then Error(c_iErrorNotAForControlVariable, 2);
    End
    Else Error(c_iErrorNotAForControlVariable, 2);
  End;
  If ((g_iCmdCnt = 1) and (iFor > 0)) or ((g_iCmdCnt = 2) and (iFor > 0) and (g_apoCmdVars[2]^.sGetName = g_arForVars[iFor].poValue^.sGetName)) Then
  Begin
    // Handled special case (No rollover possible)
    //  Sets g_bHandled to TRUE if special case code is used
    Case g_arForVars[iFor].poStep^.eGetType of
      e_LongVar, e_LongConst:
        Begin
          If g_arForVars[iFor].poValue^.m_bVarIndex Then
          Begin // Value is var Index
            If g_arForVars[iFor].poStep^.m_bVarIndex Then
            Begin // Value AND step are var indexes
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poStep^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poStep^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
                OutStr('  mov __temp1,0-0');
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                // Put step value into __temp1
                OutStr('  add '+g_arForVars[iFor].poStep^.m_sIndex+',#'+g_arForVars[iFor].poStep^.sGetName);
                OutStr('  movs $+2,'+g_arForVars[iFor].poStep^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poStep^.m_sIndex+',#'+g_arForVars[iFor].poStep^.sGetName);
                OutStr('  mov __temp1,0-0');
                // Add __temp1 to "value"
                OutStr('  add '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  movd $+2,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
              End;
              If g_arForVars[iFor].bAddStep Then OutStr('  adds 0-0,__temp1') Else OutStr('  subs 0-0,__temp1');
            End
            Else
            Begin // Value is var Index, Step is not
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                OutStr('  add '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  movd $+2,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
              End;
              If g_arForVars[iFor].bAddStep Then OutStr('  adds 0-0,'+g_arForVars[iFor].poStep^.sGetIndexName)
               Else OutStr('  subs 0-0,'+g_arForVars[iFor].poStep^.sGetIndexName);
            End
          End
          Else
          Begin // Value is not var index
            If g_arForVars[iFor].poStep^.m_bVarIndex Then
            Begin // Value is not var index, step IS var index
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poStep^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poStep^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
              End
              Else
              Begin
                OutStr('  add '+g_arForVars[iFor].poStep^.m_sIndex+',#'+g_arForVars[iFor].poStep^.sGetName);
                OutStr('  movs $+2,'+g_arForVars[iFor].poStep^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poStep^.m_sIndex+',#'+g_arForVars[iFor].poStep^.sGetName);
              End;
              If g_arForVars[iFor].bAddStep Then OutStr('  adds '+g_arForVars[iFor].poValue^.sGetIndexName+',0-0')
               Else OutStr('  subs '+g_arForVars[iFor].poValue^.sGetIndexName+',0-0');
            End
            Else
            Begin
              // Neither value nor step is var index

              // Check if we can use DJNZ
              If (g_arForVars[iFor].bAddStep) and (g_arForVars[iFor].poStep^.eGetType = e_ShortConst) and (g_arForVars[iFor].poStep^.lGetValue = -1) Then
              Begin
                bUseDJNZ:=True;
              End
              Else
              Begin
                If g_arForVars[iFor].bAddStep Then OutStr('  adds '+g_arForVars[iFor].poValue^.sGetIndexName+','+g_arForVars[iFor].poStep^.sGetName)
                 Else OutStr('  subs '+g_arForVars[iFor].poValue^.sGetIndexName+','+g_arForVars[iFor].poStep^.sGetIndexName);
              End;
            End;
          End;
        End;
      e_ShortConst:
        Begin
          If g_arForVars[iFor].poValue^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_arForVars[iFor].poValue^.sGetName);
              OutStr('  add __INDEX,'+g_arForVars[iFor].poValue^.m_sIndex);
              OutStr('  jmp #_LMM_MOVD');
            End
            Else
            Begin
              OutStr('  add '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
              OutStr('  movd $+2,'+g_arForVars[iFor].poValue^.m_sIndex);
              OutStr('  sub '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
            End;
            If g_arForVars[iFor].bAddStep Then OutStr('  adds 0-0,#'+g_arForVars[iFor].poStep^.sGetIndexName)
             Else OutStr('  subs 0-0,#'+g_arForVars[iFor].poStep^.sGetIndexName);
          End
          Else
          Begin
            If g_arForVars[iFor].bAddStep Then OutStr('  adds '+g_arForVars[iFor].poValue^.sGetIndexName+',#'+g_arForVars[iFor].poStep^.sGetIndexName)
             Else OutStr('  subs '+g_arForVars[iFor].poValue^.sGetIndexName+',#'+g_arForVars[iFor].poStep^.sGetIndexName);
          End;
        End;
    End;

    VP_WriteForVar(iFor);
    
    Case g_arForVars[iFor].poLimit^.eGetType of
      e_LongVar, e_LongConst:
        Begin
          If g_arForVars[iFor].poValue^.m_bVarIndex Then
          Begin // Value is var Index
            If g_arForVars[iFor].poLimit^.m_bVarIndex Then
            Begin // Value AND limit are var indexes
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poLimit^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poLimit^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
                OutStr('  mov __temp1,0-0');
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                // Put limit value into __temp1
                OutStr('  add '+g_arForVars[iFor].poLimit^.m_sIndex+',#'+g_arForVars[iFor].poLimit^.sGetName);
                OutStr('  movs $+2,'+g_arForVars[iFor].poLimit^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poLimit^.m_sIndex+',#'+g_arForVars[iFor].poLimit^.sGetName);
                OutStr('  mov __temp1,0-0');
                // Add __temp1 to "value"
                OutStr('  add '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  movd $+2,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
              End;
              OutStr('  cmps 0-0,__temp1 WZ, WC');
            End
            Else
            Begin // Value is var Index, Limit is not
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                OutStr('  add '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
                OutStr('  movd $+2,'+g_arForVars[iFor].poValue^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
              End;
              OutStr('  cmps 0-0,'+g_arForVars[iFor].poLimit^.sGetIndexName+' WZ, WC');
            End
          End
          Else
          Begin // Value is not var index
            If g_arForVars[iFor].poLimit^.m_bVarIndex Then
            Begin // Value is not var index, Limit IS var index
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_arForVars[iFor].poLimit^.sGetName);
                OutStr('  add __INDEX,'+g_arForVars[iFor].poLimit^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
              End
              Else
              Begin
                OutStr('  add '+g_arForVars[iFor].poLimit^.m_sIndex+',#'+g_arForVars[iFor].poLimit^.sGetName);
                OutStr('  movs $+2,'+g_arForVars[iFor].poLimit^.m_sIndex);
                OutStr('  sub '+g_arForVars[iFor].poLimit^.m_sIndex+',#'+g_arForVars[iFor].poLimit^.sGetName);
              End;
              OutStr('  cmps '+g_arForVars[iFor].poValue^.sGetIndexName+',0-0 WZ, WC');
            End
            Else OutStr('  cmps '+g_arForVars[iFor].poValue^.sGetIndexName+','+g_arForVars[iFor].poLimit^.sGetIndexName+' WZ, WC');
          End;
        End;
      e_ShortConst:
        Begin
          If g_arForVars[iFor].poValue^.m_bVarIndex Then
          Begin
            If g_bLMM Then
            Begin
              OutStr('  mov __INDEX,#'+g_arForVars[iFor].poValue^.sGetName);
              OutStr('  add __INDEX,'+g_arForVars[iFor].poValue^.m_sIndex);
              OutStr('  jmp #_LMM_MOVD');
            End
            Else
            Begin
              OutStr('  add '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
              OutStr('  movd $+2,'+g_arForVars[iFor].poValue^.m_sIndex);
              OutStr('  sub '+g_arForVars[iFor].poValue^.m_sIndex+',#'+g_arForVars[iFor].poValue^.sGetName);
            End;
            OutStr('  cmps 0-0,#'+g_arForVars[iFor].poLimit^.sGetName+' WZ, WC');
          End
          Else OutStr('  cmps '+g_arForVars[iFor].poValue^.sGetIndexName+',#'+g_arForVars[iFor].poLimit^.sGetIndexName+' WZ, WC');
        End;
    End;
    If g_arForVars[iFor].bDirPositive Then
    Begin
      If g_bLMM Then
      Begin
        OutStr('  IF_BE rdlong __PC,__PC');
        OutStr('  long @@@__FOR'+g_arForVars[iFor].sLabel);
      End
      Else OutStr('  IF_BE jmp #__FOR'+g_arForVars[iFor].sLabel)
    End
    Else
    Begin
      If g_bLMM Then
      Begin
        OutStr('  IF_AE rdlong __PC,__PC');
        OutStr('  long @@@__FOR'+g_arForVars[iFor].sLabel);
      End
      Else OutStr('  IF_AE jmp #__FOR'+g_arForVars[iFor].sLabel);
    End;

    OutStr('__NEXT'+g_arForVars[iFor].sLabel); // Need in case EXIT is used

    If g_arForVars[iFor].bDeleteValue Then Dispose(g_arForVars[iFor].poValue, Done);
    If g_arForVars[iFor].bDeleteLimit Then Dispose(g_arForVars[iFor].poLimit, Done);
    If g_arForVars[iFor].bDeleteStep Then Dispose(g_arForVars[iFor].poStep, Done);
    g_bHandled:=True;
    // remove from list
    For I:=iFor To g_iForCnt-1 Do g_arForVars[I]:=g_arForVars[I+1];
    Dec(g_iForCnt);
    Dec(g_iExitLabelCnt);
  End
  Else Error(c_iErrorSyntaxError, 0);
End;

End.
