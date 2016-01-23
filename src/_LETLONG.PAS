Unit _LETLONG;

Interface

Procedure ProcessLetLongVar;

Implementation

Uses Global;

Procedure ProcessLetLongVar;
Var
  poTempVar: PVarObj;
  sTemp: String;
  bTemp: Boolean;
  iSubroutinePlace: Integer;
  bDestArray: Boolean;

  Procedure ProcessLetLongFunction;
  Var
    iParamCnt: Integer;
  Begin
    iSubroutinePlace:=SubIndex(g_asCmdLine[4]);
    iParamCnt:=0;
    If (g_iCmdCnt > 4) and (Not Odd(g_iCmdCnt)) Then Error(c_iErrorInvalidNumberOfParameters, 0);
    If g_iCmdCnt > 4 Then
    Begin
      iParamCnt:=SetupParameters(5);
    End;
    If iSubroutinePlace > 0 Then
    Begin
      g_arSubroutines[iSubroutinePlace].bUsed:=True;
      If (iParamCnt < g_arSubroutines[iSubroutinePlace].iMinParams) or
         (iParamCnt > g_arSubroutines[iSubroutinePlace].iMaxParams) Then
      Begin
        Error(c_iErrorInvalidNumberOfParameters, 0);
      End
      Else
      Begin
        If g_arSubroutines[iSubroutinePlace].iMinParams <> g_arSubroutines[iSubroutinePlace].iMaxParams Then
        Begin
          OutStr('  mov __paramcnt,#'+IntStr(iParamCnt));
        End;
        If g_bLMM Then
        Begin
          OutStr('  jmp #_LMM_CALL');
          OutStr('  long @@@'+g_asCmdLine[4]);
        End
        Else OutStr('  call #'+g_asCmdLine[4]);
        If Upper(g_apoCmdVars[2]^.sGetName) <> '__PARAM1' Then
        Begin
          If g_apoCmdVars[2]^.m_bVarIndex = False Then
          Begin
            OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__param1');
            VP_WriteVar(2);
          End
          Else
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
            OutStr('  mov 0-0,__param1');
          End;
        End;
        g_bHandled:=True;
      End;
    End
    Else
    Begin { Not a declared sub }
      OutStr('  mov __paramcnt,#'+IntStr(iParamCnt));
      If g_bLMM Then
      Begin
        OutStr('  jmp #_LMM_CALL');
//        OutStr('  long @@@'+g_asCmdLine[4]+'_ret');
        OutStr('  long @@@'+g_asCmdLine[4]);
      End
      Else OutStr('  call #'+g_asCmdLine[4]);
      If Upper(g_apoCmdVars[2]^.sGetName) <> '__param1' Then OutStr('  mov '+g_apoCmdVars[2]^.sGetName+',__param1');
      g_bHandled:=True;
    End;
  End;

  Procedure ProcessLetLongAssign;
  Var
    sOp: String[6];
  Begin { Simple Assignment }
    If ((g_asCmdLine[5] = '-') or (g_asCmdLine[5] = '<<') or (g_asCmdLine[5] = '>>')) and
     (g_iCmdCnt = 6) and (g_apoCmdVars[2]^.m_sAlias = g_apoCmdVars[6]^.m_sAlias) and
     (g_apoCmdVars[2]^.m_sIndex = g_apoCmdVars[6]^.m_sIndex) Then
    Begin
      { w1 = 1000 - w1 }
      OutStr('  mov __temp1,'+g_apoCmdVars[6]^.sGetIndexName);
    End;
    If (g_asUnaryOperator[4] = '~') or (g_asUnaryOperator[4] = '-') Then sOp:='  neg ' Else sOp:='  mov ';
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[4]^.m_lElements > 1 Then
          Begin // var = pin group
            OutStr('  mov __temp1,ina');
            OutStr('  and __temp1,'+g_apoCmdVars[4]^.sGetName);
            If g_asUnaryOperator[4] = '~' Then OutStr('  xor __temp1,'+g_apoCmdVars[4]^.sGetName);
            If g_apoCmdVars[4]^.m_byPinNumber > 0 Then OutStr('  shr __temp1,#'+IntStr(g_apoCmdVars[4]^.m_byPinNumber));
            If g_apoCmdVars[4]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[4]^.m_lElements));
            If Upper(g_apoCmdVars[2]^.sGetName) <> '__TEMP1' Then OutStr('  mov '+g_apoCmdVars[2]^.sGetName+',__temp1');
          End
          Else
          Begin // var = pin
            OutStr('  and '+g_apoCmdVars[4]^.sGetName+',ina WZ, NR');
            If g_asUnaryOperator[4] = '~' Then
            Begin
              OutStr('  IF_Z   mov '+g_apoCmdVars[2]^.sGetName+',#1');
              OutStr('  IF_NZ  mov '+g_apoCmdVars[2]^.sGetName+',#0');
            End
            Else
            Begin
              OutStr('  IF_Z '+sOp+g_apoCmdVars[2]^.sGetName+',#0');
              OutStr('  IF_NZ '+sOp+g_apoCmdVars[2]^.sGetName+',#1');
            End;
          End;
          g_bHandled:=True;
        End;
      e_LongConst:
        Begin
          If g_apoCmdVars[2]^.m_bVarIndex = False Then
          Begin
            If (g_asUnaryOperator[4] = '~') or ((g_asUnaryOperator[4] ='-') and (Not (g_asCmdLine[4, 2] In ['0'..'9']))) Then OutStr('  neg '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName)
             Else OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
          End
          Else
          Begin // Index is a var
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
            If (g_asUnaryOperator[4] = '~') or ((g_asUnaryOperator[4] ='-') and (Not (g_asCmdLine[4, 2] In ['0'..'9']))) Then OutStr('  neg 0-0,'+g_apoCmdVars[4]^.sGetIndexName)
             Else OutStr('  mov 0-0,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.m_bVarIndex = False Then
          Begin
            If (g_asUnaryOperator[4] = '~') or ((g_asUnaryOperator[4] ='-') and (Not (g_asCmdLine[4, 2] In ['0'..'9']))) Then OutStr('  neg '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName)
             Else OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
          End
          Else
          Begin // Index is a var
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
            If (g_asUnaryOperator[4] = '~') or ((g_asUnaryOperator[4] ='-') and (Not (g_asCmdLine[4, 2] In ['0'..'9']))) Then OutStr('  neg 0-0,#'+g_apoCmdVars[4]^.sGetName)
             Else OutStr('  mov 0-0,#'+g_apoCmdVars[4]^.sGetName);
          End;
          g_bHandled:=True;
        End;
      e_LongVar:
        Begin
          If g_apoCmdVars[4]^.m_bVarIndex = False Then
          Begin
            If g_apoCmdVars[2]^.m_bVarIndex = False Then
            Begin // source is NOT var indexed, dest is NOT var indexed
              If (g_apoCmdVars[2]^.m_sAlias <> g_apoCmdVars[4]^.m_sAlias) or
                 (g_apoCmdVars[2]^.m_sIndex <> g_apoCmdVars[4]^.m_sIndex) or
                 (sOp <> '  mov ') Then
              Begin
                OutStr(sOp+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              End
              Else OutStr('  '); // Do not move a variable into itself
            End
            Else
            Begin // dest has var index, source is NOT var indexed
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
              OutStr(sOp+'0-0,'+g_apoCmdVars[4]^.sGetIndexName);
            End;
          End
          Else
          Begin // source has var index, dest may or may not be var indexed
            If g_apoCmdVars[2]^.m_bVarIndex Then
            Begin // source is var index, dest is var index
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
                OutStr('  mov __temp1,0-0');
                OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
                OutStr(sOp+'0-0,__temp1');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  movs $+5,'+g_apoCmdVars[4]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr(sOp+'0-0,0-0');
              End;
            End
            Else
            Begin // source is var index, dest is NOT var index
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
              OutStr(sOp+g_apoCmdVars[2]^.sGetIndexName+',0-0');
            End;
          End;
          g_bHandled:=True;
        End;
      Else { Assume a label }
        Begin
          OutStr('  '+sOp+g_apoCmdVars[2]^.sGetIndexName+',#'+g_asCmdLine[4]);
          g_bHandled:=True;
        End;
    End; { Case eGetType}
    If (g_asUnaryOperator[4] = '~') and (g_apoCmdVars[4]^.eGetType <> e_Pin) Then OutStr('  sub '+g_apoCmdVars[2]^.sGetName+',#1');
  End; { Long Assignment }


  Procedure ProcessLetLongVarPlus;
  Begin { Addition }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long '+' }


  Procedure ProcessLetLongVarSUB;
  Begin { Subtraction }
    // Special case W1 = x - W1 ; Here W1's original value is put into __TEMP1
    Case g_apoCmdVars[6]^.eGetType of
      e_LongConst:
        Begin
          If g_asCmdLine[6, 1] ='-' Then
           OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName)
          Else OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;  
      e_LongVar:
        Begin
          If (g_apoCmdVars[6]^.m_sAlias = g_apoCmdVars[2]^.m_sAlias) and
             (g_apoCmdVars[6]^.m_sIndex = g_apoCmdVars[2]^.m_sIndex) Then
          Begin
            OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',__temp1');
          End
          Else
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
              OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
            End
            Else OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long '-' }


  Procedure ProcessLetLongVarAND;
  Begin { AND '&' }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  and '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  and '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  and '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long 'AND' }


  Procedure ProcessLetLongVarAndN;
  Begin { ANDN '&~' }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  andn '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  andn '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  andn '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long ANDN '&~' }


  Procedure ProcessLetLongVarOR;
  Begin { OR '|' }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  or '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  or '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  or '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long 'OR' }


  Procedure ProcessLetLongVarXOR;
  Begin { XOR '^' }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  xor '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  xor '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  xor '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long 'XOR' }


  Procedure ProcessLetLongVarSHL;
  // Special case W1 = x << W1 ; Here W1's original value is put into __TEMP1
  Begin { SHL }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
        Begin
          If (g_apoCmdVars[6]^.m_sAlias = g_apoCmdVars[2]^.m_sAlias) and
             (g_apoCmdVars[6]^.m_sIndex = g_apoCmdVars[2]^.m_sIndex) Then
          Begin
            OutStr('  shl '+g_apoCmdVars[2]^.sGetName+',__temp1');
          End
          Else
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
              OutStr('  shl '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
            End
            Else OutStr('  shl '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  shl '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long SHL }


  Procedure ProcessLetLongVarSHR;
  // Special case W1 = x >> W1 ; Here W1's original value is put into __TEMP1
  Begin { SHR }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
        Begin
          If (g_apoCmdVars[6]^.m_sAlias = g_apoCmdVars[2]^.m_sAlias) and
             (g_apoCmdVars[6]^.m_sIndex = g_apoCmdVars[2]^.m_sIndex) Then
          Begin
            OutStr('  shr '+g_apoCmdVars[2]^.sGetName+',__temp1');
          End
          Else
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
              OutStr('  shr '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
            End
            Else OutStr('  shr '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  shr '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long SHR }


  Procedure ProcessLetLongVarMIN;
  Begin { MIN }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  mins '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  mins '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  mins '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long 'MIN' }


  Procedure ProcessLetLongVarMAX;
  Begin { MAX }
    Case g_apoCmdVars[6]^.eGetType of
      e_LongVar, e_LongConst:
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
            OutStr('  maxs '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
          End
          Else OutStr('  maxs '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          OutStr('  maxs '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[6]^.sGetIndexName);
          g_bHandled:=True;
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; { Case [6] }
  End; { Long 'Max' }


  Procedure ProcessLetLongVarMult;
  Var
    lShift1: LongInt;
    lShift2: LongInt;
    bShift: Boolean;
  Begin { Multiplication }
    If g_iCmdCnt = 6 Then
    Begin
      lShift1:=-1;
      lShift2:=-1;
      bShift:=False;
      If g_apoCmdVars[6]^.eGetType In [e_ShortConst, e_LongConst] Then
      Begin
        lShift1:=0;
        Repeat
          If (1 SHL lShift1) = g_apoCmdVars[6]^.lGetValue Then
          Begin // Single shift multiply
            lShift2:=-1;
            bShift:=True;
          End
          Else
          Begin
            lShift2:=lShift1+1;
            Repeat
              If ((1 SHL lShift1) + (1 SHL lShift2)) = g_apoCmdVars[6]^.lGetValue Then
              Begin
                bShift:=True;
              End
              Else Inc(lShift2);
            Until bShift or (lShift2 > 30);
          End;
          If Not bShift Then Inc(lShift1);
        Until bShift or (lShift1 > 30);
//        Repeat
//          Inc(lShift);
//          If (1 SHL lShift) = g_apoCmdVars[6]^.lGetValue Then bShift:=True;
//        Until (bShift = True) or (lShift = 31);
      End;
      If bShift = True Then
      Begin
        If g_apoCmdVars[2]^.m_bVarIndex Then
        Begin
          If g_apoCmdVars[4]^.m_bVarIndex Then
          Begin // Both var indexes
            If (g_apoCmdVars[2]^.sGetIndexName <> g_apoCmdVars[4]^.sGetIndexName) or
               (g_apoCmdVars[2]^.m_sIndex <> g_apoCmdVars[4]^.m_sIndex) Then
            Begin // not a(x) = a(x) * y
              If g_bLMM Then
              Begin
                OutStr('  mov __INDEX,#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[4]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVS');
                OutStr('  mov __temp1,0-0');
// is this needed ???                OutStr('  shl __temp1,#'+IntStr(lShift1));
                OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
                OutStr('  jmp #_LMM_MOVD');
              End
              Else
              Begin
                OutStr('  add '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  movs $+2,'+g_apoCmdVars[4]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[4]^.m_sIndex+',#'+g_apoCmdVars[4]^.sGetName);
                OutStr('  mov __temp1,0-0');
                OutStr('  shl __temp1,#'+IntStr(lShift1));
                OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
                OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
                OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
              End;
              OutStr('  mov 0-0,__temp1');
            End
            Else
            Begin // array and index are the same, so don't move value
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
              OutStr('  shl 0-0,#'+IntStr(lShift1));
            End;
          End
          Else
          Begin // [2] is var index, [4] is not
            Case g_apoCmdVars[4]^.eGetType of
              e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetName);
              e_LongConst, e_LongVar: OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
              Else Error(c_iErrorInvalidParameter, 4);
            End; // Case
            If lShift2 > 0 Then
            Begin
              OutStr('  mov __temp2,__temp1');
              OutStr('  shl __temp1,#'+IntStr(lShift1));
              OutStr('  shl __temp2,#'+IntStr(lShift2));
              OutStr('  add __temp1,__temp2');
            End
            Else OutStr('  shl __temp1,#'+IntStr(lShift1));
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
            OutStr('  mov 0-0,__temp1');
          End
        End
        Else
        Begin // [2] is not var index, [4] ???
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
            OutStr('  mov __temp1,0-0');
            OutStr('  shl __temp1,#'+IntStr(lShift1));
            OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp1');
          End
          Else
          Begin // [2] and [4] are not var indexes
            Case g_apoCmdVars[4]^.eGetType of
              e_ShortConst: OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
              e_LongConst: OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              e_LongVar: If Upper(g_apoCmdVars[2]^.sGetIndexName) <> Upper(g_apoCmdVars[4]^.sGetIndexName) Then OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              Else Error(c_iErrorInvalidParameter, 4);
            End; // Case

            If lShift2 > 0 Then
            Begin
              OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
              OutStr('  shl '+g_apoCmdVars[2]^.sGetIndexName+',#'+IntStr(lShift1));
              OutStr('  shl __temp1,#'+IntStr(lShift2));
              OutStr('  add '+g_apoCmdVars[2]^.sGetIndexname+',__temp1');
            End
            Else OutStr('  shl '+g_apoCmdVars[2]^.sGetIndexName+',#'+IntStr(lShift1));
          End;
        End;
        g_bHandled:=True;
      End
      Else
      Begin
        Case g_apoCmdVars[4]^.eGetType of
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
              OutStr('  mov __temp1,0-0');
            End
            Else If Upper(g_apoCmdVars[4]^.sGetName) <> '__TEMP1' Then OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
          e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetIndexName);
        End;
        Case g_apoCmdVars[6]^.eGetType of
          e_LongConst, e_LongVar:
            If g_apoCmdVars[4]^.m_bVarIndex Then
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
              OutStr('  mov __temp2,0-0');
            End
            Else OutStr('  mov __temp2,'+g_apoCmdVars[6]^.sGetIndexName);
          e_ShortConst: OutStr('  mov __temp2,#'+g_apoCmdVars[6]^.sGetIndexName);
          Else Error(c_iErrorInvalidParameter, 6);
        End;

        g_bUsesMult:=True;
        If g_bLMM Then
        Begin
          OutStr('  jmp #_LMM_CALL');
          OutStr('  long @@@_MULTIPLY');
        End
        Else OutStr('  call #_MULTIPLY');
{
        // Set Zero flag to sign
        OutStr('  abs __temp1,__temp1 WC');
        OutStr('  muxc __temp3,#1');
        OutStr('  abs __temp2,__temp2 WC');
        OutStr('  IF_C xor __temp3,#1');

        OutStr('  mov __temp4,#0');
        OutStr('  mov __temp5,#32');
        OutStr('  shr __temp1,#1 WC');
        OutStr(NewLabelStr);
        OutStr('  IF_C add __temp4,__temp2 WC');
        OutStr('  rcr __temp4,#1 WC');
        OutStr('  rcr __temp1,#1 WC');
        If g_bLMM Then
        Begin
          OutStr('  djnz __temp5,#_LMM_JUMP');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  djnz __temp5,#'+LabelStr);
        OutStr('  test __temp3,#1 WZ');
        OutStr('  IF_NZ neg __temp4,__temp4');
        OutStr('  IF_NZ neg __temp1,__temp1 WZ');
        OutStr('  IF_NZ sub __temp4,#1');
}
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
          OutStr('  mov 0-0,__temp1');
        End
        Else OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp1');
        g_bHandled:=True;
      End;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End; { Long '*' }


  Procedure ProcessLetLongVarMultMid;
  Begin { Multiplication Mid */ }
    If g_iCmdCnt = 6 Then
    Begin
      Case g_apoCmdVars[4]^.eGetType of
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
            OutStr('  mov __temp1,0-0');
          End
          Else If Upper(g_apoCmdVars[4]^.sGetName) <> '__TEMP1' Then OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
        End;
        e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetIndexName);
      End;
      Case g_apoCmdVars[6]^.eGetType of
        e_LongConst, e_LongVar:
          If g_apoCmdVars[4]^.m_bVarIndex Then
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
            OutStr('  mov __temp2,0-0');
          End
          Else OutStr('  mov __temp2,'+g_apoCmdVars[6]^.sGetIndexName);
        e_ShortConst: OutStr('  mov __temp2,#'+g_apoCmdVars[6]^.sGetIndexName);
        Else Error(c_iErrorInvalidParameter, 6);
      End;

      g_bUsesMult:=True;
      If g_bLMM Then
      Begin
        OutStr('  jmp #_LMM_CALL');
        OutStr('  long @@@_MULTIPLY');
      End
      Else OutStr('  call #_MULTIPLY');
{
      OutStr('  abs __temp1,__temp1 WC');
      OutStr('  muxc __temp3,#1');
      OutStr('  abs __temp2,__temp2 WC, WZ');
      OutStr('  IF_C xor __temp3,#1');
      OutStr('  mov __temp4,#0');
      OutStr('  mov __temp5,#32');
      OutStr('  shr __temp1,#1 WC');
      OutStr(NewLabelStr);
      OutStr('  IF_C add __temp4,__temp2 WC');
      OutStr('  rcr __temp4,#1 WC');
      OutStr('  rcr __temp1,#1 WC');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp5,#_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  djnz __temp5,#'+LabelStr);
      OutStr('  shr __temp1,#16');
      OutStr('  shl __temp4,#16');
      OutStr('  or __temp1,__temp4');
      OutStr('  test __temp3,#1 WZ');
      OutStr('  IF_NZ neg __temp1,__temp1');
//      OutStr('  IF_NZ neg __temp1,__temp1 WZ');
//     OutStr('  IF_NZ sub __temp4,#1');
}
      OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp2');
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End; { Long '*/' }


  Procedure ProcessLetLongVarMultHigh;
  Begin { Multiplication High ** }
    If g_iCmdCnt = 6 Then
    Begin
      Case g_apoCmdVars[4]^.eGetType of
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
            OutStr('  mov __temp1,0-0');
          End
          Else OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
        End;
        e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetIndexName);
      End;
      Case g_apoCmdVars[6]^.eGetType of
        e_LongConst, e_LongVar:
          If g_apoCmdVars[4]^.m_bVarIndex Then
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
            OutStr('  mov __temp2,0-0');
          End
          Else OutStr('  mov __temp2,'+g_apoCmdVars[6]^.sGetIndexName);
        e_ShortConst: OutStr('  mov __temp2,#'+g_apoCmdVars[6]^.sGetIndexName);
        Else Error(c_iErrorInvalidParameter, 6);
      End;

      g_bUsesMult:=True;
      If g_bLMM Then
      Begin
        OutStr('  jmp #_LMM_CALL');
        OutStr('  long @@@_MULTIPLY');
      End
      Else OutStr('  call #_MULTIPLY');

{
      OutStr('  abs __temp1,__temp1 WC');
      OutStr('  muxc __temp3,#1');
      OutStr('  abs __temp2,__temp2 WC, WZ');
      OutStr('  IF_C xor __temp3,#1');
      OutStr('  mov __temp4,#0');
      OutStr('  mov __temp5,#32');
      OutStr('  shr __temp1,#1 WC');
      OutStr(NewLabelStr);
      OutStr('  IF_C add __temp4,__temp2 WC');
      OutStr('  rcr __temp4,#1 WC');
      OutStr('  rcr __temp1,#1 WC');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp5,#_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  djnz __temp5,#'+LabelStr);
      OutStr('  test __temp3,#1 WZ');
      OutStr('  IF_NZ neg __temp4,__temp4');
      OutStr('  IF_NZ neg __temp1,__temp1 WZ');
      OutStr('  IF_NZ sub __temp4,#1');
}
      OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp4');
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End; { Long '**' }


  Procedure ProcessLetLongVarDiv;
  Var
    lShift: LongInt;
    bShift: Boolean;
    poVar: pVarObj;
  Begin // Division
    If g_iCmdCnt = 6 Then
    Begin
      lShift:=-1;
      bShift:=False;
      If g_apoCmdVars[6]^.eGetType In [e_ShortConst, e_LongConst] Then
      Begin
        Repeat
          Inc(lShift);
          If (1 SHL lShift) = g_apoCmdVars[6]^.lGetValue Then bShift:=True;
        Until (bShift = True) or (lShift = 31);
      End;
      If bShift = True Then
      Begin
        If g_apoCmdVars[4]^.m_bVarIndex Then
        Begin // [4] is var index
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
          OutStr('  mov __remainder,0-0');
          OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__remainder');
          OutStr('  sar '+g_apoCmdVars[2]^.sGetIndexName+',#'+IntStr(lShift));
          If lShift < 10 Then
           OutStr('  and __remainder,#'+IntStr((1 SHL lShift)-1))
          Else
          Begin
            poVar:=g_oDevice.pGetVarPtr('_'+IntStr((1 SHL lShift)-1));
            If poVar = Nil Then g_oDevice.pAddLongConst('_'+IntStr((1 SHL lShift)-1), (1 SHL lShift)-1, IntStr((1 SHL lShift)-1));
            OutStr('  and __remainder,_'+IntStr((1 SHL lShift)-1));
          End;
        End
        Else
        Begin // [4] is not var index
          Case g_apoCmdVars[4]^.eGetType of
            e_ShortConst: OutStr('  mov __remainder,#'+g_apoCmdVars[4]^.sGetName);
            e_LongConst, e_LongVar: OutStr('  mov __remainder,'+g_apoCmdVars[4]^.sGetIndexName);
            Else Error(c_iErrorInvalidParameter, 4);
          End; // Case
          If lShift < 10 Then
           OutStr('  and __remainder,#'+IntStr((1 SHL lShift)-1))
          Else
          Begin
            poVar:=g_oDevice.pGetVarPtr('_'+IntStr((1 SHL lShift)-1));
            If poVar = Nil Then g_oDevice.pAddLongConst('_'+IntStr((1 SHL lShift)-1), (1 SHL lShift)-1, IntStr((1 SHL lShift)-1));
            OutStr('  and __remainder,_'+IntStr((1 SHL lShift)-1));
          End;
          If g_apoCmdVars[2]^.sGetIndexName <> g_apoCmdVars[4]^.sGetIndexName Then
          Begin
            Case g_apoCmdVars[4]^.eGetType of
              e_ShortConst: OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_apoCmdVars[4]^.sGetName);
              e_LongConst, e_LongVar: OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[4]^.sGetIndexName);
              Else Error(c_iErrorInvalidParameter, 4);
            End; // Case
          End;
          OutStr('  sar '+g_apoCmdVars[2]^.sGetIndexName+',#'+IntStr(lShift));
        End;
        g_bHandled:=True;
      End // If bShift = True
      Else
      Begin
        Case g_apoCmdVars[4]^.eGetType of
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
              OutStr('  mov __temp1,0-0');
            End
            Else OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
          End;
          e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetIndexName);
          Else Error(c_iErrorInvalidParameter, 4);
        End;
        Case g_apoCmdVars[6]^.eGetType of
          e_LongConst, e_LongVar:
            If g_apoCmdVars[4]^.m_bVarIndex Then
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
              OutStr('  mov __temp2,0-0');
            End
            Else OutStr('  mov __temp2,'+g_apoCmdVars[6]^.sGetIndexName);
          e_ShortConst: OutStr('  mov __temp2,#'+g_apoCmdVars[6]^.sGetIndexName);
          Else Error(c_iErrorInvalidParameter, 6);
        End; // Case

        If g_bLMM Then
        Begin
          OutStr('  jmp #_LMM_CALL');
          OutStr('  long @@@_DIV');
        End
        Else OutStr('  call #_DIV');
        g_bUsesDiv:=True;

{
        If (g_apoCmdVars[6]^.eGetType In [e_ShortConst, e_LongConst]) and (g_apoCmdVars[6]^.lGetValue = 0) Then Error(c_iErrorInvalidParameter, 6);
        OutStr('  mov __temp1,#0'); // Result
        OutStr('  abs __temp4,__temp4 WC');
        OutStr('  muxc __temp5,#1');
        OutStr('  abs __temp2,__temp2 WC, WZ');
        If (g_apoCmdVars[6]^.eGetType = e_LongVar) or (g_apoCmdVars[6]^.lGetValue = c_lComputedConstantValue) Then
        Begin
          OutStr('  IF_Z mov __temp4,#0');
          sDivZeroLabel:=NewLabelStr;
          If g_bLMM Then
          Begin
            OutStr('  IF_Z jmp #_LMM_JUMP');
            OutStr('  long @@@'+sDivZeroLabel);
          End
          Else OutStr('  IF_Z jmp #'+sDivZeroLabel);
        End;
        OutStr('  IF_C xor __temp5,#1');
//a        OutStr('  mov __temp3,#0'); // Bit Counter [changed method]
        OutStr('  mov __temp3,#33'); // Bit Counter
//a        OutStr('  min __temp2,#1'); // Prevent lockup for divide by zero [Not needed, previous code jumps to end if divisor is zero]
//a        OutStr(NewLabelStr);
//a        OutStr('  add __temp3,#1');
//a        OutStr('  shl __temp2,#1 WC');
//a        If g_bLMM Then
//a        Begin
//a          OutStr('  IF_NC jmp #_LMM_JUMP');
//a          OutStr('  long @@@'+LabelStr);
//a        End
//a        Else OutStr('  IF_NC jmp #'+LabelStr);
//a        OutStr('  rcr __temp2,#1');
        OutStr(NewLabelStr);
//a        OutStr('  cmpsub __temp1,__temp2 WC');
        OutStr('  cmpsub __temp1,__temp2 WC');
//a        OutStr('  rcl __temp4,#1');
        OutStr('  rcl __temp4,#1 WC');
//a        OutStr('  shr __temp2,#1');
        OutStr('  rcl __temp1,#1');
        If g_bLMM Then
        Begin
          OutStr('  djnz __temp3,#_LMM_JUMP');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  djnz __temp3,#'+LabelStr);
        OutStr('  shr __temp1,#1'); // fix remainder (new method)

        OutStr('  test __temp5,#1 WZ');
        OutStr('  IF_NZ neg __temp4,__temp4');
        OutStr('  IF_NZ neg __temp1,__temp1');
        If (g_apoCmdVars[6]^.eGetType = e_LongVar) or (g_apoCmdVars[6]^.lGetValue = c_lComputedConstantValue) Then OutStr(sDivZeroLabel);
}
        If Upper(g_apoCmdVars[2]^.sGetIndexName) <> '__TEMP1' Then
         OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp1');
        g_bHandled:=True;
      End;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End; // Long '/'


  Procedure ProcessLetLongVarRem;
  Begin { Remainder }
    If g_iCmdCnt = 6 Then
    Begin
      Case g_apoCmdVars[4]^.eGetType of
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
            OutStr('  mov __temp1,0-0');
          End
          Else If Upper(g_apoCmdVars[4]^.sGetName) <> '__TEMP1' Then OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
        End;
        e_ShortConst: OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetIndexName);
      End;
      Case g_apoCmdVars[6]^.eGetType of
        e_LongConst, e_LongVar:
          If g_apoCmdVars[4]^.m_bVarIndex Then
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
            OutStr('  mov __temp2,0-0');
          End
          Else OutStr('  mov __temp2,'+g_apoCmdVars[6]^.sGetIndexName);
        e_ShortConst: OutStr('  mov __temp2,#'+g_apoCmdVars[6]^.sGetIndexName);
        Else Error(c_iErrorInvalidParameter, 6);
      End;

      If g_bLMM Then
      Begin
        OutStr('  jmp #_LMM_CALL');
        OutStr('  long @@@_DIV');
      End
      Else OutStr('  call #_DIV');
      g_bUsesDiv:=True;

      {
      OutStr('  mov __temp1,#0'); // Result
      OutStr('  abs __temp4,__temp4 WC');
      OutStr('  muxc __temp5,#1');
      OutStr('  abs __temp2,__temp2 WC, WZ');
      If (g_apoCmdVars[6]^.eGetType = e_LongVar) or (g_apoCmdVars[6]^.lGetValue = c_lComputedConstantValue) Then
      Begin
        OutStr('  IF_Z mov __temp4,#0');
        sDivZeroLabel:=NewLabelStr;
        If g_bLMM Then
        Begin
          OutStr('  IF_Z jmp #_LMM_JUMP');
          OutStr('  long @@@'+sDivZeroLabel);
        End
        Else OutStr('  IF_Z jmp #'+sDivZeroLabel);
      End;
      OutStr('  IF_C xor __temp5,#1');
//a        OutStr('  mov __temp3,#0'); // Bit Counter [changed method]
      OutStr('  mov __temp3,#33'); // Bit Counter
//a        OutStr('  min __temp2,#1'); // Prevent lockup for divide by zero [Not needed, previous code jumps to end if divisor is zero]
//a        OutStr(NewLabelStr);
//a        OutStr('  add __temp3,#1');
//a        OutStr('  shl __temp2,#1 WC');
//a        If g_bLMM Then
//a        Begin
//a          OutStr('  IF_NC jmp #_LMM_JUMP');
//a          OutStr('  long @@@'+LabelStr);
//a        End
//a        Else OutStr('  IF_NC jmp #'+LabelStr);
//a        OutStr('  rcr __temp2,#1');
      OutStr(NewLabelStr);
//a        OutStr('  cmpsub __temp1,__temp2 WC');
      OutStr('  cmpsub __temp1,__temp2 WC');
//a        OutStr('  rcl __temp4,#1');
      OutStr('  rcl __temp4,#1 WC');
//a        OutStr('  shr __temp2,#1');
      OutStr('  rcl __temp1,#1');
      If g_bLMM Then
      Begin
        OutStr('  djnz __temp3,#_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  djnz __temp3,#'+LabelStr);
      OutStr('  shr __temp1,#1'); // fix remainder (new method)

      OutStr('  test __temp5,#1 WZ');
      OutStr('  IF_NZ neg __temp4,__temp4');
      OutStr('  IF_NZ neg __temp1,__temp1');

      If (g_apoCmdVars[6]^.eGetType = e_LongVar) or (g_apoCmdVars[6]^.lGetValue = c_lComputedConstantValue) Then OutStr(sDivZeroLabel);

}
      If g_apoCmdVars[2]^.sGetIndexName <> '__temp4' Then
       OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp4');
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End; { Long '//' }


  Procedure ProcessLetLongVarSgn;
  Begin { 'SGN' }
    If g_iCmdCnt = 5 Then
    Begin
      If g_oDevice.pGetVarPtr('_minus1') = Nil Then g_oDevice.pAddLongConst('_minus1', -1, '-1');
      Case g_apoCmdVars[5]^.eGetType of
        e_LongVar, e_LongConst:
          Begin
            If g_apoCmdVars[2]^.m_bVarIndex = False Then
            Begin // dest does NOT have a var index
              If g_apoCmdVars[5]^.m_bVarIndex = False Then
              Begin // dest and source do not have var index
                OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[5]^.sGetIndexName);
                OutStr('  maxs '+g_apoCmdVars[2]^.sGetIndexName+',#1');
                OutStr('  mins '+g_apoCmdVars[2]^.sGetIndexName+',_minus1');
              End
              Else
              Begin // dest does NOT have var index, source DOES have var index
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
                OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
                OutStr('  maxs '+g_apoCmdVars[2]^.sGetIndexName+',#1');
                OutStr('  mins '+g_apoCmdVars[2]^.sGetIndexName+',_minus1');
              End;
            End
            Else
            Begin // dest DOES have var index
              If g_apoCmdVars[5]^.m_bVarIndex = False Then
              Begin // dest is var index, source is NOT var index
                OutStr('  mov __temp1,'+g_apoCmdVars[5]^.sGetIndexName);
                OutStr('  maxs __temp1,#1');
                OutStr('  mins __temp1,_minus1');
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
                OutStr('  mov 0-0,__temp1');
              End
              Else
              Begin // dest is var index, source is var index
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
                OutStr('  maxs __temp1,#1');
                OutStr('  mins __temp1,_minus1');
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
                OutStr('  mov 0-0,__temp1');
              End;
            End;
            g_bHandled:=True;
          End;
        Else Error(c_iErrorInvalidParameter, 5);
      End; // Case
    End;
  End; { Long 'SGN' }


  Procedure ProcessLetLongVarAbs;
  Begin { 'ABS' }
    If g_iCmdCnt = 5 Then
    Begin
      Case g_apoCmdVars[5]^.eGetType of
        e_LongVar, e_LongConst:
          Begin
            If g_apoCmdVars[2]^.m_bVarIndex = False Then
            Begin // dest does NOT have a var index
              If g_apoCmdVars[5]^.m_bVarIndex = False Then
              Begin // dest and source do not have var index
                OutStr('  abs '+g_apoCmdVars[2]^.sGetIndexName+','+g_apoCmdVars[5]^.sGetIndexName);
              End
              Else
              Begin // dest does NOT have var index, source DOES have var index
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
                OutStr('  abs '+g_apoCmdVars[2]^.sGetIndexName+',0-0');
              End;
            End
            Else
            Begin // dest DOES have var index
              If g_apoCmdVars[5]^.m_bVarIndex = False Then
              Begin // dest is var index, source is NOT var index
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
                OutStr('  abs 0-0,'+g_apoCmdVars[5]^.sGetIndexName);
              End
              Else
              Begin // dest is var index, source is var index
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
                OutStr('  abs 0-0,__temp1');
              End;
            End;
            g_bHandled:=True;
          End;
        Else Error(c_iErrorInvalidParameter, 5);
      End; // Case
    End;
  End; { Long 'ABS' }


Procedure ProcessLetLongVarLen; // longvar = LEN string
Begin // LEN
  ProcessHubAddrLit(5, '__temp1', 1);
  OutStr('  mov __temp2,__temp1');
  OutStr(NewLabelStr);
  OutStr('  rdbyte __temp3,__temp1 WZ');
  OutStr('  IF_NZ add __temp1,#1');
  If g_bLMM Then
  Begin
    OutStr('  IF_NZ jmp #_LMM_JUMP');
    OutStr('  long @@@'+LabelStr);
  End
  Else OutStr('  IF_NZ jmp #'+LabelStr);
  OutStr('  sub __temp1,__temp2');
  If g_apoCmdVars[2]^.eGetType = e_LongVar Then
  Begin
    If g_apoCmdVars[2]^.m_bVarIndex = False Then
    Begin
      OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp1');
    End
    Else
    Begin // Index is a var
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
      OutStr('  mov 0-0,__temp1');
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidParameter, 2);
End; // LEN


Procedure ProcessLetLongVarVal; // longvar = VAL string
Begin
  ProcessHubAddrLit(5, '__temp1', 1);
  // Get first character
  OutStr('  rdbyte __temp4,__temp1 WZ'); // __temp2 holds "-" if number is negative
  OutStr('  mov __temp2,__temp4');
  OutStr('  mov __temp3,#0');

  OutStr(NewLabelStr);
  OutStr('  min __temp4,#"0"');
  OutStr('  sub __temp4,#"0"');
  OutStr('  add __temp1,#1'); // Move to next char in string

  // Mult by 10 (if a zero was NOT read)
  OutStr('  IF_NZ shl __temp3,#1'); // * 2
  OutStr('  IF_NZ mov __temp5,__temp3');
  OutStr('  IF_NZ shl __temp3,#2'); // * 8
  OutStr('  IF_NZ add __temp3,__temp5');

  // Add new character (if it's not zero)
  OutStr('  IF_NZ add __temp3,__temp4');
  // Read next character (if first char was not zero)
  OutStr('  IF_NZ rdbyte __temp4,__temp1 WZ');
  If g_bLMM Then
  Begin
    OutStr('  IF_NZ jmp #_LMM_JUMP');
    OutStr('  long @@@'+LabelStr);
  End
  Else OutStr('  IF_NZ jmp #'+LabelStr);
  OutStr('  cmp __temp2,#"-" WZ');
  OutStr('  IF_E neg __temp3,__temp3');
  If g_apoCmdVars[2]^.eGetType = e_LongVar Then
  Begin
    If g_apoCmdVars[2]^.m_bVarIndex = False Then
    Begin
      OutStr('  mov '+g_apoCmdVars[2]^.sGetIndexName+',__temp3');
    End
    Else
    Begin // Index is a var
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
      OutStr('  mov 0-0,__temp3');
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidParameter, 2);
End; // VAL


Procedure ProcessLetLongVarGetAddr; // longvar = GetAddr string
Begin
  If g_apoCmdVars[2]^.m_bVarIndex Then
  Begin
    Case g_apoCmdVars[5]^.eGetType of
      e_HubByte, e_DataLabel: ProcessHubAddrLit(5, '__temp1', 1);
      e_HubWord: ProcessHubAddrLit(5, '__temp1', 2);
      e_HubLong: ProcessHubAddrLit(5, '__temp1', 4);
      Else Error(c_iErrorInvalidParameter, 5);
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
    OutStr('  mov 0-0,__temp1');
  End
  Else
  Begin
    Case g_apoCmdVars[5]^.eGetType of
      e_HubByte, e_DataLabel: ProcessHubAddrLit(5, g_apoCmdVars[2]^.sGetIndexName, 1);
      e_HubWord: ProcessHubAddrLit(5, g_apoCmdVars[2]^.sGetIndexName, 2);
      e_HubLong: ProcessHubAddrLit(5, g_apoCmdVars[2]^.sGetIndexName, 4);
      Else Error(c_iErrorInvalidParameter, 5);
    End;
  End;
  g_bHandled:=True;
End; // GetAddr


Begin { ProcessLetLong }
  iSubroutinePlace:=1;
  While (iSubroutinePlace < g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel <> Upper(g_asCmdLine[4])) Do Inc(iSubroutinePlace);
  If (iSubroutinePlace <= g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel = Upper(g_asCmdLine[4])) Then
  Begin
    If g_arSubroutines[iSubroutinePlace].bIsFunc Then
    Begin
      ProcessLetLongFunction;
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End
  Else
  Begin
    If g_iCmdCnt = 4 Then
    Begin
      ProcessLetLongAssign;
      VP_WriteVar(2);
    End
    Else If (g_iCmdCnt = 5) or ((g_iCmdCnt = 8) and (g_asCmdLine[6] = '(') and (g_asCmdLine[8] = ')')) Then
    Begin
      If Upper(g_asCmdLine[4]) = 'ABS' Then ProcessLetLongVarAbs
      Else If Upper(g_asCmdLine[4]) = 'SGN' Then ProcessLetLongVarSgn
      Else If Upper(g_asCmdLine[4]) = 'LEN' Then ProcessLetLongVarLen
      Else If Upper(g_asCmdLine[4]) = 'VAL' Then ProcessLetLongVarVal
      Else If Upper(g_asCmdLine[4]) = 'GETADDR' Then ProcessLetLongVarGetAddr
      Else Error(c_iErrorSyntaxError, 4);
    End
    Else If g_iCmdCnt = 6 Then
    Begin
      If (g_asCmdLine[5] ='+') or (g_asCmdLine[5] = '&') or (Upper(g_asCmdLine[5]) = 'AND') or
           (g_asCmdLine[5] = '|') or (Upper(g_asCmdLine[5]) = 'OR') or(g_asCmdLine[5] = '^') or
           (Upper(g_asCmdLine[5]) = 'XOR') or (g_asCmdLine[5] = '*') or
           (g_asCmdLine[5] = '**') or (g_asCmdLine[5] = '*/') Then
      Begin
        If (g_apoCmdVars[4]^.eGetType = e_ShortConst) and
           (g_apoCmdVars[6]^.eGetType <> e_ShortConst) Then
        Begin { Make constant last A=5+B -> A=B+5 }
          poTempVar:=g_apoCmdVars[4];
          g_apoCmdVars[4]:=g_apoCmdVars[6];
          g_apoCmdVars[6]:=poTempVar;
          sTemp:=g_asCmdLine[4];
          g_asCmdLine[4]:=g_asCmdLine[6];
          g_asCmdLine[6]:=sTemp;
          bTemp:=g_abDeleteTemp[4];
          g_abDeleteTemp[4]:=g_abDeleteTemp[6];
          g_abDeleteTemp[6]:=bTemp;
        End;
        { If 2nd variable is same as dest then make it the first variable }
        { A=B+A -> A=A+B }
        If (g_apoCmdVars[2]^.m_sAlias = g_apoCmdVars[6]^.m_sAlias) and (g_apoCmdVars[2]^.m_sIndex = g_apoCmdVars[6]^.m_sIndex) Then
        Begin
          poTempVar:=g_apoCmdVars[4];
          g_apoCmdVars[4]:=g_apoCmdVars[6];
          g_apoCmdVars[6]:=poTempVar;
          sTemp:=g_asCmdLine[4];
          g_asCmdLine[4]:=g_asCmdLine[6];
          g_asCmdLine[6]:=sTemp;
          bTemp:=g_abDeleteTemp[4];
          g_abDeleteTemp[4]:=g_abDeleteTemp[6];
          g_abDeleteTemp[6]:=bTemp;
        End;
      End;

      // If a VarIndex array element is the dest, use __temp4 instead
      bDestArray:=False;
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin
        bDestArray:=True;
        g_apoCmdVars[c_iMaxCmdCnt]:=g_apoCmdVars[2];
        g_apoCmdVars[2]:=g_oDevice.pGetVarPtr('__temp4');
      End;

      If (g_asCmdLine[5,1] <> '*') and (g_asCmdLine[5,1] <> '/'){ and (g_asCmdLine[5,1] <> ',')} Then
      Begin
        ProcessLetLongAssign;
        g_bHandled:=False;
      End;

      If g_asCmdLine[5] = '*' Then ProcessLetLongVarMult;
      If g_asCmdLine[5] = '*/' Then ProcessLetLongVarMultMid;
      If g_asCmdLine[5] = '**' Then ProcessLetLongVarMultHigh;
      If g_asCmdLine[5] = '/' Then ProcessLetLongVarDiv;
      If g_asCmdLine[5] = '//' Then ProcessLetLongVarRem;

      If g_asCmdLine[5] = '+' Then ProcessLetLongVarPlus;
      If g_asCmdLine[5] = '-' Then ProcessLetLongVarSub;
      If (g_asCmdLine[5] = '&') or (Upper(g_asCmdLine[5]) = 'AND') Then ProcessLetLongVarAND;
      If (g_asCmdLine[5] = '|') or (Upper(g_asCmdLine[5]) = 'OR') Then ProcessLetLongVarOR;
      If (g_asCmdLine[5] = '^') or (Upper(g_asCmdLine[5]) = 'XOR') Then ProcessLetLongVarXOR;
      If Upper(g_asCmdLine[5]) = 'MIN' Then ProcessLetLongVarMIN;
      If Upper(g_asCmdLine[5]) = 'MAX' Then ProcessLetLongVarMAX;
      If (g_asCmdLine[5] = '<<') or (Upper(g_asCmdLine[5]) = 'SHL') Then ProcessLetLongVarSHL;
      If (g_asCmdLine[5] = '>>') or (Upper(g_asCmdLine[5]) = 'SHR') Then ProcessLetLongVarSHR;
      If (g_asCmdLine[5] = '&~') or (Upper(g_asCmdLine[5]) = 'ANDN') Then ProcessLetLongVarAndN;
      If Not g_bHandled Then Error(c_iErrorInvalidParameter, 5);

      If bDestArray Then
      Begin
        g_apoCmdVars[2]:=g_apoCmdVars[c_iMaxCmdCnt];
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
        OutStr('  mov 0-0,__temp4');
      End;

      VP_WriteVar(2);

    End { count = 6 }


    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End;
End; { ProcessLetLongVar }

End.
