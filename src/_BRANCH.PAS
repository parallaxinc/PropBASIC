Unit _BRANCH;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessBranch; // BRANCH var, label{,label{,etc}}
Procedure ProcessOn; // ON var GOTO|GOSUB|{= value, value} label{,label{,etc}}

Implementation

Procedure ProcessBranch;
Var
  I: Integer;
Begin
  If g_iCmdCnt > 5 Then
  Begin
    If Odd(g_iCmdCnt) Then Error(c_iErrorInvalidNumberOfParameters, 0);
    Case g_apoCmdVars[2]^.eGetType of
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
          OutStr('  mov __temp1,0-0');
        End
        Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
      End
      Else Error(c_iErrorLongVariableExpected, 2);
    End;
    For I:=2 to (g_iCmdCnt Div 2) Do
    Begin
      If g_asCmdLine[I*2-1] <> ',' Then Error(c_iErrorCommaExpected, I*2-1);
      If g_bLMM Then
      Begin
        OutStr('  tjz __temp1,#_LMM_JUMP');
        OutStr('  long @@@'+g_asCmdLine[I*2]);
      End
      Else OutStr('  tjz __temp1,#'+g_asCmdLine[I*2]);
      If I <> (g_iCmdCnt Div 2) Then OutStr('  subs __temp1,#1');
    End; // For I
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessOn;
// ON var GOTO|GOSUB label0, label1, etc
// ON var = value0, value1, value2 GOTO|GOSUB label0, label1, etc
Var
  I: Integer;
  Place: Integer;
  sName, sNextLabel, sDoneLabel: String;
Begin
  If g_asCmdLine[3] = '=' Then
  Begin
    Place:=0;
    For I:=5 to g_iCmdCnt Do
    Begin
      If Upper(g_asCmdLine[I]) = 'GOTO' Then Place:=I;
      If Upper(g_asCmdLine[I]) = 'GOSUB' Then Place:=I;
    End;
    If Place <> 0 Then
    Begin
      If Upper(g_asCmdLine[Place]) = 'GOSUB' Then sDoneLabel:=NewLabelStr;
      If g_apoCmdVars[2]^.eGetType <> e_LongVar Then Error(c_iErrorLongVariableExpected, 2);
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
        sName:='__temp1';
      End
      Else sName:=g_apoCmdVars[2]^.sGetIndexName;
      For I:=2 to (Place Div 2) Do
      Begin
        If (I > 2) and (g_asCmdLine[I*2-1] <> ',') Then Error(c_iErrorCommaExpected, I*2-1);
        Case g_apoCmdVars[I*2]^.eGetType of
          e_LongVar, e_LongConst:
            Begin
              If g_apoCmdVars[I*2]^.m_bVarIndex Then
              Begin
                If g_bLMM Then
                Begin
                  OutStr('  mov __INDEX,#'+g_apoCmdVars[I*2]^.sGetName);
                  OutStr('  add __INDEX,'+g_apoCmdVars[I*2]^.m_sIndex);
                  OutStr('  jmp #_LMM_MOVS');
                End
                Else
                Begin
                  OutStr('  add '+g_apoCmdVars[I*2]^.m_sIndex+',#'+g_apoCmdVars[I*2]^.sGetName);
                  OutStr('  movs $+2,'+g_apoCmdVars[I*2]^.m_sIndex);
                  OutStr('  sub '+g_apoCmdVars[I*2]^.m_sIndex+',#'+g_apoCmdVars[I*2]^.sGetName);
                End;
                OutStr('  cmp '+sName+',0-0 WZ');
              End
              Else OutStr('  cmps '+sName+','+g_apoCmdVars[I*2]^.sGetIndexName+' WZ');
            End;
          e_ShortConst: OutStr('  cmps '+sName+',#'+g_apoCmdVars[I*2]^.sGetIndexName+' WZ');
          Else Error(c_iErrorInvalidParameter, I*2);
        End; // Case
        If (I > 2) and (g_asCmdLine[Place+I*2-4] <> ',') Then Error(c_iErrorCommaExpected, Place+I*2-4);
        If Upper(g_asCmdLine[Place]) = 'GOTO' Then
        Begin
          If g_bLMM Then
          Begin
            OutStr('  IF_E rdlong __PC,__PC');
            OutStr('  long @@@'+g_asCmdLine[Place+I*2-3]);
          End
          Else OutStr('  IF_E jmp #'+g_asCmdLine[Place+I*2-3])
        End
        Else If Upper(g_asCmdLine[Place]) = 'GOSUB' Then
        Begin
          If g_bLMM Then
          Begin
            sNextLabel:=NewLabelStr;
            OutStr('  IF_NE rdlong __PC,__PC');
            OutStr('  long @@@'+sNextLabel);
            OutStr('  jmp #_LMM_CALL');
//            OutStr('  long @@@'+g_asCmdLine[Place+I*2-3]+'_ret');
            OutStr('  long @@@'+g_asCmdLine[Place+I*2-3]);
            OutStr('  rdlong __PC,__PC');
            OutStr('  long @@@'+sDoneLabel);
            OutStr(sNextLabel);
          End
          Else
          Begin
            OutStr('  IF_E movd '+sDoneLabel+',#'+g_asCmdLine[Place+I*2-3]+'_ret');
            OutStr('  IF_E movs '+sDoneLabel+',#'+g_asCmdLine[Place+I*2-3]);
            OutStr('  IF_E jmp #'+sDoneLabel);
          End;
        End
        Else Error(c_iErrorSyntaxError, Place);
      End; // For
      OutStr(sDoneLabel);
      If Not g_bLMM Then OutStr('  IF_E  jmpret 0-0,#0-0');
      g_bHandled:=True;
    End
    Else Error(c_iErrorSyntaxError, 0);
  End
  Else If Upper(g_asCmdLine[3]) = 'GOTO' Then
  Begin
    If g_iCmdCnt > 5 Then
    Begin
      If Odd(g_iCmdCnt) Then Error(c_iErrorInvalidNumberOfParameters, 0);
      Case g_apoCmdVars[2]^.eGetType of
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
              OutStr('  mov __temp1,0-0');
            End
            Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        Else Error(c_iErrorLongVariableExpected, 2);
      End;
      For I:=2 to (g_iCmdCnt Div 2) Do
      Begin
        If (I > 2) and (g_asCmdLine[I*2-1] <> ',') Then Error(c_iErrorCommaExpected, I*2-1);
        If g_bLMM Then
        Begin
          OutStr('  tjz __temp1,#_LMM_JUMP');
          OutStr('  long @@@'+g_asCmdLine[I*2]);
        End
        Else OutStr('  tjz __temp1,#'+g_asCmdLine[I*2]);
        If I <> (g_iCmdCnt Div 2) Then OutStr('  subs __temp1,#1');
      End; // For I
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End
  Else If Upper(g_asCmdLine[3]) = 'GOSUB' Then
  Begin
    sDoneLabel:=NewLabelStr;
    If g_iCmdCnt > 5 Then
    Begin
      If Odd(g_iCmdCnt) Then Error(c_iErrorInvalidNumberOfParameters, 0);
      Case g_apoCmdVars[2]^.eGetType of
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
              OutStr('  mov __temp1,0-0');
            End
            Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          End;
        Else Error(c_iErrorLongVariableExpected, 2);
      End; // Case
      OutStr('  cmp __temp1,#0 WZ');
      For I:=2 to (g_iCmdCnt Div 2) Do
      Begin
        If (I > 2) and (g_asCmdLine[I*2-1] <> ',') Then Error(c_iErrorCommaExpected, I*2-1);

        If g_bLMM Then
        Begin
          sNextLabel:=NewLabelStr;
          OutStr('  IF_NE rdlong __PC,__PC');
          OutStr('  long @@@'+sNextLabel);
          OutStr('  jmp #_LMM_CALL');
//          OutStr('  long @@@'+g_asCmdLine[I*2]+'_ret'); // [Place+I*2]
          OutStr('  long @@@'+g_asCmdLine[I*2]); // [Place+I*2]
          OutStr('  rdlong __PC,__PC');
          OutStr('  long @@@'+sDoneLabel);
          OutStr(sNextLabel);
        End
        Else
        Begin
          OutStr('  IF_E movd '+sDoneLabel+',#'+g_asCmdLine[I*2]+'_ret'); // [Place+I*2]
          OutStr('  IF_E movs '+sDoneLabel+',#'+g_asCmdLine[I*2]); // [Place+I*2]
          OutStr('  IF_E jmp #'+sDoneLabel);
        End;

        If I <> (g_iCmdCnt Div 2) Then OutStr('  cmp __temp1,#'+IntStr(I-1)+' WZ');
      End; // For I
      OutStr(sDoneLabel);
      If Not g_bLMM Then OutStr('  IF_E  jmpret 0-0,#0-0');
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End
  Else Error(c_iErrorSyntaxError, 3);
End;

End.
