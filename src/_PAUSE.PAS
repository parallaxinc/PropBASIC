Unit _PAUSE;

{$MODE Delphi}

 // LMM Index

Interface

Uses GLOBAL;

Procedure ProcessPause;
Procedure ProcessPauseUS;

Implementation

Procedure ProcessPause;
Var
  dTemp: Double;
  iError: Integer;
  lTemp: LongInt;
  poLongConst: PVarObj;
  iWaitCnt, iShift, iValue: Integer;
Begin
  g_bUsesPause:=True;
  iWaitCnt:=g_lFreq Div 1000;
  iShift:=0;
  While iWaitCnt < g_iMinWaitCnt Do
  Begin
    iWaitCnt:=iWaitCnt * 2;
    Inc(iShift);
  End;
  poLongConst:=g_oDevice.pGetVarPtr('_'+IntStr(1 SHL iShift)+'mSec');
  If poLongConst = Nil Then poLongConst:=g_oDevice.pAddLongConst('_'+IntStr(1 SHL iShift)+'mSec', iWaitCnt, IntStr(iWaitCnt));
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst:
        Begin
          iValue:=g_apoCmdVars[2]^.lGetValue SHR iShift;
          If iValue > 0 Then
          Begin
            OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName);
//            If iShift = 0 Then OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName)
//             Else OutStr('  mov __temp1,#'+g_apoCmdVars[2]^.sGetName+' >> '+IntStr(iShift));
//            OutStr('  mov __temp1,'+poLongConst^.sGetName);
//            OutStr('  add __temp1,cnt');
//            OutStr(NewLabelStr);
//            OutStr('  waitcnt __temp1,'+poLongConst^.sGetName);
            If g_bLMM Then
            Begin
//              OutStr('  djnz __temp2,#_LMM_JUMP');
//              OutStr('  long @@@'+LabelStr);
              OutStr('  jmp #_LMM_CALL');
              OutStr('  long @@@_PAUSE');
            End
//            Else OutStr('  djnz __temp2,#'+LabelStr);
            Else OutStr('  call #_PAUSE');
          End
          Else OutStr('  NOP');
        End;
      e_LongConst:
        Begin
          iValue:=g_apoCmdVars[2]^.lGetValue SHR iShift;
          If iValue > 0 Then
          Begin
//            If iValue < 512 Then
//            Begin // Do as a short constant
//              If iShift = 0 Then OutStr('  mov __temp2,#'+g_apoCmdVars[2]^.sGetName)
//               Else OutStr('  mov __temp2,#'+IntStr(g_apoCmdVars[2]^.lGetValue)+' >> '+IntStr(iShift));
//              OutStr('  mov __temp1,'+poLongConst^.sGetName);
//              OutStr('  add __temp1,cnt');
//              OutStr(NewLabelStr);
//              OutStr('  waitcnt __temp1,'+poLongConst^.sGetName);
//              If g_bLMM Then
//              Begin
//                OutStr('  djnz __temp2,#_LMM_JUMP');
//                OutStr('  long @@@'+LabelStr);
//              End
//              Else OutStr('  djnz __temp2,#'+LabelStr);
//            End
//            Else
//            Begin
            OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
//              If iShift > 0 Then OutStr('  shr __temp2,#'+IntStr(iShift));
//              OutStr('  mov __temp1,'+poLongConst^.sGetName);
//              OutStr('  add __temp1,cnt');
//              OutStr(NewLabelStr);
//              OutStr('  waitcnt __temp1,'+poLongConst^.sGetName);
            If g_bLMM Then
            Begin
              OutStr('  jmp #_LMM_CALL');
              OutStr('  long @@@_PAUSE');
//              OutStr('  djnz __temp2,#_LMM_JUMP');
//              OutStr('  long @@@'+LabelStr);
            End
            Else OutStr('  call #_PAUSE'); //OutStr('  djnz __temp2,#'+LabelStr);
          End
          Else OutStr('  NOP');
        End;
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
          Else OutStr('  mov __temp1,'+g_apoCmdVars[2]^.sGetName);
//          If iShift > 0 Then OutStr('  shr __temp2,#'+IntStr(iShift));
//          OutStr('  mins __temp2,#1 WC');
//          OutStr('  mov __temp1,'+poLongConst^.sGetName);
//          OutStr('  add __temp1,cnt');
//          OutStr(NewLabelStr);
//          OutStr('  IF_NC waitcnt __temp1,'+poLongConst^.sGetName);
          If g_bLMM Then
          Begin
            OutStr('  jmp #_LMM_CALL');
            OutStr('  long @@@_PAUSE');
//            OutStr('  IF_NC djnz __temp2,#_LMM_JUMP');
//            OutStr('  long @@@'+LabelStr);
          End
          Else OutStr('  call #_PAUSE'); //OutStr('  IF_NC djnz __temp2,#'+LabelStr);
        End;
      Else Error(c_iErrorInvalidParameter, 2);  
    End; // Case
    g_bHandled:=True;
  End
  Else If (g_iCmdCnt = 4) and (g_asCmdLine[3] = '.') and (g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst]) and
    (g_apoCmdVars[4]^.eGetType In [e_ShortConst, e_LongConst]) Then
  Begin // PAUSE 4.7
    Val(g_asCmdLine[2]+'.'+g_asCmdLine[4], dTemp, iError);
    If iError <> 0 Then Error(c_iErrorSyntaxError, 0);
    dTemp:=dTemp * g_lFreq;
    dTemp:=dTemp / 1000;
    lTemp:=Trunc(dTemp);
    If lTemp < g_iMinWaitCnt Then lTemp:=g_iMinWaitCnt;
    If lTemp > 511 Then
    Begin
      If g_oDevice.pGetVarPtr('_'+IntStr(lTemp)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lTemp), lTemp, IntStr(lTemp));
      OutStr('  mov __temp1,_'+IntStr(lTemp));
    End
    Else OutStr('  mov __temp1,#'+IntStr(lTemp));
    OutStr('  add __temp1,cnt');
    OutStr('  waitcnt __temp1,#0');
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessPauseUS;
Var
  dTemp: Double;
  iError: Integer;
  lTemp: LongInt;
  iWaitCnt, iShift: Integer;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    Case g_apoCmdVars[2]^.eGetType of
      e_ShortConst, e_LongConst:
        Begin
          dTemp:=g_lFreq;
          dTemp:=dTemp / 1000000.0;
          dTemp:=dTemp * g_apoCmdVars[2]^.lGetValue;
          lTemp:=Trunc(dTemp);
          If lTemp < 512 Then
          Begin
            If lTemp < g_iMinWaitCnt Then
            Begin
              If g_bLMM Then lTemp:=lTemp Div 20 Else lTemp:=lTemp Div 4;
              If lTemp = 0 Then OutStr('  NOP'); // Output at least one NOP
              For iShift:=1 to lTemp Do OutStr('  NOP');
            End
            Else
            Begin
              OutStr('  mov __temp1,#'+IntStr(lTemp));
              OutStr('  add __temp1,cnt');
              OutStr('  waitcnt __temp1,#0');
            End;  
            g_bHandled:=True;
          End
          Else
          Begin
            If g_oDevice.pGetVarPtr('_'+IntStr(lTemp)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lTemp), lTemp, IntStr(lTemp));
            OutStr('  mov __temp1,_'+IntStr(lTemp));
            OutStr('  add __temp1,cnt');
            OutStr('  waitcnt __temp1,#0');
            g_bHandled:=True;
          End
        End;
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
            OutStr('  mov __temp2,0-0');
          End
          Else OutStr('  mov __temp2,'+g_apoCmdVars[2]^.sGetIndexName);

          iWaitCnt:=g_lFreq Div 1000000;
          iShift:=0;
          While iWaitCnt < g_iMinWaitCnt Do
          Begin
            Inc(iShift);
            iWaitCnt:=g_lFreq Div (1000000 SHR iShift);
          End;
          If iShift > 0 Then OutStr('  shr __temp2,#'+IntStr(iShift));
          OutStr('  mins __temp2,#1 WC');
          OutStr('  mov __temp1,#'+IntStr(iWaitCnt));
          OutStr('  add __temp1,cnt');
          OutStr(NewLabelStr);
          OutStr('  IF_NC waitcnt __temp1,#'+IntStr(iWaitCnt));
          If g_bLMM Then
          Begin
            OutStr('  IF_NC djnz __temp2,#_LMM_JUMP');
            OutStr('  long @@@'+LabelStr);
          End
          Else OutStr('  IF_NC djnz __temp2,#'+LabelStr);
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End; // Case
    g_bHandled:=True;
  End
  Else If (g_iCmdCnt = 4) and (g_asCmdLine[3] = '.') and (g_apoCmdVars[2]^.eGetType In [e_ShortConst, e_LongConst]) and
    (g_apoCmdVars[4]^.eGetType In [e_ShortConst, e_LongConst]) Then
  Begin // PAUSEUS 4.7
    Val(g_asCmdLine[2]+'.'+g_asCmdLine[4], dTemp, iError);
    If iError <> 0 Then Error(c_iErrorSyntaxError, 0);
    dTemp:=dTemp * g_lFreq;
    dTemp:=dTemp / 1000000;
    lTemp:=Trunc(dTemp);
    If lTemp > 511 Then
    Begin
      If g_oDevice.pGetVarPtr('_'+IntStr(lTemp)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lTemp), lTemp, IntStr(lTemp));
      OutStr('  mov __temp1,_'+IntStr(lTemp));
      OutStr('  add __temp1,cnt');
      OutStr('  waitcnt __temp1,#0');
    End
    Else
    Begin
      If lTemp < g_iMinWaitCnt Then OutStr('  NOP')
      Else
      Begin
        OutStr('  mov __temp1,#'+IntStr(lTemp));
        OutStr('  add __temp1,cnt');
        OutStr('  waitcnt __temp1,#0');
      End;
    End;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;

End.
