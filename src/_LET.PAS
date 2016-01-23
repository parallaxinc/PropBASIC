Unit _LET;

{$MODE Delphi}

Interface

Procedure ProcessLet;

Implementation

Uses GLOBAL, _LETLONG, _LETSTR;

Procedure ProcessLetPin;
Var
  lShiftCnt: LongInt;
Begin
  If g_iCmdCnt = 4 Then
  Begin
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin:
        Begin
          If g_apoCmdVars[2]^.sGetName = g_apoCmdVars[4]^.sGetName Then
          Begin
            If g_asUnaryOperator[4] = '~' Then OutStr('  xor outa,'+g_apoCmdVars[2]^.sGetName);
          End
          Else
          Begin
            If g_apoCmdVars[2]^.m_lElements > 1 Then
            Begin
              lShiftCnt:=g_apoCmdVars[2]^.m_byPinNumber - g_apoCmdVars[4]^.m_byPinNumber;

              // Assume pin group = pin group
              OutStr('  mov __temp1,ina');
              OutStr('  and __temp1,'+g_apoCmdVars[4]^.sGetName);

              If g_apoCmdVars[4]^.m_bRevPins And (Not g_apoCmdVars[2]^.m_bRevPins) Then
              Begin // source is reversed
                OutStr('  rev __temp1,#0');
                lShiftCnt:=g_apoCmdVars[2]^.m_byPinNumber - (32 - g_apoCmdVars[4]^.m_byPinNumber - g_apoCmdVars[4]^.m_lElements);
              End;

              If g_apoCmdVars[2]^.m_bRevPins And (Not g_apoCmdVars[4]^.m_bRevPins) Then
              Begin // Dest is reversed
                OutStr('  rev __temp1,#0');
                lShiftCnt:=(32 - g_apoCmdVars[2]^.m_byPinNumber - g_apoCmdVars[2]^.m_lElements) - g_apoCmdVars[4]^.m_byPinNumber;
                lShiftCnt:=-lShiftCnt; // Shift is performed on source
              End;

              If lShiftCnt > 0 Then
               OutStr('  shl __temp1,#'+IntStr(lShiftCnt));
              If lShiftCnt < 0 Then
               OutStr('  shr __temp1,#'+IntStr(-lShiftCnt));

              OutStr('  and __temp1,'+g_apoCmdVars[2]^.sGetName);

              OutStr('  mov __temp2,outa');
              OutStr('  andn __temp2,'+g_apoCmdVars[2]^.sGetName);
              OutStr('  or __temp2,__temp1');
              OutStr('  mov outa,__temp2');
            End
            Else
            Begin // Assume pin = pin
              OutStr('  and '+g_apoCmdVars[4]^.sGetName+',ina WZ, NR');
              If g_asUnaryOperator[4] = '~' Then OutStr('  muxz outa,'+g_apoCmdVars[2]^.sGetName)
               Else OutStr('  muxnz outa,'+g_apoCmdVars[2]^.sGetName);
            End;
          End;
          g_bHandled:=True;
        End;
      e_ShortConst:
        Begin
          If g_apoCmdVars[2]^.m_lElements = 1 Then
          Begin
            If g_apoCmdVars[4]^.lGetValue = 0 Then
            Begin
              OutStr('  andn outa,'+g_apoCmdVars[2]^.sGetName);
              g_bHandled:=True;
            End
            Else
            Begin
              OutStr('  or outa,'+g_apoCmdVars[2]^.sGetName);
              g_bHandled:=True;
            End;
          End
          Else
          Begin // Pin group
            OutStr('  mov __temp1,#'+g_apoCmdVars[4]^.sGetName);
            If g_apoCmdVars[2]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[2]^.m_lElements));
            If g_apoCmdVars[2]^.m_byPinNumber > 0 Then OutStr('  shl __temp1,#'+IntStr(g_apoCmdVars[2]^.m_byPinNumber));
            OutStr('  and __temp1,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  andn outa,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  or outa,__temp1');
            g_bHandled:=True;
          End;
        End;
      e_LongConst:
        Begin
          If g_apoCmdVars[2]^.m_lElements = 1 Then Error(c_iErrorInvalidParameter, 4);
          // Pin group
          OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetName);
          If g_apoCmdVars[2]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[2]^.m_lElements));
          If g_apoCmdVars[2]^.m_byPinNumber > 0 Then OutStr('  shl __temp1,#'+IntStr(g_apoCmdVars[2]^.m_byPinNumber));
          OutStr('  and __temp1,'+g_apoCmdVars[2]^.sGetName);
          OutStr('  andn outa,'+g_apoCmdVars[2]^.sGetName);
          OutStr('  or outa,__temp1');
          g_bHandled:=True;
        End;
      e_LongVar:
        Begin
          If g_apoCmdVars[2]^.m_lElements = 1 Then
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
              OutStr('  cmp 0-0,#0');
            End
            Else OutStr('  cmp '+g_apoCmdVars[4]^.sGetIndexName+',#0 WZ');
            OutStr('  muxnz outa,'+g_apoCmdVars[2]^.sGetName);
            g_bHandled:=True;
          End
          Else
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
            Else
            Begin
              If Upper(g_apoCmdVars[4]^.sGetName) <> '__TEMP1' Then OutStr('  mov __temp1,'+g_apoCmdVars[4]^.sGetIndexName);
            End;
            If g_apoCmdVars[2]^.m_bRevPins Then OutStr('  rev __temp1,#'+IntStr(32-g_apoCmdVars[2]^.m_lElements));
            If g_apoCmdVars[2]^.m_byPinNumber > 0 Then OutStr('  shl __temp1,#'+IntStr(g_apoCmdVars[2]^.m_byPinNumber));
            OutStr('  andn outa,'+g_apoCmdVars[2]^.sGetName);
            OutStr('  or outa,__temp1');
            g_bHandled:=True;
          End;
        End;
    End; // Case
  End
  Else Error(c_iErrorSyntaxError, 0);
End;


Procedure ProcessLet;
Begin
  Case g_apoCmdVars[2]^.eGetType of
    e_LongVar: ProcessLetLongVar; //_LETLong
    e_Pin: ProcessLetPin; // _Let
    e_ByteData, e_HubByte, e_DataLabel: ProcessLetString; // _LetStr
    Else Error(c_iErrorInvalidParameter, 2);
  End; // Case
End;

End.
