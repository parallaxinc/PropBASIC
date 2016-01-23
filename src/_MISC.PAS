Unit _MISC;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessDJNZ;

Procedure ProcessDefineDirective(sLine: String);
Procedure ProcessUnDefineDirective(sLine: String);
Procedure ProcessIfDefDirective(sLine: String);
Procedure ProcessIfNDefDirective(sLine: String);
Procedure ProcessIfFreqDirective(sLine: String);
Procedure ProcessElseDirective;
Procedure ProcessEndIfDirective;
Procedure ProcessWarningDirective(sLine: String);
Procedure ProcessErrorDirective(sLine: String);
Procedure ProcessUsesDirective(sLine: String);
Procedure ProcessIfUsedDirective(sLine: String);
Procedure ProcessIfNUsedDirective(sLine: String);
Procedure ProcessCodeDirective;
Procedure ProcessTasksDirective;

Implementation

Procedure ProcessDJNZ; // DJNZ var,label
Begin
  If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[2]^.m_bVarIndex Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
          OutStr('  djnz 0-0,#_LMM_JUMP');
          OutStr('  long @@@'+g_asCmdLine[4]);
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  djnz 0-0,#'+g_asCmdLine[4]);
        End;
      End
      Else
      Begin
        If g_bLMM Then
        Begin
          OutStr('  djnz '+g_apoCmdVars[2]^.sGetIndexName+',#_LMM_JUMP');
          OutStr('  long @@@'+g_asCmdLine[4]);
        End
        Else
        Begin
          If g_apoCmdVars[2]^.m_iWatchPos <> 0 Then
          Begin
            OutStr('  subs '+g_apoCmdVars[2]^.sGetIndexName+',#1');
            VP_WriteVar(2);
            OutStr('  adds '+g_apoCmdVars[2]^.sGetIndexName+',#1');
          End;  
          OutStr('  djnz '+g_apoCmdVars[2]^.sGetIndexName+',#'+g_asCmdLine[4]);
        End;
      End;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessDefineDirective(sLine: String);
Var
  I: Integer;
Begin
  If g_iDefinesCnt < c_iMaxDefines Then
  Begin
    While (Length(sLine) > 0) and (sLine[1] = ' ') Do Delete(sLine, 1, 1);
    I:=1;
    While (I < Length(sLine)) and (sLine[I] In ['0'..'9', 'A'..'Z', 'a'..'z', '_']) Do Inc(I);
    If I > 1 Then
    Begin
      AddDirective(Copy(sLine, 1, I));
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberofParameters, 0);
  End
  Else Error(c_iErrorTooManyDefines, 0);
End;

Procedure ProcessUnDefineDirective(sLine: String);
Var
  I: Integer;
Begin
  While (Length(sLine) > 0) and (sLine[1] = ' ') Do Delete(sLine, 1, 1);
  I:=1;
  While (I < Length(sLine)) and (sLine[I] In ['0'..'9', 'A'..'Z', 'a'..'z', '_']) Do Inc(I);
  If I > 1 Then
  Begin
    RemoveDirective(Copy(sLine, 1, I));
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;

Procedure ProcessIfDefDirective(sLine: String);
Var
  I: Integer;
  bFound: Boolean;
Begin
  Inc(g_iCondCnt);
  g_bCondState[g_iCondCnt]:=g_bCompile;
  If g_bCompile = False Then
  Begin
    g_bElseState[g_iCondCnt]:=False;
    g_bHandled:=True;
  End
  Else
  Begin
    bFound:=False;
    While (Length(sLine) > 0) and (sLine[1] = ' ') Do Delete(sLine, 1, 1);
    I:=1;
    While (I < Length(sLine)) and (sLine[I] In ['0'..'9', 'A'..'Z', 'a'..'z', '_']) Do Inc(I);
    If I > 1 Then
    Begin
      sLine:=Copy(sLine, 1, I);
      For I:=1 to g_iDefinesCnt Do
      Begin
        If Upper(sLine) = Upper(g_asDefines[I, 0]) Then bFound:=True;
      End;
      If bFound Then g_bCompile:=True Else g_bCompile:=False;
      g_bElseState[g_iCondCnt]:=Not g_bCompile;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidNumberOfParameters, 0);
  End;
End;

Procedure ProcessIfNDefDirective(sLine: String);
Var
  bTemp: Boolean;
Begin
  ProcessIfDefDirective(sLine);
  If g_bHandled Then
  Begin
    bTemp:=g_bCompile;
    g_bCompile:=g_bElseState[g_iCondCnt];
    g_bElseState[g_iCondCnt]:=bTemp;
  End;
End;

Procedure ProcessIfFreqDirective(sLine: String);
Var
  I: Integer;
  sTemp: String;
  sCompare1: String[2];
  sFreq1: String;
  lFreq1: LongInt;
  sCompare2: String[2];
  sFreq2: String;
  lFreq2: LongInt;
  iError1, iError2: Integer;
Begin // '{$IFFREQ >= 4_000_000 <= 20_000_000
  lFreq2:=0;
  Inc(g_iCondCnt);
  g_bCondState[g_iCondCnt]:=g_bCompile;
  If g_bCompile = False Then
  Begin
    g_bElseState[g_iCondCnt]:=False;
    g_bHandled:=True;
  End
  Else
  Begin
    iError1:=0;
    iError2:=0;
    sCompare2:='';
    While (Length(sLine) > 0) and (sLine[1] = ' ') Do Delete(sLine, 1, 1);
    sTemp:='';
    For I:=1 to Length(sLine) Do If (sLine[I] <> '_') and (sLine[I] <> ' ') Then sTemp:=sTemp + sLine[I];
    If sTemp[1] In ['>', '<', '='] Then
    Begin
      sCompare1:=sTemp[1];
      Delete(sTemp, 1, 1);
      If ((sTemp[1] = '>') and (sCompare1[1] = '<')) or
         ((sTemp[1] = '=') and (sCompare1[1] In ['>', '<'])) Then
      Begin
        sCompare1:=sCompare1 + sTemp[1];
        Delete(sTemp, 1, 1);
      End;
      sFreq1:='';
      While (Length(sTemp) > 0) and (sTemp[1] In ['0'..'9']) Do
      Begin
        sFreq1:=sFreq1 + sTemp[1];
        Delete(sTemp, 1, 1);
      End;
      Val(sFreq1, lFreq1, iError1);

      Inc(iError1); // avoid lazarus warning

      If Length(sTemp) > 0 Then
      Begin // Second comparison
        If sTemp[1] In ['>', '<', '='] Then
        Begin
          sCompare2:=sTemp[1];
          Delete(sTemp, 1, 1);
          If ((sTemp[1] = '>') and (sCompare2[1] = '<')) or
             ((sTemp[1] = '=') and (sCompare2[1] In ['>', '<'])) Then
          Begin
            sCompare2:=sCompare2 + sTemp[1];
            Delete(sTemp, 1, 1);
          End;
          sFreq2:='';
          While (Length(sTemp) > 0) and (sTemp[1] In ['0'..'9']) Do
          Begin
            sFreq2:=sFreq2 + sTemp[1];
            Delete(sTemp, 1, 1);
          End;
          Val(sFreq2, lFreq2, iError2);

          Inc(iError2); // avoid lazarus error

        End
        Else Error(c_iErrorSyntaxError, 0);
      End;
      g_bCompile:=True;
      If (sCompare1 = '=') and (g_lFreq <> lFreq1) Then g_bCompile:=False;
      If (sCompare1 = '<>') and (g_lFreq = lFreq1) Then g_bCompile:=False;
      If (sCompare1 = '>') and (g_lFreq <= lFreq1) Then g_bCompile:=False;
      If (sCompare1 = '<') and (g_lFreq >= lFreq1) Then g_bCompile:=False;
      If (sCompare1 = '>=') and (g_lFreq < lFreq1) Then g_bCompile:=False;
      If (sCompare1 = '<=') and (g_lFreq > lFreq1) Then g_bCompile:=False;

      If (sCompare2 = '=') and (g_lFreq <> lFreq2) Then g_bCompile:=False;
      If (sCompare2 = '<>') and (g_lFreq = lFreq2) Then g_bCompile:=False;
      If (sCompare2 = '>') and (g_lFreq <= lFreq2) Then g_bCompile:=False;
      If (sCompare2 = '<') and (g_lFreq >= lFreq2) Then g_bCompile:=False;
      If (sCompare2 = '>=') and (g_lFreq < lFreq2) Then g_bCompile:=False;
      If (sCompare2 = '<=') and (g_lFreq > lFreq2) Then g_bCompile:=False;
      g_bElseState[g_iCondCnt]:=Not g_bCompile;
      g_bHandled:=True;
    End
    Else Error(c_iErrorSyntaxError, 0);
  End;
End;

Procedure ProcessElseDirective;
Begin
{  g_bCompile:=Not g_bCompile; }
  g_bCompile:=g_bElseState[g_iCondCnt];
End;

Procedure ProcessEndIfDirective;
Begin
  If g_iCondCnt > 0 Then
  Begin
    g_bCompile:=g_bCondState[g_iCondCnt];
    Dec(g_iCondCnt);
  End
  Else g_bCompile:=True;
End;

Procedure ProcessWarningDirective(sLine: String);
Begin
  WarningStr(c_iWarningDirective, sLine);
End;

Procedure ProcessErrorDirective(sLine: String);
Begin
  ErrorStr(c_iErrorDirective, sLine);
End;

Procedure ProcessUsesDirective(sLine: String);
Begin
  If g_oDevice.pGetVarPtr(sLine) = Nil Then Error(c_iErrorInvalidParameter, 0);
End;

Procedure ProcessIfUsedDirective(sLine: String);
Var
  i: Integer;
Begin
  Inc(g_iCondCnt);
  g_bCondState[g_iCondCnt]:=g_bCompile;
  If g_bCompile = False Then
  Begin
    g_bElseState[g_iCondCnt]:=False;
    g_bHandled:=True;
  End
  Else
  Begin
    OutStr(''); // Output the IFUSED directive
    g_bCompile:=False; // Assume not used
    i:=SubIndex(sLine);
    If i > 0 Then
    Begin
      g_bCompile:=g_arSubroutines[i].bUsed
    End;
    If UsesIndex(sLine) > 0 Then g_bCompile:=True;
    g_bElseState[g_iCondCnt]:=Not g_bCompile;
    g_bHandled:=True;
  End;
End;

Procedure ProcessIfNUsedDirective(sLine: String);
Var
  bTemp: Boolean;
Begin
  ProcessIfUsedDirective(sLine);
  If g_bHandled Then
  Begin
    bTemp:=g_bCompile;
    g_bCompile:=g_bElseState[g_iCondCnt];
    g_bElseState[g_iCondCnt]:=bTemp;
  End;
End;

Procedure ProcessCodeDirective;
Begin
  If Not g_bLoadTasks Then
  Begin
    If g_bInLoadCmd Then g_bCompile:=False Else g_bCompile:=True;
    g_bHandled:=True;
  End;
End;

Procedure ProcessTasksDirective;
Begin
  If g_bLoadTasks Then g_bCompile:=True Else g_bCompile:=False;
  g_bHandled:=True;
End;

End.
