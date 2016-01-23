Unit _READ;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessData_Declare;
Procedure ProcessData;
Procedure ProcessWData;
Procedure ProcessLData;
Procedure ProcessFile;

Implementation

Procedure ProcessData_Declare;
Begin
  If g_asCmdLine[g_iCmdCnt] = ',' Then Error(c_iErrorSyntaxError, 0);
  If Upper(g_asCmdLine[2]) = 'DATA' Then
  Begin
    g_oDevice.pAddDataLabel(Upper(g_asCmdLine[1]), 1);
    ProcessData;
  End
  Else If Upper(g_asCmdLine[2]) = 'WDATA' Then
  Begin
    g_lHubOffset:=(g_lHubOffset + 1) and $FFFFFFFE;
    g_oDevice.pAddDataLabel(Upper(g_asCmdLine[1]), 2);
    ProcessWData;
  End
  Else If Upper(g_asCmdLine[2]) = 'LDATA' Then
  Begin
    g_lHubOffset:=(g_lHubOffset + 3) and $FFFFFFFC;
    g_oDevice.pAddDataLabel(Upper(g_asCmdLine[1]), 4);
    ProcessLData;
  End
  Else Error(c_iErrorSyntaxError, 0);
  g_bHandled:=True; // for pass = 2 just mark as handled
End;


Procedure ProcessData;
Var
  sTemp: String;
  sOutput: String;
  I, J: Integer;
  iHubBytes: Integer;
Begin
  iHubBytes:=0; // Avoid lazarus warning
  sTemp:=Copy(g_sOrigLine, 1, Length(g_sOrigLine) - Length(g_asCmdLine[c_iMaxCmdCnt+1]))+' ';
  OutSpin('''' + sTemp);
  // Convert tabs to spaces
  While Pos(#9, sTemp) > 0 Do sTemp[Pos(#9, sTemp)]:=' ';
  I:=Pos(' DATA ', Upper(sTemp))+6;
  sTemp:=Copy(sTemp, I, Length(sTemp)-I+1);
  If Upper(g_asCmdLine[1]) <> 'DATA' Then J:=3 Else J:=2;
  If sTemp <> '' Then
  Begin
    sOutput:='';
    For I:=J to g_iCmdCnt Do
    Begin
      If Not Odd(I-J) Then
      Begin
        If g_apoCmdVars[I] = Nil Then
        Begin
          sOutput:=sOutput + g_asCmdLine[I]+',' // Assume it is a string
        End
        Else
        Begin
          Case g_apoCmdVars[I]^.eGetType of
            e_ShortConst:
              Begin
                If g_bIsTask Then
                Begin
                  If g_apoCmdVars[I]^.sGetName[1] In ['0'..'9', '$', '%', '"'] Then sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+','
                   Else sOutput:=sOutput + g_apoCmdVars[I]^.m_sTaskPrefix+g_apoCmdVars[I]^.sGetName+',';
                End
                Else sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+',';
              End;
            e_LongConst:
              Begin
                If g_bIsTask Then sOutput:=sOutput + g_apoCmdVars[I]^.sGetValue+','
                 Else sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+',';
              End;
            e_Ukn: sOutput:=sOutput + g_asCmdLine[I]+','; // String
          End; // Case
        End;
      End;
    End;
    sOutput:=Copy(sOutput, 1, Length(sOutput)-1); // Remove last comma

    g_oDevice.pAddByteData(sOutput, iHubBytes);
    Inc(g_lHubOffset, iHubBytes);

  End;
  g_bHandled:=True; // for pass 2 mark as handled
End;


Procedure ProcessWData;
Var
  sTemp: String;
  I, J: Integer;
  sOutput: String;
Begin
  sTemp:=g_sOrigLine+' ';
  OutSpin('''' + sTemp);
  I:=Pos(' WDATA ', Upper(sTemp))+7;
  sTemp:=Copy(sTemp, I, Length(sTemp)-I+1);
  If Upper(g_asCmdLine[1]) <> 'WDATA' Then J:=3 Else J:=2;
  If sTemp <> '' Then
  Begin
    sOutput:='';
    For I:=J to g_iCmdCnt Do
    Begin
      If Not Odd(I-J) Then
      Begin
        If g_apoCmdVars[I] = Nil Then
        Begin
          Error(c_iErrorInvalidParameter, I); // Assume it is a string or something
        End
        Else
        Begin
          Case g_apoCmdVars[I]^.eGetType of
            e_ShortConst:
              Begin
                If g_bIsTask Then
                Begin
                  If g_apoCmdVars[I]^.sGetName[1] In ['$', '%', '0'..'9'] Then sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+','
                   Else sOutput:=sOutput + g_apoCmdVars[I]^.m_sTaskPrefix+g_apoCmdVars[I]^.sGetName+',';
                End
                Else sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+',';
              End;
            e_LongConst:
              Begin
                If g_asCmdLine[I, 1] In ['$','%','0'..'9'] Then sOutput:=sOutput + g_asCmdLine[I]+',' // g_apoCmdVars[I]^.sGetName+','
                 Else sOutput:=sOutput + g_apoCmdVars[I]^.sGetValue+',';
              End;
            e_Ukn: Error(c_iErrorInvalidParameter, I); // String or something
          End; // Case
        End;
      End;
    End;

    sOutput:=Copy(sOutput, 1, Length(sOutput)-1); // Remove last comma
    g_oDevice.pAddWordData(sOutput);

    Inc(g_lHubOffset, g_iCmdCnt AND $FE);
    // Delete any longconst vars that where created for this line of code
    I:=g_oDevice.m_oVars.m_iLastVarCnt + 1; // xxx
    While I <= g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      If g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_LongConst Then
      Begin
        Dispose(g_oDevice.m_oVars.m_apoVars[I], Done);
        Dec(g_oDevice.m_oVars.m_iVarCnt);
        For J:=I To g_oDevice.m_oVars.m_iVarCnt Do g_oDevice.m_oVars.m_apoVars[J]:=g_oDevice.m_oVars.m_apoVars[J+1];
      End
      Else Inc(I);
    End;
  End;
  g_bHandled:=True;
End;

Procedure ProcessLData;
Var
  sTemp: String;
  I, J: Integer;
  sOutput: String;
Begin
  sTemp:=g_sOrigLine;
  OutSpin('''' + sTemp);
  I:=Pos(' LDATA ', Upper(sTemp))+7;
  sTemp:=Copy(sTemp, I, Length(sTemp)-I+1);
  If Upper(g_asCmdLine[1]) <> 'LDATA' Then J:=3 Else J:=2;
  If sTemp <> '' Then
  Begin
    sOutput:='';
    For I:=J to g_iCmdCnt Do
    Begin
      If Not Odd(I-J) Then
      Begin
        If g_apoCmdVars[I] = Nil Then
        Begin
          Error(c_iErrorInvalidParameter, I); // Assume it is a string or something
        End
        Else
        Begin
          Case g_apoCmdVars[I]^.eGetType of
            e_ShortConst:
              Begin
                If g_bIsTask Then
                Begin
                  If g_apoCmdVars[I]^.sGetName[1] In ['$', '%', '0'..'9'] Then sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+','
                   Else sOutput:=sOutput + g_apoCmdVars[I]^.m_sTaskPrefix+g_apoCmdVars[I]^.sGetName+',';
                End
                Else sOutput:=sOutput + g_apoCmdVars[I]^.sGetName+',';
              End;
            e_LongConst:
              Begin
                If g_asCmdLine[I, 1] = '#' Then
                Begin // Handle floating point value
                  If pos('.', g_asCmdLine[I]) = 0 Then sOutput:=sOutput + Copy(g_asCmdLine[I], 2, Length(g_asCmdLine[I])-1)+'.0,'
                   Else sOutput:=sOutput + Copy(g_asCmdLine[I], 2, Length(g_asCmdLine[I])-1)+',';
                End
                Else
                Begin
                  If g_asCmdLine[I, 1] In ['$','%','0'..'9'] Then sOutput:=sOutput + g_asCmdLine[I]+',' // g_apoCmdVars[I]^.sGetName+','
                   Else sOutput:=sOutput + g_apoCmdVars[I]^.sGetValue+',';
                End;
              End;
            e_Ukn: Error(c_iErrorInvalidParameter, I); // String or something
          End; // Case
        End;
      End;
    End;

    sOutput:=Copy(sOutput, 1, Length(sOutput)-1); // Remove last comma
    g_oDevice.pAddLongData(sOutput);

    Inc(g_lHubOffset, (g_iCmdCnt AND $FE) * 2);
    // Delete any longconst vars that where created for this line of code
    I:=g_oDevice.m_oVars.m_iLastVarCnt + 1; // xxx
    While I <= g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      If g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_LongConst Then
      Begin
        Dispose(g_oDevice.m_oVars.m_apoVars[I], Done);
        Dec(g_oDevice.m_oVars.m_iVarCnt);
        For J:=I To g_oDevice.m_oVars.m_iVarCnt Do g_oDevice.m_oVars.m_apoVars[J]:=g_oDevice.m_oVars.m_apoVars[J+1];
      End
      Else Inc(I);
    End;
  End;
  g_bHandled:=True;
End;

Procedure ProcessFile;
Var
  fTemp: File;
Begin
  If g_iCmdCnt = 3 Then
  Begin // label FILE "filename"
    g_oDevice.pAddDataLabel(g_asCmdLine[1], 1);
    OutStr('');
    Assign(fTemp, Copy(g_asCmdLine[3], 2, Length(g_asCmdLine[3])-2));
    {$I-}
    Reset(fTemp, 1);
    {$I+}
    If IOResult = 0 Then
    Begin
      g_lHubOffset:=g_lHubOffset + FileSize(fTemp);
      Close(fTemp);
      g_oDevice.pAddDataFile(g_asCmdLine[3]);
      g_bHandled:=True;
    End
    Else ErrorStr(c_iErrorCouldNotReadSourceFile, Copy(g_asCmdLine[3], 2, Length(g_asCmdLine[3])-2));
  End
  Else If g_iCmdCnt = 2 Then
  Begin // FILE "filename"
    OutStr('');
    Assign(fTemp, Copy(g_asCmdLine[2], 2, Length(g_asCmdLine[2])-2));
    {$I-}
    Reset(fTemp, 1);
    {$I+}
    If IOResult = 0 Then
    Begin
      g_lHubOffset:=g_lHubOffset + FileSize(fTemp);
      Close(fTemp);
      g_oDevice.pAddDataFile(g_asCmdLine[2]);
      g_bHandled:=True;
    End
    Else ErrorStr(c_iErrorCouldNotReadSourceFile, Copy(g_asCmdLine[2], 2, Length(g_asCmdLine[2])-2));
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

End.
