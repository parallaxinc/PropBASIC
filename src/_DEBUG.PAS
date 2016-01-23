Unit _DEBUG;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessBreak;
Procedure ProcessWatch;
Procedure ProcessUnwatch;
Procedure ProcessVPConfig;
Procedure ProcessUpdate;
Procedure ProcessVPVideo;
Procedure ProcessVPWait;

Implementation

Procedure ProcessBreak;
Begin
  If g_eDebugger = e_PASD Then OutStr('  nop '' <BP>');
  g_bHandled:=True;
End;


Procedure ProcessWatch;
Var
  iPlace: Integer;
  
  Procedure WatchVar(pv_poVar: PVarObj; sDef: String);
  Begin
    If pv_poVar^.eGetType = e_LongVar Then
    Begin
      OutSpin(''' ' + g_sOrigLine);
      If pv_poVar^.m_iWatchPos = 0 Then
      Begin
        If sDef = '' Then g_sWatchLongs:=g_sWatchLongs+pv_poVar^.sGetName+','
         Else g_sWatchLongs:=g_sWatchLongs+pv_poVar^.sGetName+'('+sDef+'),';
        Inc(g_iWatchPos);
        pv_poVar^.m_iWatchPos:=g_iWatchPos;
        Inc(g_iWatchIndex);
        pv_poVar^.m_iWatchIndex:=g_iWatchIndex;
      End
      Else pv_poVar^.m_iWatchPos:=Abs(pv_poVar^.m_iWatchPos);
      g_bHandled:=True;
    End
    Else If pv_poVar^.eGetType = e_HubLong Then // A HUB array
    Begin
      OutSpin(''' ' + g_sOrigLine);
      If pv_poVar^.m_iWatchPos = 0 Then
      Begin
        Inc(g_iVPArrayCnt);
        g_asVPArrays[g_iVPArrayCnt]:=pv_poVar^.sGetName+','+IntStr(pv_poVar^.m_lElements);
        If sDef = '' Then g_sWatchArrays:=g_sWatchArrays+pv_poVar^.sGetName+','
         Else g_sWatchArrays:=g_sWatchArrays+pv_poVar^.sGetName+'('+sDef+'),';
      End;
      g_bHandled:=True;
    End
    Else If pv_poVar^.eGetType = e_HubByte Then // Assume it is a string
    Begin
      OutSpin(''' ' + g_sOrigLine);
      If pv_poVar^.m_iWatchPos = 0 Then
      Begin
        g_sWatchStrings:=g_sWatchStrings+pv_poVar^.sGetName+'(string='+IntStr((pv_poVar^.m_lElements + 3) AND $FFFC)+'),'; // Make LONG aligned
        Inc(g_iWatchPos);
        pv_poVar^.m_iWatchPos:=g_iWatchPos;
      End;
      g_bHandled:=True;
    End
    Else If pv_poVar^.eGetType = e_Pin Then // IO(bits=[LEDs[16..23], switches[0..7], 30TX, 31Rx], base=2)
    Begin
      OutSpin(''' ' + g_sOrigLine);
      If pv_poVar^.m_iWatchPos = 0 Then
      Begin
        If pv_poVar^.m_lElements = 1 Then
         g_sWatchPins:=g_sWatchPins+IntStr(pv_poVar^.m_byPinNumber)+pv_poVar^.sGetName+','
        Else
         g_sWatchPins:=g_sWatchPins+pv_poVar^.sGetName+'['+IntStr(pv_poVar^.m_byPinNumber)+'..'+IntStr(pv_poVar^.m_byPinNumber+pv_poVar^.m_lElements-1)+'],';
      End;
      g_bHandled:=True;
    End;
  End; // Procedure WatchVar

Begin
  If g_eDebugger = e_PASD Then
  Begin
    OutStr('  nop '' <WP>');
    g_bHandled:=True;
  End
  Else
  Begin
    If g_iCmdCnt = 1 Then
    Begin // Watch all variables
      For iPlace:=1 to g_oDevice.m_oVars.m_iVarCnt Do
       If Not g_oDevice.m_oVars.m_apoVars[iPlace]^.m_bPermanent Then WatchVar(g_oDevice.m_oVars.m_apoVars[iPlace], '');
      g_bHandled:=True;
    End
    Else
    Begin
      iPlace:=2; // start at param 2
      Repeat
        If (iPlace < g_iCmdCnt) and (g_asCmdLine[iPlace+1, 1] = '"') Then
        Begin
          g_bHandled:=False; // Assume won't be handled
          WatchVar(g_apoCmdVars[iPlace], Copy(g_asCmdLine[iPlace+1], 2, Length(g_asCmdLine[iPlace+1]) - 2));
        End
        Else
        Begin
          g_bHandled:=False; // Assume won't be handled
          WatchVar(g_apoCmdVars[iPlace], '');
        End;
        If Not g_bHandled Then Error(c_iErrorInvalidParameter, iPlace);
        Inc(iPlace);
        If (iPlace <= g_iCmdCnt) and (g_asCmdLine[iPlace, 1] = '"') Then Inc(iPlace);
        If (iPlace <= g_iCmdCnt) and (g_asCmdLine[iPlace] = ',') Then Inc(iPlace);
      Until iPlace > g_iCmdCnt;
    End;
  End;
End;


Procedure ProcessUnwatch;
Var
  iPlace: Integer;
Begin
  If g_eDebugger = e_PASD Then
  Begin
    g_bHandled:=True;
  End
  Else
  Begin
    If g_iCmdCnt = 1 Then
    Begin // UnWatch all COG variables
      For iPlace:=1 to g_oDevice.m_oVars.m_iVarCnt Do
      Begin
        With g_oDevice.m_oVars.m_apoVars[iPlace]^ Do
        Begin
          If (eGetType = e_LongVar) and (m_iWatchPos <> 0) Then m_iWatchPos:=-m_iWatchPos;
        End; // With
      End; // For iPlace
      g_bHandled:=True;
    End
    Else
    Begin
      iPlace:=2;
      Repeat
        If g_apoCmdVars[iPlace]^.eGetType = e_LongVar Then
        Begin
          OutSpin(''' ' + g_sOrigLine);
          g_apoCmdVars[iPlace]^.m_iWatchPos:=-Abs(g_apoCmdVars[iPlace]^.m_iWatchPos);
          g_bHandled:=True;
        End
        Else Error(c_iErrorInvalidParameter, 2);
        Inc(iPlace);
        If g_asCmdLine[iPlace] = ',' Then Inc(iPlace);
      Until iPlace > g_iCmdCnt;
    End;
  End;
End;

Procedure ProcessVPConfig;
Begin
  If g_iCmdCnt = 2 Then
  Begin
    If g_asCmdLine[2, 1] = '"' Then
    Begin
      Inc(g_iVPConfigCnt);
      g_asVPConfig[g_iVPConfigCnt]:=g_apoCmdVars[2]^.sGetName;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);
End;


Procedure ProcessUpdate;
Begin
  If g_eDebugger = e_PASD Then
  Begin
    OutStr('  nop '' <WP>');
    g_bHandled:=True;
  End
  Else
  Begin
    If g_iCmdcnt = 2 Then
    Begin
      If g_apoCmdVars[2]^.eGetType = e_LongVar Then
      Begin
        If (g_eDebugger = e_Viewport) and (g_apoCmdVars[2]^.m_iWatchPos <> 0) Then
        Begin
          OutStr('  rdlong '+g_apoCmdVars[2]^.sGetName+',__'+g_apoCmdVars[2]^.sGetName+'_adr');
        End;
        g_bHandled:=True;
      End
      Else Error(c_iErrorInvalidParameter, 2);
    End
    Else Error(c_iErrorInvalidNumberofParameters, 0);
  End;
End;



Procedure ProcessVPVideo; // VPVideo buffer
Begin
  If g_iCmdCnt = 2 Then
  Begin
    OutSpin(''' ' + g_sOrigLine);
    g_sVPVideoBuffer:=g_asCmdLine[2];
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);  
End;


Procedure ProcessVPWait;  // VPWait clocks
Begin
  If g_iCmdCnt = 2 Then
  Begin
    OutStr('  mov __temp1,par');
    OutStr('  add __temp1,#4');
    If g_apoCmdVars[2]^.eGetType = e_ShortConst Then
    Begin
      OutStr('  mov __temp2,#'+g_apoCmdVars[2]^.sGetName);
      OutStr('  wrlong __temp2,__temp1');
    End
    Else If g_apoCmdVars[2]^.eGetType In [e_LongVar, e_LongConst] Then
    Begin
      OutStr('  wrlong '+g_apoCmdVars[2]^.sGetIndexName+',__temp1');
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End;
  If g_iCmdCnt In [1,2] Then
  Begin
    OutStr('  rdlong __temp1,par');
    OutStr(NewLabelStr);
    OutStr('  rdlong __temp2,par');
    OutStr('  cmp __temp1,__temp2 WZ');
    If g_bLMM Then
    Begin
      OutStr('  IF_E rdlong __PC,__PC');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  IF_E jmp #'+LabelStr);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberofParameters, 0);  
End;


End.
