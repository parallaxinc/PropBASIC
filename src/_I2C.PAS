Unit _I2C;

{$MODE Delphi}

Interface

Uses GLOBAL;

Procedure ProcessI2CStart; //I2CStart SDAPin, SCLPin
Procedure ProcessI2CStop;  //I2CStop SDAPin, SCLPin
Procedure ProcessI2CWrite;  //I2CWrite SDAPin, SCLPin, bytevalue[,ackbitvar]
Procedure ProcessI2CRead;  //I2CRead SDAPin, SCLPin, bytevar,ackbitvalue
Procedure ProcessI2CSpeed; //I2CSpeed value

Implementation

Procedure ProcessI2CStart; // I2CStart SDAPin, SCLPin
// __temp1 = SDA pin
// __temp2 = SCL pin
// __temp3 = waitcnt value
Var
  lDelay: LongInt;
  dDelay: Double;
  sDelay: String[32];
  sSDA, sSCL: String[32];
Begin
  sSDA:=''; // Avoid Lazarus warning
  sSCL:=''; // Avoid Lazarus warning
  If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin: sSDA:=g_apoCmdVars[2]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          sSDA:='__temp1';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,'+g_apoCmdvars[2]^.sGetIndexName);
          sSDA:='__temp1';
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End;  // Case
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin: sSCL:=g_apoCmdVars[4]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,#'+g_apoCmdVars[4]^.sGetName);
          sSCL:='__temp2';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,'+g_apoCmdvars[4]^.sGetIndexName);
          sSCL:='__temp2';
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End;  // Case
    // Make SDA an input with latch low
    OutStr('  andn dira,'+sSDA);
    OutStr('  andn outa,'+sSDA);
    // Make SCL an output
    OutStr('  or dira,'+sSCL);
    // Setup try limit
    OutStr('  mov __temp4,#10');
    // Create loop label
    OutStr(NewLabelStr);
    // Make SCL low
    OutStr('  andn outa,'+sSCL);

    dDelay:=g_lFreq Div 200000;
    dDelay:=dDelay / g_dI2CSpeed;
    lDelay:=Trunc(dDelay);
    If lDelay < 20 Then lDelay:=20;
    If g_bLMM and (lDelay < 150) Then lDelay:=150;
    If lDelay > 511 Then
    Begin
      If g_oDevice.pGetVarPtr('_'+IntStr(lDelay)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lDelay), lDelay, IntStr(lDelay));
      sDelay:='_'+IntStr(lDelay);
    End
    Else sDelay:='#'+IntStr(lDelay);

    OutStr('  mov __temp3,'+sDelay);
    OutStr('  add __temp3,cnt');
    OutStr('  waitcnt __temp3,'+sDelay);
    // Make SCL high
    OutStr('  or outa,'+sSCL);
    // Delay 5uSec
    OutStr('  waitcnt __temp3,'+sDelay);
    // Make SDA an input
    OutStr('  andn dira,'+sSDA);
    OutStr('  waitcnt __temp3,'+sDelay);
    // If SDA didn't go high, then pulse SCL again
    OutStr('  and '+sSDA+',ina  NR, WZ');
    If g_bLMM Then
    Begin
      //OutStr('  IF_Z sub __PC,#(($-'+LabelStr+')*4)+4');
      OutStr('  IF_Z djnz __temp4,#_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  IF_Z djnz __temp4,#'+LabelStr);
    // Make SDA low
    OutStr('  or dira,'+sSDA);
    OutStr('  waitcnt __temp3,'+sDelay);
    // Make SCL low
    OutStr('  andn outa,'+sSCL);
    OutStr('  waitcnt __temp3,'+sDelay);
    g_bHandled:=True;
  End
End;

Procedure ProcessI2CStop;  // I2CStop SDAPin, SCLPin
// __temp1 = SDA pin
// __temp2 = SCL pin
// __temp3 = waitcnt value
Var
  lDelay: LongInt;
  sSDA, sSCL: String[32];
  dDelay: Double;
  sDelay: String[32];
Begin
  sSDA:=''; // Avoid Lazarus warning
  sSCL:=''; // Avoid Lazarus warning
  If g_iCmdCnt = 4 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin: sSDA:=g_apoCmdVars[2]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          sSDA:='__temp1';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          sSDA:='__temp1';
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End;  // Case
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin: sSCL:=g_apoCmdVars[4]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,#'+g_apoCmdVars[4]^.sGetName);
          sSCL:='__temp2';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,'+g_apoCmdVars[4]^.sGetIndexName);
          sSCL:='__temp2';
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End;  // Case

    dDelay:=g_lFreq Div 200000;
    dDelay:=dDelay / g_dI2CSpeed;
    lDelay:=Trunc(dDelay);
    If lDelay < 20 Then lDelay:=20;
    If g_bLMM and (lDelay < 150) Then lDelay:=150;
    If lDelay > 511 Then
    Begin
      If g_oDevice.pGetVarPtr('_'+IntStr(lDelay)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lDelay), lDelay, IntStr(lDelay));
      sDelay:='_'+IntStr(lDelay);
    End
    Else sDelay:='#'+IntStr(lDelay);

    OutStr('  mov __temp3,'+sDelay);
    OutStr('  add __temp3,cnt');
    OutStr('  waitcnt __temp3,'+sDelay);
    // Make SDA low
    OutStr('  or dira,'+sSDA); // Make pin an output
    OutStr('  waitcnt __temp3,'+sDelay);
    // Make SCL an high
    OutStr('  or outa,'+sSCL);
    OutStr('  waitcnt __temp3,'+sDelay);
    // Make SDA an input (high)
    OutStr('  andn dira,'+sSDA);
    OutStr('  waitcnt __temp3,'+sDelay);
    g_bHandled:=True;
  End
End;

Procedure ProcessI2CWrite;  // I2CWrite SDAPin, SCLPin, value[,var]
// __temp1 = SDA pin
// __temp2 = SCL pin
// __temp3 = value to send
// __temp4 = bit counter
// __temp5 = waitcnt value
Var
  lDelay: LongInt;
  sSDA, sSCL: String[32];
  dDelay: Double;
  sDelay: String[32];
Begin
  sSDA:=''; // Avoid Lazarus warning
  sSCL:=''; // Avoid Lazarus warning
  If g_iCmdCnt In [6,8] Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin: sSDA:=g_apoCmdVars[2]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          sSDA:='__temp1';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          sSDA:='__temp1';
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End;  // Case
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin: sSCL:=g_apoCmdVars[4]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,#'+g_apoCmdVars[4]^.sGetName);
          sSCL:='__temp2';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,'+g_apoCmdVars[4]^.sGetIndexName);
          sSCL:='__temp2';
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End;  // Case
    Case g_apoCmdVars[6]^.eGetType of
      e_ShortConst: OutStr('  mov __temp3,#'+g_apoCmdVars[6]^.sGetName);
      e_LongConst: OutStr('  mov __temp3,'+g_apoCmdVars[6]^.sGetName);
      e_LongVar:
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
            OutStr('  mov __temp3,0-0');
          End
          Else OutStr('  mov __temp3,'+g_apoCmdVars[6]^.sGetIndexName);
        End;
      Else Error(c_iErrorInvalidParameter, 6);
    End; // Case
    OutStr('  mov __temp4,#9');
    // Insure that the 9th bit is a 1 so SDA is an INPUT
    OutStr('  cmpsub 0,0 WC, NR '' Set Carry flag');
    OutStr('  rcl __temp3,#24');

    dDelay:=g_lFreq Div 200000;
    dDelay:=dDelay / g_dI2CSpeed;
    lDelay:=Trunc(dDelay);
    If lDelay < 20 Then lDelay:=20;
    If g_bLMM and (lDelay < 150) Then lDelay:=150;
    If lDelay > 511 Then
    Begin
      If g_oDevice.pGetVarPtr('_'+IntStr(lDelay)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lDelay), lDelay, IntStr(lDelay));
      sDelay:='_'+IntStr(lDelay);
    End
    Else sDelay:='#'+IntStr(lDelay);

    OutStr('  mov __temp5,'+sDelay);
    OutStr('  add __temp5,cnt');
    OutStr(NewLabelStr);
    // Make SCL low
    OutStr('  andn outa,'+sSCL);
    // Delay 5uSec
    OutStr('  waitcnt __temp5,'+sDelay);
    // Make SDA direction = databit (0 = output, 1 = input)
    OutStr('  rcl __temp3,#1 WC');
    OutStr('  muxnc dira,'+sSDA);
    OutStr('  andn outa,'+sSDA);
    OutStr('  waitcnt __temp5,'+sDelay);
    // Make SCL high
    OutStr('  or outa,'+sSCL);
    // Delay 5uSec
    OutStr('  waitcnt __temp5,'+sDelay);
    // Loop for next bit
    If g_bLMM Then
    Begin
      OutStr('  djnz __temp4,#_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  djnz __temp4,#'+LabelStr);
    // If we need ack bit then read it now (SDA)
    If g_iCmdCnt = 8 Then
    Begin
      If g_asCmdLine[7] <> ',' Then Error(c_iErrorCommaExpected, 7);
      If g_apoCmdVars[8]^.eGetType = e_LongVar Then
      Begin
        // Make param6 = state of SDA pin
        OutStr('  and '+sSDA+',ina WC, NR');
        OutStr('  mov '+g_apoCmdVars[8]^.sGetName+',#0');
        OutStr('  rcl '+g_apoCmdVars[8]^.sGetName+',#1');
      End
      Else Error(c_iErrorInvalidParameter, 8);
    End;
    // Make SCL low
    OutStr('  andn outa,'+sSCL);
    OutStr('  waitcnt __temp5,'+sDelay);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessI2CRead;  // I2CRead SDAPin, SCLPin, bytevar, 0|1|var
// __temp1 = SDA pin
// __temp2 = SCL pin
// __temp3 = value read
// __temp4 = bit counter
// __temp5 = waitcnt value
Var
  lDelay: LongInt;
  sSDA, sSCL: String[32];
  dDelay: Double;
  sDelay: String[32];
Begin
  sSDA:=''; // Avoid Lazarus warning
  sSCL:=''; // Avoid Lazarus warning
  If g_iCmdCnt = 8 Then
  Begin
    If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
    If g_asCmdLine[5] <> ',' Then Error(c_iErrorCommaExpected, 5);
    If g_asCmdLine[7] <> ',' Then Error(c_iErrorCommaExpected, 7);
    Case g_apoCmdVars[2]^.eGetType of
      e_Pin: sSDA:=g_apoCmdVars[2]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,#'+g_apoCmdVars[2]^.sGetName);
          sSDA:='__temp1';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp1,#1');
          OutStr('  shl __temp1,'+g_apoCmdVars[2]^.sGetIndexName);
          sSDA:='__temp1';
        End;
      Else Error(c_iErrorInvalidParameter, 2);
    End;  // Case
    Case g_apoCmdVars[4]^.eGetType of
      e_Pin: sSCL:=g_apoCmdVars[4]^.sGetName;
      e_ShortConst:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,#'+g_apoCmdVars[4]^.sGetName);
          sSCL:='__temp2';
        End;
      e_LongVar:
        Begin
          OutStr('  mov __temp2,#1');
          OutStr('  shl __temp2,'+g_apoCmdVars[4]^.sGetIndexName);
          sSCL:='__temp2';
        End;
      Else Error(c_iErrorInvalidParameter, 4);
    End;  // Case
    OutStr('  mov __temp3,#0');
    OutStr('  mov __temp4,#8');

    dDelay:=g_lFreq Div 200000;
    dDelay:=dDelay / g_dI2CSpeed;
    lDelay:=Trunc(dDelay);
    If lDelay < 20 Then lDelay:=20;
    If g_bLMM and (lDelay < 150) Then lDelay:=150;
    If lDelay > 511 Then
    Begin
      If g_oDevice.pGetVarPtr('_'+IntStr(lDelay)) = Nil Then g_oDevice.pAddLongConst('_'+IntStr(lDelay), lDelay, IntStr(lDelay));
      sDelay:='_'+IntStr(lDelay);
    End
    Else sDelay:='#'+IntStr(lDelay);

    OutStr('  mov __temp5,'+sDelay);
    OutStr('  add __temp5,cnt');
    OutStr('  andn dira,'+sSDA); // Make SDA an input
    OutStr(NewLabelStr);
    // Make SCL low
    OutStr('  andn outa,'+sSCL);
    // Delay 5uSec
    OutStr('  waitcnt __temp5,'+sDelay);
    // Make SCL high
    OutStr('  or outa,'+sSCL);
    OutStr('  waitcnt __temp5,'+sDelay);
    // Read SDA bit
    OutStr('  and '+sSDA+',ina WC, NR');
    OutStr('  rcl __temp3,#1');
    // Repeat for all 8 bits
    If g_bLMM Then
    Begin
      OutStr('  djnz __temp4,#_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  djnz __temp4,#'+LabelStr);
    // Make SCL low
    OutStr('  andn outa,'+sSCL);
    OutStr('  waitcnt __temp5,'+sDelay);
    // Set SDA for ACK bit
    Case g_apoCmdVars[8]^.eGetType of
      e_ShortConst:
        Begin
          If g_apoCmdVars[8]^.lGetValue = 0 Then
          Begin // Make SDA low
            OutStr('  or dira,'+sSDA);
          End
          Else OutStr('  andn dira,'+sSDA); // Make SDA high (input)
        End;
      e_LongVar:
        Begin
          OutStr('  and '+g_apoCmdVars[8]^.sGetName+',ina NR, WZ');
          OutStr('  muxz dira,'+sSDA);
        End
      Else Error(c_iErrorInvalidParameter, 8);
    End; // Case
    OutStr('  waitcnt __temp5,'+sDelay);
    // Make SCL high
    OutStr('  or outa,'+sSCL);
    OutStr('  waitcnt __temp5,'+sDelay);
    // Make SCL low
    OutStr('  andn outa,'+sSCL);
    OutStr('  waitcnt __temp5,'+sDelay);
    // Make SDA an input (high)
    OutStr('  andn dira,'+sSDA);
    OutStr('  waitcnt __temp5,'+sDelay);
    If  g_apoCmdVars[6]^.eGetType = e_LongVar Then
    Begin
      If g_apoCmdVars[6]^.m_bVarIndex Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[6]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[6]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[6]^.m_sIndex+',#'+g_apoCmdVars[6]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[6]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[6]^.m_sIndex+',#'+g_apoCmdVars[6]^.sGetName);
        End;
        OutStr('  mov 0-0,__temp3');
      End
      Else OutStr('  mov '+g_apoCmdVars[6]^.sGetName+',__temp3');
    End
    Else Error(c_iErrorInvalidParameter, 6);
    VP_WriteVar(6);
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidNumberOfParameters, 0);
End;

Procedure ProcessI2CSpeed;
Var
  sTemp: String;
  dTemp: Double;
  iError: Integer;
Begin
  If g_asCmdLine[3] = '.' Then sTemp:=g_asCmdLine[2]+'.'+g_asCmdLine[4]
   Else sTemp:=g_asCmdLine[2];
  Val(sTemp, dTemp, iError);
  If iError = 0 Then
  Begin
    OutStr(' ');
    If dTemp < 0.001 Then dTemp:=0.001;
    g_dI2CSpeed:=dTemp;
    g_bHandled:=True;
  End
  Else Error(c_iErrorInvalidParameter, 2);
End;


End.
