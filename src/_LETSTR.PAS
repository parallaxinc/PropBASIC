Unit _LetStr;

Interface

Procedure ProcessLetString;

Implementation

Uses Global;

Procedure ProcessLetStr; // string = STR var, digits[,option(0=leading "0"; 1=leading " "; 2=signed leading "0"; 3=signed first leading " ";)]
Var
  poLongConst: pVarObj;
  sLabel, sSignLabel: String;
  lOption: LongInt;
  lDigits: LongInt;
  iPlace: Integer;
Begin
  // __temp1 already points to dest string
  lOption:=1; // Defaults to mode 1, unsigned leading spaces
  iPlace:=5; // skip "LET string = STR" longvar, digits, option
  If g_asCmdLine[3] = '(' Then Inc(iPlace, 3);

  If g_iCmdCnt = iPlace+4 Then // Is option specified ?
  Begin
    If g_asCmdLine[iPlace+3] <> ',' Then Error(c_iErrorCommaExpected, iPlace+3);
    If g_apoCmdVars[iPlace+4]^.eGetType = e_ShortConst Then
    Begin
      lOption:=g_apoCmdVars[iPlace+4]^.lGetValue;
      If (lOption < 0) or (lOption > 7) Then Error(c_iErrorInvalidParameter, iPlace+4);
    End;
  End;

  // __temp2 = value to convert
  If (g_iCmdCnt >= iPlace+2) and (g_apoCmdVars[iPlace]^.eGetType = e_LongVar) Then
  Begin
    If g_apoCmdVars[iPlace]^.m_bVarIndex Then
    Begin
      If g_bLMM Then
      Begin
        OutStr('  mov __INDEX,#'+g_apoCmdVars[iPlace]^.sGetName);
        OutStr('  add __INDEX,'+g_apoCmdVars[iPlace]^.m_sIndex);
        OutStr('  jmp #_LMM_MOVS');
      End
      Else
      Begin
        OutStr('  add '+g_apoCmdVars[iPlace]^.m_sIndex+',#'+g_apoCmdVars[iPlace]^.sGetName);
        OutStr('  movs $+2,'+g_apoCmdVars[iPlace]^.m_sIndex);
        OutStr('  sub '+g_apoCmdVars[iPlace]^.m_sIndex+',#'+g_apoCmdVars[iPlace]^.sGetName);
      End;
      OutStr('  mov __temp2,0-0');
    End
    Else OutStr('  mov __temp2,'+g_apoCmdVars[iPlace]^.sGetIndexName);

    // __temp4 = digit counter
    Inc(iPlace, 2);
    If g_apoCmdVars[iPlace]^.eGetType <> e_ShortConst Then Error(c_iErrorInvalidParameter, iPlace);
    lDigits:=g_apoCmdVars[iPlace]^.lGetValue;
    If lOption In [0,1,4,5] Then
    Begin
      If lDigits < 10 Then OutStr('  mov __temp4,#'+g_apoCmdVars[iPlace]^.sGetName)
       Else OutStr('  mov __temp4,#'+g_apoCmdVars[iPlace]^.sGetName+'-1');
    End
    Else
    Begin
      Dec(lDigits);
      If lDigits < 10 Then OutStr('  mov __temp4,#'+g_apoCmdVars[iPlace]^.sGetName+'-1')
        Else OutStr('  mov __temp4,#'+g_apoCmdVars[iPlace]^.sGetName+'-2');
    End;

    // Leading zero or leading spaces
    If Odd(lOption) Then OutStr('  mov __temp3,#" "') Else OutStr('  mov __temp3,#"0"');

    // Handle sign if option = 2 or 3
    If lOption In[2, 3, 6, 7] Then
    Begin
      OutStr('  abs __temp2,__temp2 WC');
      OutSTr('  mov __temp5,#" "');
      OutStr('  IF_C mov __temp5,#"-"');
      If g_apoCmdVars[2]^.eGetType = e_LongVar Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,__temp1');
          OutStr('  add __temp1,#1');
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  movd $+2,__temp1');
          OutStr('  add __temp1,#1');
        End;
        OutStr('  mov 0-0,__temp5');
      End
      Else
      Begin
        OutStr('  wrbyte __temp5,__temp1');
        OutStr('  add __temp1,#1');
      End;
    End;

    // Handle 10 digits (or 11 digits if negative)
    If lDigits = 10 Then
    Begin
      If g_oDevice.pGetVarPtr('_1000000000') = Nil Then g_oDevice.pAddLongConst('_1000000000', 1000000000, '1000000000');
      lDigits:=9;
      If lOption In [1, 3, 5, 7] Then
      Begin
        OutStr('  mov __temp5,#" "');
        OutStr('  cmp __temp2,_1000000000 WC');
        sSignLabel:=NewLabelStr;
        If g_bLMM Then
        Begin
          OutStr('  IF_C jmp #_LMM_JUMP');
          OutStr('  long @@@'+sSignLabel);
        End
        Else OutStr('  IF_C jmp #'+sSignLabel);
        OutStr('  mov __temp3,#"0"');  // if leading digit then use zeros for rest of digits
      End;
      OutStr('  mov __temp5,#"0"');
      OutStr(NewLabelStr);
      OutStr('  cmpsub __temp2,_1000000000 WC, WR');
      OutStr('  IF_C adds __temp5,#1');

      If g_bLMM Then
      Begin
        OutStr('  IF_C jmp #_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  IF_C jmp #'+LabelStr);
      If lOption In [1, 3, 5, 7] Then OutStr(sSignLabel);

      If g_apoCmdVars[2]^.eGetType = e_LongVar Then
      Begin
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,__temp1');
          OutStr('  add __temp1,#1');
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  movd $+2,__temp1');
          OutStr('  add __temp1,#1');
        End;
        OutStr('  mov 0-0,__temp5');
      End
      Else
      Begin
        OutStr('  wrbyte __temp5,__temp1');
        OutStr('  add __temp1,#1');
      End;
    End;

    sLabel:=NewLabelStr;

    OutStr(sLabel); // next loop label

    // __temp5 = character
    OutStr('  mov __temp5,__temp3');

    OutStr(NewLabelStr); // increment label

    // Subtraction value
    Case lDigits of
      1: OutStr('  cmpsub __temp2,#1 WC, WR');
      2: OutStr('  cmpsub __temp2,#10 WC, WR');
      3: OutStr('  cmpsub __temp2,#100 WC, WR');
      4: Begin
           poLongConst:=g_oDevice.pGetVarPtr('_1000');
           If poLongConst = Nil Then g_oDevice.pAddLongConst('_1000', 1000, '1000');
           OutStr('  cmpsub __temp2,_1000 WC, WR');
         End;
      5: Begin
           poLongConst:=g_oDevice.pGetVarPtr('_10000');
           If poLongConst = Nil Then g_oDevice.pAddLongConst('_10000', 10000, '10000');
           OutStr('  cmpsub __temp2,_10000 WC, WR');
         End;
      6: Begin
           poLongConst:=g_oDevice.pGetVarPtr('_100000');
           If poLongConst = Nil Then g_oDevice.pAddLongConst('_100000', 100000, '100000');
           OutStr('  cmpsub __temp2,_100000 WC, WR');
         End;
      7: Begin
           poLongConst:=g_oDevice.pGetVarPtr('_1000000');
           If poLongConst = Nil Then g_oDevice.pAddLongConst('_1000000', 1000000, '1000000');
           OutStr('  cmpsub __temp2,_1000000 WC, WR');
         End;
      8: Begin
           poLongConst:=g_oDevice.pGetVarPtr('_10000000');
           If poLongConst = Nil Then g_oDevice.pAddLongConst('_10000000', 10000000, '10000000');
           OutStr('  cmpsub __temp2,_10000000 WC, WR');
         End;
      9: Begin
           poLongConst:=g_oDevice.pGetVarPtr('_100000000');
           If poLongConst = Nil Then g_oDevice.pAddLongConst('_100000000', 100000000, '100000000');
           OutStr('  cmpsub __temp2,_100000000 WC, WR');
         End;
      Else Error(c_iErrorInvalidParameter, 7);
    End; // Case

    If lOption In [1, 3, 5, 7] Then
    Begin // Leading spaces
      OutStr('  cmp __temp4,#1 WZ');          // Is this the last digit ?
      OutStr('  IF_C  mov __temp3,#"0"');     // If digit was greater than zero, start at zero
      OutStr('  IF_C_OR_Z min __temp5,#"0"'); // If last digit or a non-zero digit, use "0" from now on
    End;
    OutStr('  IF_C adds __temp5,#1');

    If g_bLMM Then
    Begin
      OutStr('  IF_C jmp #_LMM_JUMP');
      OutStr('  long @@@'+LabelStr);
    End
    Else OutStr('  IF_C jmp #'+LabelStr);

    // Store character in __temp5 at address __temp1
    Begin
      OutStr('  wrbyte __temp5,__temp1');
      OutStr('  add __temp1,#1');
    End;

    // Multiply __temp2 by 10
    OutStr('  mov __temp5,__temp2');
    OutStr('  shl __temp2,#2'); // * 4
    OutStr('  add __temp2,__temp5'); // * 5
    OutStr('  shl __temp2,#1'); // * 10

    If g_bLMM Then
    Begin
      OutStr('  djnz __temp4,#_LMM_JUMP');
      OutStr('  long @@@'+sLabel);
    End
    Else OutStr('  djnz __temp4,#'+sLabel);
    If lOption < 4 Then OutStr('  wrbyte __temp4,__temp1');
    g_bHandled:=True;
  End
  Else
  Begin
    If g_iCmdCnt < iPlace+2 Then Error(c_iErrorInvalidNumberOfParameters, 0) Else Error(c_iErrorInvalidParameter, iPlace);
  End;  
End;


Procedure ProcessLetString;
Var
  iCmdPlace: Integer;
Begin
  ProcessHubAddrLit(2, '__temp1', 1);
  iCmdPlace:=3;
  If g_asCmdLine[iCmdPlace] <> '=' Then Error(c_iErrorSyntaxError, 0)
  Else
  Begin
    Inc(iCmdPlace);
    If Upper(g_asCmdLine[iCmdPlace]) = 'STR' Then ProcessLetStr
    Else If Upper(g_asCmdLine[iCmdPlace]) = 'LEFT' Then
    Begin
      Inc(iCmdPlace); // point to source string
      If g_apoCmdVars[iCmdPlace]^.eGetType In [e_HubByte, e_DataLabel] Then
      Begin
        OutStr('  mov __temp2,__'+g_apoCmdVars[iCmdPlace]^.sGetName+'_adr');
        Inc(iCmdPlace, 2); // Point to count
        Case g_apoCmdVars[iCmdPlace]^.eGetType of
          e_ShortConst: OutStr('  mov __temp4,#'+g_apoCmdVars[iCmdPlace]^.sGetName);
          e_LongVar: OutStr('  mov __temp4,'+g_apoCmdVars[iCmdPlace]^.sGetName);
          Else Error(c_iErrorInvalidParameter, iCmdPlace);
        End; // Case
        OutStr(NewLabelStr);
        OutStr('  rdbyte __temp3, __temp2');
        OutStr('  wrbyte __temp3, __temp1');
        OutStr('  add __temp1,#1');
        OutStr('  add __temp2,#1');
        If g_bLMM Then
        Begin
          OutStr('  sub __temp4,#1 WZ');
          OutStr('  IF_NZ jmp #_LMM_JUMP');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  djnz __temp4,#'+LabelStr);
        OutStr('  wrbyte __temp4,__temp1');
      End
      Else Error(c_iErrorInvalidParameter, iCmdPlace);
      g_bHandled:=True;
    End
    Else If Upper(g_asCmdLine[iCmdPlace]) = 'RIGHT' Then
    Begin
      Inc(iCmdPlace); // point to source string
      If g_apoCmdVars[iCmdPlace]^.eGetType In [e_HubByte, e_DataLabel] Then
      Begin
        OutStr('  mov __temp2,__'+g_apoCmdVars[iCmdPlace]^.sGetName+'_adr');
        // Emit code to find end of string
        OutStr(NewLabelStr);
        OutStr('  rdbyte __temp3,__temp2 WZ');
        OutStr('  add __temp2,#1');
        If g_bLMM Then
        Begin
          OutStr('  IF_NZ jmp #_LMM_JUMP');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  IF_NZ jmp #'+LabelStr);
        // __param2 points 1 place past zero
        Inc(iCmdPlace, 2); // Point to count
        Case g_apoCmdVars[iCmdPlace]^.eGetType of
          e_ShortConst: OutStr('  sub __temp2,#('+g_apoCmdVars[iCmdPlace]^.sGetName+'+1)');
          e_LongVar:
            Begin
              OutStr('  sub __temp2,'+g_apoCmdVars[iCmdPlace]^.sGetName);
              OutStr('  sub __temp2,#1');
            End;
          Else Error(c_iErrorInvalidParameter, iCmdPlace);
        End; // Case
        OutStr(NewLabelStr);
        OutStr('  rdbyte __temp3, __temp2 WZ');
        OutStr('  wrbyte __temp3, __temp1');
        OutStr('  add __temp1,#1');
        OutStr('  add __temp2,#1');
        If g_bLMM Then
        Begin
          OutStr('  IF_NZ jmp #_LMM_JUMP');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  IF_NZ jmp #'+LabelStr);
      End
      Else Error(c_iErrorInvalidParameter, iCmdPlace);
    End
    Else If Upper(g_asCmdLine[iCmdPlace]) = 'MID' Then
    Begin
      Inc(iCmdPlace); // point to source string
      If g_apoCmdVars[iCmdPlace]^.eGetType In [e_HubByte, e_DataLabel] Then
      Begin
        OutStr('  mov __temp2,__'+g_apoCmdVars[iCmdPlace]^.sGetName+'_adr');
        Inc(iCmdPlace, 2); // Point to offset
        Case g_apoCmdVars[iCmdPlace]^.eGetType of
          e_ShortConst: OutStr('  adds __temp2,#('+g_apoCmdVars[iCmdPlace]^.sGetName+'-1)');
          e_LongVar:
            Begin
              OutStr('  add __temp2,'+g_apoCmdVars[iCmdPlace]^.sGetName);
              OutStr('  sub __temp2,#1');
            End;
          Else Error(c_iErrorInvalidParameter, iCmdPlace);
        End; // Case
        Inc(iCmdPlace, 2); // Point to count
        Case g_apoCmdVars[iCmdPlace]^.eGetType of
          e_ShortConst: OutStr('  mov __temp4,#'+g_apoCmdVars[iCmdPlace]^.sGetName);
          e_LongVar: OutStr('  mov __temp4,'+g_apoCmdVars[iCmdPlace]^.sGetName);
          Else Error(c_iErrorInvalidParameter, iCmdPlace+4);
        End; // Case

        OutStr(NewLabelStr);
        OutStr('  rdbyte __temp3, __temp2');
        OutStr('  wrbyte __temp3, __temp1');
        OutStr('  add __temp1,#1');
        OutStr('  add __temp2,#1');
        If g_bLMM Then
        Begin
          OutStr('  sub __temp4,#1 WZ');
          OutStr('  IF_NZ jmp #_LMM_JUMP');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  djnz __temp4,#'+LabelStr);
        OutStr('  wrbyte __temp4,__temp1');
      End
      Else Error(c_iErrorInvalidParameter, iCmdPlace);
      g_bHandled:=True;
    End
    Else If g_apoCmdVars[iCmdPlace]^.eGetType = e_ShortConst Then
    Begin
      If g_apoCmdVars[iCmdPlace]^.lGetValue <> 0 Then
      Begin
        OutStr('  mov __temp3,#'+g_apoCmdVars[iCmdPlace]^.sGetName);
        OutStr('  wrbyte __temp3,__temp1');
        OutStr('  add __temp1,#1');
      End;
      OutStr('  mov __temp3,#0');
      OutStr('  wrbyte __temp3,__temp1');
      Inc(iCmdPlace);
    End
    Else If g_apoCmdVars[iCmdPlace]^.eGetType In [e_HubByte, e_DataLabel] Then
    Begin
      OutStr('  mov __temp2,__'+g_apoCmdVars[iCmdPlace]^.sGetName+'_adr');
      Inc(iCmdPlace);

      OutStr(NewLabelStr);
      OutStr('  rdbyte __temp3, __temp2 WZ');
      OutStr('  wrbyte __temp3, __temp1');
      OutStr('  add __temp1,#1');
      OutStr('  add __temp2,#1');
      If g_bLMM Then
      Begin
        OutStr('  IF_NZ jmp #_LMM_JUMP');
        OutStr('  long @@@'+LabelStr);
      End
      Else OutStr('  IF_NZ jmp #'+LabelStr);
    End
    Else If g_apoCmdVars[iCmdPlace]^.eGetType = e_ShortConst Then
    Begin
      OutStr('  mov __temp3,#'+g_apoCmdVars[iCmdPlace]^.sGetName);
      OutStr('  wrbyte __temp3, __temp1');
      OutStr('  add __temp1,#1');
      OutStr('  mov __temp3,#0');
      OutStr('  wrbyte __temp3,__temp1');
    End
    Else Error(c_iErrorInvalidParameter, iCmdPlace);

    If iCmdPlace < g_iCmdCnt Then
    Begin // Process myStr = str + str
      If g_asCmdLine[iCmdPlace] = '+' Then
      Begin
        OutStr('  sub __temp1,#1');
        Inc(iCmdPlace);
        If Upper(g_asCmdLine[iCmdPlace]) = Upper(g_asCmdLine[2]) Then Error(c_iErrorInvalidParameter, iCmdPlace); // cannot do "str1 = xxx + str1"
        If g_apoCmdVars[iCmdPlace]^.eGetType = e_ShortConst Then
        Begin
          OutStr('  mov __temp3,#'+g_apoCmdVars[iCmdPlace]^.sGetName);
          OutStr('  wrbyte __temp3,__temp1');
          OutStr('  add __temp1,#1');
          OutStr('  mov __temp3,#0');
          OutStr('  wrbyte __temp3,__temp1');
        End
        Else If g_apoCmdVars[iCmdPlace]^.eGetType = e_LongVar Then
        Begin
          OutStr('  wrbyte '+g_apoCmdVars[iCmdPlace]^.sGetIndexName +',__temp1');
          OutStr('  add __temp1,#1');
          OutStr('  mov __temp3,#0');
          OutStr('  wrbyte __temp3,__temp1');
        End
        Else If g_apoCmdVars[iCmdPlace]^.eGetType In [e_HubByte, e_DataLabel] Then
        Begin
          OutStr('  mov __temp2,__'+g_apoCmdVars[iCmdPlace]^.sGetName+'_adr');
//          Inc(iCmdPlace);

          OutStr(NewLabelStr);
          OutStr('  rdbyte __temp3, __temp2 WZ');
          OutStr('  wrbyte __temp3, __temp1');
          OutStr('  add __temp1,#1');
          OutStr('  add __temp2,#1');
          If g_bLMM Then
          Begin
            OutStr('  IF_NZ jmp #_LMM_JUMP');
            OutStr('  long @@@'+LabelStr);
          End
          Else OutStr('  IF_NZ jmp #'+LabelStr);
        End
        Else Error(c_iErrorInvalidParameter, iCmdPlace);
      End; // If g_asCmdLine[iCmdPlace] = '+'
      g_bHandled:=True;
    End
    Else g_bHandled:=True;
  End; // [3] <> '='
End;

End.
