Unit _VAR;

{$MODE Delphi}

Interface

Uses GLOBAL;

Function IsInvalid(pv_sGiven: String): Boolean;

Procedure ProcessVar;
Procedure ProcessHub;
Procedure ProcessPin;
Procedure ProcessCon;
Procedure ProcessSub_Declare;
Procedure ProcessFunc_Declare;
Procedure ProcessSub_Define;
Procedure ProcessFunc_Define;
Procedure ProcessEndSub;
Procedure ProcessEndFunc;

Implementation

Const
  c_iInvalidCnt = 309;

Var
  asInvalidNames: Array[1..c_iInvalidCnt] of String[20] = (
'__INIT','__RAM','__TEMP1','__TEMP2','__TEMP3','__TEMP4','__TEMP5',
'__PARAM1','__PARAM2','__PARAM3','__PARAM4','__PARAMCNT','_CLKFREQ',
'_CLKMODE','_FREE','_STACK','_XINFREQ','-','&','&~','*','/','//','^',
'|','+','<<','>>','ABORT','ABS','ABSNEG','ADD','ADDRESS','ADDS','ADDSX',
'ADDX','AND','ANDN','ASM','BIT','BRANCH','BREAK','BYTE','BYTEFILL','BYTEMOVE',
'CALL','CASE','CHIPVER','CLKFREQ','CLKMODE','CLKSET','CMP','CMPS','CMPSUB',
'CMPSX','CMPX','CNT','COGID','COGINIT','COGNEW','COGSTOP','CON','COMPARE','CON',
'CONSTANT','COUNT','COUNTERA','COUNTERB','CTRA','CTRB','DAT','DATA','DEC',
'DEVICE','DIRA','DIRB','DJNZ','DO','ELSE','ELSEIF','ELSEIFNOT','ENC','END',
'ENDASM','ENDFUNC','ENDIF','ENDSUB','ENDTASK','EXIT','FALSE','FILE','FIT',
'FLOAT','FOR','FREQ','FREQOUT','FROM','FRQA','FRQB','FUNC','GOSUB','GOTO',
'HIGH','HUB','HUBOP','I2CREAD','I2CWRITE','I2CSPEED','I2CSTART','I2CSTOP',
'IF','IFNOT','IF_A','IF_AE','IF_ALWAYS','IF_B','IF_BE','IF_C','IF_C_AND_NZ',
'IF_C_AND_Z','IF_C_EQ_Z','IF_C_NE_Z','IF_C_OR_Z','IF_C_OR_NZ','IF_E','IF_NC',
'IF_NC_AND_NZ','IF_NC_AND_Z','IF_NC_OR_NZ','IF_NC_OR_Z','IF_NE','IF_NEVER',
'IF_NZ','IF_NZ_AND_C','IF_NZ_AND_NC','IF_NZ_OR_C','IF_NZ_OR_NC','IF_Z',
'IF_Z_AND_C','IF_Z_AND_NC','IF_Z_EQ_C','IF_Z_NE_C','IF_Z_OR_C','IF_Z_OR_NC',
'INA','INB','INC','INCLUDE','INIT','INPUT','JMP','JMPRET','LEFT','LEN',
'LET','LOAD','LOCKCLR','LOCKNEW','LOCKRET','LOCKSET','LONG','LONGFILL',
'LONGMOVE','LOOKDOWN','LOOKUP','LOOKUPZ','LOOP','LOW','MAX','MAXS','MID',
'MIN','MINS','MOV','MOVD','MOVI','MOVS','MUL','MULS','MUXC','MUXNC','MUXNZ',
'MUXZ','NEG','NEGC','NEGNC','NEGNZ','NEGX','NEGZ','NEVER','NEXT','NOCODE',
'NOP','NOT','NR','OBJ','ON','ONES','OR','OTHER','OUTA','OUTB','OUTPUT',
'OWREAD','OWRESET','OWWRITE','P8X32A','PAR','PAUSE','PAUSEUS','PHSA','PHSB',
'PI','PIN','PLL1X','PLL2X','PLL4X','PLL8X','PLL16X','POSX','PRI','PROGRAM',
'PUB','PULSIN','PULSOUT','QUIT','RANDOM','RCFAST','RCL','RCR','RCSLOW',
'RCTIME','RDBYTE','RDSBYTE','RDLONG','RDSWORD','RDWORD','REBOOT','REM',
'REPEAT','RES','RESULT','RET','RETURN','REV','REVERSE','RIGHT','ROL','ROR',
'ROUND','SAR','SERIN','SEROUT','SGN','SHIFTIN','SHIFTOUT','SHL','SHR','SOUND',
'SPR','STACK','STEP','STR','STRCOMP','STRING','STRSIZE','SUB','SUBABS','SUBS',
'SUBSX','SUBX','SUMC','SUMNC','SUMNZ','SUMZ','TASK','TEST','THEN','TJNZ',
'TJZ','TO','TOGGLE','TRUE','TRUNC','UNTIL','VAL','VAR','VCFG','VSCL','WAITCNT',
'WAITPEQ','WAITPNE','WAITVID','WATCH','WC','WHILE','WORD','WORDFILL','WORDMOVE',
'WR','WRBYTE','WRLONG','WRWORD','WZ','XINPUT','XOR','XTAL1','XTAL2','XTAL3');

Function IsInvalid(pv_sGiven: String): Boolean;
Var
  i: Integer;
Begin
  i:=1;
  while (i <= c_iInvalidCnt) and (pv_sGiven <> asInvalidNames[i]) do Inc(i);
  If i > c_iInvalidCnt Then IsInvalid:=False Else IsInvalid:=True;
End;


Procedure ProcessVar;
Var
  poLongVar: pLongVarObj;
  poVar: PVarObj;
  I, J: LongInt;
Begin
  poLongVar:=Nil;
  // Check for invalid variable names
  If Not IsInvalid(Upper(g_asCmdLine[1])) Then
  Begin
    If Upper(g_asCmdLine[3]) = 'LONG' Then
    Begin
      If (g_iCmdCnt = 3) or ((g_iCmdCnt = 5) and (g_asCmdLine[4] = '=')) or
          ((g_iCmdCnt = 6) and (g_asCmdLine[4] = '(') and (g_asCmdLine[6] = ')')) Then
      Begin
        If g_iCmdCnt = 6 Then
        Begin // Array
          If g_apoCmdVars[5]^.eGetType = e_ShortConst Then
          Begin
            poLongVar:=g_oDevice.pAddLongVar(g_asCmdLine[1], False, '0');
            If poLongVar <> Nil Then poLongVar^.m_lElements:=g_apoCmdVars[5]^.lGetValue;
          End
          Else Error(c_iErrorInvalidParameter, 5);
        End
        Else poLongVar:=g_oDevice.pAddLongVar(g_asCmdLine[1], False, '0');
        OutSpin(''' '+g_sOrigLine);
        If g_iCmdCnt = 5 Then
        Begin
          If g_apoCmdVars[5]^.eGetType In [e_ShortConst, e_LongConst] Then
          Begin
            If poLongVar <> Nil Then
            Begin
              poLongVar^.m_lValue:=g_apoCmdVars[5]^.lGetValue;
              poLongVar^.m_bHasInitValue:=True;
              poLongVar^.m_sFormat:=g_asCmdLine[5];
              If g_apoCmdVars[5]^.eGetType = e_LongConst Then poLongVar^.m_sFormat:=IntStr(poLongVar^.m_lValue);
            End;
            Begin
              // Delete any longconst vars that where created for this line of code
              For I:=g_oDevice.m_oVars.m_iLastVarCnt+1 to g_oDevice.m_oVars.m_iVarCnt Do
              Begin
                If g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_LongConst Then
                Begin
                  Dispose(g_oDevice.m_oVars.m_apoVars[I], Done);
                  Dec(g_oDevice.m_oVars.m_iVarCnt);
                  For J:=I To g_oDevice.m_oVars.m_iVarCnt Do g_oDevice.m_oVars.m_apoVars[I] := g_oDevice.m_oVars.m_apoVars[I+1];
                End;
              End;
            End;
          End
          Else Error(c_iErrorConstantExpected, 5);
        End;
        g_bHandled:=True;
      End;
    End
    Else If g_apoCmdVars[3]^.eGetType = e_LongVar Then
    Begin // alias
      OutSpin(''' '+g_sOrigLine);
      poLongVar:=g_oDevice.pAddLongVar(g_asCmdLine[1], False, '0');
      If poLongVar <> Nil Then  // IF duplicate variable name, pointer will be nil
      Begin
        poLongVar^.m_lValue:=g_apoCmdVars[3]^.m_lValue;
        poLongVar^.m_lHubOffset:=g_apoCmdVars[3]^.m_lHubOffset;
        poLongVar^.m_lElements:=g_apoCmdVars[3]^.m_lElements;
        poLongVar^.m_byPinNumber:=g_apoCmdVars[3]^.m_byPinNumber;
        poLongVar^.m_bHubAbs:=g_apoCmdVars[3]^.m_bHubAbs;
        poLongVar^.m_sFormat:=g_apoCmdVars[3]^.m_sFormat;
        poLongVar^.m_bVarIndex:=False;

        poLongVar^.m_sIndex:='';
        If g_apoCmdVars[3]^.m_sIndex <> '' Then
        Begin
          If g_apoCmdVars[3]^.m_sIndex[1] In ['0'..'9'] Then
          Begin
            poLongVar^.m_sIndex:=g_apoCmdVars[3]^.m_sIndex;
          End
          Else
          Begin
            poVar:=g_oDevice.pGetVarPtr(g_apoCmdVars[3]^.m_sIndex);
            If poVar <> Nil Then poLongVar^.m_sIndex:=IntStr(poVar^.lGetValue);
          End;
        End;

        poLongVar^.m_sAlias:=g_apoCmdVars[3]^.m_sAlias;
        If g_apoCmdVars[3]^.m_bVarIndex Then Error(c_iErrorSyntaxError, 0);
      End;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 3);
  End;
End;


Procedure ProcessHub;
Var
  poVar: PVarObj;
  lElements: LongInt;
  lValue: LongInt;
  sValue: String;
  I, J: LongInt;
Begin
  // Check for invalid variable names
  If Not IsInvalid(Upper(g_asCmdLine[1])) Then
  Begin
    lElements:=1; // Assume 1 element
    lValue:=0;    // Assume value of zero
    sValue:='';   // Assume null string for HUB STRING types

    // Check if an array
    If g_iCmdCnt In [6, 8] Then // "name HUB size ( elements )" or "name HUB size ( elements ) = value"
    Begin
      Case g_apoCmdVars[5]^.eGetType of
        e_ShortConst, e_LongConst: lElements:=g_apoCmdVars[5]^.lGetValue;
        Else Error(c_iErrorInvalidParameter, 5);
      End;
      If (g_asCmdLine[4] <> '(') or (g_asCmdLine[6] <> ')') Then  Error(c_iErrorSyntaxError, 0);

      If g_iCmdCnt = 8 Then
      Begin
        Case g_apoCmdVars[8]^.eGetType of
          e_ShortConst: lValue:=g_apoCmdVars[8]^.lGetValue;
          e_LongConst: lValue:=g_apoCmdVars[8]^.lGetValue;
          e_DataLabel:
            Begin
              If (g_asCmdLine[8, 1] = '"') and (g_asCmdLine[8, Length(g_asCmdLine[8])] = '"') Then sValue:=g_asCmdLine[8];
              If Length(sValue)-2 > lElements Then Error(c_iErrorInvalidParameter, 8);
              If Length(sValue)-2 < lElements Then sValue:=sValue + ',0['+IntStr(lElements - Length(sValue) + 3)+']';
            End;
        End;
        If g_asCmdLine[7] <> '=' Then Error(c_iErrorSyntaxError, 0);
      End;
    End;

    If g_iCmdCnt = 5 Then // "name HUB size = value"
    Begin
        Case g_apoCmdVars[5]^.eGetType of
          e_ShortConst: lValue:=g_apoCmdVars[5]^.lGetValue;
          e_LongConst: lValue:=g_apoCmdVars[5]^.lGetValue;
        End;
      If g_asCmdLine[4] <> '=' Then Error(c_iErrorSyntaxError, 0);
    End;

    // Delete any longconst vars or Stings that where created for this line of code
    I:=g_oDevice.m_oVars.m_iLastVarCnt + 1;
    While I <= g_oDevice.m_oVars.m_iVarCnt Do
    Begin
      If (g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_LongConst) or
         (g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_DataLabel) or
         (g_oDevice.m_oVars.m_apoVars[I]^.eGetType = e_ByteData) Then
      Begin
        Dispose(g_oDevice.m_oVars.m_apoVars[I], Done);
        Dec(g_oDevice.m_oVars.m_iVarCnt);
        For J:=I To g_oDevice.m_oVars.m_iVarCnt Do g_oDevice.m_oVars.m_apoVars[J]:=g_oDevice.m_oVars.m_apoVars[J+1];
      End
      Else Inc(I);
    End;

    If Upper(g_asCmdLine[3]) = 'BYTE' Then
    Begin
      g_oDevice.pAddHubByte(g_asCmdLine[1], g_lHubOffset, lElements, lValue);
      Inc(g_lHubOffset, lElements);
      If lElements = 1 Then OutSpin(''' HUB BYTE '+g_asCmdLine[1])
      Else OutSpin(''' HUB BYTE '+g_asCmdLine[1]+'('+IntStr(lElements)+')');
      g_bHandled:=True;
    End
    Else If Upper(g_asCmdLine[3]) = 'STRING' Then
    Begin
      If g_iCmdCnt = 3 Then Error(c_iErrorSyntaxError, 0);
      poVar:=g_oDevice.pAddHubByte(g_asCmdLine[1], g_lHubOffset, lElements+1, lValue);
      poVar^.m_sValue:=sValue;
      Inc(g_lHubOffset, lElements+1);
      If lElements = 1 Then OutSpin(''' HUB STRING '+g_asCmdLine[1])
      Else
      Begin
        If sValue = '' Then OutSpin(''' HUB STRING '+g_asCmdLine[1]+'('+IntStr(lElements)+')')
         Else OutSpin(''' HUB STRING '+g_asCmdLine[1]+'('+IntStr(lElements)+') = '+sValue);
      End;
      g_bHandled:=True;
    End
    Else If Upper(g_asCmdLine[3]) = 'WORD' Then
    Begin
      g_lHubOffset:=(g_lHubOffset + 1) and $FFFFFFFE;
      g_oDevice.pAddHubWord(g_asCmdLine[1], g_lHubOffset, lElements, lValue);
      Inc(g_lHubOffset, lElements*2);
      If lElements = 1 Then OutSpin (''' HUB WORD '+g_asCmdLine[1])
      Else OutSpin(''' HUB WORD '+g_asCmdLine[1]+'('+IntStr(lElements)+')');
      g_bHandled:=True;
    End
    Else If Upper(g_asCmdLine[3]) = 'LONG' Then
    Begin
      g_lHubOffset:=(g_lHubOffset + 3) and $FFFFFFFC;
      g_oDevice.pAddHubLong(g_asCmdLine[1], g_lHubOffset, lElements, lValue);
      Inc(g_lHubOffset, lElements*4);
      If lElements = 1 Then OutSpin(''' HUB LONG '+g_asCmdLine[1])
      Else OutSpin(''' HUB LONG '+g_asCmdLine[1]+'('+IntStr(lElements)+')');
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 4);
  End;
End;


Procedure ProcessPin;
Var
  poPin: PPinObj;
Begin
  // Check for invalid variable names
  If IsInvalid(Upper(g_asCmdLine[1])) Then Error(c_iErrorInvalidVariableName, 1)
  Else
  Begin
    If g_iCmdCnt = 2 Then
     Error(c_iErrorSyntaxError, 0)
    Else If g_iCmdCnt In [3, 4] Then
    Begin
      If g_apoCmdVars[3]^.eGetType = e_ShortConst Then
      Begin
        If g_apoCmdVars[3]^.lGetValue > g_iMaxPin Then Error(c_iErrorInvalidParameter, 3);
        g_oDevice.pAddPin(g_asCmdLine[1], g_apoCmdVars[3]^.lGetValue);
        OutSpin(''' '+g_sOrigLine);
        g_bHandled:=True;
        If g_iCmdCnt = 4 Then
        Begin  // TO, INPUT, OUTPUT, HIGH, LOW
          If Upper(g_asCmdLine[4]) = 'INPUT' Then
          Begin

          End
          Else If (Upper(g_asCmdLine[4]) = 'OUTPUT') or (Upper(g_asCmdLine[4]) = 'LOW') Then
          Begin
            If g_bIsTask Then
            Begin
              If g_apoCmdVars[3]^.lGetValue < 32 Then g_lTaskInitDirA:=g_lTaskInitDirA OR (1 SHL g_apoCmdVars[3]^.lGetValue)
               Else g_lTaskInitDirB:=g_lTaskInitDirB OR (1 SHL g_apoCmdVars[3]^.lGetValue);
            End
            Else
            Begin
              If g_apoCmdVars[3]^.lGetValue < 32 Then g_lProgInitDirA:=g_lProgInitDirA OR (1 SHL g_apoCmdVars[3]^.lGetValue)
               Else g_lProgInitDirB:=g_lProgInitDirB OR (1 SHL g_apoCmdVars[3]^.lGetValue);
            End;
          End
          Else If Upper(g_asCmdLine[4]) = 'HIGH' Then
          Begin
            If g_bIsTask Then
            Begin
              Case g_apoCmdVars[3]^.lGetValue of
                0..31: Begin
                         g_lTaskInitDirA:=g_lTaskInitDirA OR (1 SHL g_apoCmdVars[3]^.lGetValue);
                         g_lTaskInitOutA:=g_lTaskInitOutA OR (1 SHL g_apoCmdVars[3]^.lGetValue);
                       End;
                32..63: Begin
                          g_lTaskInitDirB:=g_lTaskInitDirB OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 32));
                          g_lTaskInitPinB:=g_lTaskInitPinB OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 32));
                        End;
                64..95: Begin
                          g_lTaskInitDirC:=g_lTaskInitDirC OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 64));
                          g_lTaskInitPinC:=g_lTaskInitPinC OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 64));
                        End;
                96..127: Begin
                           g_lTaskInitDirD:=g_lTaskInitDirD OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 96));
                           g_lTaskInitPinD:=g_lTaskInitPinD OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 96));
                         End;
              End; // case
            End
            Else
            Begin
              Case g_apoCmdVars[3]^.lGetValue of
                0..31: Begin
                         g_lProgInitDirA:=g_lProgInitDirA OR (1 SHL g_apoCmdVars[3]^.lGetValue);
                         g_lProgInitOutA:=g_lProgInitOutA OR (1 SHL g_apoCmdVars[3]^.lGetValue);
                       End;
                32..63: Begin
                          g_lProgInitDirB:=g_lProgInitDirB OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 32));
                          g_lProgInitPinB:=g_lProgInitPinB OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 32));
                        End;
                64..95: Begin
                          g_lProgInitDirC:=g_lProgInitDirC OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 64));
                          g_lProgInitPinC:=g_lProgInitPinC OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 64));
                        End;
                96..127: Begin
                           g_lProgInitDirD:=g_lProgInitDirD OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 96));
                           g_lProgInitPinD:=g_lProgInitPinD OR (1 SHL (g_apoCmdVars[3]^.lGetValue - 96));
                         End;
              End; // case           
            End;
          End
          Else Error(c_iErrorInvalidParameter, 4);
        End;
      End
      Else Error(c_iErrorInvalidParameter, 3);
      g_bHandled:=True;
    End
    Else If g_iCmdCnt In [6, 7] Then
    Begin // Pin group "name PIN x..y"
      If (g_asCmdLine[4] = '.') and (g_asCmdLine[5] = '.') Then
      Begin
        If g_apoCmdVars[3]^.lGetValue > g_iMaxPin Then Error(c_iErrorInvalidParameter, 3);
        If (g_apoCmdVars[6]^.lGetValue > g_iMaxPin) or
           ((g_apoCmdVars[3]^.lGetValue Div 32) <> (g_apoCmdVars[6]^.lGetValue Div 32)) Then Error(c_iErrorInvalidParameter, 6);
        poPin:=g_oDevice.pAddPinGroup(g_asCmdLine[1], g_apoCmdVars[3]^.lGetValue, g_apoCmdVars[6]^.lGetValue);
        OutSpin(''' '+g_sOrigLine);
        g_bHandled:=True;
        If g_iCmdCnt = 7 Then
        Begin  // INPUT, OUTPUT, HIGH, LOW
          If Upper(g_asCmdLine[7]) = 'INPUT' Then
          Begin

          End
          Else If (Upper(g_asCmdLine[7]) = 'OUTPUT') or (Upper(g_asCmdLine[7]) = 'LOW') Then
          Begin
            If g_bIsTask Then
            Begin
              If poPin^.m_bIOA Then g_lTaskInitDirA:=g_lTaskInitDirA OR poPin^.lGetValue
               Else g_lTaskInitDirB:=g_lTaskInitDirB OR poPin^.lGetValue;
            End
            Else
            Begin
              If poPin^.m_bIOA Then g_lProgInitDirA:=g_lProgInitDirA OR poPin^.lGetValue
               Else g_lProgInitDirB:=g_lProgInitDirB OR poPin^.lGetValue;
            End;
          End
          Else If Upper(g_asCmdLine[7]) = 'HIGH' Then
          Begin
            If g_bIsTask Then
            Begin
              If poPin^.m_bIOA Then
              Begin
                g_lTaskInitDirA:=g_lTaskInitDirA OR poPin^.lGetValue;
                g_lTaskInitOutA:=g_lTaskInitDirA OR poPin^.lGetValue;
              End
              Else
              Begin
                g_lTaskInitDirA:=g_lTaskInitDirB OR poPin^.lGetValue;
                g_lTaskInitOutA:=g_lTaskInitDirB OR poPin^.lGetValue;
              End;
            End
            Else
            Begin
              If poPin^.m_bIOA Then
              Begin
                g_lProgInitDirA:=g_lProgInitDirA OR poPin^.lGetValue;
                g_lProgInitOutA:=g_lProgInitDirA OR poPin^.lGetValue;
              End
              Else
              Begin
                g_lProgInitDirA:=g_lProgInitDirB OR poPin^.lGetValue;
                g_lProgInitOutA:=g_lProgInitDirB OR poPin^.lGetValue;
              End;
            End
          End
          Else If Upper(g_asCmdLine[7]) = 'LOW' Then
          Begin
          End
          Else Error(c_iErrorInvalidParameter, 7);
        End;
      End
      Else Error(c_iErrorInvalidParameter, 4);
    End
    Else Error(c_iErrorSyntaxError, 0);
  End;
End;


Procedure ProcessCon;
Var
  sInStr: String;
  I: Integer;
  poVar: PVarObj;
Begin
  // Check for invalid variable names
  If IsInvalid(Upper(g_asCmdLine[1])) Then Error(c_iErrorInvalidVariableName, 1)
  Else
  Begin
    If g_iCmdCnt = 2 Then Error(c_iErrorSyntaxError, 0);
    If g_iCmdCnt = 3 Then
    Begin
      If (g_asCmdLine[3,1] = '"') and (Length(g_asCmdLine[3]) > 3) Then
      Begin // Literal
        AddLiteral(g_asCmdLine[1], Copy(g_asCmdLine[3], 2, Length(g_asCmdLine[3])-2));
        OutSpin(''' '+PadStr(g_asCmdLine[1], 12)+'  =  '+g_asCmdLine[3]);
        g_bHandled:=True;
      End
      Else
      Begin
        If g_apoCmdVars[3]^.eGetType = e_LongConst Then
        Begin
          g_oDevice.pAddLongConst(g_asCmdLine[1], g_apoCmdVars[3]^.lGetValue, g_asCmdLine[3]);
          OutSpin(''' '+g_sOrigLine);
          g_bHandled:=True;
        End
        Else If g_apoCmdVars[3]^.eGetType = e_ShortConst Then
        Begin
          poVar:=g_oDevice.pAddShortConst(g_asCmdLine[1], g_apoCmdVars[3]^.lGetValue, g_asCmdLine[3]);
          If g_bIsTask Then poVar^.m_sTaskPrefix:=g_sTaskName+'#';
          OutSpin(''' '+g_sOrigLine);
          g_bHandled:=True;
        End
        Else Error(c_iErrorInvalidParameter, 3);
      End;
    End
    Else
    Begin // Calculated constant
      OutSpin(''' '+g_sOrigLine);
      sInStr:='';
      For I:=3 To g_iCmdCnt Do
      Begin
        If (g_asCmdLine[I] <> '-') or (g_asCmdLine[I+1, 1] <> '-') Then
        Begin
          If (g_apoCmdVars[I] <> Nil) and (g_apoCmdVars[I]^.eGetType = e_LongConst) Then
          Begin
            sInStr:=sInStr + g_apoCmdVars[I]^.m_sFormat+' ';
          End
          Else sInStr:=sInStr + g_asCmdLine[I]+' ';
          If (g_apoCmdVars[I] <> Nil) and (g_apoCmdVars[I]^.eGetType = e_LongVar) Then Error(c_iErrorInvalidParameter, I);
        End;
      End;
      poVar:=g_oDevice.pAddLongVar(g_asCmdLine[1], False, '0');
      poVar^.m_lValue:=c_lComputedConstantValue; //g_apoCmdVars[5]^.lGetValue;
      poVar^.m_bHasInitValue:=True;
      poVar^.m_sFormat:=sInStr;
      g_bHandled:=True;
    End;
  End;
End;


Procedure ProcessSub_Declare;
Var
  I: Integer;
Begin
  If SubIndex(g_asCmdLine[1]) > 0 Then
  Begin
    Error(c_iErrorDuplicateVariableName, 1);
  End
  Else
  Begin
    If g_iSubroutineCnt >= (c_iMaxSubroutines-1) Then
    Begin
      Error(c_iErrorTooManySubs, 0);
    End
    Else
    Begin
      Inc(g_iSubroutineCnt);
      With g_arSubroutines[g_iSubroutineCnt] Do
      Begin
        sLabel:=Upper(g_asCmdLine[1]);
        bIsFunc:=False;
        iMinParams:=0;
        iMaxParams:=4;
        If UsesIndex(g_asCmdLine[1]) > 0 Then bUsed:=True Else bUsed:=False;
        bIsNativeInLMM:=False;
      End;
      If g_iCmdCnt > 2 Then
      Begin
        If g_apoCmdVars[3]^.eGetType = e_ShortConst Then
        Begin
          If g_apoCmdVars[3]^.lGetValue In [0..20] Then
          Begin
            g_arSubroutines[g_iSubroutineCnt].iMinParams:=g_apoCmdVars[3]^.lGetValue;
            g_arSubroutines[g_iSubroutineCnt].iMaxParams:=g_apoCmdVars[3]^.lGetValue;
          End
          Else Error(c_iErrorInvalidParameter, 3);
          If g_iCmdCnt > 4 Then
          Begin
            If g_asCmdLine[4] <> ',' Then Error(c_iErrorCommaExpected, 4);
            If g_apoCmdVars[5]^.eGetType = e_ShortConst Then
            Begin
              If g_apoCmdVars[5]^.lGetValue In [0..20] Then
              Begin
                g_arSubroutines[g_iSubroutineCnt].iMaxParams:=g_apoCmdVars[5]^.lGetValue;
              End
              Else Error(c_iErrorInvalidParameter, 5);
            End
            Else Error(c_iErrorConstantExpected, 5);
          End;
          If g_iCmdCnt > 6 Then
          Begin
            If g_asCmdLine[6] <> ',' Then Error(c_iErrorCommaExpected, 4);
            If Upper(g_asCmdLine[7]) = 'NATIVE' Then g_arSubroutines[g_iSubroutineCnt].bIsNativeInLMM:=True;
          End;
        End
        Else Error(c_iErrorConstantExpected, 3);
      End;
      OutSpin(''' ' + g_sOrigLine);

      If g_arSubroutines[g_iSubroutineCnt].iMaxParams > g_iMaxParams Then
      Begin // Create additional param variables
        For I:=g_iMaxParams+1 To g_arSubroutines[g_iSubroutineCnt].iMaxParams Do
        Begin
          g_oDevice.pAddLongVar('__param'+IntStr(I), False, '0');
        End;
        g_iMaxParams:=g_arSubroutines[g_iSubroutineCnt].iMaxParams;
      End;

      g_bHandled:=True;
    End;
  End;
End;

Procedure ProcessFunc_Declare;
Var
  I: Integer;
Begin
  If g_iSubroutineCnt = c_iMaxSubroutines Then
  Begin
    Error(c_iErrorTooManySubs, 0);
  End
  Else
  Begin
    Inc(g_iSubroutineCnt);
    If g_iSubroutineCnt < c_iMaxSubroutines Then
    Begin
      With g_arSubroutines[g_iSubroutineCnt] Do
      Begin
        sLabel:=Upper(g_asCmdLine[1]);
        bIsFunc:=True;
        iMinParams:=0;
        iMaxParams:=4;
        If UsesIndex(g_asCmdLine[1]) > 0 Then bUsed:=True Else bUsed:=False;
        bIsNativeInLMM:=False;
      End;
    End
    Else Error(c_iErrorTooManySubs, 0);
    If g_iCmdCnt > 2 Then
    Begin
      If g_apoCmdVars[3]^.eGetType = e_ShortConst Then
      Begin
        If g_apoCmdVars[3]^.lGetValue In [0..20] Then
        Begin
          If g_apoCmdVars[3]^.lGetValue >= g_arSubroutines[g_iSubroutineCnt].iMinParams Then
          Begin
            g_arSubroutines[g_iSubroutineCnt].iMinParams:=g_apoCmdVars[3]^.lGetValue;
            g_arSubroutines[g_iSubroutineCnt].iMaxParams:=g_apoCmdVars[3]^.lGetValue;
          End
          Else Error(c_iErrorInvalidParameter, 3);
        End
        Else Error(c_iErrorInvalidParameter, 3);
      End
      Else Error(c_iErrorConstantExpected, 3);
    End;

    If g_iCmdCnt > 4 Then
    Begin
      If g_asCmdLine[4] <> ',' Then Error(c_iErrorCommaExpected, 4);
      If g_apoCmdVars[5]^.eGetType = e_ShortConst Then
      Begin
        g_arSubroutines[g_iSubroutineCnt].iMaxParams:=g_apoCmdVars[5]^.lGetValue;
      End
      Else Error(c_iErrorConstantExpected, 5);
    End;
    OutSpin(''' '+g_sOrigLine);

    If g_arSubroutines[g_iSubroutineCnt].iMaxParams > g_iMaxParams Then
    Begin // Create additional param variables
      For I:=g_iMaxParams+1 To g_arSubroutines[g_iSubroutineCnt].iMaxParams Do
      Begin
        g_oDevice.pAddLongVar('__param'+IntStr(I), False, '0');
      End;
      g_iMaxParams:=g_arSubroutines[g_iSubroutineCnt].iMaxParams;
    End;

    g_bHandled:=True;
  End;
End;

Procedure ProcessSub_Define;
Var
  iSubroutinePlace: Integer;
Begin
  If Not g_bInSub Then
  Begin
    iSubroutinePlace:=1;
    While (iSubroutinePlace < g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel <> Upper(g_asCmdLine[2])) Do Inc(iSubroutinePlace);
    If (iSubroutinePlace <= g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel = Upper(g_asCmdLine[2])) Then
    Begin
      If g_arSubroutines[iSubroutinePlace].bIsFunc = True Then Error(c_iErrorInvalidParameter, 2);
      OutStr(g_asCmdLine[2]);
      g_sSubName:=g_asCmdLine[2];
      g_bInSub:=True;
      g_bIsFunc:=False;
      g_bReturnUsed:=False;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorSubOrFuncCannotBeNested, 1);
End;

Procedure ProcessFunc_Define;
Var
  iSubroutinePlace: Integer;
Begin
  If Not g_bInSub Then
  Begin
    iSubroutinePlace:=1;
    While (iSubroutinePlace < g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel <> Upper(g_asCmdLine[2])) Do Inc(iSubroutinePlace);
    If (iSubroutinePlace <= g_iSubroutineCnt) and (g_arSubroutines[iSubroutinePlace].sLabel = Upper(g_asCmdLine[2])) Then
    Begin
      If g_arSubroutines[iSubroutinePlace].bIsFunc = False Then Error(c_iErrorInvalidParameter, 2);
      OutStr(g_asCmdLine[2]);
      g_sSubName:=g_asCmdLine[2];
      g_bInSub:=True;
      g_bIsFunc:=True;
      g_bReturnUsed:=False;
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
  End
  Else Error(c_iErrorSubOrFuncCannotBeNested, 1);
End;

Procedure ProcessEndSub;
Begin
  If g_bInSub and Not g_bIsFunc Then
  Begin
    If g_bLMM Then OutStr('  jmp #_LMM_RET')
    Else
    Begin
      OutStr(g_sSubName+'_ret');
      OutStr('  ret');
    End;
    g_bReturnUsed:=True;
    OutStr('  ');
    g_bInSub:=False;
    g_bHandled:=True;
  End
  Else Error(c_iErrorNotInSubOrFunc, 1);
End;

Procedure ProcessEndFunc;
Begin
  If g_bInSub and g_bIsFunc Then
  Begin
    If Not g_bReturnUsed Then
    Begin
      Warning(c_iWarningEndFuncWithoutReturn, 1);
      If g_bLMM Then OutStr('  jmp #_LMM_RET')
      Else
      Begin
        OutStr(g_sSubName+'_ret');
        OutStr('  ret');
      End;
    End;
    OutStr('  ');
    g_bInSub:=False;
    g_bHandled:=True;
  End
  Else Error(c_iErrorNotInSubOrFunc, 1);
End;

End.
