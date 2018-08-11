Unit DEVICES;

{$MODE Delphi}

INTERFACE

Procedure DeviceP8X32A;
Procedure DeviceP2;

IMPLEMENTATION

Uses GLOBAL;

Procedure DeviceP8X32A;
Var
  sTemp: String;
  poVar: PVarObj;
Begin
  sTemp:=g_sOrigLine;
  If Pos('''', sTemp) > 0 Then sTemp:=Copy(sTemp, 1, Pos('''', sTemp)-1);
  With g_oDevice Do
  Begin
    m_oVars.Init;
    m_eType:=e_P8X32A;
    AddDirective('P8X32A');
    WriteLn(g_tfSrc);

    // The following are already included in the Propeller IDE
    poVar:=pAddLongVar('dira', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('ina', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('outa', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('cnt', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('ctra', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('ctrb', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('frqa', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('frqb', False, '0');
    poVar^.m_bPermanent:=True;

    poVar:=pAddLongVar('phsa', False, '0');
    poVar^.m_bShadowReg:=True;
    poVar^.m_bPermanent:=True;

    poVar:=pAddLongVar('phsb', False, '0');
    poVar^.m_bShadowReg:=True;
    poVar^.m_bPermanent:=True;

    poVar:=pAddLongVar('vcfg', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('vscl', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('par', False, '0');
    poVar^.m_bPermanent:=True;

    poVar:=pAddLongVar('__RAM', False, '0');
    poVar^.m_bPermanent:=True;

    poVar:=pAddHubByte('__STRING', 0, 0, 0);
    poVar^.m_bHubAbs:=True;
    poVar^.m_bPermanent:=True;

//    pAddHubByte('__HUBBYTE', 0, 0, 0);
//    pAddHubWord('__HUBWORD', 0, 0, 0);
//    pAddHubLong('__HUBLONG', 0, 0, 0);

    poVar:=pAddLongVar('__temp1', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__temp2', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__temp3', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__temp4', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__remainder', False, '0');
    poVar^.m_sAlias:='__temp4';
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__temp5', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__param1', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__param2', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__param3', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__param4', False, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddLongVar('__paramcnt', False, '0');
    poVar^.m_bPermanent:=True;
    
    poVar:=pAddHubLong('clkfreq', 0, 1, 0);
    poVar^.m_bHubAbs:=True;
    poVar^.m_bPermanent:=True;

    // PropBASIC constants
    poVar:=pAddShortConst('LSBFIRST', 0, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddShortConst('MSBFIRST', 1, '1');
    poVar^.m_bPermanent:=True;
    poVar:=pAddShortConst('MSBPRE', 0, '0');
    poVar^.m_bPermanent:=True;
    poVar:=pAddShortConst('LSBPRE', 1, '1');
    poVar^.m_bPermanent:=True;
    poVar:=pAddShortConst('MSBPOST', 2, '2');
    poVar^.m_bPermanent:=True;
    poVar:=pAddShortConst('LSBPOST', 3, '3');
    poVar^.m_bPermanent:=True;
  End; { With oDevice }
  WriteLn(g_tfSrc);
  g_bIsP1:=True;
  g_bIsP2:=False;
  g_iMaxPin:=31;
  g_bHandled:=True;
End;

Procedure DeviceP2;
Var
  sTemp: String;
  poVar: PVarObj;
Begin
  sTemp:=g_sOrigLine;
  If Pos('''', sTemp) > 0 Then sTemp:=Copy(sTemp, 1, Pos('''', sTemp)-1);
  With g_oDevice Do
  Begin
    m_oVars.Init;
    m_eType:=e_P2;
    AddDirective('P2');
    WriteLn(g_tfSrc);

    // The following are already included in the Propeller IDE
    pAddLongVar('dira', False, '0');
    pAddLongVar('ina', False, '0');
    pAddLongVar('outa', False, '0');
    pAddLongVar('dirb', False, '0');
    pAddLongVar('inb', False, '0');
    pAddLongVar('outb', False, '0');
    pAddLongVar('cnt', False, '0');
    pAddLongVar('ctra', False, '0');
    pAddLongVar('ctrb', False, '0');
    pAddLongVar('frqa', False, '0');
    pAddLongVar('frqb', False, '0');

    poVar:=pAddLongVar('phsa', False, '0');
    poVar^.m_bShadowReg:=True;

    poVar:=pAddLongVar('phsb', False, '0');
    poVar^.m_bShadowReg:=True;

    pAddLongVar('vcfg', False, '0');
    pAddLongVar('vscl', False, '0');
    pAddLongVar('par', False, '0');

    pAddLongVar('__RAM', False, '0');

    poVar:=pAddHubByte('__STRING', 0, 0, 0);
    poVar^.m_bHubAbs:=True;

//    pAddHubByte('__HUBBYTE', 0, 0, 0);
//    pAddHubWord('__HUBWORD', 0, 0, 0);
//    pAddHubLong('__HUBLONG', 0, 0, 0);


    pAddLongVar('__temp1', False, '0');
    poVar:=pAddLongVar('__remainder', False, '0');
    poVar^.m_sAlias:='__temp1';
    pAddLongVar('__temp2', False, '0');
    pAddLongVar('__temp3', False, '0');
    pAddLongVar('__temp4', False, '0');
    pAddLongVar('__temp5', False, '0');
    pAddLongVar('__param1', False, '0');
    pAddLongVar('__param2', False, '0');
    pAddLongVar('__param3', False, '0');
    pAddLongVar('__param4', False, '0');
    pAddLongVar('__paramcnt', False, '0');
    poVar:=pAddHubLong('clkfreq', 0, 1, 0);
    poVar^.m_bHubAbs:=True;

    // PropBASIC constants
    pAddShortConst('LSBFIRST', 0, '0');
    pAddShortConst('MSBFIRST', 1, '1');
    pAddShortConst('MSBPRE', 0, '0');
    pAddShortConst('LSBPRE', 1, '1');
    pAddShortConst('MSBPOST', 2, '2');
    pAddShortConst('LSBPOST', 3, '3');
  End; { With oDevice }
  WriteLn(g_tfSrc);
  g_bIsP1:=False;
  g_bIsP2:=True;
  g_iMaxpin:=127;
  g_bHandled:=True;
End;


End.
