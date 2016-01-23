Unit _RANDOM;

{$MODE Delphi}

 // LMM Index

Interface

Uses GLOBAL;

Procedure ProcessRandom;

Implementation

Procedure ProcessRandom;
Begin
  If (g_iCmdCnt = 2) or (g_iCmdCnt = 4) Then
  Begin
    If g_apoCmdVars[2]^.eGetType = e_LongVar Then
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
        OutStr('  min __temp2,#1');
        OutStr('  mov __temp1,#32');
        OutStr(NewLabelStr);
        OutStr('  test __temp2,#%10111 WC');
        OutStr('  rcr __temp2,#1');
        If g_bLMM Then
        Begin
          OutStr('  djnz __temp1,#_LMM_JUMP WC');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  djnz __temp1,#'+LabelStr+' WC');
        If g_bLMM Then
        Begin
          OutStr('  mov __INDEX,#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  add __INDEX,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  jmp #_LMM_MOVD');
        End
        Else
        Begin
          OutStr('  add '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
          OutStr('  movd $+2,'+g_apoCmdVars[2]^.m_sIndex);
          OutStr('  sub '+g_apoCmdVars[2]^.m_sIndex+',#'+g_apoCmdVars[2]^.sGetName);
        End;
        OutStr('  mov 0-0,__temp2');
      End
      Else
      Begin
        OutStr('  min '+g_apoCmdVars[2]^.sGetIndexName+',#1');
        OutStr('  mov __temp1,#32');
        OutStr(NewLabelStr);
        OutStr('  test '+g_apoCmdVars[2]^.sGetIndexName+',#%10111 WC');
        OutStr('  rcr '+g_apoCmdVars[2]^.sGetIndexName+',#1');
        If g_bLMM Then
        Begin
          OutStr('  djnz __temp1,#_LMM_JUMP WC');
          OutStr('  long @@@'+LabelStr);
        End
        Else OutStr('  djnz __temp1,#'+LabelStr+' WC');
      End;
      VP_WriteVar(2);
      g_bHandled:=True;
    End
    Else Error(c_iErrorInvalidParameter, 2);
    If g_iCmdCnt = 4 Then
    Begin
      If g_asCmdLine[3] <> ',' Then Error(c_iErrorCommaExpected, 3);
      If g_apoCmdVars[4]^.eGetType = e_LongVar Then
      Begin
        If g_apoCmdVars[2]^.m_bVarIndex Then
        Begin // [2] is var index, value in __temp2
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
            OutStr('  mov 0-0,__temp2');
          End
          Else OutStr('  mov '+g_apoCmdVars[4]^.sGetIndexName+',__temp2');
        End
        Else
        Begin // [2] is not var index
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
            OutStr('  mov 0-0,'+g_apoCmdVars[2]^.sGetIndexName);
          End
          Else OutStr('  mov '+g_apoCmdVars[4]^.sGetIndexName+','+g_apoCmdVars[2]^.sGetIndexName);
        End;
      End
      Else Error(c_iErrorInvalidParameter, 4);
      VP_WriteVar(4);
    End; // IF g_icmdCnt = 4
  End;
End;

{ code from interpreter.spin
  "z" flag indicates a forward of backward random
  we only need to do forward

:rnd                    min     x,#1                    '?var/var?
                        mov     y,#32
                        mov     a,#%10111
        if_nz           ror     a,#1
:rndlp                  test    x,a             wc
        if_z            rcr     x,#1
        if_nz           rcl     x,#1
                        djnz    y,#:rndlp       wc      'c=0


Below is code for if z is set

:rnd                    min     x,#1                    '?var/var?
                        mov     y,#32
                        mov     a,#%10111

:rndlp                  test    x,a             wc
                        rcr     x,#1
                        djnz    y,#:rndlp       wc      'c=0

}

End.

