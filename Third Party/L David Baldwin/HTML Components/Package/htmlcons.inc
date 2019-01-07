{Version 9.03}

{$ifDef ver150}  {Delphi 7}
{$WARN Unsafe_Type off}
{$WARN Unsafe_Code off}
{$WARN Unsafe_Cast off}
{$endif}

{$A+,B-,F-,G+,I+,P+,T-,V+,X+,R-}

{$ifdef Win32}
  {$J+}  {typed constants are modifiable}
  {$H+}  {LongStrings On}
{$endif}

{$ifndef DebugIt}
  {$W-}  {Stack frames off}
  {$Q-}  {overflow checking off}
  {$S-}  {stack checking off}
  {$C-}  {Assertions off}
  {$ifdef Win32}
    {$O+}  {optimization on}
  {$endif}
{$else}
  {$W+}  {Stack frames on}
  {$Q+}  {overflow checking on}
  {$S+}  {stack checking on}
  {$C+}  {Assertions on}
  {$ifdef Win32}
    {$O-}  {optimization off}
  {$endif}
{$endif}

{$Define ver100_plus}
{$Define ver120_plus}
{$Define Delphi6_Plus}

{$ifdef ver90}     {Delphi 2}
{$UnDef ver100_plus}
{$UnDef ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef ver93}     {C++Builder 1}
{$Define CppBuilder}
{$UnDef ver100_plus}
{$UnDef ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef ver100}   {Delphi 3}
{$UnDef ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef ver110}    {C++Builder 3}
{$ObjExportAll On}
{$Define CppBuilder}
{$UnDef ver120_plus}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef Ver120}   {Delphi 4}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef ver125}    {C++Builder 4}
{$ObjExportAll On}
{$Define CppBuilder}
{$UnDef Delphi6_Plus}
{$endif}

{$ifdef Ver130}   {Delphi 5, C++Builder 5}
  {$ifdef BCB}   {C++Builder}
    {$Define CppBuilder}
    {$ObjExportAll On}
  {$endif}
{$UnDef Delphi6_Plus}
{$endif}

{$ifDef ver140}  {Delphi 6}
{$endif}

{$ifdef Delphi6_Plus}
  {$Warn Symbol_Platform Off}
{$endif}

{.$Define NoGIF} {To eliminate GIF image capability, define "NoGIF" by
           removing the '.'.}

{.$Define NoPNG} {To eliminate PNG image capability, define "NoPNG" by
           removing the '.'.}

{.$Define NoTabLink}  {To eliminate tabbing between links, define "NoTabLink" by
        removing th '.'.  For documents with hundreds of links, tabbing may not
        be desirable and also may cause delays in loading.}

{$Define FastRadio}
       {if you have a form with a great many radio buttons, you may want to
        define FastRadio to help speed the action.  Note that this option
        changes the way tabbing works.  With FastRadio off, tabs move only to
        the next control that is visible.  With FastRadion on, tabs move to
        the next control even if it is not visible.}           

{Note:  Starting with V 8.2, the UseDLL option no longer exists.  The graphics
 DLL is no longer used.

{Note:  The NoUnderline and PrintTableBackground compiler options are now options
 set in the htOptions and fvOptions properties. }

