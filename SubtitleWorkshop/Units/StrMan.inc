{===============================================================================
StrMan.pas include file (Version:  2.4i)

CONDITIONAL DEFINES:
StrManEnableERaise
  If False then normal raise.exception.create calls are used.

StrManIncludeGeneralTypes
  You can "export" general types/constants to another central unit and place it
  in the uses clause below.

StrManEnableSMinstance
  You may choose too create another instance of TStringCollection.

StrManEnableASM
  If you disable this define you will use pure Delphi code. (no assembler)    
-------------------------------------------------------------------------------}


{$DEFINE StrManEnableERaise}
{$DEFINE StrManIncludeGeneralTypes}
{$DEFINE StrManEnableSMinstance}
{$DEFINE StrManEnableASM}

//uses clause
uses Windows, SysUtils,
     {$IfDef VER140} //Delphi 6
     Variants,
     {$EndIf}
     {$IfDef VER150} //Delphi 7
     Variants,
     {$EndIf}
     Classes;
