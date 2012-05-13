{===============================================================================
TStringManager
 Class with string functions - by Eric Grobler (egrobler@quoka.com)

   Status:  Freeware - No restrictions apply - without a warranty of any kind.
            (This library is based on my previous badly named TStringCollection class)
 Compiler:  Delphi4, Delphi5, Delphi6, Delphi7
  Version:  2.4j
 Lastdate:  04 May 2003
      Url:  www.geocities.com/ericdelphi/StrMan.html

 This library does not support multibyte (MBCS) strings (Japanese, Chinese, etc).
 (Note that most Delphi string functions does not support MBCS either)
 It does however support European Ansi case characters.
 Please report any bugs, suggestions to egrobler@quoka.com
--------------------------------------------------------------------------------

USAGE:
 This unit contains a TStringManager class with a global instance "sm".
 The class contains a large number of general purpose string functions.
 Example usage:
   s := sm.After(',', s); (sm is created in the implementation section)
   s := sm.UpCase(s);
 Please see StrMan.Inc for compiler directives

ASSEMBLER:
 - This library has some asm functions.
   (including some of Peter Morris routines - see note/credits at FastMemPos)
 - All assembler routines has pascal equivalents.
   Disable directive StrManEnableASM to revert to pure Delphi code.

CLASS ADVANTAGES:
 Some advantages of putting string functions inside a class:
 * Functions are neatly grouped together.
 * Global function name conflicts are removed.
 * You can choose the "namespace" - does not have to be "sm".
 * Function names can be shorter and more logical than their global counterparts.
 * The object can be extended. (you can add or override your own functions)

CLASS DISADVANTAGES:
 * Speed - Every method call contains an extra 4 byte self parameter.
   Note however that code inside a function is far more critical, and that the
   TStringManager methods are generally faster that their standard Delphi counterparts.

   TStringManager is actually a fast string library because:
     * It makes use of prefilled case and chartype arrays.
     * It has a lot of "compound" functions like TrimUp, PTrimUp.
     * Most string result functions calculate the result length first,
       (Result := Result + s[i] is very slow in Delphi5)
     * Some asm code
   Functions that use case are very fast because an AnsiCase array is used and
   MBCS are not supported:
   Routine:                    MilliSeconds
    TStringManager.UpCase:      2053  (ansi)
    System.UpperCase            2563  (7 bit)
    SysUtils.AnsiUpperCase      9204  (ansi/mbcs)
   As you can see the TStringManager version is much faster.
   If a routine is slow in TStringManager then it is badly written, please
   email me your improved version!

--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
Note:
  Function overloading and default parameters are used. (Delphi 4,5 or higher)
  Most functions have examples at code implementation.

Conventions:
  An "IC" suffix indicates "Ignore case".
  A "P" prefix normally indicates a procedural version with a var param.
  UpCase, LoCase are ANSI case that includes chars like é
  Alpha in function names refers to a combination of Upper and Lower letters.


Some examples:
 sm.Strip('   hello    world  ') > 'hello world'
 sm.GetChars('123Hello',['A'..'z']) > 'Hello'
 sm.Purge('123Hello',['A'..'z']) > '123'
 sm.After('.','c:\folder.files\name.txt') > 'files\name.txt'
 sm.AfterRev('.','c:\folder.files\name.txt') > 'txt'
 sm.BeforeRev('.','c:\folder.files\name.txt') > 'c:\folder.files\name'
 sm.UpCase('polé') > 'POLÉ'
 sm.BaseToStr(12,'01',8) > '00001100'
 sm.Str(12.1) > '12.10'
 sm.ToF('12.1') > 12.1
 sm.WordPos('the','where is the word?') > 10
 sm.Replace('dog','cat','The dog is Dogmeat') > 'The cat is Dogmeat'

--------------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
Modifications, additions:
 16 Nov 2000  (version 2.0)
    IsLastIC
    IsFirstIC
    WordPosIC
    PosRev CharSet version
    PosRevIC string version
    Pos and PosRev Char versions
    DelEnd, DelEndP
    Included some of Peter Morris asm routines - please see note at FastMemPos.
    Speed improvements, Pos, PosIC, PosRev, PosRevIC, Strip, RepeatStr etc.
    Added Mop, MopUp
    Renamed StripUpCase to StripUp, and TrimUpCase to TrimUp
    IsSame, IsSameFirst are now calling Mop internally instead of Strip
    IIF, GetAlpha, GetNumericChars
    RefCount, MakeUnique, CopyPos, IsEmpty, HasSame, FindSame
    HasType, OnlyType and Encrypt routines
    Changed LastIfNot and FirstIfNot to EnsureLast, EnsureFirst
 24 Jan 2002
    Changed PtrReplace in PReplace from PString to Pointer - fixes apparent memory loss
 27 Jan 2002
    Delphi4 compatiblity problems fixed with Encrypt, Decrypt
 20 Jan 2002
    Added missing Delims parameter in ReplaceWord function
  3 May 2002
    Fixed range error in PReplace
 21 Aug 2002 gcDefaultCurFormat introduced and StrCur changed
 24 Jan 2003 Delphi7 Variants in StrMan.inc, Bugfix in Replace
 02 Feb 2003 Changed bug regarding roPreserveCase - now clones source case
 06 Feb 2003 Changed global typed constants to variables
 04 May 2003 Bug fixes to functions; ToC and UnQuote by Donovan J. Edye 
===============================================================================}

{//////////////////////////////////////////////////////////////////////////////}
{                                 INTERFACE                                    }
{//////////////////////////////////////////////////////////////////////////////}
unit StrMan;

interface

{$EXTENDEDSYNTAX ON}
{$WRITEABLECONST OFF}
{$LONGSTRINGS ON}
{$VARSTRINGCHECKS OFF}
{$RANGECHECKS ON}

{$I StrMan.inc}  //includes compiler directives and uses clause

{//////////////////////////////////////////////////////////////////////////////}
{                            GLOBAL TYPES CONTANTS                             }
{//////////////////////////////////////////////////////////////////////////////}
{$IFDEF StrManIncludeGeneralTypes}
//You may choose to move these types/constants to another unit.
type
  TCharSet = set of Char;
  TCurrency = packed record c : Currency; end; //TCurrency is defined as a record type to differenciate it from Double and TDateTime values
  TCharByteArray = array[#0..#255] of Char;
  TOpenStringArray = array of String;
  TOpenIntegerArray = array of Integer;
  TOpenByteArray = array of Byte;


  P2GBCharArray  = ^T2GBCharArray;
  T2GBCharArray  = array[1..MaxInt] of Char;

  PAnsiStrHeader = ^TAnsiStrHeader;
  TAnsiStrHeader = packed record
    RefCount   : Integer;
    AllocSize  : Integer;
  end;

const
  gcsAll           = [#0..#255];
  gcsAZHi          = ['A'..'Z'];
  gcsAZLo          = ['a'..'z'];
  gcsAZAlpha       = ['A'..'Z','a'..'z'];
  gcsDigit         = ['0'..'9'];
  gcsInt           = ['0'..'9','-','+'];
  gcsNumeric       = ['0'..'9',',','.','-','+'];
  gcsFloat         = ['0'..'9',',','.','-','+','E','e'];
  gcsControl       = [#0..#31];
  gcCRSet          = [#13,#10];
  gcCR             = #13#10;
  gcBaseBinary     = '01';
  gcBaseHex        = '0123456789ABCDEF';
  gcWordDelims     = [#1..#47,#58..#64,#91..#96,#123..#126,'¡','¿']-['`'];
  gcBooleanTrueSet   : TCharSet = ['1','T','t','Y','y','J','j'];

{$ENDIF}

var
  gcCharSetVisualRange : TCharSet = ['\','$','%','&','@','*','+','/','A'..'Z','a'..'z','{','}','(',')','>','<','?','0'..'9','[',']'];
  gcDefaultCurFormat : String = ''; //see initialization
type
  TIntNoOfBytes = 1..4;
  TInt64NoOfBytes = 1..8;

  TAskUpCaseWord = function(const s : String) : Boolean;
  TStrReplaceOptions = set of (roIgnoreCase, roWholeWords, roReplaceOnce, roPreserveCase, roRemoveDelim); //Delphi's is TReplaceFlags

  TCharTypeFlag =
  (cfUpper,      //A..Z and ƒŠŒŽßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞŸ
   cfLower,      //a..z and ƒšœžßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ
   cfAZUpper,    //only A..Z
   cfAZLower,    //only a..z
   cfPunct,      //!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~€‚ƒ„…†‡ˆ‰‹‘’“”•–—˜™› etc.
   cfDigit,      //0..9
   cfInt,        //0..9 and [+-]
   cfNumeric,    //0..9 and [+-,.]
   cfFloat,      //0..9 and [+-,.Ee]
   cfDelim,      //see gcWordDelims
//   cfWhite,      //#9,#10,#13,' '
   cfControl);   //#0..#31

   TCharTypeFlags = set of TCharTypeFlag;

type
{//////////////////////////////////////////////////////////////////////////////}
{                           TStringManager                                     }
{//////////////////////////////////////////////////////////////////////////////}
TStringManager = class //Global instance "sm" in initialization section
  private
    procedure DoRaise(const aMsg : String; aFunc : String = '');
  public
  //Trim/Mop/Strip - Remove characters depending on the position in a string
  function  Trim(const s : String; DoUpcase : Boolean = False) : String; overload;
  function  TrimUp(const s : String) : String;
  procedure PTrim(var s : String; DoUpcase : Boolean = False);
  procedure PTrimUp(var s : String);
  function  TrimLeft(const s : String; DoUpCase : Boolean = False) : String;
  function  TrimRight(const s : String; DoUpCase : Boolean = False) : String;
  function  TrimLen(const s : String) : Integer;  

  function  Mop(const s : String; DoUpcase : Boolean = False): String; overload;
  function  MopUp(const s : String): String;

  procedure PMop(var s : String; DoUpcase : Boolean = False);
  procedure PMopUp(var s : String);

  function  Strip(const s : String; c : Char = ' '; DoUpcase : Boolean = False) : String;
  function  StripUp(const s : String; c : Char = ' ') : String;
  procedure PStrip(var s: String; c : Char = ' '; DoUpcase : Boolean = False);
  procedure PStripUp(var s: String; c : Char = ' ');

  //TStrings versions
  function  Mop(const aList : TStrings; const RemoveEmptyLines : Boolean = False) : Integer; overload;
  function  Trim(const aList : TStrings; const RemoveEmptyLines : Boolean = False) : Integer; overload;

  //Purge/GetChars  Remove/exclude characters
  function  Purge(const s : String; const aSet : TCharSet; DoUpcase : Boolean = False) : String; overload;
  function  Purge(const s : String; c : Char) : String; overload;
  function  PurgeWhile(const s : String; const aSet : TCharSet) : String;
  function  PurgeWhileRev(const s : String; const aSet : TCharSet) : String;
  function  PurgeWhileOuter(const s : String; const aSet : TCharSet) : String;

  function  PurgeType(const s : String; aFlags : TCharTypeFlags) : String;
  function  PurgeAlpha(const s : String) : String;
  function  PurgeUp(const s: String): String;
  function  PurgeLo(const s: String): String;
  function  PurgeDigit(const s: String): String;

  //Return sets of chars
  function  GetChars(const s : String; const aSet : TCharSet) : String; overload;
  function  GetChars(const s : String; const aStrSet : String) : String; overload;

  function  GetType(const s : String; aFlags : TCharTypeFlags) : String;
  function  GetAlpha(const s : String) : String;
  function  GetUp(const s: String): String;
  function  GetLo(const s: String): String;
  function  GetDigit(const s: String): String;
  //other
  function  StripDouble(const s : String; const IgnoreSet : TCharSet = []; const PurgeSet : TCharSet = []; const DoUpcase : Boolean = False) : String;
  function  GetSingleSorted(const s1 : String; const s2 : String = '') : String;

  //Compare functions-----------------------------------------------------------
  function  Equal(c1, c2 : Char; IgnoreCase : Boolean) : Boolean; overload;
  function  Equal(const s1, s2 : String; IgnoreCase : Boolean) : Boolean; overload;
  function  EqualIC(c1, c2 : Char) : Boolean; overload;
  function  EqualIC(const s1, s2 : String) : Boolean; overload;

  //Mop like compare
  function  IsSame(const s1, s2 : String; IgnoreCase : Boolean = False) : Boolean; overload;
  function  IsSameIC(const s1, s2 : String) : Boolean; overload;
  function  IsSameFirst(const SubStr, s : String; IgnoreCase : Boolean = False) : Boolean; overload;
  function  IsSameFirstIC(const SubStr, s : String) : Boolean; overload;

  //Purge like compare
  function  IsSameIC(const s1, s2 : String; const PurgeSet : TCharSet) : Boolean; overload;
  function  IsSame(const s1, s2 : String; const PurgeSet : TCharSet; IgnoreCase : Boolean = False) : Boolean; overload;
  function  IsSameFirst(const SubStr, s : String; const PurgeSet : TCharSet; IgnoreCase : Boolean = False) : Boolean; overload;
  function  IsSameFirstIC(const SubStr, s : String; const PurgeSet : TCharSet) : Boolean; overload;

  //Trim like compare
  function  TrimSame(const s1, s2 : String; IgnoreCase : Boolean = False) : Boolean;
  function  TrimSameIC(const s1, s2 : String) : Boolean;
  function  TrimSameFirst(const SubStr, s : String; IgnoreCase : Boolean = False) : Boolean;
  function  TrimSameFirstIC(const SubStr, s : String) : Boolean;

  //First/Last
  function  IsFirst(const SubStr : String; const s : String) : Boolean; overload;
  function  IsFirst(const SubStr : String; const s : String; IgnoreCase : Boolean) : Boolean; overload;
  function  IsFirstIC(const SubStr : String; const s : String) : Boolean;
  function  IsLast(const SubStr : String; const s : String) : Boolean; overload;
  function  IsLast(const SubStr : String; const s : String; Ignorecase : Boolean) : Boolean; overload;
  function  IsLastIC(const SubStr : String; const s : String) : Boolean;

  //Informational functions-----------------------------------------------------
  function  IsEmpty(const s : String) : Boolean;
  function  NotEmpty(const s : String) : Boolean;


  function  HasWord(const Word, s : String; const Delims : TCharSet = gcWordDelims) : Boolean;
  function  HasWordIC(const Word, s : String; const Delims : TCharSet = gcWordDelims) : Boolean;
  function  HasChar(c : Char; const s : String) : Boolean; 
  function  HasCharIC(c : Char; const s : String) : Boolean;

  function  CountChars(const s : String; const aSet : TCharSet) : Integer;
  function  CountLines(const s : String) : Integer;
  function  CountWords(const s : String; const Delims : TCharSet = gcWordDelims) : Integer;
  function  CountSet(const aSet : TCharSet) : Integer;

  function  IsShortString(s : array of const) : Boolean;

  //Character info
  function  IsType(const c : Char; const Flags : TCharTypeFlags) : Boolean;
  function  IsAlpha(const c: Char): Boolean;
  function  IsUp(const c: Char): Boolean;
  function  IsLo(const c: Char): Boolean;
  function  IsDigit(const c: Char): Boolean;     //0..9

  //Char info on strings
  function  HasType(const s : String; const Flags : TCharTypeFlags) : Boolean;
  function  HasAlpha(const s : String) : Boolean;
  function  HasUp(const s : String) : Boolean;
  function  HasLo(const s : String) : Boolean;
  function  HasDigit(const s : String) : Boolean;
  function  HasChars(const s : String; const aSet : TCharSet) : Boolean;

  function  OnlyType(const s : String; const Flags : TCharTypeFlags) : Boolean;
  function  OnlyAlpha(const s : String) : Boolean;
  function  OnlyUp(const s : String) : Boolean;
  function  OnlyLo(const s : String) : Boolean;
  function  OnlyDigit(const s : String) : Boolean;
  function  OnlyChars(const s : String; const aSet : TCharSet) : Boolean;

  //Lookup in array of String
  function  HasSame(const s: String; const aList: array of String): Boolean; overload;
  function  FindSame(const s: String; const aList: array of String): Integer; overload;
  function  HasSameIC(const s: String; const aList: array of String): Boolean; overload;
  function  FindSameIC(const s: String; const aList: array of String): Integer; overload;

  //Lookup in TStrings
  function  HasSame(const s: String; const aList: TStrings): Boolean; overload;
  function  FindSame(const s: String; const aList: TStrings): Integer; overload;
  function  HasSameIC(const s: String; const aList: TStrings): Boolean; overload;
  function  FindSameIC(const s: String; const aList: TStrings): Integer; overload;


  //Case------------------------------------------------------------------------
  function  UpCase(const s : String) : String; overload;
  function  LoCase(const s : String) : String; overload;
  function  LoCase(c : Char) : Char; overload;
  function  UpCase(c : Char) : Char; overload;

  procedure PUpCase(var s : String); overload;
  procedure PUpCase(var c : Char); overload;
  procedure PLoCase(var s : String); overload;
  procedure PLoCase(var c : Char); overload;

  function  AZUpCase(const s : String) : String; overload;//non ANSI
  function  AZLoCase(const s : String) : String; overload;//non ANSI
  function  AZUpCase(c : Char) : Char; overload;//non ANSI
  function  AZLoCase(c : Char) : Char; overload;//non ANSI

  function  ProperCase(const s : String; AskUpCaseWordFunc : TAskUpCaseWord = nil; UseBasicFuncIfNil : Boolean = True) : String;
  function  WordCase(const s : String; const UpCaseDelims : TCharSet = [' ',',',';'];
             const WordDelims : TCharSet = []; AskUpCaseWordFunc : TAskUpCaseWord = nil): String;

  //Manipulation----------------------------------------------------------------
  function  Ins(const SubStr : String; const s : String; Index : Integer = 1) : String;
  function  SetAt(const s : String; const c : Char; Index : Integer) : String;
  function  EnsureFirst(const SubStr : String; const s : String) : String;
  function  EnsureLast(const SubStr : String; const s : String) : String;
  function  DelEnclosed(const IfInSet : TCharSet; const s : String; Repeatedly : Boolean = False; CanRemoveOnlyOne : Boolean = False) : String;
  function  EnsureEnclosed(c : char; const s : String; CanAddOne : Boolean = False) : String;

  function  Center(const s: String; Len: Integer; PadChar : Char = ' '): String;

  //Replacement-----------------------------------------------------------------
  function  ReplaceChars(const s : String; const OldChars : TCharSet; NewChar : Char) : String;

  function  Replace(OldChar, NewChar : Char; const InStr : String; const IgnoreCase : Boolean = False) : string; overload;
  function  ReplaceIC(OldChar, NewChar : Char; const InStr : String) : String; overload;

  function  Replace(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              const IgnoreCase   : Boolean = False;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String; overload;

  function  ReplaceIC(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String; overload;

  function  ReplaceWord(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              const IgnoreCase   : Boolean = False;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String;

  function  ReplaceWordIC(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String;

  function  PReplace(
              const FindStr      : String;
              const ReplaceStr   : String;
              var InStr          : String;
              var StartPos       : Integer;
              const Flags        : TStrReplaceOptions = [];
              const Delims       : TCharSet = gcWordDelims) : Integer; overload;

  function  PReplace(
              const FindStr      : String;
              const ReplaceStr   : String;
              var InStr          : String;
              const IgnoreCase   : Boolean = False;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False) : Integer; overload;

  function  PReplaceIC(
              const FindStr      : String;
              const ReplaceStr   : String;
              var InStr          : String;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False) : Integer; overload;

  function  ReplaceOnce(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              var StartPos       : Integer;
              const IgnoreCase   : Boolean = False;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String;

  function  StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;

  //Delete----------------------------------------------------------------------
  function  Del(const SubStr : String; const s : String): String;
  function  DelIC(const SubStr : String; const s : String): String;
  function  DelWord(const SubStr : String; const s : String; RemoveDelim : Boolean = False; const Delims : TCharSet = gcWordDelims): String;
  function  DelWordIC(const SubStr : String; const s : String; RemoveDelim : Boolean = False; const Delims : TCharSet = gcWordDelims): String;
  function  DelIfFirst(const IfInSet : TCharSet; const s : String) : String; overload;
  function  DelIfFirst(const SubStr : String; const s : String; IgnoreCase : Boolean = False) : String; overload;
  function  PDelIfFirst(const SubStr: String; var s  : String; IgnoreCase : Boolean = False) : Boolean; overload;
  function  PDelIfFirst(const IfInSet : TCharSet; var s  : String) : Boolean; overload;
  function  DelIfLast(const SubStr : String; const s : String; IgnoreCase : Boolean = False) : String; overload;
  function  DelIfLast(const IfInSet : TCharSet; const s : String) : String; overload;
  function  PDelIfLast(const SubStr : String; var s : String; IgnoreCase : Boolean = False) : Boolean; overload;
  function  PDelIfLast(const IfInSet : TCharSet; var s : String) : Boolean; overload;
            

  function  DelEnd(const s : String; Count : Integer = 1) : String;
  procedure DelEndP(var s : String; Count : Integer = 1);

  function  Cut(var s : String; Index, Count : Integer) : String;

  //Padding---------------------------------------------------------------------
  function  Pad(const s : String; PadLen : SmallInt; c : Char = ' ') : String; {-1 will pad left}
  function  PadCut(const s : String; PadLen : SmallInt; c : Char = ' ') : String;
  function  PadLeft(const s : String; PadLen : Word; c : Char = ' ') : String;

  //Positional------------------------------------------------------------------
  {$IFDEF StrManEnableASM}
  function  Pos(const SubStr, s : String; StartPos : Integer = 1) : Integer; overload;
  {$ELSE}
  function  Pos(const SubStr, s : String) : Integer; overload;
  function  Pos(const SubStr, s : String; StartPos : Integer) : Integer; overload;
  {$ENDIF}
  function  Pos(const SubStr, s : String; IgnoreCase : Boolean; StartPos : Integer = 1) : Integer; overload;
  function  PosIC(const SubStr, s : String; StartPos : Integer = 1) : Integer; overload;
  function  PosRev(const SubStr, s : String; StartPos : Integer = -1) : Integer; overload;
  function  PosRevIC(const SubStr, s : String; StartPos : Integer = -1) : Integer; overload;

  //Char versions
  function  Pos(c : Char; const s : String; StartPos : Integer = 1) : Integer; overload;
  function  PosIC(c : Char; const s : String; StartPos : Integer = 1) : Integer; overload;
  function  PosRev(c : Char; const s : String) : Integer; overload;
  function  PosRevIC(c : Char; const s : String) : Integer; overload;

  function  PosBefore(const inSet : TCharSet; const s : String; const BeforeSet : TCharSet; StartPos : Integer = 1) : Integer;

  //Set versions
  function  Pos(const aSet : TCharSet; const s : String; StartPos : Integer = 1) : Integer; overload;
  function  PosRev(const aSet : TCharSet; const s : String) : Integer; overload;

  //Whole word versions
  function  WordPos(const SubStr, s : String; const Delims : TCharSet = gcWordDelims; StartPos : Integer = 1) : Integer;
  function  WordPosIC(const SubStr, s : String; const Delims : TCharSet = gcWordDelims; StartPos : Integer = 1) : Integer;

  //Between
  function  PosBetween(const SubStr1, SubStr2 : String; const s : String; var StartPos, EndPos : Integer; IgnoreCase : Boolean = False) : Integer; overload;
  function  PosBetweenRev(const SubStr1, SubStr2 : String; const s : String; var StartPos, EndPos : Integer; IgnoreCase : Boolean = False) : Integer; overload;
  function  PosBetween(Delim1, Delim2 : Char; const s : String; var StartPos, EndPos : Integer) : Integer; overload;
  function  PosBetweenRev(Delim1, Delim2 : Char; const s : String; var StartPos, EndPos : Integer) : Integer; overload;

  //Return part of a string-----------------------------------------------------
  function  After(const SubStr : String; const s : String; Position : Integer = 1) : String; overload;
  function  AfterOrAll(const SubStr : String; const s : String; Position : Integer = 1) : String; overload;  
  function  After(const aSet : TCharSet; const s : String) : String; overload;
  function  AfterRev(const SubStr : String; const s : String) : String; overload;
  function  AfterRevIC(const SubStr : String; const s : String) : String; overload;
  function  AfterRev(const aSet : TCharSet; const s : String) : String; overload;
  function  AfterIC(const SubStr : String; const s : String; Position : Integer = 1) : String;
  function  AfterOrAllIC(const SubStr : String; const s : String; Position : Integer = 1) : String; overload;

  function  AfterNth(const SubStr : String; const s : String; Nth : Integer = 1) : String;

  function  Before(const SubStr : String; const s : String; Position : Integer = 1) : String; overload;
  function  Before(const aSet : TCharSet; const s : String) : String; overload;
  function  BeforeRev(const SubStr : String; const s : String) : String; overload;
  function  BeforeRevIC(const SubStr : String; const s : String) : String;
  function  BeforeRev(const aSet : TCharSet; const s : String) : String; overload;
  function  BeforeIC(const SubStr : String; const s : String; Position : Integer = 1) : String;
  function  BeforeNth(const SubStr : String; const s: String; Nth : Integer = 1): String;

  function  Between(const SubStr1, SubStr2 : String; const s : String; StartPos : Integer = 1) : String; overload;
  function  BetweenRev(const SubStr1, SubStr2 : String; const s : String; StartPos : Integer = -1) : String; overload;
  function  Between(const Delim1, Delim2 : Char; const s : String; StartPos : Integer = 1) : String; overload;
  function  BetweenRev(const Delim1, Delim2 : Char; const s : String; StartPos : Integer = -1) : String; overload;
  function  Mid(const SubStr1, SubStr2 : String; const s : String; Position : Integer = 1) : String;
  function  MidRev(const SubStr1, SubStr2 : String; const s : String) : String;

  function  At(const s : String; Index : Integer) : Char;
  function  FirstChar(const s: String) : Char;
  function  LastChar(const s: String) : Char;
  function  FirstWord(const s: String; const Delims : TCharSet = gcWordDelims) : String;
  function  LastWord(const s: String; const Delims : TCharSet = gcWordDelims) : String;
  function  NthWord(Nth : Integer; const s : String; const Delims : TCharSet = gcWordDelims) : String;
  function  ExtractWords(const s : String; const aList : TStrings = nil; const Delims : TCharSet = gcWordDelims) : TStrings;


  function  Parse(const DelimStr: String; var s: String; IgnoreDelimAtStart : Boolean = True): String; overload;
  function  Parse(const aSet: TCharSet; var s: String; IgnoreDelimAtStart : Boolean = True): String; overload;
  procedure Parse(const DelimStr : String; s: String; const aList : TStrings; MopAlso : Boolean = True; IgnoreDelimAtStart : Boolean = True); overload;

  //Copy functions--------------------------------------------------------------
  function  Copy(const s : String; Index: Integer; Count : Integer = $7FFFFFFF) : String;
  function  First(const s : String; Count : Integer = 1) : String;
  function  Last(const s : String; Count : Integer = 1) : String;
  function  LeftAt(const s : String; Position : Integer) : String;
  function  RightAt(const s : String; Position : Integer) : String;
  function  CopyPos(const s: String; StartPos, EndPos: Integer): String;

  //Numeric To String conversions-----------------------------------------------
  function  Str(Value : Integer) : String; overload;
  function  Str(Value : Byte; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : SmallInt; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : Word; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : Integer; PadLen : SmallInt; PadChar : Char = '0') : String; overload;
  function  Str(Value : Cardinal; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : Int64; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : Pointer) : String; overload;

  function  Str(Value : Double; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : Extended; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;
  function  Str(Value : TCurrency; PadLen : SmallInt = 0; PadChar : Char = '0') : String; overload;

  function  StrCur(Value : Currency; PadLen : SmallInt = 0; PadChar : Char = ' ') : String;
  function  StrFloat(Value : Extended; aFormatStr : String = '0.00###'; PadLen : SmallInt = 0; PadChar : Char = ' ') : String;
  function  FormatSize(Value : Int64) : String;

  //String to Numeric conversions-----------------------------------------------
  //String to numeric procedures
  procedure Val(const s : String; var Value : Byte); overload;
  procedure Val(const s : String; var Value : SmallInt); overload;
  procedure Val(const s : String; var Value : Word); overload;
  procedure Val(const s : String; var Value : Integer); overload;
  procedure Val(const s : String; var Value : Cardinal); overload;
  procedure Val(const s : String; var Value : Int64); overload;
  procedure Val(const s : String; var Value : Double); overload;
  procedure Val(const s : String; var Value : Extended); overload;
  procedure Val(const s : String; var Value : TCurrency); overload;

  //String to numeric functions
  function  ToI(const s : String) : Integer;
  function  ToF(const s : String) : Extended;
  function  ToC(const s : String) : TCurrency;
  function  ToL(const s : String) : Cardinal;
  function  ToI64(const s : String) : Int64;

  //Misc type to String conversions---------------------------------------------
  function  Str(Value : Boolean) : String; overload;
  function  Str(const Value : TCharSet; AsVisual : Boolean = False) : String; overload;
  function  StrDate(Value : TDateTime; const aFormat : String = '') : String;
  function  StrDateTime(Value : TDateTime; const aFormat : String = '') : String;
  function  Str(const aFormat: String; const Args: array of const): String; overload;
  function  StrVar(var Source; Count : Integer = -1; Terminators : TCharSet = []) : String;
  function  Str(aFlags : TCharTypeFlags) : String; overload;

  //String to Misc type conversions---------------------------------------------
  procedure Val(const s : String; var Value : Boolean); overload;
  procedure Val(const s : String; var Value : TCharSet; FromVisualSet : Boolean = False); overload;

  function  ToSet(const s : String; FromVisualSet : Boolean = False) : TCharSet; overload;
  function  ToSet(aFlags : TCharTypeFlags) : TCharSet; overload;
  function  ToBool(const s : String) : Boolean;
  procedure ToVar(const Source : String; var Target; TargetSize : Integer; EnsureAsciiZ : Boolean = False; ClearData : Boolean = True);
  function  ToSL(const s : String = '') : TStringList;
  function  ToDate(const s : String) : TDateTime;

  //Numeric base conversions----------------------------------------------------
  function  BaseToStr(Value : Cardinal; const Key : String; Digits : Byte = 0) : String; overload;
  function  BaseToStr(Value : Int64; const Key : String; Digits : Byte = 0) : String; overload;
  function  BaseVal(const s : String; const Key : String) : Int64; overload;
  function  BaseVal(const s : String; const Key : TCharSet) : Int64; overload;
  function  BaseStr(Value : Int64; const Key : String) : String; overload;
  function  BaseStr(Value : Cardinal; const Key : String) : String; overload;
  function  BaseStr(Value : Integer; const Key : String) : String; overload;
  function  BaseStr(Value : ShortInt; const Key : String) : String; overload;
  function  BaseStr(Value : SmallInt; const Key : String) : String; overload;
  function  BaseStr(Value : Word; const Key : String) : String; overload;
  function  BaseStr(Value : Byte; const Key : String) : String; overload;
  function  BaseStrR(Value : Int64; const Key : String; Digits : Byte) : String; //with range check
  function  BaseRange(const Key : String; Digits : Byte) : Int64; overload;
  function  BaseRange(const Key : TCharSet; Digits : Byte) : Int64; overload;
  function  BaseStrLen(const Key : String; ByteSize : TInt64NoOfBytes) : Byte;

  //Encryption -----------------------------------------------------------------
  procedure Encrypt224(const Keys : TOpenByteArray; var aText; const aTextLen : Integer; const EncryptTable : TCharByteArray); overload;
  procedure Decrypt224(const Keys : TOpenByteArray; var aText; const aTextLen : Integer; const EncryptTable : TCharByteArray); overload;
  function  Encrypt224(const s : String; const aKey : ShortString; const EncryptSet : TCharSet = [#32..#255]) : String; overload;
  function  Decrypt224(const s : String; const aKey : ShortString; const EncryptSet : TCharSet = [#32..#255]) : String; overload;

  procedure Encrypt(const Keys : TOpenByteArray; var aText; const aTextLen : Integer); overload;
  procedure Decrypt(const Keys : TOpenByteArray; var aText; const aTextLen : Integer); overload;
  function  Encrypt(const s : ShortString; const aKey : ShortString) : ShortString; overload;
  function  Decrypt(const s : ShortString; const aKey : ShortString) : ShortString; overload;

  //TStrings/TStringList related------------------------------------------------
  function  DelEmptyLines(const aList : TStrings) : Integer;
  function  DelExtraLines(const aList : TStrings) : Integer;

  //Memory related--------------------------------------------------------------
  function  StrHeader(const s : AnsiString) : PAnsiStrHeader;
  function  RefCount(const s : String) : Integer;
  function  NTLen(const s : AnsiString) : Integer; overload;
  function  NTLen(const pc : PChar) : Integer; overload;
  function  EnsureNTLen(var s : AnsiString) : Integer;
  procedure MakeUnique(var s: String);
  function  AsUnique(const s : String) : String;
  function  MakePChar(const s : String) : PChar;
  procedure FreePChar(var p : PChar);

  //Text file related-----------------------------------------------------------
  procedure FileWrite(const aFileName : string; const aText : string);
  procedure FileAppend(const aFileName : string; const aText : string; EnsureOnNewLine : Boolean = False);
  function  FileRead(const aFileName : string; MaxLen : Integer = MaxInt) : String;
  function  FileReadEnd(const aFileName : string; MaxLen : Integer = MaxInt) : String;
  procedure StreamWrite(const aStream : TStream; const aText : String);
  function  StreamRead(const aStream : TStream) : String;

  //Miscellaneous---------------------------------------------------------------
  function  Rev(const s : String) : String;
  function  RepeatChar(c : Char; Count : Integer) : String;
  function  RepeatStr(const s : String; Count : Integer) : String;
  function  Sort(const s : String) : String;
  function  SlashAdd(const s : String) : String;
  function  SlashDel(const s : String) : String;
  function  Quote(const s : String; aQuoteChar : Char = '''') : String;
  function  QuoteIfUnQuoted(const s : String; aQuoteChar : Char = '''') : String;
  function  UnQuote(const s : string; aQuoteChar : Char = '''') : String;
  function  Hash(const s : String) : Integer;
  function  HashUnique(const s : String; const ValidChars : TCharSet = ['A'..'Z']) : Cardinal;
  function  CharSetToVisualStr(const aSet: TCharSet; DoRange: Boolean = True): String;
  function  VisualStrToCharSet(s: String): TCharSet;
  function  IIF(const Condition : Boolean; const sTrue : String; const sFalse : String = ''): String; overload;
  function  IIF(const Condition : Boolean; const csTrue : TCharSet; const csFalse : TCharSet = []): TCharSet; overload;
  function  FindChar(const s : String; const aSet : TCharSet; const FromPos : Integer = 1) : Integer;
  function  FindNotChar(const s : String; const aSet : TCharSet; const FromPos : Integer = 1) : Integer;
  function  ToCharArray(const  aCharSet : TCharSet; AppendExcludedChars : Boolean = False) : TCharByteArray;
end;

{//////////////////////////////////////////////////////////////////////////////}
{  OBJECT TStrBufPtr                                                           }
{//////////////////////////////////////////////////////////////////////////////}
//Fast buffered string concatenation - see notes/example at implementation
type
  TStrBufPtr = object
  protected
    Fp       : PAnsiString;
    FCharLen : Integer;
    function  GetMemLen : Integer;
    procedure SetMemLen(Value : Integer);

    function  GetCharPtr: PChar;
    procedure SetCharLen(Value : Integer);

  public
    procedure Init(var AnsiStr : AnsiString); //Set's AnsiStr to ''
    procedure Truncate;
    procedure Add(Value : Char; Delta : Integer = 8); overload;
    procedure Add(const Value : String; Delta : Integer = 8); overload;
    property  Len : Integer read FCharLen write SetCharLen;
    property  MemLen : Integer read GetMemLen write SetMemLen;
    procedure DecLen(Count : Integer = 1);
    function  IsEmpty : Boolean; //if no characters above #32
    procedure Clear; //just sets FCharLen to 0
    function  FirstChar : Char;
    function  LastChar : Char;
  end;

  TStrBuf = object
  protected
    Fs           : String;
    FFixedLength : Integer;
    FCharLen     : Integer;
    function  GetMemLen : Integer;
    procedure SetMemLen(Value : Integer);
    procedure SetCharLen(Value : Integer);
    function  GetCharLenStr : String;
    procedure SetCharLenStr(const Value : String);
    function  GetCharPtr : PChar;
    function  GetStrPtr : PString;
    function  GetChar(Index : Integer) : Char;
    procedure SetChar(Index : Integer; Value : Char);

  public
    procedure Init; overload;
    procedure Init(aFixedLength : Integer); overload;
    procedure Clear; //just sets FCharLen to 0
    procedure Free;  //clear string mem

    procedure Add(Value : Char; Delta : Integer = 8); overload;
    procedure Add(const Value : String; Delta : Integer = 8); overload;
    procedure Truncate;
    property  Len : Integer read FCharLen write SetCharLen;
    property  MemLen : Integer read GetMemLen write SetMemLen;
    property  CharPtr : PChar read GetCharPtr;
    property  StrPtr : PString read GetStrPtr;
    procedure DecLen(Count : Integer = 1);
    //Utitlity
    property  Chars[index : Integer] : Char read GetChar write SetChar;
    property  s : String read GetCharLenStr write SetCharLenStr;
    function  IsEmpty : Boolean; //if no characters above #32
    procedure LoCase;
    procedure UpCase;
    procedure Trim(DoUpcase : Boolean = False);
    procedure TrimRight(DoUpcase : Boolean = False);
    procedure Mop(DoUpcase : Boolean = False);
    procedure Strip(DoUpcase : Boolean = False);
    function  HasChar(const aSet : TCharSet) : Boolean;
    function  FirstChar : Char;
    function  LastChar : Char;
    procedure DelEndWhiteSpaces;
  end;

{//////////////////////////////////////////////////////////////////////////////}
{                      GLOBAL FUNCTIONS, VARS                                  }
{//////////////////////////////////////////////////////////////////////////////}

//CaseArray's for fast conversion - see FillANSIArrays
var
  gaANSIUpperArray   : array[Char] of Char;
  gaANSILowerArray   : array[Char] of Char;
  gaANSICharType     : array[Char] of TCharTypeFlags;

{$IFDEF StrManEnableSMinstance}
var sm : TStringManager; //Global StringCollection variable (see Initialization)
{$ENDIF}


function BasicUpCaseWords(const s : String) : Boolean;

{$IFDEF StrManEnableERaise}
  //Exception procedures - see "Notes on the eRaise exception procedure" below
  procedure eRaise(aMessage : String; aObject : TObject); overload; //same as "on e:exception.create(aMessage+#13+#10+e.message);"
  procedure eRaise(aMessage : String; aFuncName : String= ''; aObject : TObject= nil); overload;
{$ENDIF}


{//////////////////////////////////////////////////////////////////////////////}
{                              IMPLEMENTATION                                  }
{//////////////////////////////////////////////////////////////////////////////}
implementation

var
  GUpCaseLUT: Pointer; //pointer to gaANSIUpperArray;

type
  TFastPosProc = function(const aSource, aFind; const aSourceLen, aFindLen : Integer) : Pointer;
  TPosProc = function(const SubStr, s : String; StartPos : Integer = 1) : Integer of object;
  TWordPosProc = function(const SubStr, s : String; Delims : TCharSet = gcWordDelims; StartPos : Integer = 1) : Integer of object;


{$IFDEF StrManEnableASM}
function FastMemPos(const aSource, aFind; const aSourceLen, aFindLen : Integer) : Pointer; forward;
function FastMemPosNC(const aSource, aFind; const aSourceLen, aFindLen : Integer) : Pointer; forward;
function FastPosBack(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer; forward;
function FastPosBackNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer; forward;
{$ENDIF}

{//////////////////////////////////////////////////////////////////////////////}
{                             TStringManager                                   }
{//////////////////////////////////////////////////////////////////////////////}

{-------------------------------------------------------------------------------
*UpCase - AnsiUpperCase converts all characters in the given string to uppercase.
 The conversion uses the current Windows locale.
 Faster than SysUtils.AnsiUppercase but does not support MBCS.
 Example: UpCase('polé') > 'POLÉ'
See also overloaded Char version
Note - should be 3-4 times faster than SysUtils.AnsiUppercase and 20% faster than SysUtils.UpperCase
-------------------------------------------------------------------------------}
function TStringManager.UpCase(const s : String) : String;
var
  SLen: Integer;
  Source, Dest : PChar;
begin
  SLen := Length(s);
  SetLength(Result, SLen);
  Source := Pointer(s);
  Dest := Pointer(Result);
  while SLen <> 0 do
  begin
    Dest^ := gaANSIUpperArray[Source^];
    Inc(Source);
    Inc(Dest);
    System.Dec(SLen);
  end;
end;

{-------------------------------------------------------------------------------
*UpCase
 UpCase a character according to System locale settings.
 Example: UpCase('é') > 'É'
See also overloaded String version
-------------------------------------------------------------------------------}
function TStringManager.UpCase(c : Char) : Char;
begin
  Result := gaANSIUpperArray[c];
end;

{-------------------------------------------------------------------------------
*PUpCase - Procedural version of UpCase - very fast.
See UpCase for info
-------------------------------------------------------------------------------}
procedure TStringManager.PUpCase(var s : String);
var i : Integer;
begin
  for i := 1 to Length(s) do s[i] := gaANSIUpperArray[s[i]];
end;


{-------------------------------------------------------------------------------
*PUpCase - Procedural version of UpCase
See UpCase for info
-------------------------------------------------------------------------------}
procedure TStringManager.PUpCase(var c : Char);
begin
  c := gaANSIUpperArray[c];
end;

{-------------------------------------------------------------------------------
*LoCase - AnsiLowerCase converts all characters in the given string to lowercase.
 The conversion uses the current Windows locale.
 Faster than SysUtils.AnsiLowerCase but does not support MBCS.
 Example: LoCase('POLÉ') > 'polé'
See also overloaded Char version
-------------------------------------------------------------------------------}
function TStringManager.LoCase(const s : String) : String;
var
  SLen : Integer;
  Source, Dest : PChar;
begin
  SLen := Length(s);
  SetLength(Result, SLen);
  Source := Pointer(s);
  Dest := Pointer(Result);
  while SLen <> 0 do
  begin
    Dest^ := gaANSILowerArray[Source^];
    Inc(Source);
    Inc(Dest);
    System.Dec(SLen);
  end;
end;

{-------------------------------------------------------------------------------
*LoCase - LoCase a character according to System locale settings.
 Example: LoCase('É') > 'é'
See also overloaded String version
-------------------------------------------------------------------------------}
function TStringManager.LoCase(c : Char) : Char;
begin
  Result := gaANSILowerArray[c];
end;

{-------------------------------------------------------------------------------
*PLoCase - Procedural version of LoCase
See LoCase for info
-------------------------------------------------------------------------------}
procedure TStringManager.PLoCase(var s : String);
var i : Integer;
begin
  for i := 1 to Length(s) do s[i] := gaANSILowerArray[s[i]];
end;

{-------------------------------------------------------------------------------
*PLoCase - Procedural version of LoCase
See LoCase for info
-------------------------------------------------------------------------------}
procedure TStringManager.PLoCase(var c : Char);
begin
  c := gaANSILowerArray[c];
end;

{-------------------------------------------------------------------------------
*AZUpCase - String to uppercase - converts only characters a..z
 Example: AZUpCase('Polé') > 'POLé'
See also UpCase
-------------------------------------------------------------------------------}
function TStringManager.AZUpCase(const s : String) : String;
var i : Integer;
begin
  Result := s;
  for i := 1 to Length(s) do if s[i] in ['a'..'z']
    then Result[i] := Char(Byte(s[i])-32);
end;

{-------------------------------------------------------------------------------
*AZLoCase - String to lowercase - converts only characters A..Z
 Example: AZLoCase('POLÉ') > 'polÉ'
See also LoCase
-------------------------------------------------------------------------------}
function TStringManager.AZLoCase(const s : String) : String;
var i : Integer;
begin
  Result := s;
  for i := 1 to Length(s) do if s[i] in ['A'..'Z']
    then Result[i] := Char(Byte(s[i])+32);
end;

{-------------------------------------------------------------------------------
*AZUpCase - Char UpCase -  converts only characters a..z
 Example: AZUpCase('e') > 'E'
See also UpCase
-------------------------------------------------------------------------------}
function TStringManager.AZUpCase(c : Char) : Char;
begin
  if c in ['a'..'z']
    then Result := Char(Byte(c)-32)
    else Result := c;
end;

{-------------------------------------------------------------------------------
*AZLoCase - Char LoCase - converts only characters A..Z
 Example: AZLoCase('Z') > 'z'
See also LoCase
-------------------------------------------------------------------------------}
function TStringManager.AZLoCase(c : Char) : Char;
begin
  if c in ['A'..'Z']
    then Result := Char(Byte(c)+32)
    else Result := c;
end;

{-------------------------------------------------------------------------------
*ToSet - Returns all characters that has TCharType properties as indicated by Flags.
 Example: ToSet([cfAZUpCase]) > ['A'..'Z']
See also other overloaded Str function
-------------------------------------------------------------------------------}
function  TStringManager.ToSet(aFlags : TCharTypeFlags) : TCharSet;
var c : Char;
begin
  Result := [];
  for c := Low(Char) to High(Char) do
  begin
    if aFlags*gaANSICharType[c] <> [] then Result := Result+[c];
  end;
end;

{-------------------------------------------------------------------------------
*HasType - Returns true if any character in the string has TCharType properties
 as indicated by Flags.
 Example: HasType('Hello',[ctUpCase]) > True
 Example: HasType('hello',[ctUpCase]) > False
-------------------------------------------------------------------------------}
function TStringManager.HasType(const s : String; const Flags : TCharTypeFlags) : Boolean;
var i : Integer;
begin
  Result := True;
  for i := 1 to Length(s) do if Flags*gaANSICharType[s[i]] <> [] then Exit;
  Result := False;
end;

{-------------------------------------------------------------------------------
*HasAlpha - Returns true if the string contains any alphabetical characters.
 Example: HasAlpha('Hello 12') > True
See also HasType, HasAlpha, GetAlpha
-------------------------------------------------------------------------------}
function TStringManager.HasAlpha(const s : String) : Boolean;
begin
  Result := HasType(s, [cfUpper, cfLower]);
end;

{-------------------------------------------------------------------------------
*HasUp - Returns true if the string contains any uppercase characters.
 Example: HasUp('Hello') > True
See also HasType
-------------------------------------------------------------------------------}
function TStringManager.HasUp(const s : String) : Boolean;
begin
  Result := HasType(s, [cfUpper]);
end;

{-------------------------------------------------------------------------------
*HasLo - Returns true if the string contains any lowercase characters.
 Example: HasLo('Hello') > True
See also HasType
-------------------------------------------------------------------------------}
function TStringManager.HasLo(const s : String) : Boolean;
begin
  Result := HasType(s, [cfLower]);
end;

{-------------------------------------------------------------------------------
*HasDigit - Returns true if the string contains any Digits (0..9)
 Example: HasDigit('Hello12') > True
-------------------------------------------------------------------------------}
function TStringManager.HasDigit(const s : String) : Boolean;
begin
  Result := HasType(s, [cfDigit]);
end;

{-------------------------------------------------------------------------------
*HasChars - Returns true if a string contains any characters in aSet.
 Example: HasChars('Hello',['a'..'z']) > True
 Example: HasChars('Hello',['0'..'9']) > False
See also OnlyChars, CountChars
-------------------------------------------------------------------------------}
function TStringManager.HasChars(const s : String; const aSet : TCharSet) : Boolean;
var i : Integer;
begin
  Result := True;
  for i := 1 to Length(s) do if (s[i] in aSet) then Exit;
  Result := False;
end;

{-------------------------------------------------------------------------------
*OnlyType - Returns true if all characters in the string has TCharType properties
 as indicated by Flags.
 Example:
   OnlyType('Hello',[ctUpCase]) > False
   OnlyType('hello',[ctLoCase]) > True
   OnlyType('hello.',[ctLoCase]) > False
   OnlyType('hello.',[ctLoCase, cfDelim]) > True
See also OnlyChars
-------------------------------------------------------------------------------}
function TStringManager.OnlyType(const s : String; const Flags : TCharTypeFlags) : Boolean;
var i : Integer;
begin
  Result := False;
  if s='' then Exit;
  for i := 1 to Length(s) do if Flags*gaANSICharType[s[i]] = [] then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*OnlyAlpha - Returns true if a string contains alphabetical characters only.
 Example: OnlyAlpha('Hello') > True
 Example: OnlyAlpha(' Hello') > False
-------------------------------------------------------------------------------}
function TStringManager.OnlyAlpha(const s : String) : Boolean;
begin
  Result := OnlyType(s, [cfUpper, cfLower]);
end;

{-------------------------------------------------------------------------------
*OnlyUp - Returns true if a string contains UpperCase characters only.
 Example: OnlyUp('Hello') > False
-------------------------------------------------------------------------------}
function TStringManager.OnlyUp(const s : String) : Boolean;
begin
  Result := OnlyType(s, [cfUpper]);
end;

{-------------------------------------------------------------------------------
*OnlyLo - Returns true if a string contains LowerCase characters only.
 Example: OnlyLo('Hello') > False
          OnlyLo('hello') > True
-------------------------------------------------------------------------------}
function TStringManager.OnlyLo(const s : String) : Boolean;
begin
  Result := OnlyType(s, [cfLower]);
end;

{-------------------------------------------------------------------------------
*OnlyDigit - Returns true if a string contains digits only.
 Example: OnlyDigit('Hello12') > False
 ANSI compatible
-------------------------------------------------------------------------------}
function TStringManager.OnlyDigit(const s : String) : Boolean;
begin
  Result := OnlyType(s, [cfDigit]);
end;

{-------------------------------------------------------------------------------
*OnlyChars - Returns true if a string only contains character in the Set.
 Example: OnlyChars('Hello',['a'..'z']) > False
 Example: OnlyChars('Hello',['A'..'z']) > True
See also HasChars, CountChars
-------------------------------------------------------------------------------}
function TStringManager.OnlyChars(const s : String; const aSet : TCharSet) : Boolean;
var i : Integer;
begin
  Result := False;
  if s='' then Exit;
  for i := 1 to Length(s) do if not (s[i] in aSet) then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*Pos - Overloaded - Same as the standard Pos function except that:
  - if you use enable StrManEnableASM then it is faster - thanks to Peter Morris
     (see notes and credits at FastMemPos)
  - an optional starting position can be specified.
 Example: Pos('the','the man there') > 1
 Example: Pos('the','the man there',2) > 9
 Example: Pos('THE','the man there',2) > 0
See also PosIC, After and overloaded StartPos version
-------------------------------------------------------------------------------}
{$IFDEF StrManEnableASM}
function TStringManager.Pos(const SubStr, s : String; StartPos : Integer = 1) : Integer;
begin
  if StartPos<1 then StartPos := 1;
  Result := Length(s);
  if StartPos>Result then
  begin
    Result := 0;
    Exit;
  end;
  Result := Integer(FastMemPos(s[StartPos], SubStr[1], Result-(StartPos-1), Length(SubStr)));
  if Result > 0 then
    Result := Result - Integer(@s[1]) +1;
end;
{$ELSE}
function TStringManager.Pos(const SubStr, s : String) : Integer;
begin
  Result := System.Pos(SubStr, s);
end;

function TStringManager.Pos(const SubStr, s : String; StartPos : Integer) : Integer;
var SubLen : Integer;
    SLen : Integer;
    i, j : Integer;
      si : Integer;
      c1 : Char;
begin
  Result := 0;
  SubLen := Length(SubStr);
  if SubLen=0 then Exit;
  SLen := Length(s);
  if StartPos<1 then StartPos := 1;
  if SubLen+StartPos>SLen+1 then Exit;
  c1 := SubStr[1];
  for i := StartPos to SLen do
  begin
    if s[i] = c1 then
    begin
      if SubLen+i>SLen+1 then Exit;
      si := i;
      j := 2;
      while j<=SubLen do
      begin
        Inc(si);
        if s[si] <> SubStr[j] then Break;
        Inc(j);
      end;
      if j=SubLen+1 then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
*Pos - Overloaded - Non case sensitive Pos function.
 Example: Pos('the','the man there') > 1
 Example: Pos('the','the man there',2) > 9
 Example: Pos('THE','the man there',2) > 9
See also PosIC, After and overloaded StartPos version
-------------------------------------------------------------------------------}
function TStringManager.PosIC(const SubStr, s : String; StartPos : Integer = 1) : Integer;
{$IFDEF StrManEnableASM}
begin
  if StartPos<1 then StartPos := 1;
  Result := Length(s);
  if StartPos>Result then
  begin
    Result := 0;
    Exit;
  end;
  Result := Integer(FastMemPosNC(s[StartPos], SubStr[1], Result-(StartPos-1), Length(SubStr)));
  if Result > 0 then  Result := Result - Integer(@s[1]) +1;
end;
{$ELSE}
var SubLen : Integer;
     SLen : Integer;
     i, j : Integer;
       si : Integer;
       c1 : Char;
begin
  Result := 0;
  SubLen := Length(SubStr);
  if SubLen=0 then Exit;
  SLen := Length(s);
  if StartPos=0 then StartPos := 1;
  if SubLen+StartPos>SLen+1 then Exit;
  c1 := gaANSIUpperArray[SubStr[1]];
  for i := StartPos to SLen do
  begin
    if gaANSIUpperArray[s[i]] = c1 then
    begin
      if SubLen+i>SLen+1 then Exit;
      si := i;
      j := 2;
      while j<=SubLen do
      begin
        Inc(si);
        if gaANSIUpperArray[s[si]] <> gaANSIUpperArray[SubStr[j]] then Break;
        Inc(j);
      end;
      if j=SubLen+1 then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
*Pos - Just boolean case overloaded version.
-------------------------------------------------------------------------------}
function TStringManager.Pos(const SubStr, s : String; IgnoreCase : Boolean; StartPos : Integer = 1) : Integer;
begin
  if IgnoreCase
    then Result := PosIC(SubStr, s, StartPos)
    else Result := Pos(SubStr, s);
end;

{-------------------------------------------------------------------------------
*PosRev - Same as standard Pos string function except that it scans backwards.
Use optional StartPos to specify position to start scan backwards from.
If StartPos is not specified then it will start at the end (length) of s.

Example:  PosRev('the','the man there') > 9
          PosRev('the','the man there',5) > 1
See also Pos, PosRevIC
-------------------------------------------------------------------------------}
function TStringManager.PosRev(const SubStr : String; const s : String; StartPos : Integer = -1) : Integer;
{$IFDEF StrManEnableASM}
begin
  Result := FastPosBack(s, SubStr, Length(s), Length(SubStr), StartPos);
end;
{$ELSE}
var SubLen : Integer;
      SLen : Integer;
     Match : Boolean;
      i, j : Integer;
        si : Integer;
        c1 : Char;
begin
  Result := 0;
  SubLen := Length(SubStr);
  if SubLen=0 then Exit;
  SLen := Length(s);
  if (StartPos<1) or (StartPos>SLen) then StartPos := SLen;
  if SubLen>StartPos then Exit;
  c1 := SubStr[1];
  for i := StartPos downto 1 do
  begin
    if s[i] = c1 then
    begin
      Match := True;
      si := i;
      for j := 2 to SubLen do
      begin
        Inc(si);
        if si>SLen then Match := False else if s[si] <> SubStr[j] then Match := False;
        if Match=False then Break;
      end;
      if Match=True then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
*PosRevIC - Scans backwards, and returns the index value of the first
occurance of SubStr in s irrespective of case.
Use optional StartPos to specify position to start scan backwards from.
If StartPos is not specified then it will start at the end (length) of s.
Example:  PosRevIC('The','the man there') > 9
          PosRevIC('The','the man there',5) > 1
See also PosRev, Pos
-------------------------------------------------------------------------------}
function TStringManager.PosRevIC(const SubStr : String; const s : String; StartPos : Integer = -1) : Integer;
{$IFDEF StrManEnableASM}
begin
  Result := FastPosBackNoCase(s, SubStr, Length(s), Length(SubStr), StartPos);
end;
{$ELSE}
var SubLen : Integer;
      SLen : Integer;
     Match : Boolean;
      i, j : Integer;
        si : Integer;
       c1 : Char;
begin
  Result := 0;
  SubLen := Length(SubStr);
  if SubLen=0 then Exit;
  SLen := Length(s);
  if (StartPos<1) or (StartPos>SLen) then StartPos := SLen;
  if SubLen>StartPos then Exit;
  c1 := gaANSIUpperArray[SubStr[1]];
  for i := StartPos downto 1 do
  begin
    if gaANSIUpperArray[s[i]] = c1 then
    begin
      Match := True;
      si := i;
      for j := 2 to SubLen do
      begin
        Inc(si);
        if si>SLen then Match := False
          else if gaANSIUpperArray[s[si]] <> gaANSIUpperArray[SubStr[j]] then Match := False;
        if Match=False then Break;
      end;
      if Match=True then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;
{$ENDIF}


{-------------------------------------------------------------------------------
*Pos - Char version - returns first occurance of c in s with an optional
 starting position.
 Example: Pos('t','the man there') > 1
 Example: Pos('t','the man there',2) > 9
 Example: Pos('T','the man there',2) > 0
 Note that StartPos 1 and 0 are functionally the same.
See also PosIC, After and overloaded String version
-------------------------------------------------------------------------------}
function TStringManager.Pos(c : Char; const s : String; StartPos : Integer = 1) : Integer;
var i : Integer;
begin
  Result := 0;
  if StartPos<1 then StartPos := 1;
  for i := StartPos to Length(s) do
  begin
    if c=s[i] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PosBefore - returns the position of the first occurance of any character
 in inSet in the string, unless any character in BeforeSet is not found first.
   Example: PosBefore(['='],'a=b',[';']) > 2
   Example: PosBefore(['='],'; a=b',[';']) > 0
 Note that StartPos 1 and 0 are functionally the same.
See also Pos, PosIC, After
-------------------------------------------------------------------------------}
function TStringManager.PosBefore(const inSet : TCharSet; const s : String; const BeforeSet : TCharSet; StartPos : Integer = 1) : Integer;
var i : Integer;
begin
  Result := 0;
  if StartPos<1 then StartPos := 1;
  for i := StartPos to Length(s) do
  begin
    if s[i] in BeforeSet then Exit;
    if s[i] in inSet then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PosIC - Char version - returns first occurance of c in s irrespective of case
 with an optional starting position.
 Example: PosIC('t','the man there') > 1
 Example: PosIC('t','the man there',2) > 9
 Example: PosIC('T','the man there',2) > 9
 Note that StartPos 1 and 0 are functionally the same.
See also Pos, PosIC, After and overloaded String version
-------------------------------------------------------------------------------}
function TStringManager.PosIC(c : Char; const s : String; StartPos : Integer = 1) : Integer;
var i : Integer;
begin
  Result := 0;
  if StartPos<1 then StartPos := 1;
  PUpCase(c);
  for i := StartPos to Length(s) do
  begin
    if c=gaANSIUpperArray[s[i]] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PosRev - Char version - Reverse scanning for a character in a string.
 Example: PosRev('t','the man there') > 9
 Example: PosRev('T','the man there') > 0
See also Pos, PosRevIC
-------------------------------------------------------------------------------}
function TStringManager.PosRev(c : Char; const s : String) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
  begin
    if c=s[i] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PosRev - Char version - Reverse scanning for a character in a string irrespective of case.
 Example: PosRec('t','the man there') > 9
 Example: PosRec('T','the man there') > 9
See also Pos, PosRev, PosIC
-------------------------------------------------------------------------------}
function TStringManager.PosRevIC(c : Char; const s : String) : Integer;
var i : Integer;
begin
  Result := 0;
  PUpCase(c);
  for i := Length(s) downto 1 do
  begin
    if c=gaANSIUpperArray[s[i]] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*Pos - CharSet version - returns the position of the first character found in aSet.
Note that StartPos 1 and 0 are functionally the same.

Example:
  Pos(['a'..'z'],'the man there') > 1
  Pos(['a'..'z'],'THE man there') > 5

See also overload String version
-------------------------------------------------------------------------------}
function TStringManager.Pos(const aSet : TCharSet; const s : String; StartPos : Integer = 1) : Integer;
var i : Integer;
begin
  Result := 0;
  if StartPos<1 then StartPos := 1;
  for i := StartPos to Length(s) do
  begin
    if s[i] in aSet then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PosRev - CharSet version - scans backwords and returns first character
found in aSet.

Example:
  PosRev(['a'..'z'],'the man there') > 13
  PosRev(['h'],'the man there') > 10

See also overload String and Char versions
-------------------------------------------------------------------------------}
function TStringManager.PosRev(const aSet : TCharSet; const s : String) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
  begin
    if s[i] in aSet then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*WordPos - Returns the position of a WholeWord in a string.
 An optional starting position can also be specified.
 Example: Wordpos('the','there is the word.') > 10
See also WordPosIC, DelWord
-------------------------------------------------------------------------------}
function TStringManager.WordPos(const SubStr, s : String; const Delims : TCharSet = gcWordDelims; StartPos : Integer = 1) : Integer;
var SubLen : Integer;
    SLen : Integer;
     i, j : Integer;
      si : Integer;
      c1 : Char;
begin
  Result := 0;
  SubLen := Length(SubStr);
  if SubLen=0 then Exit;
  SLen := Length(s);
  if StartPos<1 then StartPos := 1;
  if SubLen+StartPos>SLen+1 then Exit;
  c1 := SubStr[1];
  for i := StartPos to SLen do
  begin
    if s[i] = c1 then if (i=1) or (s[i-1] in Delims) then
    begin
      if SubLen+i>SLen+1 then Exit;
      si := i;
      j := 2;
      while j<=SubLen do
      begin
        Inc(si);
        if s[si] <> SubStr[j] then Break;
        Inc(j);
      end;
      if j=SubLen+1 then
      begin
        if (si>=Slen) or (s[si+1] in Delims) then
        begin
          Result := i;
          Exit;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*WordPosIC - Returns the position of a WholeWord in a string irrespective of case.
 Example: WordposIC('The','there is the word.') > 10
See also WordPos
-------------------------------------------------------------------------------}
function TStringManager.WordPosIC(const SubStr, s : String; const  Delims : TCharSet = gcWordDelims; StartPos : Integer = 1) : Integer;
var SubLen : Integer;
    SLen : Integer;
    i, j : Integer;
      si : Integer;
      c1 : Char;
begin
  Result := 0;
  SubLen := Length(SubStr);
  if SubLen=0 then Exit;
  SLen := Length(s);
  if StartPos<1 then StartPos := 1;
  if SubLen+StartPos>SLen+1 then Exit;

  c1 := gaANSIUpperArray[SubStr[1]];
  for i := StartPos to SLen do
  begin
    if gaANSIUpperArray[s[i]] = c1 then if (i=1) or (s[i-1] in Delims) then
    begin
      if SubLen+i>SLen+1 then Exit;
      si := i;
      j := 2;
      while j<=SubLen do
      begin
        Inc(si);
        if gaANSIUpperArray[s[si]] <> gaANSIUpperArray[SubStr[j]] then Break;
        Inc(j);
      end;
      if j=SubLen+1 then
      begin
        if (si>=Slen) or (s[si+1] in Delims) then
        begin
          Result := i;
          Exit;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*FindSame - Returns 0 based index of first s found in aList while
  ignoring control characters and leading/trailing and double spaces.
  Example:
    FindSame('Peter',[' joe','Mary','  Peter ','Jack']) > 2
    FindSame('peter',[' joe','Mary','  Peter ','Jack']) > -1
-------------------------------------------------------------------------------}
function TStringManager.FindSame(const s : String; const aList : array of String) : Integer;
var i : Integer;
begin
  Result := -1;
  for i := Low(aList) to High(aList) do
  begin
    if IsSame(s, aList[i]) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*FindSameIC - Returns 0 based index of first s found in aList while
  ignoring case, control characters and leading/trailing and double spaces.
  Example:
    FindSame('peter',[' joe','Mary','  Peter ','Jack']) > 2
-------------------------------------------------------------------------------}
function TStringManager.FindSameIC(const s : String; const aList : array of String) : Integer;
var i : Integer;
begin
  Result := -1;
  for i := Low(aList) to High(aList) do
  begin
    if IsSameIC(s, aList[i]) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*FindSame - Returns 0 based index of first s found in aList while
  ignoring control characters and leading/trailing and double spaces.
  Example:
    FindSame('Peter',[' joe','Mary','  Peter ','Jack']) > 2
    FindSame('peter',[' joe','Mary','  Peter ','Jack']) > -1
-------------------------------------------------------------------------------}
function TStringManager.FindSame(const s : String; const aList : TStrings) : Integer;
var i : Integer;
begin
  Result := -1;
  for i := 0 to aList.Count-1 do
  begin
    if IsSame(s, aList[i]) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*FindSameIC - Returns 0 based index of first s found in aList while
  ignoring case, control characters and leading/trailing and double spaces.
  Example:
    FindSame('peter',[' joe','Mary','  Peter ','Jack']) > 2
-------------------------------------------------------------------------------}
function TStringManager.FindSameIC(const s : String; const aList : TStrings) : Integer;
var i : Integer;
begin
  Result := -1;
  for i := 0 to aList.Count-1 do
  begin
    if IsSameIC(s, aList[i]) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*HasSame - Just a boolean version of the FindSame function
-------------------------------------------------------------------------------}
function TStringManager.HasSame(const s : String; const aList : array of String) : Boolean;
begin
  Result := FindSame(s, aList)>=0;
end;

{-------------------------------------------------------------------------------
*HasSameIC - Just a boolean version of the FindSameIC function
-------------------------------------------------------------------------------}
function TStringManager.HasSameIC(const s : String; const aList : array of String) : Boolean;
begin
  Result := FindSameIC(s, aList)>=0;
end;

{-------------------------------------------------------------------------------
*HasSame - Just a boolean version of the FindSame function
-------------------------------------------------------------------------------}
function TStringManager.HasSame(const s : String; const aList : TStrings) : Boolean;
begin
  Result := FindSame(s, aList)>=0;
end;

{-------------------------------------------------------------------------------
*HasSameIC - Just a boolean version of the FindSameIC function
-------------------------------------------------------------------------------}
function TStringManager.HasSameIC(const s : String; const aList : TStrings) : Boolean;
begin
  Result := FindSameIC(s, aList)>=0;
end;

{-------------------------------------------------------------------------------
*After
 Scans for SubStr in s - if found the characters after SubStr is returned else
 an empty string is returned.
 Examples:
   After('Funny','Those Funny People') > ' People';
   After('land','Your land is my land ok',1) > ' is my land ok'
   After('land','Your land is my land ok',7) > ' ok'
   After('not there','Your land is my land ok') > ''
See also AfterOrAll, Before, Parse, AfterIC, AfterRev and Set overloaded versions.
-------------------------------------------------------------------------------}
function TStringManager.After(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p : Integer;
begin
  p := Pos(SubStr, s, Position);
  if p<=0
    then Result := ''
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*AfterOrAll - Alternative to the After function.
 Scans for SubStr in s - if found the characters after SubStr is returned else
 the complete string is returned.
 Examples:
   AfterOrAll('a','abc') > 'bc'
   AfterOrAll('1','abc') > 'abc'
        After('1','abc') > ''
See also After, AfterOrAllIC, Before.
-------------------------------------------------------------------------------}
function TStringManager.AfterOrAll(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p : Integer;
begin
  p := Pos(SubStr, s, Position);
  if p<=0
    then Result := s
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*AfterIC - Same as function After except case insensitive.
 Example: AfterIC('FUNNY','Those Funny People' ) > ' People';
See also After, BeforeIC
-------------------------------------------------------------------------------}
function TStringManager.AfterIC(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p : Integer;
begin
  p := PosIC(SubStr, s, Position);
  if p<=0
    then Result := ''
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*AfterOrAllIC - Same as function AfterOrAll except case insensitive.
 Example: AfterOrAllIC('1','abc') > 'abc'
See also After
-------------------------------------------------------------------------------}
function TStringManager.AfterOrAllIC(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p : Integer;
begin
  p := PosIC(SubStr, s, Position);
  if p<=0
    then Result := s
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*AfterNth - Returns the string after the <Nth> appearance of SubStr.
 Example:
   AfterNth('tree','Green tree, blue tree everywhere',1) > ', blue tree everywhere'
   AfterNth('tree','Green tree, blue tree everywhere',2) > ' everywhere'
   AfterNth('a','aaaaa',3) > 'aa'
   AfterNth('aa','aaaaa',2) > 'a' 
See also After, BeforeNth
-------------------------------------------------------------------------------}
function TStringManager.AfterNth(const SubStr : String; const s : String; Nth : Integer = 1) : String;
var   i : Integer;
      p : Integer;
begin
  if Nth<1 then Nth := 1;
  p := 0;
  for i := 1 to Nth do
  begin
    p := Pos(SubStr, s, p+1);
    if p<=0 then Break else if i<Nth then Inc(p, Length(SubStr)-1);
  end;
  if p<=0
    then Result := ''
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*BeforeNth
 Returns the string before the <Nth> appearance of SubStr.
 Example:
  BeforeNth('tree','Green tree, blue tree everywhere',1) > 'Green '
  BeforeNth('tree','Green tree, blue tree everywhere',2) > 'Green tree, blue '
  BeforeNth('a','aaaaa',3) > 'aa'

 Note that as function Before, if the SubStr is not found the complete string is returned.
 Example: BeforeNth('Z','aaaaa',3) > 'aaaaa'

 See also Before, AfterNth
-------------------------------------------------------------------------------}
function TStringManager.BeforeNth(const SubStr : String; const s: String; Nth : Integer = 1): String;
var i : Integer;
    p : Integer;
begin
  if Nth<1 then Nth := 1;
  p := 0;
  for i := 1 to Nth do
  begin
    p := Pos(SubStr, s, p+1);
    if p<=0 then Break else if i<Nth then Inc(p, Length(SubStr)-1);
  end;
  if p<=0
    then Result := s
    else Result := System.Copy(s, 1, p-1);
end;

{-------------------------------------------------------------------------------
*Before
 Scans for SubStr in s - if found the characters before SubStr is returned else
 the complete string is returned.
 Example: Before(' People','Those Funny People') > 'Those Funny';
 Example: Before('land','Your land is my land',1) > 'Your';
 Example: Before('land','Your land is my land',7) > 'Your land is my ';
 Example: Before('not there','Your land is my land') > 'Your land is my land'
See also After, BeforeIC, BeforeRev
-------------------------------------------------------------------------------}
function TStringManager.Before(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p : Integer;
begin
  p := Pos(SubStr, s, Position);
  if p<=0
    then Result := s
    else Result := System.Copy(s, 1, p-1);
end;

{-------------------------------------------------------------------------------
*BeforeIC - Same as function Before but is case insensitive.
 Example: BeforeIC(' PEOPLE','Those Funny People') > 'Those Funny';
See also Before
-------------------------------------------------------------------------------}
function TStringManager.BeforeIC(const SubStr : String; const s : String; Position : Integer = 1) : String;
var p : Integer;
begin
  p := PosIC(SubStr, s, Position);
  if p<=0
    then Result := s
    else Result := System.Copy(s, 1, p-1);
end;

{-------------------------------------------------------------------------------
*BeforeRev
 This function scans a string from the right and returns the portion of the
 string before SubStr.
 Example: BeforeRev('.','c:\my.file.txt') > 'c:\my.file'
 Example: BeforeRev('[','c:\my.file.txt') > 'c:\my.file.txt' 
See also Before, AfterRev
-------------------------------------------------------------------------------}
function TStringManager.BeforeRev(const SubStr : String; const s : String) : String;
 var p : Integer;
begin
  p := PosRev(SubStr, s);
  if p=0
    then Result := s
    else Result := System.Copy(s, 1, p-1);
end;

{-------------------------------------------------------------------------------
*BeforeRevIC
 Case insensitive version of the BeforeRev function.
 Example:
   BeforeRevIC('FILE.','c:\file.file.txt') > 'c:\file'
See also Before, AfterRevIC
-------------------------------------------------------------------------------}
function TStringManager.BeforeRevIC(const SubStr : String; const s : String) : String;
 var p : Integer;
begin
  p := PosRevIC(SubStr, s);
  if p=0
    then Result := s
    else Result := System.Copy(s, 1, p-1);
end;

{-------------------------------------------------------------------------------
*AfterRev
 This functions scans s for SubStr from the right and returns the portion after
 SubStr.
 Example: AfterRev('.','c:\my.file.txt') > '.txt'
See also After, BeforeRev
-------------------------------------------------------------------------------}
function TStringManager.AfterRev(const SubStr : String; const s : String) : String;
var p : Integer;
begin
  p := PosRev(SubStr, s);
  if p<=0
    then Result := ''
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*AfterRevIC - Case insensitive version of the AfterRev function.
 Example: AfterRevIC('FILE.','c:\file.file.txt') > 'txt'
See also After, BeforeRev
-------------------------------------------------------------------------------}
function TStringManager.AfterRevIC(const SubStr : String; const s : String) : String;
var p : Integer;
begin
  p := PosRevIC(SubStr, s);
  if p<=0
    then Result := ''
    else Result := System.Copy(s, p+Length(SubStr), Length(s));
end;

{-------------------------------------------------------------------------------
*Between - Scans s for the start/end combination of SubStr1, SubStr2 and
 returns the text between them.
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.
 If SubStr2 is not found, then an empty string is returned (as opposed to function Mid).

Example:
  Between('<-','->','<-a->  <-b->') > 'a'
  Between('(',')','(a(b(c)))  (d(e))') > 'a(b(c))'
  Between('(',')','(a(b(c))  (d(e') > ''
  Between('\','\','c:\winnt\system\util.dll') > 'winnt'
  Between('\','\','c:\winnt') > ''

See also BetweenRev, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.Between(const SubStr1, SubStr2 : String; const s : String; StartPos : Integer = 1) : String;
var EndPos : Integer;
begin
  if PosBetween(SubStr1, SubStr2, s, StartPos, EndPos)>0
    then Result := System.Copy(s, StartPos, (EndPos-StartPos)+1)
    else Result := '';
end;

{-------------------------------------------------------------------------------
*BetweenRev - Scans s backwards for the start/end combination of SubStr1, SubStr2 and
 returns the text between them.
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.
 If SubStr2 is not found, then an empty string is returned (as opposed to function Mid).

Example:
  BetweenRev('<-','->','<-a->  <-b->') > '-b'
  BetweenRev('(',')','(a(b(c)))  (d(e))') > 'd(e)'
  BetweenRev('(',')','(a(b(c))  (d(e') > ''
  BetweenRev('\','\','c:\winnt\system\util.dll') > 'system'
  BetweenRev('\','\','c:\winnt') > ''

See also Between, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.BetweenRev(const SubStr1, SubStr2 : String; const s : String; StartPos : Integer = -1) : String;
var EndPos : Integer;
begin
  if PosBetweenRev(SubStr1, SubStr2, s, StartPos, EndPos)>0
    then Result := System.Copy(s, StartPos, (EndPos-StartPos)+1)
    else Result := '';
end;

{-------------------------------------------------------------------------------
*Between - Char - Scans s for the start/end combination of Delim1, Delim2 and
 returns the text between them.
 If Delim2 is empty Delim2 will be regarded as identical to Delim1.
 If Delim2 is not found, then an empty string is returned (as opposed to function Mid).

Example:
  Between(['('],[')'],'(a(b(c)))  (d(e))') > 'a(b(c))'
  Between(['('],[')'],'(a(b(c))  (d(e') > ''
  Between(['\'],['\'],'c:\winnt\system\util.dll') > 'winnt'
  Between(['\'],['\'],'c:\winnt') > ''
Extraction:
   s := 'call(b(c(4*(1+3))))';
   while s<>'' do
   begin
     s := st.Between('(',')', s);
   end;

See also BetweenRev, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.Between(const Delim1, Delim2 : Char; const s : String; StartPos : Integer = 1) : String;
var EndPos : Integer;
begin
  if PosBetween(Delim1, Delim2, s, StartPos, EndPos)>0
    then Result := System.Copy(s, StartPos, (EndPos-StartPos)+1)
    else Result := '';
end;

{-------------------------------------------------------------------------------
*BetweenRev - Char - Scans s backwards for the start/end combination
 of Delim1, Delim2 and returns the text between them.
 If Delim2 is empty Delim2 will be regarded as identical to Delim1.
 If Delim2 is not found, then an empty string is returned (as opposed to function Mid).
Example:
  BetweenRev(['('],[')'],'(a(b(c)))  (d(e))') > 'd(e)'
  BetweenRev(['('],[')'],'(a(b(c))  (d(e') > ''
  BetweenRev(['\'],['\'],'c:\winnt\system\util.dll') > 'system'
  BetweenRev(['\'],['\'],'c:\winnt') > ''
See also Between, Mid, Before, After

-------------------------------------------------------------------------------}
function TStringManager.BetweenRev(const Delim1, Delim2 : Char; const s : String; StartPos : Integer = -1) : String;
var EndPos : Integer;
begin
  if PosBetweenRev(Delim1, Delim2, s, StartPos, EndPos)>0
    then Result := System.Copy(s, StartPos, (EndPos-StartPos)+1)
    else Result := '';
end;

{-------------------------------------------------------------------------------
*Mid - Scans s and returns text after the first SubStr1 and before SubStr2 if found.
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.
 If SubStr2 is not found the complete string after SubStr1 is returned (as opposed to function Between).
 This function is the equivalent of a Before(After combination, but faster.
Example:
  Mid('(',')','(a) (b) (c)') > a
  Mid('(',')','(a(b)(c)  ((d)e)') > a(b
  Mid('(',')','(a(b(c)))') > a(b(c
  Mid('(',')','(a(b(c)') > a(b(c
  Mid('\','','c:\winnt\system\util.dll') > winnt
  Mid('\','','c:\winnt') > winnt
Note that function Between will return the first innermost text:
  Between('(',')','(a(b)(c)' > 'b'
See also MidRev, Between, Before, After
-------------------------------------------------------------------------------}
function TStringManager.Mid(const SubStr1, SubStr2 : String; const s : String; Position : Integer = 1) : String;
var p1,p2 : Integer;
begin
  p1 := Pos(SubStr1, s, Position);
  if p1<=0
  then Result := '' else
  begin
    if SubStr2=''
    then p2 := Pos(SubStr1, s, p1+Length(SubStr1))
    else p2 := Pos(SubStr2, s, p1+Length(SubStr1));
    if p2<=0
      then Result := System.Copy(s, p1+Length(SubStr1), Length(s))
      else Result := CopyPos(s, p1+Length(SubStr1), p2-1);
  end;
end;

{-------------------------------------------------------------------------------
*MidRev - Scans s backwards and returns text after the first SubStr1 and before SubStr2 if found.
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.
 If SubStr2 is not found the complete string after SubStr1 is returned (as opposed to function Between).
 This function is the equivalent of a Before(AfterRev combination, but faster.
Example:
  MidRev('(',')','(a) (b) (c)') > c
  MidRev('(',')','(a(b)(c)  (d)(e') > e
  MidRev('(',')','(a(b(c)))') > c
  MidRev('(',')','(a(b(c)') > c
  MidRev('\','','c:\winnt\system\util.dll') > util.dll
  MidRev('\','','c:\winnt') > winnt
Note that function BetweenRev will return the last innermost text:
  BetweenRev('(',')','(a(b)(c)  (d)(e'' > 'd'
See also Mid, BetweenRev, Before, After
-------------------------------------------------------------------------------}
function TStringManager.MidRev(const SubStr1, SubStr2 : String; const s : String) : String;
var p1,p2 : Integer;
begin
  p1 := PosRev(SubStr1, s);
  if p1<=0
  then Result := '' else
  begin
    if SubStr2=''
    then p2 := Pos(SubStr1, s, p1+Length(SubStr1))
    else p2 := Pos(SubStr2, s, p1+Length(SubStr1));
    if p2<=0
      then Result := System.Copy(s, p1+Length(SubStr1), Length(s))
      else Result := CopyPos(s, p1+Length(SubStr1), p2-1);
  end;
end;

{-------------------------------------------------------------------------------
*Pad
 Ads a character at the end of the string until the length is equal to PadLen.
 If PadLen is negative the character will be inserted to the left.
 Example: Pad('hello',7)  > 'hello  '
 Example: Pad('hello',7,' ')  > 'hello  '
 Example: Pad('hello',-7,'.') > '..hello'
 Example: Pad('hello',1,'.') > 'hello'
See also PadCut
-------------------------------------------------------------------------------}
function TStringManager.Pad(const s : String; PadLen : SmallInt; c : Char = ' ') : String;
var  i : Integer;
  More : Integer;
  Slen : Integer;
begin
  SLen := Length(s);
  More := Abs(PadLen) - Slen;
  if More>0 then
  begin
    if PadLen<0 then
    begin
      SetLength(Result, Abs(PadLen));
      System.Move(s[1], Result[More+1], Slen);
      for i := 1 to More do Result[i] := c;
    end else
    begin
      Result := s;
      SetLength(Result, Abs(PadLen));
      for i := SLen+1 to Slen+More do Result[i] := c;
    end;
  end else Result := s;
end;

{-------------------------------------------------------------------------------
*PadCut
 Pads a character at the end of s while Length(s)<PadLen but allways returns
 the length as specified by PadLen.
 Example:
 PadCut('Warm Breeze',14) > 'Warm Breeze  '
 PadCut('Warm Breeze',-14,'.') > '..Warm Breeze'
 PadCut('Warm Breeze',2,' ') > 'Wa'
See also Pad
-------------------------------------------------------------------------------}
function TStringManager.PadCut(const s : String; PadLen : SmallInt; c : Char = ' ') : String;
var  i : Integer;
  More : Integer;
  Slen : Integer;
begin
  SLen := Length(s);
  More := Abs(PadLen) - Slen;
  if PadLen < 0 then
  begin
    if More > 0 then
    begin
      SetLength(Result, Abs(PadLen));
      System.Move(s[1], Result[More+1], Slen);
      for i := 1 to More do Result[i] := c;
    end else
    begin
      Result := s;
      SetLength(Result, Abs(PadLen));
    end;
  end else
  begin
    Result := s;
    SetLength(Result, Abs(PadLen));
    if More>0 then for i := SLen+1 to Slen+More do Result[i] := c;
  end;
end;

{-------------------------------------------------------------------------------
*PadLeft - Pads a character to the left while Length(s)<PadLen.
 PadLeft('hello',7)  > '  hello'
 PadLeft('hello',7,'.')  > '..hello'
Note that function Pad with a negative PadLen is functionally then same.
See also Pad
-------------------------------------------------------------------------------}
function TStringManager.PadLeft(const s : String; PadLen : Word; c : Char = ' ') : String;
begin
  Result := Pad(s, -PadLen, c);
end;

{-------------------------------------------------------------------------------
*BaseToStr - General function for converting binary, hex, octal or a custom base Key
 to a string.
  Example1:  Hex (Base 16)
    BaseToStr(255,'0123456789ABCDEF') >'FF'
    Note if i=255 and i is an Integer, overloaded BaseStr(i,'0123456789ABCDEF')
     will return '000000FF' (knows it is an 32 bit Integer)
  Example2: Binary (Base 2)
    BaseToStr(5,'01',8) > '00000101'
  Example3:  A..Z (Base 26)
    BaseToStr(675,'ABCDEFGHIJKLMNOPQRSTUVWXYZ',4) > 'AAZZ'
  Example4: 0..9, A..V (Base 32)
    BaseToStr(1023,'0123456789ABCDEFGHIJKLMNOPQRSTUV',4) > '00VV'

See also BaseStr, BaseVal, BaseRange, SysUtils.IntToHex
-------------------------------------------------------------------------------}
function TStringManager.BaseToStr(Value : Cardinal; const Key : String; Digits : Byte = 0) : String;
var x    : Cardinal;
    Base : Word;
begin
  Result := '';
  try
    Base := Length(Key);
    if Base=0 then DoRaise('No Key');
    Value := Value;
    repeat
      x := Value mod Base;
      Value := Value div Base;
      Result := Key[x+1]+Result;
    until Value=0;
    if Digits<>0 then Result := PadCut(Result, -Digits, Key[1]);
  except
    {$IFDEF StrManEnableERaise}
      eRaise('Key: '+Key, 'BaseToStr');
    {$ELSE}
      on e:Exception do raise Exception.Create('Key: '+Key+gcCR+'BaseToStr'+gcCR+e.Message);
    {$ENDIF}
  end;
end;

{-------------------------------------------------------------------------------
*BaseToStr - overloaded Int64 version
-------------------------------------------------------------------------------}
function TStringManager.BaseToStr(Value : Int64; const Key : String; Digits : Byte = 0) : String;
var x    : Int64;
    Base : Word;
begin
  Result := '';
  try
    Base := Length(Key);
    if Base=0 then DoRaise('No Key');
    if Value<0 then DoRaise('Value negative or too large'); //cannot do unsigned 64bit div
    Value := Value;
    repeat
      x := Value mod Base;
      Value := Value div Base;
      Result := Key[x+1]+Result;
    until Value=0;
    if Digits<>0 then Result := PadCut(Result, -Digits, Key[1]);
  except
    {$IFDEF StrManEnableERaise}
      eRaise('Key: '+Key, 'BaseToStr');
    {$ELSE}
      on e:Exception do raise Exception.Create('Key: '+Key+gcCR+'BaseToStr'+gcCR+e.Message);
    {$ENDIF}
  end;
end;

{-------------------------------------------------------------------------------
*BaseVal - General function for converting binary, hex, octal or a custom string
 to an integer according to a specified base Key.
 See notes at BaseToStr
 Example: BaseVal('FF','0123456789ABCDEF') > 255
 Example: BaseVal('0011','01') > 3
See also BaseToStr, BaseStr, BaseRange
-------------------------------------------------------------------------------}
function TStringManager.BaseVal(const s : String; const Key : String) : Int64;
var x : Int64;
    i : Integer;
 Base : Int64;
begin
  Result := 0;
  try
    Base := Length(Key);
    if Base=0 then DoRaise('No Key');
    for i := 1 to Length(s) do
    begin
      x := Pos(s[i], Key);
      if x=0 then DoRaise(Key[i]+' missing in "'+s+'" - not found in Key');
      System.Dec(x);
      Result := (Result*Base)+x;
    end;
  except
    {$IFDEF StrManEnableERaise}
      eRaise('Key: '+Key, 'BaseVal');
    {$ELSE}
      on e:Exception do raise Exception.Create('Key: '+Key+gcCR+'BaseVal'+gcCR+e.Message);
    {$ENDIF}
  end;
end;

{-------------------------------------------------------------------------------
*BaseVal - overloaded TCharSet version
-------------------------------------------------------------------------------}
function TStringManager.BaseVal(const s : String; const Key : TCharSet) : Int64;
begin
  Result := BaseVal(s, Str(Key));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'0000000F'
 Note only a positive Int64 supported: 0..7FFFFFFFFFFFFFFF
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : Int64; const Key : String) : String;
begin
  Result := BaseToStr(Value, Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'0000000F'
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : Cardinal; const Key : String) : String;
begin
  Result := BaseToStr(Value, Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'0F'
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : ShortInt; const Key : String) : String;
begin
  Result := BaseToStr(Byte(Value), Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'000F'
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : SmallInt; const Key : String) : String;
begin
  Result := BaseToStr(Word(Value), Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'000F'
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : Word; const Key : String) : String;
begin
  Result := BaseToStr(Value, Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'0F'
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : Byte; const Key : String) : String;
begin
  Result := BaseToStr(Value, Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStr - overload function - passes the correct digits according to the
 Byte size of Value - Example: BaseStr($F,'0123456789ABCDEF') >'0000000F'
-------------------------------------------------------------------------------}
function TStringManager.BaseStr(Value : Integer; const Key : String) : String;
begin
  Result := BaseToStr(Cardinal(Value), Key, BaseStrLen(Key, SizeOf(Value)));
end;

{-------------------------------------------------------------------------------
*BaseStrR - see notes at function BaseToStr.
 This function performs an additional range check.
-------------------------------------------------------------------------------}
function TStringManager.BaseStrR(Value : Int64; const Key : String; Digits : Byte) : String;
begin
  if Value<0
    then DoRaise('Only positive Int64 values supported '+gcCR+'Value: '+IntToStr(Value), 'BaseStrR');
  Result := BaseToStr(Value, Key, Digits);
end;

{-------------------------------------------------------------------------------
*BaseRange - returns the biggest possible Integer if using n Digits
 according to a specified base Key (zero based range)
 Example: BaseRange('0123456789ABCDEF',1) > 15  ($f)
 Example: BaseRange('0123456789ABCDEF',2) > 255 ($ff)
 See notes at BaseToStr
--------------------------------------------------------------------------------}
function TStringManager.BaseRange(const Key : String; Digits : Byte) : Int64;
    function IntPower(x : Int64): Int64;
    var y: Int64;
    begin
      y := Digits;
      Result := 1;
      while y > 0 do
      begin
        while not Odd(y) do
        begin
          y := y shr 1;
          x := x * x
        end;
        Dec(y);
        Result := Result * x
      end;
    end;
begin
  Result := IntPower(Length(Key));
  Dec(Result);
  if Result<0 then Result := $7FFFFFFFFFFFFFFF;
end;

{-------------------------------------------------------------------------------
*BaseRange - overloaded charset version
-------------------------------------------------------------------------------}
function TStringManager.BaseRange(const Key : TCharSet; Digits : Byte) : Int64;
begin
  Result := BaseRange(Str(Key), Digits);
end;

{-------------------------------------------------------------------------------
*BaseStrLen - returns the length of a string if a byte, word or cardinal is
 converted to a base string.
 Example: BaseStrLen('0123456789ABCDEF',4) > 8 (Cardinal is 'FFFFFFFF')
 Example: BaseStrLen('01',1) > 8 (Byte is '11111111')
 Example: BaseStrLen('0123',1) > 4 (Byte is '3333')
 Example: BaseStrLen('01', SizeOf(Word)) > 16 (Word is '1111111111111111')
--------------------------------------------------------------------------------}
function TStringManager.BaseStrLen(const Key : String; ByteSize : TInt64NoOfBytes) : Byte;
begin
  case ByteSize of
    1 : Result := Length(BaseToStr($ff, Key));
    2 : Result := Length(BaseToStr($ffff, Key));
    3 : Result := Length(BaseToStr($ffffff, Key));
    4 : Result := Length(BaseToStr($ffffffff, Key));
    5 : Result := Length(BaseToStr($ffffffffff, Key));
    6 : Result := Length(BaseToStr($ffffffffffff, Key));
    7 : Result := Length(BaseToStr($ffffffffffffff, Key));
    8 : Result := Length(BaseToStr($7fffffffffffffff, Key));
    else Result := 0;
  end;
end;

{-------------------------------------------------------------------------------
*RepeatChar - Sets a string with a certain character and length.
 Example: RepeatChar('a',3) > 'aaa'
See also RepeatStr
-------------------------------------------------------------------------------}
function TStringManager.RepeatChar(c : Char; Count : Integer) : String;
begin
  SetLength(Result, Count);
  if Count>0
    then FillChar(Result[1], Count, c);
end;

{-------------------------------------------------------------------------------
*RepeatStr - Repeats a string count times.
 Example: RepeatStr('hello',3) > 'hellohellohello'
-------------------------------------------------------------------------------}
function TStringManager.RepeatStr(const s : String; Count : Integer) : String;
var  p : PChar;
  Slen : Integer;
begin
  SLen := Length(s);
  SetLength(Result, Count*SLen);
  p := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(s)^, p^, SLen);
    Inc(p, SLen);
    Dec(Count);
  end;
end;

{-------------------------------------------------------------------------------
*FirstChar - safe method to get the first character of a string.
 Example: FirstChar('hello world') > 'h'
 Example: FirstChar('') > #0
See also LastChar
-------------------------------------------------------------------------------}
function TStringManager.FirstChar(const s: String) : Char;
begin
  Result := #0;
  if s<>'' then Result := s[1];
end;

{-------------------------------------------------------------------------------
*LastChar - retrurns the last character in a String.
 Example: LastChar('hello world') > 'd'
 Example: LastChar('') > #0
See also FirstChar
-------------------------------------------------------------------------------}
function TStringManager.LastChar(const s: String) : Char;
begin
  Result := #0;
  if s<>'' then Result := s[Length(s)];
end;

{-------------------------------------------------------------------------------
*FirstWord - returns the first word from a string. (Starting delimiters are ignored)
 Example: FirstWord('Delphi programming is fun') > 'Delphi'
 Example: FirstWord('  fun  ') > 'fun'
 Example: FirstWord('Delphi, programming is fun',[' ','.',',']) >'Delphi'
 Example: FirstWord('Delphiprogramming') > 'Delphiprogramming'
See also LastWord, NthWord, Before
-------------------------------------------------------------------------------}
function TStringManager.FirstWord(const s: String; const Delims : TCharSet = gcWordDelims) : String;
begin
  Result := NthWord(1, s, Delims);
end;

{-------------------------------------------------------------------------------
*LastWord - returns the last word in a string. (Trailing delimiters are ignored)
 Example: LastWord('Delphi programming is fun') > 'fun'
 Example: LastWord('fun') > 'fun'
 Example: LastWord(' fun ') > 'fun'
See also FirstWord, NthWord, AfterRev
-------------------------------------------------------------------------------}
function TStringManager.LastWord(const s: String; const Delims : TCharSet = gcWordDelims) : String;
var Slen : Integer;
   Start : Integer;
begin
  Result := '';
  Slen := Length(s);
  while (Slen>0) and (s[Slen] in Delims) do Dec(SLen);
  if Slen=0 then Exit;
  if Slen=1 then
  begin
    Result := s[1];
  end else
  begin
    Start := Slen;
    while (Start>1) and (not (s[Start-1] in Delims)) do Dec(Start);
    Result := System.Copy(s, Start, (Slen+1)-Start);
  end;
end;

{-------------------------------------------------------------------------------
*NthWord - returns the nth word in a string - 1 based. (Starting delimiters are ignored)
 Example: NthWord(2,'Delphi programming is fun') > 'programming'
 Example: NthWord(2,'   Delphi programming is fun') > 'programming'
See also FirstWord, CountWords, ExtractWords
-------------------------------------------------------------------------------}
function TStringManager.NthWord(Nth : Integer; const s : String; const Delims : TCharSet = gcWordDelims) : string;
var  i : Integer;
     j : Integer;
 Count : Integer;
  SLen : Integer;
begin
  Result := '';
  SLen := Length(s);
  Count := 0;
  i := 1;
  while i<=SLen do
  begin
    while (i<=SLen) and (s[i] in Delims) do Inc(i);
    Inc(Count);
    if Count = Nth then
    begin
      j := i;
      while (i<=SLen) and (not (s[i] in Delims)) do Inc(i);
      Result := System.Copy(s, j, i-j);
      Exit;
    end else
    begin
      while (i<=SLen) and (not (s[i] in Delims)) do Inc(i);
    end;
  end;
end;

{-------------------------------------------------------------------------------
*ExtractWords - Extracts all words between delimiters and appends it to a string list.
 If aList is nil an internally created TStringList will be returned, else
 the list will be appended and returned. Thus the function can either append an existing
 list or return an created list.

 Example: ExtractWords('get the words.') > a list with 3 lines, 'get','the','words'
See also NthWord, Parse
-------------------------------------------------------------------------------}
function TStringManager.ExtractWords(const s : String; const aList : TStrings = nil; const Delims : TCharSet = gcWordDelims) : TStrings;
var  i : Integer;
     j : Integer;
  SLen : Integer;
begin
  if aList=nil
    then Result := TStringList.Create
    else Result := aList;
  SLen := Length(s);
  i := 1;
  while i<=SLen do
  begin
    while (i<=SLen) and (s[i] in Delims) do Inc(i);
    j := i;
    while (i<=SLen) and (not (s[i] in Delims)) do Inc(i);
    if i-j>0 then Result.Add(System.Copy(s, j, i-j));
  end;
end;

{-------------------------------------------------------------------------------
*First - Simply returns the first count characters of a string.
 Example: First('hello',2) > 'he'
See also Copy, Last, LeftAt, RightAt
-------------------------------------------------------------------------------}
function TStringManager.First(const s : String; Count : Integer = 1) : String;
begin
  Result := System.Copy(s, 1, Count);
end;

{-------------------------------------------------------------------------------
*LeftAt
 Simple function that returns all chars to the left from a specific (including) Position
 Example: LeftAt('hello',2) > 'he'
See also RightAt, Copy, First, Last
-------------------------------------------------------------------------------}
function TStringManager.LeftAt(const s : String; Position : Integer) : String;
begin
  Result := System.Copy(s, 1, Position);
end;

{-------------------------------------------------------------------------------
*RightAt
 Simple function that returns all chars to the right from a specific (including) Position
 Example: RightAt('hello',2) > 'ello'
See also LeftAt, Copy, First, Last
-------------------------------------------------------------------------------}
function TStringManager.RightAt(const s : String; Position : Integer) : String;
begin
  Result := System.Copy(s, Position, Length(s));
end;

{------------------------------------------------------------------------------
*Last - Simply returns the last count characters of a String
 Example: Last('hello',2) > 'lo'
 Example: Last('hello',10) > 'hello'
See also LastFrom, First, Copy, LeftAt, RightAt
-------------------------------------------------------------------------------}
function TStringManager.Last(const s : String; Count : Integer = 1) : String;
begin
  Result := System.Copy(s, (Length(s)-Count)+1, Length(s));
end;

{------------------------------------------------------------------------------
*CopyPos - Returns text from the StartPos until the EndPos.
 Example: CopyPos('hello',2,3) > 'el'
 Example: CopyPos('hello',2,5) > 'ello'
See also Last, First, Copy, LeftAt, RightAt
-------------------------------------------------------------------------------}
function TStringManager.CopyPos(const s : String; StartPos, EndPos : Integer) : String;
begin
  Result := System.Copy(s, StartPos, (EndPos-StartPos)+1);
end;

{-------------------------------------------------------------------------------
*Strip - Strips leading, trailing and double characters from a string.
 Not the same as the standard Trim function. Where trim strips only leading and
 trailing spaces, this function also strips extra spaces between words.
 Note that control characters are not affected.
 Please see Mop for a "stronger" strip that also treats control chars as spaces.
 Example: Strip('   hello    world  ') > 'hello world'
See also Trim, StripUp, Mop
-------------------------------------------------------------------------------}
function TStringManager.Strip(const s : String; c : Char = ' '; DoUpcase : Boolean = False) : String;
var
      i : Integer;
 aStart : Integer;
  aStop : Integer;
   RLen : Integer;
begin
  aStop := Length(s);
  //we first calculate the length without the outer spaces
  while (aStop>0) and (s[aStop] = c) do System.Dec(aStop);
  if aStop=0 then //only has spaces
  begin
    Result := '';
    Exit;
  end;
  aStart := 1;
  while (aStart<=aStop) and (s[aStart] = c) do Inc(aStart);

  //Set length of result minus the outer spaces for fast Result[i] : =
  SetLength(Result, (aStop-aStart)+1);
  RLen := 0;
  //Set relevant characters to result
  i := aStart;

  while i<=aStop do
  begin
    if (s[i] = c) then
    begin
      while (i+1<=aStop) and (s[i+1] = c) do Inc(i);
    end;
    System.Inc(RLen);
    Result[RLen] := s[i];
    Inc(i);
  end;
  SetLength(Result, RLen);  //set final length
  if DoUpCase then for i := 1 to RLen do Result[i] := gaANSIUpperArray[Result[i]];
end;

{-------------------------------------------------------------------------------
*StripUp - UpCase version of Strip.
 Example: StripUp('   Hello    World  ') > 'HELLO WORLD'
See also Strip, Trim, TrimUp
-------------------------------------------------------------------------------}
function TStringManager.StripUp(const s : String; c : Char = ' ') : String;
begin
  Result := Strip(s, c, True);
end;

{-------------------------------------------------------------------------------
*PStrip - procedural version of the Strip function.
See also Strip, Mop, Purge
-------------------------------------------------------------------------------}
procedure TStringManager.PStrip(var s: String; c : Char = ' '; DoUpcase : Boolean = False);
var   i : Integer;
  aStop : Integer;
begin
  aStop := Length(s);
  while (aStop>0) and (s[aStop] = c) do System.Dec(aStop);
  SetLength(s, aStop);
  if aStop=0 then Exit;
  i := 1;
  while (i<=aStop) and (s[i] = c) do Inc(i);
  if i>1 then
  begin
    System.Delete(s, 1, i-1);
    aStop := Length(s);
    i := 1;
  end;
  while i < aStop do
  begin
    if (s[i] = ' ') and (s[i+1] = c) then
    begin
      System.Delete(s, i, 1);
      Dec(aStop);
    end else Inc(i);
  end;
  if DoUpCase then for i := 1 to Length(s) do s[i] := gaANSIUpperArray[s[i]];
end;

{-------------------------------------------------------------------------------
*PStripUp - procedural version of the StripUp function.
See also Strip, Mop, Purge
-------------------------------------------------------------------------------}
procedure TStringManager.PStripUp(var s: String; c : Char = ' ');
begin
  PStrip(s, c, True);
end;

{-------------------------------------------------------------------------------
*Mop - Mop treats control characters as spaces and then removes leading/trailing
       and double spaces.
       It is in a sense a combination of the Tim and Strip functions.

 Example:
   Mop(#9'  Remove'#9#9'Tabs   ') > 'Remove Tabs'
   Mop(#8+'   please   strip'#9+'the  lines'+#13+#10+'   clean   ')
     >  'please strip the lines clean'
Note that any characters <=ord(32) are treated as spaces - doubles are then removed.
See also MopUp, Strip, Purge, PMop
-------------------------------------------------------------------------------}
{$IFNDEF StrManEnableASM}
function TStringManager.Mop(const s : String; DoUpCase : Boolean = False) : String;
var
      i : Integer;
 aStart : Integer;
  aStop : Integer;
   RLen : Integer;
begin
  aStop := Length(s);
  //we first calculate the length without the outer spaces
  while (aStop>0) and (s[aStop] <= ' ') do System.Dec(aStop);
  if aStop=0 then //only has spaces
  begin
    Result := '';
    Exit;
  end;
  aStart := 1;
  while (aStart<=aStop) and (s[aStart] <= ' ') do Inc(aStart);

  //Set length of result minus the outer spaces for fast Result[i] :=
  SetLength(Result, (aStop-aStart)+1);
  RLen := 0;
  //Set relevant characters to result
  i := aStart;
  while i<=aStop do
  begin
    if (s[i] <= ' ') then
    begin
      while (i+1<=aStop) and (s[i+1] <= ' ') do Inc(i);
      System.Inc(RLen);
      Result[RLen] := ' ';
    end else
    begin
      System.Inc(RLen);
      Result[RLen] := s[i];
    end;
    Inc(i);
  end;
  SetLength(Result, RLen);  //set final length
  if DoUpCase then for i := 1 to RLen do Result[i] := gaANSIUpperArray[Result[i]];
end;
{$ENDIF}

{-------------------------------------------------------------------------------
*MopUp - Mop treats control ch
aracters as spaces and then removes leading/trailing
         and double spaces and uppercase the string.
 Example:
   MopUp(#9'  Remove'#9#9'Tabs   ') > 'REMOVE TABS'
   MopUp(#8+'   please   strip'#9+'the  lines'+#13+#10+'   clean   ')
     >  'PLEASE STRIP THE LINES CLEAN'
Note that any characters <=ord(32) are treated as spaces - doubles are then removed.
See also Mop, Strip, Purge, PMop
-------------------------------------------------------------------------------}
{$IFNDEF StrManEnableASM}
function TStringManager.MopUp(const s : String) : String;
begin
  Result := Mop(s, True);
end;
{$ENDIF}

{-------------------------------------------------------------------------------
*PMop - procedural version of the Mop function
See also Mop, PMopUp, Strip, Purge
-------------------------------------------------------------------------------}
procedure TStringManager.PMop(var s: String; DoUpcase : Boolean = False);
var   i : Integer;
  aStop : Integer;
begin
  aStop := Length(s);
  while (aStop>0) and (s[aStop] <= ' ') do System.Dec(aStop);
  SetLength(s, aStop);
  if aStop=0 then Exit;
  i := 1;
  while (i<=aStop) and (s[i] <= ' ') do Inc(i);
  if i>1 then
  begin
    System.Delete(s,1, i-1);
    aStop := Length(s);
    i := 1;
  end;
  while i < aStop do
  begin
    if s[i] in [#1..#32] then s[i] := ' ';
    if (s[i] = ' ') and (s[i+1] = ' ') then
    begin
      System.Delete(s, i, 1);
      Dec(aStop);
    end else Inc(i);
  end;
  if DoUpCase
    then for i := 1 to Length(s) do s[i] := gaANSIUpperArray[s[i]];
end;

{-------------------------------------------------------------------------------
*PMopUp - Upcase version of the PMop procedure.
See also PMop, Mop, Strip, Purge
-------------------------------------------------------------------------------}
procedure TStringManager.PMopUp(var s: String);
begin
  PMop(s, True);
end;

{-------------------------------------------------------------------------------
*TrimLen - The length of a string it it would be trimmed with the Trim function.
 Example:
   TrimLen('  o  k   ') > 4
See also Trim
-------------------------------------------------------------------------------}
function TStringManager.TrimLen(const s : String) : Integer;
var i, l: Integer;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] <= ' ') do Inc(i);
  if i > l then Result := 0 else
  begin
    while s[l] <= ' ' do System.Dec(l);
    Result := l - i + 1;
  end;
end;

{-------------------------------------------------------------------------------
*Trim - Strips leading/trailing spaces and control characters.
  Same as standard Trim function but with optional DoUpcase.
Example: Trim('  hello   world  ') > 'hello   world'
See also TrimRight, Strip, Mop
-------------------------------------------------------------------------------}
function TStringManager.Trim(const s : String; DoUpcase : Boolean = False) : String;
var i, l: Integer;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] <= ' ') do Inc(i);
  if i > l then Result := '' else
  begin
    while s[l] <= ' ' do System.Dec(l);
    Result := System.Copy(s, i, l - i + 1);
  end;
  if DoUpCase
    then for i := 1 to Length(Result) do Result[i] := gaANSIUpperArray[Result[i]];
end;

{-------------------------------------------------------------------------------
*TrimLeft - Strips leading spaces and control characters from a string.
  Same as standard TrimLeft function but with optional DoUpcase.
Example: TrimLeft('  hello   world  ') > 'hello   world  '
See also Trim, Strip, Mop
-------------------------------------------------------------------------------}
function TStringManager.TrimLeft(const s : String; DoUpCase : Boolean = False) : String;
var i, l : Integer;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] <= ' ') do Inc(i);
  Result := System.Copy(s, i, Maxint);
  if DoUpcase
    then for i := 1 to Length(Result) do Result[i] := gaANSIUpperArray[Result[i]];
end;

{-------------------------------------------------------------------------------
*TrimRight - Strips trailing spaces and control characters from a string.
  Same as standard TrimRight function but with optional DoUpcase.
Example: TrimRight('  hello   world  ') > '  hello   world'
See also Trim, Strip, Mop
-------------------------------------------------------------------------------}
function TStringManager.TrimRight(const s : String; DoUpCase : Boolean = False) : String;
var i : Integer;
begin
  i := Length(s);
  while (i > 0) and (S[i] <= ' ') do Dec(i);
  Result := System.Copy(s, 1, i);
  if DoUpcase
    then for i := 1 to Length(Result) do Result[i] := gaANSIUpperArray[Result[i]];
end;

{-------------------------------------------------------------------------------
*TrimUp - Strips leading/trailing spaces and control characters and returns
 all characters uppercase.
Example: Trim('   Hello    World  ') > 'HELLO    WORLD'
See also Trim, Strip
-------------------------------------------------------------------------------}
function TStringManager.TrimUp(const s : String) : String;
begin
  Result := Trim(s, True);
end;

{-------------------------------------------------------------------------------
*Trim - Strips leading/trailing spaces and leading/trailing control characters
 from every line in the StringList.
See also Strip, Mop
-------------------------------------------------------------------------------}
function TStringManager.Trim(const aList : TStrings; const RemoveEmptyLines : Boolean = False) : Integer;
var i: Integer;
begin
  if aList=nil then
  begin
    Result := 0;
    Exit;
  end;
  for i := aList.Count - 1 downto 0 do
  begin
    aList[i] := Trim(aList[i]);
    if RemoveEmptyLines then if aList[i] = '' then aList.Delete(i);
  end;
  Result := aList.Count;
end;

{-------------------------------------------------------------------------------
*PTrim - Procedural version of the Trim function.
Example: if s='  hello   world' then PTrim(s) > 'hello   world'
See also Trim, PTrimUp, Strip, Mop
-------------------------------------------------------------------------------}
procedure TStringManager.PTrim(var s : String; DoUpcase : Boolean = False);
var i, l: Integer;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] <= ' ') do Inc(i);
  if i > l then s := '' else
  begin
    while s[l] <= ' ' do System.Dec(l);
    if i=1
     then SetLength(s, l)
     else s := System.Copy(s, i, l - i + 1);
  end;
  if DoUpcase
    then for i := 1 to Length(s) do s[i] := gaANSIUpperArray[s[i]];
end;

{-------------------------------------------------------------------------------
*PTrimUp - Upcase version of the PTrim procedure.
Example: if s='  hello   world' then PTrimUp(s) > 'HELLO WORLD'
See also Trim, PTrim, Strip, Mop
-------------------------------------------------------------------------------}
procedure TStringManager.PTrimUp(var s : String);
begin
  PTrim(s, True);
end;

{-------------------------------------------------------------------------------
*Mop - Strips leading/trailing spaces and leading/trailing control characters
 and extra spaces between words from every line in the StringList (not #13#10!).
See also Mop, Strip, Trim
-------------------------------------------------------------------------------}
function TStringManager.Mop(const aList : TStrings; const RemoveEmptyLines : Boolean = False) : Integer;
var i: Integer;
begin
  if aList=nil then
  begin
    Result := 0;
    Exit;
  end;
  for i := aList.Count - 1 downto 0 do
  begin
    aList[i] := Mop(aList[i]);
    if RemoveEmptyLines then if aList[i] = '' then aList.Delete(i);
  end;
  Result := aList.Count;
end;

{-------------------------------------------------------------------------------
*DelEmptyLines - Deletes all lines in the list without any chacaters above ord(' ')
See also DelExtraLines, IsEmpty
-------------------------------------------------------------------------------}
function TStringManager.DelEmptyLines(const aList : TStrings) : Integer;
var  i : Integer;
begin
  if aList=nil then
  begin
    Result := 0;
    Exit;
  end;
  for i := aList.Count - 1 downto 0 do
  begin
    if IsEmpty(aList[i]) then aList.Delete(i);
  end;
  Result := aList.Count;
end;

{-------------------------------------------------------------------------------
*DelExtraLines - Deletes all extra/double empty lines in the StringList.
Empty lines are lines without any characters above ord(' ')
See also DelEmptyLines, IsEmpty
-------------------------------------------------------------------------------}
function TStringManager.DelExtraLines(const aList : TStrings) : Integer;
var  i : Integer;
  Prev : Boolean;
begin
  Prev := True;
  if aList=nil then
  begin
    Result := 0;
    Exit;
  end;
  for i := aList.Count - 1 downto 0 do
  begin
    if IsEmpty(aList[i]) then
    begin
      if Prev then aList.Delete(i);
      Prev := True;
    end else Prev := False;
  end;
  if aList.Count>0 then if IsEmpty(aList[0]) then aList.Delete(0);
  Result := aList.Count;
end;

{-------------------------------------------------------------------------------
*IsEmpty - Returns true if a string has no characters above ord(' ').
 If basically means that it returns true if the string is empty or only contains
 control characters or spaces.
 Much faster than "if Trim(s)='' then..."
 Example:
   IsEmpty('   '+#9) > True
   IsEmpty(' a  ') > False
See also Trim, Mop, NotEmpty
-------------------------------------------------------------------------------}
function TStringManager.IsEmpty(const s : String) : Boolean;
var i : Integer;
begin
  Result := False;
  for i := 1 to Length(s) do if s[i]>' ' then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*NotEmpty - The inverse of IsEmpty.
 Much faster than "if Trim(s)<>'' then..."
 Example:
   NotEmpty('   '+#9) > False
   NotEmpty(' a  ') > True
See also IsEmpty, Trim, Mop
-------------------------------------------------------------------------------}
function TStringManager.NotEmpty(const s : String) : Boolean;
begin
  Result := not IsEmpty(s);
end;

{-------------------------------------------------------------------------------
*At - Returns the Nth character in a string - safer than s[x].
 Example:
  if s='hello'
  at(s,0)   > #0
  at(s,100) > #0
  at(s,2)   > 'e'
See also SetAt
-------------------------------------------------------------------------------}
function TStringManager.At(const s : String; Index : Integer) : Char;
begin
  if (Index>0) and (Index<=Length(s))
  then Result := s[Index]
  else Result := #0;
end;

{-------------------------------------------------------------------------------
*SetAt - Just a safe way to set a char in a string.
 Instead of s[4] := 'a' use s := SetAt(s,'a',4)
See also At
-------------------------------------------------------------------------------}
function TStringManager.SetAt(const s : String; const c : Char; Index : Integer) : String;
begin
  Result := s;
  if (Index=0) or (Index>Length(s)) then Exit;
  Result[Index] := c;
end;

{-------------------------------------------------------------------------------
*Equal - True if two characters are identical with option to ignore case.
  Basically to avoid code like:
   if IgnoreCase=False
     then Result := c1=c2
     else Result := Upcase(c1)=Upcase(c2)
-------------------------------------------------------------------------------}
function TStringManager.Equal(c1, c2 : Char; IgnoreCase : Boolean) : Boolean;
begin
  if IgnoreCase
    then Result := gaANSIUpperArray[c1] = gaANSIUpperArray[c2]
    else Result := c1=c2;
end;

{-------------------------------------------------------------------------------
*Equal - True if two strings are identical with the option to ignore case.
You can use
  "Result := Equal(s1, s2, IgnoreCase)"
instead of bulky code like:
  "if IgnoreCase=False
    then Result := s1=s2
    else Result := Upcase(s1)=Upcase(s2)"
See also IsSame
-------------------------------------------------------------------------------}
function TStringManager.Equal(const s1, s2 : String; IgnoreCase : Boolean) : Boolean;
var   i : Integer;
  S1Len : Integer;
begin
  if not IgnoreCase then
  begin
    Result := s1=s2;
    Exit;
  end;
  Result := False;
  S1Len := Length(s1);
  if S1Len<>Length(s2) then Exit;
  for i := 1 to S1Len do
    if gaANSIUpperArray[s1[i]] <> gaANSIUpperArray[s2[i]] then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*EqualIC - True if two characters are identical irrespective of case.
  "if EqualIC(c1, c2) then" is faster than "if Upcase(c1)=Upcase(c2) then"
See also overloaded string version, IsSameIC
-------------------------------------------------------------------------------}
function TStringManager.EqualIC(c1, c2 : Char) : Boolean;
begin
  Result := gaANSIUpperArray[c1] = gaANSIUpperArray[c2];
end;

{-------------------------------------------------------------------------------
*EqualIC - True if two strings are identical irrespective of case.
  "if EqualIC(s1, s2) then" is faster than "if Upcase(s1)=Upcase(s2) then"
See also overloaded char version, IsSameIC
-------------------------------------------------------------------------------}
function TStringManager.EqualIC(const s1, s2 : String) : Boolean;
var   i : Integer;
  S1Len : Integer;
begin
  Result := False;
  S1Len := Length(s1);
  if S1Len<>Length(s2) then Exit;
  for i := 1 to S1Len do
    if gaANSIUpperArray[s1[i]] <> gaANSIUpperArray[s2[i]] then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsSame - Returns true if the two strings are identical when control characters,
 leading/trailing and duplicate spaces are ignored.
Note this function is functionally the same as:
  "if Mop(s1)=Mop(s2)..." but using "if IsSame(s1, s2)..." is much faster.

 Example: IsSame('  hello world','  hello    world  ') > True
 Example: IsSame('hello',#13'  hello ') > True
 Example: IsSame('hello','  Hello ') > False
See also TrimSame, Equal, IsSameIC, IsSameFirst and overloaded Set version
-------------------------------------------------------------------------------}
function TStringManager.IsSame(const s1, s2 : String; IgnoreCase : Boolean = False) : Boolean;
var Len1  : Integer;
    Len2  : Integer;
    i1      : Integer;
    i2      : Integer;
begin
  Result := False;

  Len1  := Length(s1);
  i1 := 1;
  while (Len1>0) and (s1[Len1] <= ' ') do Dec(Len1);
  while (i1<=Len1) and (s1[i1] <= ' ') do Inc(i1);

  Len2  := Length(s2);
  i2 := 1;
  while (Len2>0) and (s2[Len2] <= ' ') do Dec(Len2);
  while (i2<=Len2) and (s2[i2] <= ' ') do Inc(i2);


  while i1 <= Len1 do
  begin
    if IgnoreCase then
    begin
      while (i1<=Len1) and (s1[i1]>' ') do
      begin
        if i2>Len2 then Exit;
        if gaANSIUpperArray[s1[i1]] <> gaANSIUpperArray[s2[i2]] then Exit;
        Inc(i1);
        Inc(i2);
      end;
    end else
    begin
      while (i1<=Len1) and (s1[i1]>' ') do
      begin
        if i2>Len2 then Exit;
        if s1[i1] <> s2[i2] then Exit;
        Inc(i1);
        Inc(i2);
      end;
    end;

    if i1>Len1 then Break;

    if (s1[i1] <= ' ') then
    begin
      while (i1+1<=Len1) and (s1[i1+1] <= ' ') do Inc(i1);
    end;
    if (s2[i2] <= ' ') then
    begin
      while (i2+1<=Len2) and (s2[i2+1] <= ' ') do Inc(i2);
    end;
    Inc(i1);
    Inc(i2);
  end;
  Result := (i1=Len1+1) and (i2=Len2+1);
end;

{-------------------------------------------------------------------------------
*IsSameIC - Uppercase version of the IsSame function.
Note this function is functionally the same as:
  "if MopUp(s1)=MopUp(s2)..." but using "if IsSameIC(s1, s2)..." is much faster.

 Example: IsSameIC('  Hello world',' hello   world  ') > True
See also IsSame, IsSameFirst, TrimSame and overloaded Set version
-------------------------------------------------------------------------------}
function TStringManager.IsSameIC(const s1, s2 : String) : Boolean;
var Len1  : Integer;
    Len2  : Integer;
    i1      : Integer;
    i2      : Integer;
begin
  Result := False;

  Len1  := Length(s1);
  i1 := 1;
  while (Len1>0) and (s1[Len1] <= ' ') do Dec(Len1);
  while (i1<=Len1) and (s1[i1] <= ' ') do Inc(i1);

  Len2  := Length(s2);
  i2 := 1;
  while (Len2>0) and (s2[Len2] <= ' ') do Dec(Len2);
  while (i2<=Len2) and (s2[i2] <= ' ') do Inc(i2);


  while i1 <= Len1 do
  begin
    while (i1<=Len1) and (s1[i1]>' ') do
    begin
      if i2>Len2 then Exit;
      if gaANSIUpperArray[s1[i1]] <> gaANSIUpperArray[s2[i2]] then Exit;
      Inc(i1);
      Inc(i2);
    end;

    if i1>Len1 then Break;

    if (s1[i1] <= ' ') then
    begin
      while (i1+1<=Len1) and (s1[i1+1] <= ' ') do Inc(i1);
    end;
    if (s2[i2] <= ' ') then
    begin
      while (i2+1<=Len2) and (s2[i2+1] <= ' ') do Inc(i2);
    end;
    Inc(i1);
    Inc(i2);
  end;
  Result := (i1=Len1+1) and (i2=Len2+1);
end;

{-------------------------------------------------------------------------------
*IsSameFirst
 Returns true if the SubStr is the same as the start of the second string, while
 ignoring control characters, leading/trailing and double spaces.
 Example: IsSameFirst('hello  w','  hello   world  ') > True
 Example: IsSameFirst('hello',#13'hello world') > True
 Example: IsSameFirst('hello world','hello') > False
 Example: IsSameFirst('','hello world') > True
See also IsSame, TrimSameFirst  and overloaded set version
-------------------------------------------------------------------------------}
function TStringManager.IsSameFirst(const SubStr, s : String; IgnoreCase : Boolean = False) : Boolean;
var Len1  : Integer;
    Len2  : Integer;
    i1      : Integer;
    i2      : Integer;
begin
  Result := False;

  Len1  := Length(SubStr);
  i1 := 1;
  while (Len1>0) and (SubStr[Len1] <= ' ') do Dec(Len1);
  while (i1<=Len1) and (SubStr[i1] <= ' ') do Inc(i1);

  Len2  := Length(s);
  i2 := 1;
  while (Len2>0) and (s[Len2] <= ' ') do Dec(Len2);
  while (i2<=Len2) and (s[i2] <= ' ') do Inc(i2);

  while i1 <= Len1 do
  begin
    if IgnoreCase then
    begin
      while (i1<=Len1) and (SubStr[i1]>' ') do
      begin
        if i2>Len2 then Exit;
        if gaANSIUpperArray[SubStr[i1]] <> gaANSIUpperArray[s[i2]] then Exit;
        Inc(i1);
        Inc(i2);
      end;
    end else
    begin
      while (i1<=Len1) and (SubStr[i1]>' ') do
      begin
        if i2>Len2 then Exit;
        if SubStr[i1] <> s[i2] then Exit;
        Inc(i1);
        Inc(i2);
      end;
    end;

    if i1>Len1 then Break;

    if (SubStr[i1] <= ' ') then
    begin
      while (i1+1<=Len1) and (SubStr[i1+1] <= ' ') do Inc(i1);
    end;
    if (s[i2] <= ' ') then
    begin
      while (i2+1<=Len2) and (s[i2+1] <= ' ') do Inc(i2);
    end;
    Inc(i1);
    Inc(i2);
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsSameFirstIC - uppercase version of the IsSameFirst function.

 Example: IsSameFirstIC('hello w','   Hello   World  ') > True
See also IsSame, TrimSameFirst
-------------------------------------------------------------------------------}
function TStringManager.IsSameFirstIC(const SubStr, s : String) : Boolean;
begin
  Result := IsSameFirst(SubStr, s, True);
end;

{-------------------------------------------------------------------------------
*IsSame
 Returns true if the two strings are identical when all characters are stripped
 as specified by PurgeSet.
 Example: IsSame('hello','12  hello ',[' ','0'..'9']) > True
 Example: IsSame('Hello','12  hello ',[' ','0'..'9']) > False
 See also IsSameIC, IsSameFirst and overloaded Char version
-------------------------------------------------------------------------------}
function TStringManager.IsSame(const s1, s2 : String; const PurgeSet : TCharSet; IgnoreCase : Boolean = False) : Boolean;
var
  Len1   : Integer;
  Len2   : Integer;
  i1     : Integer;
  i2     : Integer;
begin
  Result := False;
  Len1  := Length(s1);
  Len2  := Length(s2);
  i1 := 1;
  i2 := 1;
  while i1 <= Len1 do
  begin
    if i2 > Len2 then Exit;
    while (i1<Len1) and (s1[i1] in PurgeSet) do Inc(i1);
    while (i2<Len2) and (s2[i2] in PurgeSet) do Inc(i2);
    if IgnoreCase then
    begin
      if gaANSIUpperArray[s1[i1]] <> gaANSIUpperArray[s2[i2]] then Exit;
    end else
    begin
      if s1[i1] <> s2[i2] then Exit;
    end;
    Inc(i1);
    Inc(i2);
    while (i1<=Len1) and (s1[i1] in PurgeSet) do Inc(i1);
    while (i2<=Len2) and (s2[i2] in PurgeSet) do Inc(i2);
  end;
  Result := (i1=Len1+1) and (i2=Len2+1);
end;

{-------------------------------------------------------------------------------
*IsSameIC
 Returns true if the two strings are identical when all characters are stripped
 as specified by PurgeSet and case is ignored.
 Example: IsSameIC('Hello','12  hello ',[' ','0'..'9']) > True
See also IsSame, IsSameFirst and overloaded Char version
-------------------------------------------------------------------------------}
function TStringManager.IsSameIC(const s1, s2 : String; const PurgeSet : TCharSet) : Boolean;
begin
  Result := IsSame(s1, s2, PurgeSet, True);
end;

{-------------------------------------------------------------------------------
*IsSameFirst (TCharSet version)
 Returns true if the SubStr is the same as the start of the second String,
 when all characters are stripped as specified by PurgeSet.
 Example: IsSameFirst('hello','12  hello world',[' ','0'..'9']) > True
 Example: IsSameFirst('Hello','12  hello world',[' ','0'..'9']) > False
See also IsSame, IsSameIC, IsSameFirstIC and overloaded Char version
-------------------------------------------------------------------------------}
function TStringManager.IsSameFirst(const SubStr, s : String; const PurgeSet : TCharSet; IgnoreCase : Boolean = False) : Boolean;
var
  Len1   : Integer;
  Len2   : Integer;
  i1     : Integer;
  i2     : Integer;
begin
  Result := False;
  Len1  := Length(SubStr);
  Len2  := Length(s);
  i1 := 1;
  i2 := 1;
  while i1 <= Len1 do
  begin
    if i2 > Len2 then Exit;
    while (i1<Len1) and (SubStr[i1] in PurgeSet) do Inc(i1);
    while (i2<Len2) and (s[i2] in PurgeSet) do Inc(i2);
    if IgnoreCase then
    begin
      if gaANSIUpperArray[SubStr[i1]] <> gaANSIUpperArray[s[i2]] then Exit;
    end else
    begin
      if SubStr[i1] <> s[i2] then Exit;
    end;
    Inc(i1);
    Inc(i2);
    while (i1<=Len1) and (SubStr[i1] in PurgeSet) do Inc(i1);
    while (i2<=Len2) and (s[i2] in PurgeSet) do Inc(i2);
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsSameFirstIC - Uppercase version of the Set IsSameFirst function.
 Example: IsSameFirstIC('Hello','12  hello world',[' ','0'..'9']) > True
See also IsSame, IsSameIC, IsSameFirst and overloade Char version
-------------------------------------------------------------------------------}
function TStringManager.IsSameFirstIC(const SubStr, s : String; const PurgeSet : TCharSet) : Boolean;
begin
  Result := IsSameFirst(SubStr, s, PurgeSet, True);
end;

{-------------------------------------------------------------------------------
*TrimSame - Returns true if the two strings are identical when leading/trailing
 control characters and spaces are ignored.
Note this function is functionally the same as:
  "if Trim(s1)=Trim(s2)..." but using "if TrimSame(s1, s2)..." is much faster.
 Example: TrimSame('hello world','  hello world   ') > True
 Example: TrimSame('hello world','  hello   world') > False
Note that function IsSame also ignores extra spaces between words.
See also TrimSameFirst, IsSame, Equal
-------------------------------------------------------------------------------}
function TStringManager.TrimSame(const s1, s2 : String; IgnoreCase : Boolean = False) : Boolean;
var Len1  : Integer;
    Len2  : Integer;
    i1      : Integer;
    i2      : Integer;
begin
  Result := False;

  Len1  := Length(s1);
  i1 := 1;
  while (Len1>0) and (s1[Len1] <= ' ') do Dec(Len1);
  while (i1<=Len1) and (s1[i1] <= ' ') do Inc(i1);

  Len2  := Length(s2);
  i2 := 1;
  while (Len2>0) and (s2[Len2] <= ' ') do Dec(Len2);
  while (i2<=Len2) and (s2[i2] <= ' ') do Inc(i2);

  if (Len1-i1)<>(Len2-i2) then Exit;

  if IgnoreCase then
  begin
    while i1 <= Len1 do
    begin
      if gaANSIUpperArray[s1[i1]] <> gaANSIUpperArray[s2[i2]] then Exit;
      Inc(i1);
      Inc(i2);
    end;
  end else
  begin
    while i1 <= Len1 do
    begin
      if s1[i1] <> s2[i2] then Exit;
      Inc(i1);
      Inc(i2);
    end;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*TrimSameIC - UpperCase version of the TrimSame function.
 Example: TrimSameIC('hello world','  Hello World   ') > True
See also TrimSame, IsSame
-------------------------------------------------------------------------------}
function TStringManager.TrimSameIC(const s1, s2 : String) : Boolean;
begin
  Result := TrimSame(s1, s2, True);
end;

{-------------------------------------------------------------------------------
*TrimSameFirst
 Returns true if the SubStr is the same as the start of the second string, while
 ignoring leading/trailing control characters and spaces.
 Example: TrimSameFirst('  hello w  ','  hello world') > True
 Example: TrimSameFirst(' hello  w ',' hello world') > False
Note that function IsSameFirst also ignores extra spaces between words.
See also TrimSame, IsSameFirst
-------------------------------------------------------------------------------}
function TStringManager.TrimSameFirst(const SubStr, s : String; IgnoreCase : Boolean = False) : Boolean;
var Len1  : Integer;
    Len2  : Integer;
    i1      : Integer;
    i2      : Integer;
begin
  Result := False;

  Len1  := Length(SubStr);
  i1 := 1;
  while (Len1>0) and (SubStr[Len1] <= ' ') do Dec(Len1);
  while (i1<=Len1) and (SubStr[i1] <= ' ') do Inc(i1);

  Len2  := Length(s);
  i2 := 1;
  while (Len2>0) and (s[Len2] <= ' ') do Dec(Len2);
  while (i2<=Len2) and (s[i2] <= ' ') do Inc(i2);

  if (Len1-i1)>(Len2-i2) then Exit;
  if IgnoreCase then
  begin
    while i1 <= Len1 do
    begin
      if gaANSIUpperArray[SubStr[i1]] <> gaANSIUpperArray[s[i2]] then Exit;
      Inc(i1);
      Inc(i2);
    end;
  end else
  begin
    while i1 <= Len1 do
    begin
      if SubStr[i1] <> s[i2] then Exit;
      Inc(i1);
      Inc(i2);
    end;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*TrimSameFirstIC - Uppercase version of the TrimSame function.
 Example: TrimSameFirst('  hello w  ','  Hello World') > True

See also TrimSame, IsSameFirst
-------------------------------------------------------------------------------}
function TStringManager.TrimSameFirstIC(const SubStr, s : String) : Boolean;
begin
  Result := TrimSameFirst(SubStr, s, True);
end;

{-------------------------------------------------------------------------------
*IsType - Returns true if the character has CharType properties as indicated
by Flags.
Example:
  IsType('2',[cfUpper, cfLower, cfDigit]) > True
-------------------------------------------------------------------------------}
function TStringManager.IsType(const c : Char; const Flags : TCharTypeFlags) : Boolean;
begin
  Result := Flags*gaANSICharType[c] <> [];
end;

{-------------------------------------------------------------------------------
*IsAlpha - Returns true if the character is Alphabetical.
  - not strictly A-Z, a-z
  IsAlpha('E') > True
  IsAlpha('é') > True
See also IsUp, IsAlpha
-------------------------------------------------------------------------------}
function TStringManager.IsAlpha(const c : Char) : Boolean;
begin
  Result := [cfUpper, cfLower]*gaANSICharType[c] <> [];
end;

{-------------------------------------------------------------------------------
*IsUp - Returns true if the character is UpperCase.
  IsUp('É') > True
  IsUp('é') > False
See also IsLo, IsAlpha
-------------------------------------------------------------------------------}
function TStringManager.IsUp(const c : Char) : Boolean;
begin
  Result := [cfUpper]*gaANSICharType[c] <> [];
end;

{-------------------------------------------------------------------------------
*IsLo - Returns true if the character is LowerCase.
  IsLo('É') > False
  IsLo('é') > True
See also IsUp, IsAlpha
-------------------------------------------------------------------------------}
function TStringManager.IsLo(const c : Char) : Boolean;
begin
  Result := [cfLower]*gaANSICharType[c] <> [];
end;

{-------------------------------------------------------------------------------
*IsDigit - Returns true the character is a digit.
  IsDigit('1') > True
See also GetDigit, IsUp, IsAlpha
-------------------------------------------------------------------------------}
function TStringManager.IsDigit(const c : Char) : Boolean;
begin
  Result := [cfDigit]*gaANSICharType[c] <> [];
end;

{-------------------------------------------------------------------------------
*GetChars - Returns characters as specified by aSet. (Purges characters not in set)
 Example: GetChars('123Hallo',['a'..'z']) > 'allo'
 Example: GetChars('123Hallo',['0'..'9']) > '123'
See also CountChars, Purge
-------------------------------------------------------------------------------}
function TStringManager.GetChars(const s : String; const aSet : TCharSet) : String;
var  i : Integer;
  RLen : Integer;
begin
  RLen := 0;
  //We first calculate the length to avoid repeated mem allocation
  for i := 1 to Length(s) do if s[i] in aSet then Inc(RLen);
  SetLength(Result, RLen);
  if RLen=0 then Exit;
  RLen := 0;
  for i := 1 to Length(s) do if s[i] in aSet then
  begin
    Inc(RLen);
    Result[RLen] := s[i];
  end;
end;

{-------------------------------------------------------------------------------
*GetChars- Returns only characters from s found in aStrSet.
 Example: GetChars('123Hallo123','1a') > '1a1'
See also GetChars, CountChars, Purge
-------------------------------------------------------------------------------}
function TStringManager.GetChars(const s : String; const aStrSet : String) : String;
begin
  Result := GetChars(s, ToSet(aStrSet));
end;

{-------------------------------------------------------------------------------
*GetType - Returns characters as specified by aFlags which is a set of TCharTypeFlag.
 Example: GetType('123Hallo',[cfUpper]) > 'H'
 Example: GetType('123Hallo',[cfUpper, cfLower]) > 'Hallo'
See also CountChars, Purge
-------------------------------------------------------------------------------}
function TStringManager.GetType(const s : String; aFlags : TCharTypeFlags) : String;
var  i : Integer;
  RLen : Integer;
begin
  RLen := 0;
  //We first calculate the length to avoid repeated mem allocation
  for i := 1 to Length(s) do if aFlags*gaANSICharType[s[i]] <> [] then Inc(RLen);
  SetLength(Result, RLen);
  if RLen=0 then Exit;
  RLen := 0;
  for i := 1 to Length(s) do if aFlags*gaANSICharType[s[i]] <> [] then
  begin
    Inc(RLen);
    Result[RLen] := s[i];
  end;
end;

{-------------------------------------------------------------------------------
*GetAlpha - Returns all alphabetical characters in the string.
 Example: GetAlpha('123polé') > 'polé'
See also HasAlpha, GetType, PurgeAlpha
-------------------------------------------------------------------------------}
function TStringManager.GetAlpha(const s: String): String;
begin
  Result := GetType(s, [cfUpper, cfLower]);
end;

{-------------------------------------------------------------------------------
*GetUp - Returns all UpperCase characters in the string.
 Example: GetUp('123Polé') > 'P'
See also GetLo, GetType, PurgeUp
-------------------------------------------------------------------------------}
function TStringManager.GetUp(const s: String): String;
begin
  Result := GetType(s, [cfUpper]);
end;

{-------------------------------------------------------------------------------
*GetLo - Returns all LowerCase characters from the string.
 Example: GetLo('123Polé') > 'olé'
See also GetUp, GetType, PurgeUp
-------------------------------------------------------------------------------}
function TStringManager.GetLo(const s: String): String;
begin
  Result := GetType(s, [cfLower]);
end;

{-------------------------------------------------------------------------------
*GetDigit - Returns all digits from the string.
 Example: GetDigit('123Hallo+45') > '12345'
See also IsDigit, GetType, PurgeDigit
-------------------------------------------------------------------------------}
function TStringManager.GetDigit(const s: String): String;
begin
  Result := GetType(s, [cfDigit]);
end;

{-------------------------------------------------------------------------------
*PurgeType - Removes all characters in the string that has TCharType properties
 as indicated by Flags.
 Example: PurgeType('12, eggs',[cfInt, cfDelim]) > 'eggs'
See also CountChars, Purge, GetType
-------------------------------------------------------------------------------}
function TStringManager.PurgeType(const s : String; aFlags : TCharTypeFlags) : String;
var  i : Integer;
  RLen : Integer;
begin
  RLen := 0;
  //We first calculate the length to avoid repeated mem allocation
  for i := 1 to Length(s) do if aFlags*gaANSICharType[s[i]] = [] then Inc(RLen);
  SetLength(Result, RLen);
  if RLen=0 then Exit;
  RLen := 0;
  for i := 1 to Length(s) do if aFlags*gaANSICharType[s[i]] = [] then
  begin
    Inc(RLen);
    Result[RLen] := s[i];
  end;
end;

{-------------------------------------------------------------------------------
*PurgeAlpha - Removes alphabetical characters from the string.
 Example: PurgeAlpha('123polé') > '123'
See also GetAlpha, PurgeType
-------------------------------------------------------------------------------}
function TStringManager.PurgeAlpha(const s: String): String;
begin
  Result := PurgeType(s, [cfUpper, cfLower]);
end;

{-------------------------------------------------------------------------------
*PurgeUp - Remopves all UpperCase characters from the string.
 Example: PurgeUp('123Polé') > '123olé'
See also PurgeLo, GetUp, PurgeType
-------------------------------------------------------------------------------}
function TStringManager.PurgeUp(const s: String): String;
begin
  Result := PurgeType(s, [cfUpper]);
end;

{-------------------------------------------------------------------------------
*PurgeLo - Removes all LowerCase characters from the string.
 Example: PurgeLo('123Polé') > '123P'
See also PurgeUp, GetLo, PurgeType
-------------------------------------------------------------------------------}
function TStringManager.PurgeLo(const s: String): String;
begin
  Result := PurgeType(s, [cfLower]);
end;

{-------------------------------------------------------------------------------
*PurgeDigit - Removes all digits from the string.
 Example: PurgeDigit('123Hallo+45') > 'Hallo+'
See also GetDigit, PurgeType
-------------------------------------------------------------------------------}
function TStringManager.PurgeDigit(const s: String): String;
begin
  Result := PurgeType(s, [cfDigit]);
end;

{-------------------------------------------------------------------------------
*CountChars - counts all characters specified by aSet.
 Example: CountChars('123Hallo',['a'..'z']) > 4
 Example: CountChars('123Hallo',['A'..'z']) > 5
See also CountWords, GetType, Purge
-------------------------------------------------------------------------------}
function TStringManager.CountChars(const s : String; const aSet : TCharSet) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do if s[i] in aSet then Inc(Result);
end;

{-------------------------------------------------------------------------------
*CountLines - Counts the number of lines in a string.
 Example: CountLines('a'#13#10'b' > 2
See also CountSet
-------------------------------------------------------------------------------}
function TStringManager.CountLines(const s : String) : Integer;
var p : PChar;
begin
  Result := 1;
  p := Pointer(s);
  if p=nil then Exit;
  while p^<>#0 do
  begin
    if p^ in [#13,#10] then
    begin
      if p^=#13 then Inc(p);
      if p^=#10 then Inc(p);
      Inc(Result);
    end;
    Inc(p);
  end;
end;

{-------------------------------------------------------------------------------
*CountSet - counts characters in aSet
 Example: CountSet('a'..'z']) > 26
See also CountChars, CountLines, CountWords, GetType, Purge
-------------------------------------------------------------------------------}
function TStringManager.CountSet(const aSet : TCharSet) : Integer;
var i : Char;
begin
  Result := 0;
  for i := Low(i) to High(i) do if i in aSet then Inc(Result);
end;

{-------------------------------------------------------------------------------
*CountWords - counts all words in a string as delimited by aDelims
 Example: CountWords('hello world') > 2
 Example: CountWords('hello world.hello',[' ','.']) > 3
 Example: CountWords('  hello   world  ',[' ']) > 2
See also CountChars, Replace, NthWord
-------------------------------------------------------------------------------}
function TStringManager.CountWords(const s : String; const Delims : TCharSet = gcWordDelims) : Integer;
var  i : Integer;
     j : Integer;
  SLen : Integer;
begin
  Result := 0;
  SLen := Length(s);
  i := 1;
  while i<=SLen do
  begin
    while (i<=SLen) and (s[i] in Delims) do Inc(i);
    j := i;
    while (i<=SLen) and (not (s[i] in Delims)) do Inc(i);
    if i-j>0 then Inc(Result);
  end;
end;

{-------------------------------------------------------------------------------
*GetSingleSorted - returns single (unique Set like) characters sorted.
 Example: GetSingleSorted('hhwwwhhaw') > 'ahw'
 Example: GetSingleSorted('hhwwwhhaw','123h') > '123ahw'
-------------------------------------------------------------------------------}
function TStringManager.GetSingleSorted(const s1 : String; const s2 : String = '') : String;
var i : Integer;
   cs : TCharSet;
begin
  Result := '';
  cs := [];
  for i := 1 to Length(s1) do cs := cs+[s1[i]];
  for i := 1 to Length(s2) do cs := cs+[s2[i]];
  Result := Str(cs);
end;

{-------------------------------------------------------------------------------
*StripDouble - By default it removes all adjacent double characters, optionally some
  double characters can be preserved and/or some characters completely purged.

 Example: StripDouble('Phillips 1155')               > 'Philips 15'
 Example: StripDouble('Phillips 1155',['0'..'9'])    > 'Philips 1155'
 Example: StripDouble('Phillips 1155',[],['0'..'9']) > 'Philips'

See also Strip
-------------------------------------------------------------------------------}
function TStringManager.StripDouble(const s : String; const IgnoreSet : TCharSet = []; const PurgeSet : TCharSet = []; const DoUpcase : Boolean = False) : String;
var   i : Integer;
   RLen : Integer;
   Slen : Integer;
begin
  Result := '';
  Slen := Length(s);
  if Slen=0 then Exit;

  //Pre-calculate result length - avoid memory allocations.
  i := 1;
  RLen := 0;
  while i <= Slen do
  begin
    if not (s[i] in IgnoreSet) then
      while (i<Slen) and (s[i] = s[i+1]) do Inc(i);
    if not (s[i] in PurgeSet)
      then Inc(RLen);
    Inc(i);
  end;

  if Rlen=0 then Exit;
  SetLength(Result, RLen);

  //Put characters
  i := 1;
  Rlen := 0;
  while i <= Slen do
  begin
    if not (s[i] in IgnoreSet) then
      while (i<Slen) and (s[i] = s[i+1]) do Inc(i);
    if not (s[i] in PurgeSet) then
    begin
      Inc(Rlen);
      Result[RLen] := s[i];
    end;
    Inc(i);
  end;

  if DoUpCase then for i := 1 to RLen do Result[i] := gaANSIUpperArray[Result[i]];
end;

{-------------------------------------------------------------------------------
*Purge - Removes all characters from s that are contained in aSet.
 Example: Purge('helloWORLD123Hello',['a'..'z']) --> 'WORLD123H'
 Example: Purge('helloWORLD123Hello',['A'..'z']) --> '123'
 Example: Purge('hello123',['0'..'9'], True) --> 'HELLO'
See also overloaded Char version, GetChars
-------------------------------------------------------------------------------}
function TStringManager.Purge(const s : String; const aSet : TCharSet; DoUpcase : Boolean = False) : String;
var  i : Integer;
  RLen : Integer;
begin
  RLen := 0;
  //We first calculate the length to avoid repeated mem allocation
  for i := 1 to Length(s) do
  begin
    if not (s[i] in aSet) then Inc(Rlen);
  end;
  SetLength(Result, RLen);
  if Rlen=0 then Exit;
  RLen := 0;
  for i := 1 to Length(s) do
  begin
    if not (s[i] in aSet) then begin Inc(RLen); Result[RLen] := s[i]; end;
  end;
  if DoUpCase then for i := 1 to Length(Result) do Result[i] := gaANSIUpperArray[Result[i]];
end;

{-------------------------------------------------------------------------------
*Purge - Removes a character from a string.
 Example: Purge('hello world',' ') --> 'helloworld'
See also overloaded TCharSet version
-------------------------------------------------------------------------------}
function TStringManager.Purge(const s : String; c : Char) : String;
begin
  Result := Purge(s, [c]);
end;

{-------------------------------------------------------------------------------
*PurgeWhile
 This function purges characters contained in aSet from the left until
 it encounters a character not found in the set.
 Example: PurgeWhile('helloWORLDhello',['a'..'z']) > 'WORLDhello'
See also PurgeWhileRev, PurgeWhileOuter
-------------------------------------------------------------------------------}
function TStringManager.PurgeWhile(const s : String; const aSet : TCharSet) : String;
var i : Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if not (s[i] in aSet) then
    begin
      Result := System.Copy(s, i, Length(s));
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PurgeWhileRev
 This function purges characters contained in aSet from the right util
 it encounters a character not found in the set.
 Example: PurgeWhileRev('helloWORLDhello',['a'..'z']) > 'helloWORLD'
See also PurgeWhile, PurgeWhileOuter
-------------------------------------------------------------------------------}
function TStringManager.PurgeWhileRev(const s : String; const aSet : TCharSet) : String;
var i : Integer;
begin
  Result := '';
  for i := Length(s) downto 1 do
  begin
    if not (s[i] in aSet) then
    begin
      Result := System.Copy(s, 1, i);
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*PurgeWhileOuter - a combination of functions PurgeWhile and PurgeWhileRev.
 It first returns characters from the left while they are not found in aSet,
 then repeats the process from the right.
 Example: PurgeWhileOuter('helloWORLDxWORLDhello',['a'..'z']) > 'WORLDxWORLD'
 Example: PurgeWhileOuter('123letters45letters',['A'..'z']) > '123letters45'
See also PurgeWhile, PurgeWhileRev, DelEnclosed
-------------------------------------------------------------------------------}
function TStringManager.PurgeWhileOuter(const s : String; const aSet : TCharSet) : String;
var i : Integer;
 SLen : Integer;
begin
  i := 1;
  Slen := Length(s);
  while (i<=Slen) and (s[i] in aSet) do Inc(i);
  while (SLen>i) and (s[SLen] in aSet) do Dec(SLen);
  Result := System.Copy(s, i, (SLen-i)+1);
end;

{-------------------------------------------------------------------------------
*Rev - Reverses the order of the characters in a string.
 Example: Rev('ABC') > 'CBA'
-------------------------------------------------------------------------------}
function TStringManager.Rev(const s : String) : String;
var
  i, j  : Integer;
  times : Integer;
begin
  j := Length(s);
  SetLength(Result, j);
  times := j div 2;
  if j mod 2<>0 then Inc(times);
  for i := 1 to times do
  begin
    Result[i] := s[j];
    Result[j] := s[i];
    Dec(j);
  end;
end;

{-------------------------------------------------------------------------------
*Sort - Sorts the characters in a string.
 Example: Sort('hello') > 'ehllo'
-------------------------------------------------------------------------------}
function TStringManager.Sort(const s : String) : String;
   procedure QuickSort(l, r: Integer);
   var
     i, j : Integer;
     c, t : Char;
   begin
     repeat
       i := l;
       j := r;
       c := Result[(l+r) shr 1];
       repeat
         while Result[i]<c do Inc(i);
         while Result[j]>c do System.Dec(j);
         if i<=j then
         begin
           t := Result[i];
           Result[i] := Result[j];
           Result[j] := t;
           Inc(i);
           System.Dec(j);
         end;
       until i>j;
       if l<j then QuickSort(l, j);
       l := i;
     until i>=r;
   end;
begin
  Result := s;
  if Length(Result)<2 then Exit;
  QuickSort(1, Length(Result));
end;

{-------------------------------------------------------------------------------
*Str - Converts Boolean to a string.
 Example: Str(False) > 'False'
See also ToBool
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Boolean) : String;
begin
  if Value then Result := 'True' else Result := 'False';
end;

{-------------------------------------------------------------------------------
*Str - Converts TCharSet to a string.
 Example:
  Str(['a'..'z') > 'abcdefghijklmnopqrstuvwxyz'
  Str(['a'..'z', True) > 'a..z'
See also ToSet, CharSetToVisualStr
-------------------------------------------------------------------------------}
function TStringManager.Str(const Value : TCharSet; AsVisual : Boolean = False) : String;
var   c : Char;
   RLen : Integer;
begin
  if AsVisual then Result := CharSetToVisualStr(Value) else
  begin
    RLen := 0;
    //First calculate the result length to avoid mem fragmentation
    for c := Low(c) to High(c) do if c in Value then Inc(RLen);
    SetLength(Result, RLen);
    if RLen=0 then Exit;
    RLen := 0;
    for c := Low(c) to High(c) do
    begin
      if c in Value then
      begin
        Inc(RLen);
        Result[RLen] := c;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*Str - Returns all characters that has CharType properties as indicated by Flags.
 Example: Str([cfAZDigit]) > '0123456789'
See also other overloaded ToSet function
-------------------------------------------------------------------------------}
function TStringManager.Str(aFlags : TCharTypeFlags) : String;
var  c : Char;
  RLen : Integer;
begin
  RLen := 0;
  for c := Low(Char) to High(Char) do
  begin
    if aFlags*gaANSICharType[c] <> [] then Inc(RLen);
  end;
  SetLength(Result, RLen);
  if RLen=0 then Exit;
  RLen := 0;
  for c := Low(Char) to High(Char) do
  begin
    if aFlags*gaANSICharType[c] <> [] then
    begin
      Inc(RLen);
      Result[RLen] := c;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*Str - Converts an Integer to a string.
 Identical to standard function IntToStr, but slightly faster.
 Example: Str(12) > '12'
See also the overloaded Integer Str version with padding.
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Integer) : String;
begin
  System.Str(Value:0, Result);
end;

{-------------------------------------------------------------------------------
*Str
 Converts an Integer to string and pads it left to ensure a Length of PadLen.
 If the conversion Result is >PadLen then PadLen is ignored.
 Example: Str(12) > '12'
 Example: Str(12,6) > '000012'
 Example: Str(12,5,' ') > '   12'
 If PadLen=0 then no padding is performed
 If PadLen is negative then the padding will be done to the right.
 Example: Str(12,-5,' ') > '12   '

See also FormatSize, ToI
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Integer; PadLen : SmallInt; PadChar : Char = '0') : String;
begin
  System.Str(Value, Result);
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar);
end;

{------------------------------------------------------------------------------
 See notes at the Integer version of the function
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Int64; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  try
    Result := SysUtils.IntToStr(Value);
  except
    Result := '';
  end;
  if PadLen<>0 then
  begin
    if Value<0 then
    begin
      Delete(Result, 1, 1);
      Result := '-'+Pad(Result, -(PadLen-1), PadChar);
    end else Result := Pad(Result, -PadLen, PadChar);
  end;
end;

{-------------------------------------------------------------------------------
 See notes at the Integer version of the function
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Cardinal; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  System.Str(Value, Result);
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar);
end;

{-------------------------------------------------------------------------------
 See notes at the Integer version of the function
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Byte; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  System.Str(Value, Result);
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar);
end;

{-------------------------------------------------------------------------------
 See notes at the Integer version of the function
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : SmallInt; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  System.Str(Value, Result);
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar);
end;

{-------------------------------------------------------------------------------
 See notes at the Integer version of the function
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Word; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  System.Str(Value, Result);
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar);
end;

{-------------------------------------------------------------------------------
*Str - Extended
 Converts a float to string and pads it left to ensure a length of n.
 If the conversion Result is >n then n is ignored
 If n is negative the padding is done to the right
 The Result will allways have two decimal places and a max of 5
 Example: Str(12,8)       > '00012.00'
 Example: Str(12.1,8)     > '00012.10'
 Example: Str(12.123,8)   > '0012.123'
 Example: Str(12.123,2)   > '12.123'
 Example: Str(12.123,-8)  > '12.12300'
 Example: Str(12.1,8,'x') > 'xxx12.10'
See also System.CurrToStrF
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Extended; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  try
    Result := SysUtils.FormatFloat('0.00###', Value);
  except
    Result := '';
  end;
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar)
end;

{-------------------------------------------------------------------------------
 Please see notes at the Extended version of the function
See also StrFloat
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : Double; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  try
    Result := SysUtils.FormatFloat('0.00###', Value);
  except
    Result := '';
  end;
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar)
end;

{-------------------------------------------------------------------------------
*Str - Currency
 TCurrency is defined as a record type to allow function overloading with Double
 and TDateTime values.
See notes at function StrCur
-------------------------------------------------------------------------------}
function TStringManager.Str(Value : TCurrency; PadLen : SmallInt = 0; PadChar : Char = '0') : String;
begin
  Result := StrCur(Value.c, PadLen, PadChar);
end;

{-------------------------------------------------------------------------------
*Str - Pointer
 The value is converted to an 8 character string that represents the pointers
 value in hexadecimal.
------------------------------------------------------------------------------}
function TStringManager.Str(Value : Pointer) : String;
begin
  Result := SysUtils.Format('%p', [Value]);
end;

{-------------------------------------------------------------------------------
*StrFloat - float to string.
 Example:
  StrFloat(   23.1)                   > '23.10'
  StrFloat(   23.123456)              > '23.12346'
  StrFloat(   23.12345, '#,##0.00', 12) > '       23.12'
  StrFloat(12323.12345, '#,##0.00', 12) > '   12,323.12'
See also Double, extended Str overloaded functions
-------------------------------------------------------------------------------}
function TStringManager.StrFloat(Value : Extended; aFormatStr : String = '0.00###'; Padlen : SmallInt = 0; PadChar : Char = ' ') : String;
begin
  try
    Result := SysUtils.FormatFloat(aFormatStr, Value);
  except
    Result := '';
  end;
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar)
end;

{-------------------------------------------------------------------------------
*StrCur -
 Converts a Currency value to a string by using gcDefaultCurFormat
   StrCur(   23.1)         > '€23.10'
   StrCur(   23.123456)    > '€23.12'
 Optional additional padding can be done.
   StrCur(   23.12345, 12) > '      €23.12'
   StrCur(12323.12345, 12) > '  €12,323.12'
See also Str, System.CurrToStF
--------------------------------------------------------------------------------}
function TStringManager.StrCur(Value : Currency; PadLen : SmallInt = 0; PadChar : Char = ' ') : String;
begin
  try
    Result := SysUtils.FormatCurr(gcDefaultCurFormat, Value);
  except
    Result := '';
  end;
  if PadLen<>0 then Result := Pad(Result, -PadLen, PadChar)
end;


{-------------------------------------------------------------------------------
*StrDate - Date to string.
  Same as the standard DateToStr function, except that you can optionally
  specify the DateFormat to override the default ShortDateFormat.

  Example: StrDate(now) > '21/03/2000' (output depends on ShortDateFormat)
  Example: StrDate(now,'hh:nn ddd dd-mm-yyyy') > '15:01 Wed 22-03-2000'
See also ToDate, StrDateTime
-------------------------------------------------------------------------------}
function TStringManager.StrDate(Value: TDateTime; const aFormat : String = ''): String;
begin
  if aFormat=''
    then SysUtils.DateTimeToString(Result, ShortDateFormat, Value)
    else SysUtils.DateTimeToString(Result, aFormat, Value);
end;

{-------------------------------------------------------------------------------
*StrDateTime - Date and Time to string.
  Same as the standard DateTimeToStr function, except that you can optionally
  specify the DateFormat to override the default ShortDateFormat+LongTimeFormat.

  Example: StrDateTime(now) > '21:40 12 June 2000' (output depends on System defined formats)
  Example: StrDateTime(now,'hh:nn ddd dd-mm-yyyy') > '15:01 Wed 22-03-2000'
See also ToDate, StrDate
-------------------------------------------------------------------------------}
function TStringManager.StrDateTime(Value: TDateTime; const aFormat : String = ''): String;
begin
  if aFormat=''
    then Result := SysUtils.DateTimeToStr(Value)
    else SysUtils.DateTimeToString(Result, aFormat, Value);
end;

{-------------------------------------------------------------------------------
*ToDate - String to TDateTime
  Note - the obvious function; StrToDate, only supports numerical months.
  Therefore if the user changes the ShortDateFormat in the Control Panel to
  something like dd-mmm-yyyy then the function will fail.
  The Variant function VarToDateTime, is much more intelligent, and even two
  different formats like '2001/feb/12' and '12-feb-2001' works.

  Example: MyDate := ToDate('12-Jan-2001')
See also StrDate, StrDateTime
-------------------------------------------------------------------------------}
function TStringManager.ToDate(const s : String) : TDateTime;
begin
  Result := VarToDateTime(s);
end;

{-------------------------------------------------------------------------------
*FindChar- moves index until s[index] in aSet.
-------------------------------------------------------------------------------}
function TStringManager.FindChar(const s : String; const aSet : TCharSet; const FromPos : Integer = 1) : Integer;
begin
  Result := FromPos;
  while (Result <= Length(s)) and not (s[Result] in aSet) do
  begin
    if s[Result] in LeadBytes then Inc(Result);                                             
    Inc(Result);
  end;
end;

{-------------------------------------------------------------------------------
*ScanWhile - moves index while s[index] in aSet.
-------------------------------------------------------------------------------}
function TStringManager.FindNotChar(const s : String; const aSet : TCharSet; const FromPos : Integer = 1) : Integer;
begin
  Result := FromPos;
  while (Result <= Length(s)) and (s[Result] in aSet) do
  begin
    if s[Result] in LeadBytes then Inc(Result);
    Inc(Result);
  end;
end;

{-------------------------------------------------------------------------------
*PReplace - Replaces occurences of FindStr in InStr with ReplaceStr.
 This function is much faster and more flexible that the StringUtils.StringReplace function.
 It avoids unnessary memory reallocation.
 Options:
   StartPos - specifies where to start in Instr
   Flag roIgnoreCase   - not case sensitive
   Flag roPreserveCase - normally relevant if ignorecase ('dog,'CAT','Dog dog') > 'Cat cat')
   Flag roWholeWords   - finds only whole words depending on Delims
   Flag roReplaceOnce  - exit's after first replace - StartPos is updated
   Flag roDeleteDelim  - if WholeWords then it deletes the delimiter also - useful if ReplaceStr is empty.
   Delims              - relevant if roWholeWord flag - Delims=[] and WholeWords then WholeWords will be ignored.
   Return value        - number of replacements


 Example:
   s := 'The dog is dogmeat in DogLand'
   StartPos := 1;
   -----
   PReplace('dog','cat', s, StartPos)
     > s = "The cat is catmeat in DogLand"
   -----
   PReplace('dog','cat', s, StartPos,[roIgnoreCase]);
     > s = "The cat is catmeat in catLand"
   -----
   PReplace('dog','cat', s, StartPos,[roWholeWords]);
     > s = "The cat is dogmeat in DogLand"
   -----
   PReplace('dog','cat', s, StartPos,[roIgnoreCase, roPreserveCase]);
     > s = "The cat is catmeat in CatLand"
   -----
   while StartPos>0 do
   begin
     PReplace('dog','cat', s, StartPos,[roIgnoreCase, roPreserveCase, roReplaceOnce]);
   end;
   > s = "The cat is catmeat in CatLand"
See also simpler functions; ReplaceStr, ReplaceWord, DelWord
-------------------------------------------------------------------------------}
function TStringManager.PReplace(
           const FindStr    : String;
           const ReplaceStr : String;
           var   InStr      : String;
           var   StartPos   : Integer;
           const Flags      : TStrReplaceOptions = [];
           const Delims     : TCharSet = gcWordDelims)
                            : Integer;

var   lenTarget : Integer;
        lenFind : Integer;
        lenCut  : Integer;
     lenReplace : Integer;
         lenDif : Integer;
    CurrentSize : Integer;
           fPos : Integer;
        posDest : Integer;
PreserveCaseStr : String;
     PtrReplace : Pointer;


  procedure GetMoreMem;
  var n, x : Integer;
  begin
    n := ((lenTarget+1)-StartPos) div lenFind;  //max number of FindStr's left
    x := n*lenDif;
    if x>$ffff then
    begin
      if lenDif>$ffff
      then x := 2*LenDif
      else x := 10*LenDif;
    end;
    CurrentSize := CurrentSize+x;
    SetLength(InStr, CurrentSize);
  end;

  procedure SetCasePreserveStr;
  var i : Integer;
      x : Integer;
  begin
    System.Move(ReplaceStr[1], PreserveCaseStr[1], lenReplace);
    for i := fPos to (fPos+lenFind)-1 do
    begin
      x := (i-fPos)+1;
      if x>lenReplace then Break else
      if cfUpper in gaANSICharType[InStr[i]]
        then PUpCase(PreserveCaseStr[x])
        else PLoCase(PreserveCaseStr[x]);
    end;
  end;

  procedure DoReplace;
  begin
    if (roPreserveCase in Flags) then if lenReplace>0 then SetCasePreserveStr;
    if lenDif=0 then
    begin
      if lenReplace>0 then System.Move(PtrReplace^, InStr[fPos], lenCut);
    end else if lenCut>lenReplace then
    begin
      if fPos+lenDif<=lenTarget then System.Move(InStr[fPos+lenDif], InStr[fPos], (lenTarget-fPos)+1);
      if lenReplace>0 then System.Move(PtrReplace^, InStr[fPos], lenReplace);
      System.Dec(lenTarget, lenDif);
    end else
    begin
      posDest := fPos+lenDif;
      if posDest+(lenTarget-fPos)>CurrentSize then GetMoreMem;
      System.Move(InStr[fPos], InStr[posDest], (lenTarget-fPos)+1);
      if lenReplace>0 then System.Move(PtrReplace^, InStr[fPos], lenReplace);
      System.Inc(lenTarget, lenDif);
    end;
  end;

  procedure DoAny;
  var   i : Integer;
        j : Integer;
       si : Integer;
       fc : Char;
     Upto : Integer;
  begin
    i := StartPos;
    fc := FindStr[1];
    Upto := (lenTarget-lenFind)+1;
    while i<=Upto do
    begin
      if InStr[i] = fc then
      begin
        si := i;
        j := 2;
        while j<=lenFind do
        begin
          Inc(si);
          if InStr[si] <> FindStr[j] then Break;
          Inc(j);
        end;
        if j=lenFind+1 then
        begin
          fPos := i;
          DoReplace;
          Upto := (lenTarget-lenFind)+1;
          Inc(Result);
          if (roReplaceOnce in Flags) then Break;
          Inc(i, lenReplace-1);
          if (i+lenFind>lenTarget+1) then Break;
        end;
      end;
      Inc(i);
    end;
    if Result>0 then
    begin
      if lenTarget<>CurrentSize then SetLength(InStr, lenTarget);
    end;
  end;

  procedure DoAnyIC;
  var   i : Integer;
        j : Integer;
       si : Integer;
       fc : Char;
     Upto : Integer;
  begin
    i := StartPos;
    fc := gaANSIUpperArray[FindStr[1]];
    Upto := (lenTarget-lenFind)+1;
    while i<=Upto do
    begin
      if gaANSIUpperArray[InStr[i]] = fc then
      begin
        si := i;
        j := 2;
        while j<=lenFind do
        begin
          Inc(si);
          if gaANSIUpperArray[InStr[si]] <> gaANSIUpperArray[FindStr[j]] then Break;
          Inc(j);
        end;
        if j=lenFind+1 then
        begin
          fPos := i;
          DoReplace;
          Upto := (lenTarget-lenFind)+1;          
          Inc(Result);
          if (roReplaceOnce in Flags) then Break;
          Inc(i, lenReplace-1);
          if (i+lenFind>lenTarget+1) then Break;
        end;
      end;
      Inc(i);
    end;
    if Result>0 then
    begin
      if lenTarget<>CurrentSize then SetLength(InStr, lenTarget);
    end;
  end;

  procedure DoWords;
  var   i : Integer;
        j : Integer;
       si : Integer;
       fc : Char;
     Upto : Integer;
  begin
    i := StartPos;
    fc := FindStr[1];
    Upto := (lenTarget-lenFind)+1;
    while i<=Upto do
    begin
      if (InStr[i] = fc) then if (i=1) or (InStr[i-1] in Delims) then
      begin
        si := i;
        j := 2;
        while j<=lenFind do
        begin
          Inc(si);
          if InStr[si] <> FindStr[j] then Break;
          Inc(j);
        end;
        if j=lenFind+1 then if (si>=lenTarget) or (InStr[si+1] in Delims) then
        begin
          fPos := i;
          if (roRemoveDelim in Flags) then
          begin
            if (i=1) and (si=lenTarget) then else
            begin
              if si=lenTarget then if fPos>1 then Dec(fPos);
              lenCut := lenFind +1;
              lenDif := Abs(lenCut-lenReplace);
            end;
          end;
          DoReplace;
          Upto := (lenTarget-lenFind)+1;          
          Inc(Result);
          if (roReplaceOnce in Flags) then Break;
          Inc(i, lenReplace-1);
          if (i+lenFind>lenTarget+1) then Break;
        end;
      end;
      Inc(i);
    end;
    if Result>0 then
    begin
      if lenTarget<>CurrentSize then SetLength(InStr, lenTarget);
    end;
  end;

  procedure DoWordsIC;
  var   i : Integer;
        j : Integer;
       si : Integer;
       fc : Char;
     Upto : Integer;
  begin
    i := StartPos;
    fc := gaANSIUpperArray[FindStr[1]];
    Upto := (lenTarget-lenFind)+1;
    while i<=Upto do
    begin
      if (gaANSIUpperArray[InStr[i]] = fc) then if (i=1) or (InStr[i-1] in Delims) then
      begin
        si := i;
        j := 2;
        while j<=lenFind do
        begin
          Inc(si);
          if gaANSIUpperArray[InStr[si]] <> gaANSIUpperArray[FindStr[j]] then Break;
          Inc(j);
        end;
        if j=lenFind+1 then if (si>=lenTarget) or (InStr[si+1] in Delims) then
        begin
          fPos := i;
          if (roRemoveDelim in Flags) then
          begin
            if (i=1) and (si=lenTarget) then else
            begin
              if si=lenTarget then if fPos>1 then Dec(fPos);
              lenCut := lenFind +1;
              lenDif := Abs(lenCut-lenReplace);
            end;
          end;
          DoReplace;
          Upto := (lenTarget-lenFind)+1;          
          Inc(Result);
          if (roReplaceOnce in Flags) then Break;
          Inc(i, lenReplace-1);
          if (i+lenFind>lenTarget+1) then Break;
        end;
      end;
      Inc(i);
    end;
    if Result>0 then
    begin
      if lenTarget<>CurrentSize then SetLength(InStr, lenTarget);
    end;
  end;

begin
  Result := 0;
  lenFind    := Length(FindStr);
  lenReplace := Length(ReplaceStr);
  lenTarget  := Length(InStr);
  if (lenTarget<1) or (lenFind=0) then
  begin
    StartPos := 0;
    Exit;
  end;
  if (StartPos+lenFind>lenTarget+1) then
  begin
    StartPos := 0;
    Exit;
  end;
  CurrentSize := lenTarget;
  lenCut := lenFind;
  lenDif := Abs(lenCut-lenReplace);
  if StartPos=0 then StartPos := 1;
  if (roPreserveCase in Flags) then
  begin
    SetLength(PreserveCaseStr, lenReplace);
    PtrReplace := Pointer(PreserveCaseStr);
  end else
  begin
    PreserveCaseStr := '';
    PtrReplace := Pointer(ReplaceStr);
  end;
  if (roIgnoreCase in Flags) then
  begin
    if (roWholeWords in Flags) then
    begin
      if Delims=[] then DoAnyIC else DoWordsIC;
    end else DoAnyIC;
  end else
  begin
    if (roWholeWords in Flags) then
    begin
      if Delims=[] then DoAny else DoWords;
    end else DoAny;
  end;
end;

{-------------------------------------------------------------------------------
*ReplaceOnce - Replaces the first occurance of ReplaceStr from StartPos
See PReplace for details.
See also PReplace, ReplaceIC, ReplaceWord
-------------------------------------------------------------------------------}
function TStringManager.ReplaceOnce(
              const FindStr      : String;
              const ReplaceStr   : String;
              const InStr        : String;
              var StartPos       : Integer;
              const IgnoreCase   : Boolean = False;
              const WholeWord    : Boolean = False;
              const PreserveCase : Boolean = False;
              const Delims       : TCharSet = gcWordDelims) : String;

var Flags : TStrReplaceOptions;
begin
  Result := InStr;
  Flags := [roReplaceOnce];
  if IgnoreCase then Flags := Flags + [roIgnoreCase];
  if WholeWord then Flags := Flags+[roWholeWords];
  if PreserveCase then Flags := Flags+[roPreserveCase];
  PReplace(FindStr, ReplaceStr, Result, StartPos, Flags, Delims);
end;

{-------------------------------------------------------------------------------
*PReplace - Simple version of the PReplace procedure above.
-------------------------------------------------------------------------------}
function TStringManager.PReplace(
             const FindStr      : String;
             const ReplaceStr   : String;
              var InStr          : String;
             const IgnoreCase   : Boolean = False;
             const WholeWord    : Boolean = False;
             const PreserveCase : Boolean = False) : Integer;
var StartPos : Integer;
       Flags : TStrReplaceOptions;
begin
  StartPos := 1;
  Flags := [];
  if IgnoreCase then Flags := [roIgnoreCase];
  if WholeWord then Flags := Flags+[roWholeWords];
  if PreserveCase then Flags := Flags+[roPreserveCase];
  Result := PReplace(FindStr, ReplaceStr, InStr, StartPos, Flags);
end;

{-------------------------------------------------------------------------------
*PReplaceIC - Simple case insensitive version of the PReplace procedure.
-------------------------------------------------------------------------------}
function TStringManager.PReplaceIC(
             const FindStr      : String;
             const ReplaceStr   : String;
              var InStr          : String;
             const WholeWord    : Boolean = False;
             const PreserveCase : Boolean = False) : Integer;

var StartPos : Integer;
       Flags : TStrReplaceOptions;
begin
  StartPos := 1;
  Flags := [roIgnoreCase];
  if WholeWord then Flags := Flags+[roWholeWords];
  if PreserveCase then Flags := Flags+[roPreserveCase];
  Result := PReplace(FindStr, ReplaceStr, InStr, StartPos, Flags);
end;


{-------------------------------------------------------------------------------
*Replace - Replaces all occurences of FindStr in InStr with ReplaceStr, with the
option to Ignore case and to replace whole words only.
 Example:
   Replace('Char','Byte','char, CharArray') > 'char, ByteArray'
   Replace('Char','Byte','char, CharArray', True) > 'Byte, ByteArray'
   Replace('Char','Byte','char, CharArray', True, True) > 'Byte, CharArray'      

See also PReplace, ReplaceIC, ReplaceWord
-------------------------------------------------------------------------------}
function TStringManager.Replace(
             const FindStr      : String;
             const ReplaceStr   : String;
             const InStr        : String;
             const IgnoreCase   : Boolean = False;
             const WholeWord    : Boolean = False;
             const PreserveCase : Boolean = False;
             const Delims       : TCharSet = gcWordDelims) : String;

var StartPos : Integer;
       Flags : TStrReplaceOptions;
begin
  StartPos := 1;
  Result := InStr;
  Flags := [];
  if IgnoreCase then Flags := [roIgnoreCase];
  if WholeWord then Flags := Flags+[roWholeWords];
  if PreserveCase then Flags := Flags+[roPreserveCase];
  PReplace(FindStr, ReplaceStr, Result, StartPos, Flags, Delims);
end;

{-------------------------------------------------------------------------------
*ReplaceIC - Replaces all occurences of FindStr in InStr with ReplaceStr,
  while ignoring case.
 Example:
   ReplaceIC('char','byte','char, CharArray')
    > 'byte, byteArray'
   ReplaceIC('char','byte','char, CharArray', True)
    > 'byte, ByteArray'

See also PReplace, Replace, ReplaceWordIC
-------------------------------------------------------------------------------}
function TStringManager.ReplaceIC(
           const FindStr      : String;
           const ReplaceStr   : String;
           const InStr        : String;
           const WholeWord    : Boolean = False;
           const PreserveCase : Boolean = False;
           const Delims       : TCharSet = gcWordDelims) : String;
var StartPos : Integer;
       Flags : TStrReplaceOptions;
begin
  StartPos := 1;
  Result := InStr;
  Flags := [roIgnoreCase];
  if WholeWord then Flags := Flags+[roWholeWords];
  if PreserveCase then Flags := Flags+[roPreserveCase];
  PReplace(FindStr, ReplaceStr, Result, StartPos, Flags, Delims);
end;

{-------------------------------------------------------------------------------
*ReplaceWord - Replaces all occurences of FindStr in InStr with ReplaceStr,
 using Delims as word delimiters.
 Example:
  ReplaceWord('you','I','You bought it in Ireland where you live.')
    > 'You bought it in Ireland where I live.'
See also ReplaceWordIC, PReplace, Replace
-------------------------------------------------------------------------------}
function TStringManager.ReplaceWord(
           const FindStr      : String;
           const ReplaceStr   : String;
           const InStr        : String;
           const IgnoreCase   : Boolean = False;
           const PreserveCase : Boolean = False;
           const Delims       : TCharSet = gcWordDelims) : String;
var StartPos : Integer;
       Flags : TStrReplaceOptions;
begin
  StartPos := 1;
  Result := InStr;
  Flags := [roWholeWords];
  if IgnoreCase   then Flags := Flags+[roIgnoreCase];
  if PreserveCase then Flags := Flags+[roPreserveCase];
  PReplace(FindStr, ReplaceStr, Result, StartPos, Flags, Delims);
end;

{-------------------------------------------------------------------------------
*ReplaceWordIC - Replaces all occurences of FindStr in InStr with ReplaceStr,
 using Delims as word delimiters while ignoring case.
 Example:
  ReplaceWordIC('I','you','I bought it in Ireland where I live.')
See also ReplaceWord, PReplace, ReplaceIC
-------------------------------------------------------------------------------}
function TStringManager.ReplaceWordIC(
           const FindStr      : String;
           const ReplaceStr   : String;
           const InStr        : String;
           const PreserveCase : Boolean = False;
           const Delims       : TCharSet = gcWordDelims) : String;
var StartPos : Integer;
begin
  StartPos := 1;
  Result := InStr;
  if PreserveCase
    then PReplace(FindStr, ReplaceStr, Result, StartPos, [roWholeWords, roIgnoreCase, roPreserveCase], Delims)
    else PReplace(FindStr, ReplaceStr, Result, StartPos, [roWholeWords, roIgnoreCase], Delims);
end;

{-------------------------------------------------------------------------------
*StringReplace - SysUtils.StringReplace compatible version.
 This function is about 6 times faster, but is not multibyte compatible (Japanese, Chinese, etc).
 Note the different order of the parameters to the local functions.
See also PReplace, ReplaceIC, ReplaceWord
-------------------------------------------------------------------------------}
function TStringManager.StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
var StartPos : Integer;
      lFlags : TStrReplaceOptions;
begin
  StartPos := 1;
  Result := s;
  lFlags := [];
  if not (rfReplaceAll in Flags)
    then lFlags := [roReplaceOnce];
  if (rfIgnoreCase in Flags)
    then lFlags := lFlags+[roIgnoreCase];
  PReplace(OldPattern, NewPattern, Result, StartPos, lFlags);
end;

{-------------------------------------------------------------------------------
*ReplaceChars
 Replaces all the occurences of OldChars with NewChar.
 Example:
  s := 'Replace e and a with *';
  ReplaceChars(s,['e','a'],'*') returns 'R*pl*c* * *nd * with *';
-------------------------------------------------------------------------------}
function TStringManager.ReplaceChars(const s : String; const OldChars: TCharSet; NewChar : Char) : String;
var i : Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
  begin
    if Result[i] in OldChars then Result[i] := NewChar;
  end;
end;

{-------------------------------------------------------------------------------
*Replace - just a fast char overloaded version.
-------------------------------------------------------------------------------}
function TStringManager.Replace(OldChar, NewChar : Char; const InStr : String; const IgnoreCase : Boolean = False) : String;
var i : Integer;
begin
  if IgnoreCase then Result := ReplaceIC(OldChar, NewChar, InStr) else
  begin
    Result := InStr;
    for i := 1 to Length(Result) do
    begin
      if OldChar=Result[i] then Result[i] := NewChar;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*Replace - just a fast char overloaded version
-------------------------------------------------------------------------------}
function TStringManager.ReplaceIC(OldChar, NewChar : Char; const InStr : String) : String;
var i : Integer;
begin
  Result := InStr;
  PUpcase(OldChar);
  for i := 1 to Length(Result) do
  begin
    if OldChar=gaANSIUpperArray[Result[i]] then Result[i] := NewChar;
  end;
end;

{-------------------------------------------------------------------------------
*DelIfLast - Deletes SubStr from s if it is the last text.
 Example: DelIfLast('last','You are last') > 'You are '
 Example: DelIfLast('last','You are last.') > 'You are last.'
See also DelIfFirst, and TCharSet overloaded version
-------------------------------------------------------------------------------}
function TStringManager.DelIfLast(const SubStr : String; const s : String; IgnoreCase : Boolean = False) : String;
begin
  if IsLast(SubStr, s, IgnoreCase)
    then Result := System.Copy(s, 1, Length(s)-Length(SubStr))
    else Result := s;
end;

{-------------------------------------------------------------------------------
*PDelIfLast - Returns true and deletes SubStr from s if it is the last text.
See also DelIfFirst, and TCharSet overloaded version
-------------------------------------------------------------------------------}
function TStringManager.PDelIfLast(const SubStr : String; var s : String; IgnoreCase : Boolean = False) : Boolean;
begin
  Result := IsLast(SubStr, s, IgnoreCase);
  if Result then System.SetLength(s, Length(s)-(Length(SubStr)));
end;

{-------------------------------------------------------------------------------
*PDelIfLast - Returns true and Deletes SubStr from s if it is the last text.
See also DelIfFirst, and TCharSet overloaded version
-------------------------------------------------------------------------------}
function TStringManager.PDelIfLast(const IfInSet : TCharSet; var s : String) : Boolean;
begin
  if s=''
    then Result := False
    else Result := s[Length(s)] in IfInSet;
  if Result then SetLength(s, Length(s)-1);
end;

{-------------------------------------------------------------------------------
*DelIfFirst - Deletes SubStr from s if it is the first text.
 Example: DelIfFirst('first','first text') > ' text'
 Example: DelIfFirst('*first','first text') > '*first text'
 Example: DelIfFirst('abcde','abc') > 'abc' 
See also PDelIfFirst, DelIfLast and TCharSet overloaded version
-------------------------------------------------------------------------------}
function TStringManager.DelIfFirst(const SubStr: String; const s : String; IgnoreCase : Boolean = False) : String;
begin
  if IsFirst(SubStr, s, IgnoreCase)
    then Result := System.Copy(s, Length(SubStr)+1, Length(s))
    else Result := s;
end;

{-------------------------------------------------------------------------------
*PDelIfFirst - If SubStr is the same as the start of s, it will return true
  and remove the text.
 Example:
  If s='abcde' then:
    PDelIfFirst('abc', s) > True and s= 'de'
    PDelIfFirst('abcdef', s) > False and s= 'abcde'   
See also DelIfFirst, DelIfLast and TCharSet overloaded version
-------------------------------------------------------------------------------}
function TStringManager.PDelIfFirst(const SubStr: String; var s : String; IgnoreCase : Boolean = False) : Boolean;
begin
  Result := IsFirst(SubStr, s, IgnoreCase);
  if Result then System.Delete(s, 1, Length(SubStr));
end;

{-------------------------------------------------------------------------------
*PDelIfFirst - If the first character in s is part of the CharSet,
 the function will return true, and remove the character.
 Example:
  If s='1abc'
   then PDelIfFirst(['0'..'9'], s) > True and s= 'abc'
See also DelIfFirst, DelIfLast and TCharSet overloaded version
-------------------------------------------------------------------------------}
function TStringManager.PDelIfFirst(const IfInSet : TCharSet; var s : String) : Boolean;
begin
  if s=''
    then Result := False
    else Result := s[1] in IfInSet;
  if Result then System.Delete(s, 1, 1);
end;

{-------------------------------------------------------------------------------
*DelIfLast - Deletes the last character in s if it is in IfInSet
 Example: DelIfLast(['.'],'hello') -->'hello'
 Example: DelIfLast(['.'],'hello') -->'hello'
 Example: DelIfLast(['.','']) -->''
See also PurgeWhileOuter and overloaded string version
-------------------------------------------------------------------------------}
function TStringManager.DelIfLast(const IfInSet : TCharSet; const s : String) : String;
begin
  Result := s;
  if Result='' then Exit;
  if Result[Length(Result)] in IfInSet
    then SetLength(Result, Length(Result)-1);
end;

{-------------------------------------------------------------------------------
*DelIfFirst - Deletes the first character in s if it is in IfInSet
 Example: DelIfFirst(['.'],'hello') -->'hello'
 Example: DelIfFirst(['.'],'.hello') -->'hello'
See also PurgeWhileOuter and overloaded string version
--------------------------------------------------------------------------------}
function TStringManager.DelIfFirst(const IfInSet : TCharSet; const s : String) : String;
begin
  if s='' then Result := s else
  begin
    if s[1] in IfInSet
      then Result := System.Copy(s, 2, Length(s)) else Result := s;
  end;
end;

{-------------------------------------------------------------------------------
*Del - returns s with all occurences of SubStr removed.
 Example: Del('the','the moon is therefore the light') > ' moon is refore  light'
See also DelWord
--------------------------------------------------------------------------------}
function TStringManager.Del(const SubStr : String; const s : String): String;
var StartPos : Integer;
begin
  Result := s;
  StartPos := 1;
  PReplace(SubStr, '', Result, StartPos, [], []);
end;

{-------------------------------------------------------------------------------
*DelIC - returns s with all occurences of SubStr removed - Ignores case.
 Example: DelIC('THE','the moon is therefore the light') > ' moon is refore  light'
--------------------------------------------------------------------------------}
function TStringManager.DelIC(const SubStr : String; const s : String): String;
var StartPos : Integer;
begin
  Result := s;
  StartPos := 1;
  PReplace(SubStr, '', Result, StartPos, [roIgnoreCase], []);
end;

{-------------------------------------------------------------------------------
*DelWord - returns s with all wholeword occurences of SubStr removed.
 Example: DelWord('the','the moon is therefore the light') > ' moon is therefore  light'
 Example: DelWord('the','the moon is therefore the light', True) > 'moon is therefore light'
--------------------------------------------------------------------------------}
function TStringManager.DelWord(const SubStr : String; const s : String; RemoveDelim : Boolean = False; const Delims : TCharSet = gcWordDelims) : String;
var StartPos : Integer;
begin
  Result := s;
  StartPos := 1;
  if RemoveDelim
    then PReplace(SubStr, '', Result, StartPos, [roWholeWords, roRemoveDelim], Delims)
    else PReplace(SubStr, '', Result, StartPos, [roWholeWords], Delims);
end;

{-------------------------------------------------------------------------------
*DelWordIC - returns s with all wholeword occurences of SubStr removed - ignores case.
 Example: DelWordIC('the','The moon is therefore the light') > ' moon is therefore  light'
 Example: DelWordIC('the','The moon is therefore the light', True) > 'moon is therefore light'
--------------------------------------------------------------------------------}
function TStringManager.DelWordIC(const SubStr : String; const s : String; RemoveDelim : Boolean = False; const Delims : TCharSet = gcWordDelims): String;
var StartPos : Integer;
begin
  Result := s;
  StartPos := 1;
  if RemoveDelim
    then PReplace(SubStr, '', Result, StartPos, [roWholeWords, roIgnoreCase, roRemoveDelim], Delims)
    else PReplace(SubStr, '', Result, StartPos, [roWholeWords, roIgnoreCase], Delims);
end;

{-------------------------------------------------------------------------------
*Cut - Deletes the characters defined by Index and Count from s and returns the
 deleted portion.
 Example: Cut('hello',2,2 ) > 'el'   'hello' > 'hlo';
-------------------------------------------------------------------------------}
function TStringManager.Cut(var s : String; Index, Count : Integer) : String;
begin
  Result := System.Copy(s, Index, Count);
  Delete(s, Index, Count);
end;

{-------------------------------------------------------------------------------
*DelEnd - Simply returns the string with one or more characters removed from the end.
 Example:
   if s='hello' then DelEnd(s) > 'hell';
   if s='hello' then DelEnd(s,100) > '';
-------------------------------------------------------------------------------}
function TStringManager.DelEnd(const s : String; Count : Integer = 1) : String;
begin
  if Count>Length(s) then Count := Length(s);
  Result := System.Copy(s, 1, Length(s)-Count);
end;

{-------------------------------------------------------------------------------
*DelEndP - Simply reduces the length of s by 1 or more.
 Example:
   if s='hello' then DelEndP(s) > s='hell';
   if s='hello' then DelEndP(s,100) > s='';
-------------------------------------------------------------------------------}
procedure TStringManager.DelEndP(var s : String; Count : Integer = 1);
begin
  if Count>Length(s) then Count := Length(s);
  SetLength(s, Length(s)-Count);
end;

{-------------------------------------------------------------------------------
*IsLast - Returns true if SubStr is at the end of s.
 Example: IsLast('.txt','file.txt') > True
 Example: IsLast('.txt','file.txt ') > False
 Example: IsLast('','file.txt') > True
See also IsLastIC, IsFirst, DelIfLast
-------------------------------------------------------------------------------}
function TStringManager.IsLast(const SubStr : String; const s : String) : Boolean;
var   i : Integer;
 SubLen : Integer;
   Offs : Integer;
begin
  Result := False;
  SubLen := Length(SubStr);
  Offs  := Length(s);
  if SubLen>Offs then Exit;
  Dec(Offs, SubLen);
  for i := 1 to SubLen do
  begin
    if SubStr[i] <> s[i+Offs] then Exit;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsLastIC - Returns true if SubStr is at the end of s irrespective of case
 Example: IsLastIC('.txt','FILE.TXT') > True
See also IsLast, IsFirst, DelIfLast
-------------------------------------------------------------------------------}
function TStringManager.IsLastIC(const SubStr : String; const s : String) : Boolean;
var   i : Integer;
 SubLen : Integer;
   Offs : Integer;
begin
  Result := False;
  SubLen := Length(SubStr);
   Offs  := Length(s);
  if SubLen>Offs then Exit;
  Dec(Offs, SubLen);
  for i := 1 to SubLen do
  begin
    if gaANSIUpperArray[SubStr[i]] <> gaANSIUpperArray[s[i+Offs]] then Exit;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsLastIC - Returns true if SubStr is at the end of s, optionally ignoring case
See overloaded IsLast and IsLastIC
-------------------------------------------------------------------------------}
function TStringManager.IsLast(const SubStr : String; const s : String; IgnoreCase : Boolean) : Boolean;
var   i : Integer;
 SubLen : Integer;
   Offs : Integer;
begin
  Result := False;
  SubLen := Length(SubStr);
   Offs  := Length(s);
  if SubLen>Offs then Exit;
  Dec(Offs, SubLen);
  if IgnoreCase then
  begin
    for i := 1 to SubLen do
      if gaANSIUpperArray[SubStr[i]] <> gaANSIUpperArray[s[i+Offs]] then Exit;
  end else
  begin
    for i := 1 to SubLen do
      if SubStr[i] <> s[i+Offs] then Exit;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsFirst - Returns true if SubStr is at the start of s.
 Example: IsFirst('file','file.txt') > True
 Example: IsFirst('','file.txt') > True
See also IsFirstIC, IsSameFirst, DelIfFirst, IsLast
-------------------------------------------------------------------------------}
function TStringManager.IsFirst(const SubStr : String; const s : String) : Boolean;
var   i : Integer;
 SubLen : Integer;
begin
  Result := False;
  SubLen := Length(SubStr);
  if SubLen>Length(s) then Exit;
  for i := 1 to SubLen do
  begin
    if SubStr[i] <> s[i] then Exit;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsFirst - Returns true if SubStr is at the start of s, optionally ignoring case.
See overloaded IsFirst and IsFirstIC
-------------------------------------------------------------------------------}
function TStringManager.IsFirst(const SubStr : String; const s : String; IgnoreCase : Boolean) : Boolean;
var   i : Integer;
 SubLen : Integer;
begin
  Result := False;
  SubLen := Length(SubStr);
  if SubLen>Length(s) then Exit;
  if IgnoreCase then
  begin
    for i := 1 to SubLen do
      if gaANSIUpperArray[SubStr[i]] <> gaANSIUpperArray[s[i]] then Exit;
  end else
  begin
    for i := 1 to SubLen do
      if SubStr[i] <> s[i] then Exit;
  end;
  Result := True;
end;

{-------------------------------------------------------------------------------
*IsFirstIC - Returns true if SubStr is at the start of s irrespective of case.
 Example: IsFirstIC('file','FILE.TXT') > True
See also IsFirst, IsSameFirst, DelIfFirst, IsLast
-------------------------------------------------------------------------------}
function TStringManager.IsFirstIC(const SubStr : String; const s : String) : Boolean;
var   i : Integer;
 SubLen : Integer;
begin
  Result := False;
  SubLen := Length(SubStr);
  if SubLen>Length(s) then Exit;
  for i := 1 to SubLen do
    if gaANSIUpperArray[SubStr[i]] <> gaANSIUpperArray[s[i]] then Exit;
  Result := True;
end;

{-------------------------------------------------------------------------------
*EnsureFirst - Insert's SubStr at start of s if not already at the start.
 Example: EnsureFirst('file','file.txt') > 'file.txt';
 Example: EnsureFirst('file','.txt') > 'file.txt';
See also IsFirst, EnsureLast, EnsureEnclosed
-------------------------------------------------------------------------------}
function TStringManager.EnsureFirst(const SubStr : String; const s : String) : String;
begin
  if IsFirst(SubStr, s)
    then Result := s
    else Result := SubStr+s;
end;

{-------------------------------------------------------------------------------
*EnsureLast - Append's SubStr at the end of s if not already at the end.
 Example: EnsureLast('txt','file.txt') > 'file.txt';
 Example: EnsureLast('txt','file.') > 'file.txt';
See also IsLast, EnsureFirst, EnsureEnclosed
-------------------------------------------------------------------------------}
function TStringManager.EnsureLast(const SubStr : String; const s : String) : String;
begin
  if IsLast(SubStr, s)
    then Result := s
    else Result := s+SubStr;
end;

{-------------------------------------------------------------------------------
*DelEnclosed - Removes enclosing characters

If CanRemoveOnlyOne is false then: (default)
   Only the first and last character will be removed if they are identical,
   and found in IfInSet.
If CanRemoveOnlyOne is true then:
   Either one or both the first and last characters will be removed if they are
   in IfInSet.

If Repeatedly is True then the function will remove characters while they statisfy
the conditions.

Examples:
  DelEnclosed(['.'],'.a.') --> 'a'
  DelEnclosed(['.'],'.a,') --> '.a,'  (by default both enclosing must be identical)
  DelEnclosed(['.'],'..a..') --> '.a.' (by default removal is done once)
  DelEnclosed(['.'],'..a..', True) --> 'a' (specify repeatedly)

If both options are true - all starting and trailing characters in the set are removed.
  DelEnclosed([',','.',' '],'  .hello world, ', True, True --> 'hello world'
else by default only 2 enclosing spaces in the example above will be removed.

See also EnsureEnclosed, UnQuote, PurgeWhileOuter
--------------------------------------------------------------------------------}
function TStringManager.DelEnclosed(const IfInSet : TCharSet; const s : String; Repeatedly : Boolean = False; CanRemoveOnlyOne : Boolean = False) : String;
var SLen : Integer;
       i : Integer;
begin
  Slen := Length(s);
  if Slen=0 then
  begin
    Result :='';
    Exit;
  end;
  if Repeatedly then
  begin
    if CanRemoveOnlyOne then
    begin
      i := 1;
      while (i<=Slen) and (s[i] in IfInSet) do Inc(i);
      while (SLen>i) and (s[SLen] in IfInSet) do Dec(SLen);
      Result := System.Copy(s, i, (SLen-i)+1);
    end else
    begin
      if not (s[1] in IfInSet) then
      begin
        Result := s;
      end else
      begin
        i := 1;
        while (i<=Slen) and (s[i] = s[1]) do
        begin
          if i=SLen then Break;
          if not (s[Slen] = s[1]) then Break else
          begin
            Inc(i);
            Dec(SLen);
          end;
        end;
        Result := System.Copy(s, i, (SLen-i)+1);
      end;
    end;
  end else
  begin
    if CanRemoveOnlyOne then
    begin
      if (s[1] in IfInSet) then i := 2 else i := 1;
      if (SLen>1) and (s[SLen] in IfInSet) then Dec(SLen);
      Result := System.Copy(s, i, (SLen-i)+1);
    end else
    begin
      if Slen>1 then
      begin
        if (s[1] in IfInSet) and (s[SLen] = s[1])
          then Result := Copy(s, 2, SLen-2)
          else Result := s;
      end else Result := s;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*EnsureEnclosed
If CanAddOne is false then: (default)
   If the first and last characters are not both equal to c then
   c will be added to the start and end of the string.
If CanAddOne is true then:
   The character c will be added to the start or end of the string.

Examples:
  EnsureEnclosed('"','hello') --> '"hello"'
  EnsureEnclosed('"','"hello') --> '""hello"'
  EnsureEnclosed('"','"hello', True) --> '"hello"'
See also DelEnclosed, Quote, EnsureFirst
--------------------------------------------------------------------------------}
function TStringManager.EnsureEnclosed(c : char; const s : String; CanAddOne : Boolean = False) : String;
var SLen : Integer;
begin
  SLen := Length(s);
  if Slen=0 then
  begin
    Result := c+s+c;
    Exit;
  end;
  if SLen=1 then
  begin
    if CanAddOne then
    begin
      if s[1] = c
        then Result := s+c
        else Result := c+s+c;
    end else Result := c+s+c;
  end else
  begin
    if (s[1] = c) and (s[SLen] = c) then
    begin
      Result := s;
      Exit;
    end else if CanAddOne then
    begin
      if s[1] = c then
      begin
        Result := s+c;
        Exit;
      end else if s[Slen] = c then
      begin
        Result := c+s;
        Exit;
      end;
    end;
    Result := c+s+c;
  end;
end;

{-------------------------------------------------------------------------------
*Copy - Same as the standard Copy function except Count is by default 2GB.
 Example: Copy('ABCDEF', 2) > 'BCDEF'
 Example: Copy('ABCDEF', 2, 3) > 'BCD'

See also First, Last
-------------------------------------------------------------------------------}
function TStringManager.Copy(const s : String; Index : Integer; Count : Integer = $7FFFFFFF) : String;
begin
  Result := System.Copy(s, Index, Count);
end;

{-------------------------------------------------------------------------------
*Ins - function version of standard insert procedure.
 Example: Ins('a','1234',2) > '1a234'
 Example: Ins('a','1234',200) > '1234a'
-------------------------------------------------------------------------------}
function TStringManager.Ins(const SubStr : String; const s : String; Index : Integer = 1) : String;
begin
  Result := s;
  System.Insert(SubStr, Result, Index);
end;

{-------------------------------------------------------------------------------
*IsShortString - Returns true if string parameter is of ShortString type.
 If declaration; var s1 : String[20], s2 : String then:
 IsShortString([s1]) > True
 IsShortString([s2]) > False
-------------------------------------------------------------------------------}
function TStringManager.IsShortString(s : array of const) : Boolean;
begin
  with s[0] do Result := vtype=vtstring; //long is vtAnsiString
end;

{-------------------------------------------------------------------------------
*ToI - String to Integer
 No error checking - Strips all non Integer characters first.
 Example: ToI(' 5') > 5
See also ToL, ToI64, Val
-------------------------------------------------------------------------------}
function TStringManager.ToI(const s : String) : Integer;
var e : Integer;
begin
  System.Val(GetType(s, [cfInt]), Result, e);
end;

{-------------------------------------------------------------------------------
*ToL - String to Cardinal/Longword - no error checking
 Strips all non Integer characters.
 Example: ToL('4294967295') >  4294967295  (unsigned 32 bit max Value)
See also ToI, ToI64, Val
-------------------------------------------------------------------------------}
function TStringManager.ToL(const s : String) : Cardinal;
var e : Integer;
begin
  System.Val(GetType(s, [cfInt]), Result, e);
end;

{-------------------------------------------------------------------------------
*ToI64 - String to 64bit Integer, removes all non Integer characters first.
 Example: ToI64('4294967295') >  4294967295  (unsigned 32 bit max Value)
 Example: ToI64('hello4294967295') >  4294967295
See also ToI, Val
-------------------------------------------------------------------------------}
function TStringManager.ToI64(const s : String) : Int64;
begin
  Result := StrToInt64(GetType(s, [cfInt]));
end;

{-------------------------------------------------------------------------------
*ToC - String to Currency, removes all non numeric characters first.
 Example: ToC(' 5.5') > 5.5
See also procedure Val
-------------------------------------------------------------------------------}
function TStringManager.ToC(const s : String) : TCurrency;
begin
  try
    Result.c := StrToCurr(GetType(s, [cfNumeric]));
  except
    Result.c := 0.0;
  end;
end;

{-------------------------------------------------------------------------------
*ToF - String to Float - No error checking, removes all non float characters first.
 Example: ToF(' 5.125') > 5.125
See also procedure Val
-------------------------------------------------------------------------------}
function TStringManager.ToF(const s : String) : Extended;
begin
  try
    if s=''
      then Result := 0.0
      else Result := StrToFloat(GetType(s, [cfFloat]));
  except
    Result := 0.0;
  end;
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Integer);
begin
  Value := ToI(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Cardinal);
begin
  Value := ToI(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Word);
begin
  Value := ToI(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Int64);
begin
  Value := ToI64(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : SmallInt);
begin
  Value := ToI(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Byte);
begin
  Value := ToI(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Double);
begin
  Value := ToF(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Extended);
begin
  Value := ToF(s);
end;

{-------------------------------------------------------------------------------
*Val - String to numeric - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : TCurrency);
begin
  Value := ToC(s);
end;

{-------------------------------------------------------------------------------
*Val - String to TCharSet - overloaded procedure
See also CharSetToVisualStr
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : TCharSet; FromVisualSet : Boolean = False);
begin
  Value := ToSet(s, FromVisualSet);
end;

{-------------------------------------------------------------------------------
*Val - String to Boolean - overloaded procedure
-------------------------------------------------------------------------------}
procedure TStringManager.Val(const s : String; var Value : Boolean);
begin
  Value := ToBool(s);
end;

{-------------------------------------------------------------------------------
*ToSL - String to TStringList - Just an easy way to create a string list.
-------------------------------------------------------------------------------}
function TStringManager.ToSL(const s : String = '') : TStringList;
begin
  Result := TStringList.Create;
  if s<>'' then Result.Text := s;
end;

{-------------------------------------------------------------------------------
*Center - Centers text in a string of length Len.
 Example: Center('ok',6) > '  ok  '
-------------------------------------------------------------------------------}
function TStringManager.Center(const s: String; Len: Integer; PadChar : Char = ' '): String;
begin
  if Length(s) < Len then
  begin
    Result := RepeatChar(PadChar, (Len div 2) - (Length(s) div 2))+s;
    Result := Result + RepeatChar(PadChar, Len - Length(Result));
  end else Result := s;
end;


{-------------------------------------------------------------------------------
*HasWord - Returns true if a word is found in a string.
 Example: HasWord('close','open close print') > True
 Example: HasWord('close','openclose print') > False
See also HasWordIC, WordPos
-------------------------------------------------------------------------------}
function TStringManager.HasWord(const Word, s : String; const Delims : TCharSet = gcWordDelims) : Boolean;
begin
  Result := WordPos(Word, s, Delims)>0;
end;

{-------------------------------------------------------------------------------
*HasWordIC - Returns true if a word is found in a string irrespective of case
 Example: HasWordIC('Close','open close print') > True
 Example: HasWordIC('Close','openclose print') > False
See also HasWord, WordPos
-------------------------------------------------------------------------------}
function TStringManager.HasWordIC(const Word, s : String; const Delims : TCharSet = gcWordDelims) : Boolean;
begin
  Result := WordPosIC(Word, s, Delims)>0;
end;

{-------------------------------------------------------------------------------
*HasChar - Returns true if c in s.
 Example: HasChar('a','jazz') > True
See also HasCharIC and set version
-------------------------------------------------------------------------------}
function TStringManager.HasChar(c: Char; const s: String) : Boolean;
begin
  Result := Pos(c, s)>0;
end;

{-------------------------------------------------------------------------------
*HasCharIC - Returns true if c in s irrespective of case.
 Example: HasCharIC('A','jazz') > True
See also HasChar
-------------------------------------------------------------------------------}
function TStringManager.HasCharIC(c: Char; const s: String): Boolean;
begin
  Result := Pos(gaANSIUpperArray[c], UpCase(s))>0;
end;

{-------------------------------------------------------------------------------
*Before
 This function returns the string to the left of the first character found in aSet.
 Example: Before(['e','l'],'Those Funny People') > 'Thos'
 Example: Before(['z'],'Those Funny People') > 'Those Funny People'
See also overload String version, After
-------------------------------------------------------------------------------}
function TStringManager.Before(const aSet : TCharSet; const s : String) : String;
var i : Integer;
begin
  for i := 1 to Length(s) do
  begin
    if s[i] in aSet then
    begin
      Result := System.Copy(s, 1, i-1);
      Exit;
    end;
  end;
  Result := s;
end;

{-------------------------------------------------------------------------------
*BeforeRev
 This function returns the string to the left of the first character found in aSet
 when scanning from right to left.
 Example: BeforeRev(['e','l'],'Those Funny People') > 'Those Funny Peopl'
 Example: BeforeRev(['z'],'Those Funny People') > 'Those Funny People
See also overload String version
-------------------------------------------------------------------------------}
function TStringManager.BeforeRev(const aSet : TCharSet; const s : String) : String;
var i : Integer;
begin
  for i := Length(s) downto 1 do
  begin
    if s[i] in aSet then
    begin
      Result := System.Copy(s, 1, i-1);
      Exit;
    end;
  end;
  Result := s;
end;

{-------------------------------------------------------------------------------
*After
 This function returns the string to the right of the first character found in aSet.
 Example: After(['e','l'],'Those Funny People') > ' Funny People'
 Example: After(['z'],'Those Funny People') > ''
See also AfterRev, Before, and overloaded String versions
-------------------------------------------------------------------------------}
function TStringManager.After(const aSet : TCharSet; const s : String) : String;
var i : Integer;
begin
  for i := 1 to Length(s) do
  begin
    if s[i] in aSet then
    begin
      Result := System.Copy(s, i+1, Length(s));
      Exit;
    end;
  end;
  Result := '';
end;

{-------------------------------------------------------------------------------
*AfterRev
 This function returns the string to the right of the first character found in aSet
 when scanning right to left.
 Example: AfterRev(['e','l'],'Those Funny People') > ''
 Example: AfterRev(['l'],'Those Funny People') > 'e'
 Example: AfterRev(['z'],'Those Funny People') > ''
See also After, Before and overload String versions
-------------------------------------------------------------------------------}
function TStringManager.AfterRev(const aSet : TCharSet; const s : String) : String;
var i : Integer;
begin
  for i := Length(s) downto 1 do
  begin
    if s[i] in aSet then
    begin
      Result := System.Copy(s, i+1, Length(s));
      Exit;
    end;
  end;
  Result := '';
end;

{-------------------------------------------------------------------------------
*ToSet - String to TCharSet
 Example: ToSet('CBA') > ['A'..'C']
 Example: ToSet('a..z', true) > ['a'..'z']
See also overloaded Str TCharSet version
-------------------------------------------------------------------------------}
function TStringManager.ToSet(const s : String; FromVisualSet : Boolean = False) : TCharSet;
var i : Integer;
begin
  if FromVisualSet then Result := VisualStrToCharSet(s) else
  begin
    Result := [];
    for i := 1 to Length(s) do Result := Result+[s[i]];
  end;
end;

{-------------------------------------------------------------------------------
*ToBool - String to Boolean.
 Example: ToBool('true') > True
 Example: ToBool('y') > True
See also overloaded Str TCharSet version
-------------------------------------------------------------------------------}
function TStringManager.ToBool(const s : String) : Boolean;
begin
  if s=''
    then Result := False
    else Result := s[1] in gcBooleanTrueSet;
end;

{-------------------------------------------------------------------------------
*FormatSize - Integer to string with thousand separator.
 Example: FormatSize(23000) > '23,000'
-------------------------------------------------------------------------------}
function TStringManager.FormatSize(Value : Int64) : String;
var i : Integer;
    p : Integer;
begin
  p := 0;
  i := 3;
  Result := IntToStr(Abs(Value));
  while i + p < Length(Result) do
  begin
    Insert(SysUtils.ThousandSeparator, Result, Length(Result) - (i + p)+ 1);
    Inc(p);
    Inc(i, 3);
  end;
  if Value<0 then Result := '-'+Result;
end;

{-------------------------------------------------------------------------------
*SlashAdd - add's a slash (typically a file/path name) at the end if needed
 Example: SlashAdd('c:\windows') > 'c:\windows\'
 Example: SlashAdd('c:\windows\') > 'c:\windows\'
See also SlashDel
-------------------------------------------------------------------------------}
function TStringManager.SlashAdd(const s : String) : String;
begin
  if (Length(s) > 0) and (s[Length(s)] <> '\')
    then Result := s+'\'
    else Result := s;
end;

{-------------------------------------------------------------------------------
*SlashDel- reMoves a slash (typically a file/path name) if at end.
 Example: SlashDel('c:\windows') > 'c:\windows'
 Example: SlashDel('c:\windows\') > 'c:\windows'
See also SlashAdd
-------------------------------------------------------------------------------}
function TStringManager.SlashDel(const s : String) : String;
begin
  if (Length(s) > 0) and (s[Length(s)] = '\')
    then Result := System.Copy(s, 1, Pred(Length(s)))
    else Result := s;
end;

{-------------------------------------------------------------------------------
*Quote - Use Quote to convert the string s to a Quote string.
A single Quote character (') is inserted at the beginning and end of s,
and each single quote character in the string is repeated.
Functionally the same as SysUtils.QuotedStr, but twice as fast.
Example:
  Quote('hello') > #39+'hello'+#39
  Quote('hello "jack"','"') > '"hello ""jack"""'
See also UnQuote, QuoteIfUnQuoted, EnsureEnclosed
-------------------------------------------------------------------------------}
function TStringManager.Quote(const s : String; aQuoteChar : Char = '''') : String;
var SLen : Integer;
    n : Integer;
    i : Integer;
begin
  Result := '';
  SLen := Length(s);

  //calculate the result length
  n := SLen +2;
  for i := 1 to SLen do
  begin
    if s[i] = aQuoteChar then Inc(n);
  end;
  SetLength(Result, n);

  n := 1;
  Result[1] := aQuoteChar;
  for i := 1 to SLen do
  begin
    if s[i] = aQuoteChar then
    begin
      Inc(n);
      Result[n] := aQuoteChar;
    end;
    Inc(n);
    Result[n] := s[i];
  end;
  Result[n+1] := aQuoteChar;
end;

{-------------------------------------------------------------------------------
*QuoteIfUnQuoted - This function only convert the string s to a Quote string,
if the first and last character is not the aQuoteChar (already quoted).
Example:
  QuoteIfUnQuoted('hello "jack"','"')     > '"hello ""jack"""'
  QuoteIfUnQuoted('"hello ""jack"""','"') > '"hello ""jack"""'
See also Quote, UnQuote
-------------------------------------------------------------------------------}
function TStringManager.QuoteIfUnQuoted(const s : String; aQuoteChar : Char = '''') : String;
var SLen : Integer;
begin
  SLen := Length(s);
  if SLen=0 then
  begin
    Result := s;
  end else
  begin
    if (s[1] = aQuoteChar) and (s[SLen] = aQuoteChar)
      then Result := Quote(s, aQuoteChar)
      else Result := s;
  end;
end;

{-------------------------------------------------------------------------------
*UnQuote - This function removes quotes around the string and any double
 quotes inside.
 If the string does not start and end with the aQuoteChar (already quoted),
 then the string is returned unchanged.
Example:
  UnQuote('"hello ""jack"""','"') > 'hello "jack"'
  UnQuote('hello "jack"','"') > 'hello "jack"'
See also Quote, QuoteIfUnQuoted, DelEnclosed
-------------------------------------------------------------------------------}
function TStringManager.UnQuote(const s : string; aQuoteChar : Char = '''') : String;
var SLen : Integer;
    n : Integer;
    i : Integer;
begin
  Result := '';
  SLen := Length(s);
  if SLen<1 then Exit;
  if (s[1] <> aQuoteChar) or (s[SLen] <> aQuoteChar) then
  begin
    Result := s;
    Exit;
  end;
  Dec(SLen);

  //calculate the result length
  n := 0;
  i := 2;
  while i<=SLen do
  begin
    if s[i] = aQuoteChar then
    begin
      Inc(i);
      if i>SLen then Break;
    end;
    Inc(i);
    Inc(n);
  end;

  if n=0 then Exit;
  SetLength(Result, n);

  //return valid chars
  i := 2;
  n := 0;
  while i<=SLen do
  begin
    if s[i] = aQuoteChar then
    begin
      Inc(i);
      if i>SLen then Break;
    end;
    Inc(n);
    Result[n] := s[i];
    Inc(i);
  end;
end;

{-------------------------------------------------------------------------------
*Hash - returns a hash code for the string.
Given the same string the function will always return the same "hash" value.
The hash value is however not guaranteed to be unique.
Example:
  Hash('hello') > 7258927
-------------------------------------------------------------------------------}
function TStringManager.Hash(const s : String) : Integer;
var v, g, i : Integer;
begin
  v := 0;
  for i := 1 to Length(s) do
  begin
    v := v shl 4;
    Inc(v, Ord(s[i]));
    g := v and ($f shl 28);
    if g<>0 then
    begin
      v := v xor (g shr 24);
      v := v xor g;
    end;
  end;
  if v=0
   then Result := -1
   else Result := v;
end;


{-------------------------------------------------------------------------------
*HashUnique - returns a hash code for the string that is guaranteed  to be
unique provided all the characters in s are in ValidChars and the total
possible combinations are less than the max value of the Integer.
If ValidChars are A..Z the the string can only be upto 6 characters.
Example:
  HashUnique('HELLO') > 7258927
-------------------------------------------------------------------------------}
function TStringManager.HashUnique(const s : String; const ValidChars : TCharSet = ['A'..'Z']) : Cardinal;
var x : Int64;
begin
  x := BaseRange(ValidChars, Length(s));
  if x>$FFFFFFFF then DoRaise('String too long', 'HashUnique');
  Result := BaseVal(s, ValidChars);
end;

{-------------------------------------------------------------------------------
*Parse - Extracts words, characters between delimiters.
The function is ideal to extract delimited words in a string.
It basically returns text before "DelimStr" and set's s to after DelimStr.
By default, the function will not return an empty string if the delimiter is at the start
of the string - it will look for the next delimter.
Concept by Reinder de Jager

 Example:
    s := '-a-b-c';
    while s<>'' do
    begin
      param := Parse('-', s);
      memo1.Lines.Add(param); //do what you want with param
    end;
    Memo Result: lines 'a','b','c'

  Example2:
    s := 'get-the-words';
    param := Parse('-', s);
    param > 'get'
        s > 'the-words'

    Note - if s was '-get-the-words' you will have the same result.

See also the overloaded Set-version and Before, After
-------------------------------------------------------------------------------}
function TStringManager.Parse(const DelimStr : String; var s : String; IgnoreDelimAtStart : Boolean = True): String;
var p : Integer;
begin
  p := Pos(DelimStr, s);
  if p=1 then if IgnoreDelimAtStart then
  begin
    s := System.Copy(s, p+Length(DelimStr), Length(s));
    p := Pos(DelimStr, s);
  end;
  if p=0 then
  begin
    Result := s;
    s := '';
    Exit;
  end;
  Result := System.Copy(s, 1, p-1);
  s := System.Copy(s, p+Length(DelimStr), Length(s));
end;

{-------------------------------------------------------------------------------
*Parse - Extracts words, characters between delimiters.
 See notes at string version.

 Example:
    s := 'please,extract words;between delims.';
    while s<>'' do
    begin
      param := Parse([';',' ','.',','], s);
      memo1.Lines.Add(param); //do what you want with param
    end;
See also overloaded String version and Before, After, ExtractWords
-------------------------------------------------------------------------------}
function TStringManager.Parse(const aSet: TCharSet; var s: String; IgnoreDelimAtStart : Boolean = True): String;
var p : Integer;
begin
  p := Pos(aSet, s);
  if p=1 then if IgnoreDelimAtStart then
  begin
    s := System.Copy(s, p+1, Length(s));
    p := Pos(aSet, s);
  end;
  if p=0 then
  begin
    Result := s;
    s := '';
    Exit;
  end;
  Result := System.Copy(s, 1, p-1);
  s := System.Copy(s, p+1, Length(s));
end;

{-------------------------------------------------------------------------------
*Parse - Extracts text between delimiters and add it to a StringList.
Note that the StringList must be created beforehand, and that the function
appends to any existing lines.
 Example:
   Parse('-','-a-b-c', Memo1.Lines) will add 3 lines to the Memo.
See also the overloaded String result version and ExtractWords.
-------------------------------------------------------------------------------}
procedure TStringManager.Parse(const DelimStr : String; s : String; const aList : TStrings; MopAlso : Boolean = True; IgnoreDelimAtStart : Boolean = True);
begin
  if aList=nil then DoRaise('aList=nil','Parse');
  while s<>'' do
  begin
    if MopAlso
      then aList.Add(Mop(Parse(DelimStr, s, IgnoreDelimAtStart)))
      else aList.Add(Parse(DelimStr, s, IgnoreDelimAtStart));
  end;
end;

{-------------------------------------------------------------------------------
*WordCase - By default UpCases the first letter of every word in a sentence.
 Example:
   s := 'CASING IS PROBLEMATIC. ON SUNDAY I CODE IN PASCAL.';
   WordCase(s) >  'Casing Is Problematic. On Sunday I Code In Pascal.'
   WordCase(s,['.'],[' ',',']) >  'Casing is problematic. On sunday i code in pascal.'
   Note that WordDelims is used to locate the start of a sentence and is normally ['.']

   The logic looks something like this:
   While not end of text do
   begin
     If IsFirstWordInSentence then UpCaseFirstLetter else
     begin
       If assigned(AskUpCaseWordFunc) then
       begin
         If AskUpCaseWordFunc(CurrentWord) then UpCaseFirstLetter
       end else UpCaseFirstLetter
     end
     FindNextWord
   end
See also ProperCase, BasicUpCaseWords
-------------------------------------------------------------------------------}
function TStringManager.WordCase(const s : String; const UpCaseDelims : TCharSet = [' ',',',';'];
  const WordDelims : TCharSet = []; AskUpCaseWordFunc : TAskUpCaseWord = nil): String;
var
  i : Integer;
  n : Integer;
  WasWordDelim : Integer;
   procedure CheckUpCase;
   begin
     if AskUpCaseWordFunc(System.Copy(Result, WasWordDelim, i-WasWordDelim))
      then Result[WasWordDelim] := Self.UpCase(Result[WasWordDelim]);
   end;
begin
  Result := Locase(s);
  WasWordDelim := 0;
  i := 1;
  n := Length(Result);
  if Assigned(AskUpCaseWordFunc) then
  begin
    while i <= n do
    begin
      while (i <= n) and (Result[i] in UpCaseDelims+WordDelims) do Inc(i);
      if i <= n then Result[i] := Self.UpCase(Result[i]);
      WasWordDelim := 0;
      while (i <= n) and not (Result[i] in UpCaseDelims) do
      begin
        if Result[i] in WordDelims then
        begin
          if WasWordDelim>0 then CheckUpCase;
          while (i <= n) and (Result[i] in WordDelims) do Inc(i);
          WasWordDelim := i;
        end;
        Inc(i);
      end;
      if WasWordDelim>0 then CheckUpCase;
    end;
  end else
  begin
    while i <= n do
    begin
      while (i <= n) and (Result[i] in UpCaseDelims+WordDelims) do Inc(i);
      if i <= n then Result[i] := Self.UpCase(Result[i]);
      while (i <= n) and not (Result[i] in UpCaseDelims) do Inc(i);
    end;
  end;
end;

{-------------------------------------------------------------------------------
*ProperCase - By default it UpCase the first letter of the first word in every
              sentence, and words that function BasicUpCaseWords returns true.
 Example:
   s := 'CASING IS PROBLEMATIC. ON SUNDAY I CODE IN PASCAL.';
   ProperCase(s) > 'Casing is problematic. On Sunday I code in pascal.'
 Note that you can pass an alternative AskUpCaseWordFunc function.
See also WordCase, BasicUpCaseWords
-------------------------------------------------------------------------------}
function TStringManager.ProperCase(const s : String; AskUpCaseWordFunc : TAskUpCaseWord = nil; UseBasicFuncIfNil : Boolean = True) : String;
begin
  if (UseBasicFuncIfNil) and (not assigned(AskUpCaseWordFunc))
    then Result := WordCase(s, ['.'], [' ',',',';'], BasicUpCaseWords)
    else Result := WordCase(s, ['.'], [' ',',',';'], AskUpCaseWordFunc);
end;

{-------------------------------------------------------------------------------
*CharSetToVisualStr - Converts a CharSet to a string in a visual presentation form.

NOTE that only chars in gcCharSetVisualRange will be represented as literal values.
Because special characters like control characters, space and other extended
characters are not easy to identify in a string, or depends visually on the
System locale they are presended in their Byte value with a hash prefix.
Therefore the escape character will be #27.

 The result is similar to inspecting a set of chars in the integrated debugger.
 Example: CharSetToVisualStr(['A'..'Z',#13,#12,#11,#9], True) > '#9,#11..#13, A..Z'
 Example: CharSetToVisualStr(['A'..'D', False) > 'A,B,C,D'
See also VisualStrToCharSet and overloaded Str function
-------------------------------------------------------------------------------}
function TStringManager.CharSetToVisualStr(const aSet : TCharSet; DoRange : Boolean = True) : String;
var
    i : Char;
 f, l : SmallInt;

     function GetS(b : Byte) : String;
     begin
       if Char(b) in gcCharSetVisualRange
       then Result := Char(b)
       else Result := '#'+IntToStr(b);
     end;

     procedure Add;
     begin
       if f=-1 then Exit;
       if f=l then
       begin
         if Result=''
         then Result := Gets(f)
         else Result := Result+','+Gets(f);
       end else
       begin
         if Result=''
         then Result := Gets(f)+'..'+GetS(l)
         else Result := Result+','+Gets(f)+'..'+GetS(l);
       end;
     end;
begin
  Result := '';
  f := -1;
  for i := #0 to #255 do
  begin
    if DoRange then
    begin
      if i in aSet then
      begin
        if f=-1 then f := Byte(i);
        l := Byte(i);
      end else
      begin
        Add;
        f := -1;
      end;
    end else
    begin
      if i in aSet then
      begin
        f := Byte(i); l := f;
        Add;
        f := -1;
      end;
    end;
  end;
  Add;
end;

{-------------------------------------------------------------------------------
VisualStrToCharSet - Converts a string in visual CharSet form back to a CharSet
Note that the characters in the string do not have single quotes around them.

Example:
VisualStrToCharSet('A..Z, a..z') >  ['A'..'Z','a'..'z']
VisualStrToCharSet('A..Z,1,2,3,4') >  ['1'..'4','A'..'Z']

if aSet1:=['A'..'D','2'..'9',' ',#1] then:
  s := CharSetToVisualStr(aSet, True) >  '#1,#32,2..9, A..D'
  aSet2 := VisualStrToCharSet(s) > aSet2 should equal aSet1

See also CharSetToVisualStr
-------------------------------------------------------------------------------}
function TStringManager.VisualStrToCharSet(s : String) : TCharSet;
var t1, t2 : String;

    function getc(t : String) : Char;
    begin
      Result := #0;
      if t<>'' then
      begin
        if t[1] = '#'
         then Result := Char(ToI(Copy(t, 2, Length(t))))
         else Result := t[1];
      end;
    end;

    procedure doit;
     var a, b : Char;
           x : Char;
    begin
      a := getc(t1);
      b := getc(t2);
      if b=#0 then Result := Result+[a] else for x := a to b do Result := Result+[x];
    end;
begin
  Result := [];
  while s<>'' do
  begin
    t1 := Mop(Before(',', s));
    if t1<>'' then
    begin
      t2 := Mop(After('..', t1));
      t1 := Mop(Before('..', t1));
      doit;
    end;
    s := After(',', s);
  end;
end;

{-------------------------------------------------------------------------------
*StrVar - Returns characters from an UnTyped variable as a string.
Source is typically an array of Char or memory address.
Note that a Count of -1 means that the function will return all characters until
a terminator is found, and therefore Terminators will be set to [#0] if an empty set.
This is not intended as an asciiz function.


Example:
If Source is an array with the values 'h','e','l','l','o',#0 then:
 StrVar(Source, SizeOf(Source)) > 'hello'+#0 //terminator an empty set
 StrVar(Source) > 'hello' //scans until first Terminator (default is #0)
 StrVar(Source, SizeOf(Source),[#0]) > 'hello'
 StrVar(Source, SizeOf(Source),['e']) > 'h'

If you have a PChar which points to 'hello' then:
 StrVar(p^,-1) > 'hello'
In Delphi 3+ you would actually just state s := p. Conversely Delphi allows p := s;

See also ToVar, System.Move
-------------------------------------------------------------------------------}
function TStringManager.StrVar(var Source; Count : Integer = -1; Terminators : TCharSet = []) : String;
var
  p : PChar;
  i : Word;
begin
  p := @Source;
  Result := '';
  if Count=0 then Exit;
  if Count = -1 then
  begin
    Count := System.MaxInt-4;
    if Terminators = [] then Terminators := [#0];
  end;
  if Terminators = [] then
  begin
    SetLength(Result, Count);
    System.Move(p^, Result[1], Count);
  end else
  begin
    i := 1;
    while (i <= Count) and (not (p^ in Terminators)) do
    begin
      Result := Result + p^;
      Inc(p);
    end;
  end;
end;

{-------------------------------------------------------------------------------
*ToVar - Moves characters from a Source string to an UnTyped variable, typically
an array of Char.
Note that the function does not create a NullTerminated array by default, EnsureAsciiZ has
to be true to ensure a #0 at the end.

If target is declared as array[0..5] then:
 ToVar('hello', target, SizeOf(target)) > 'h','e','l','l','o',#0

If target is declared as array[0..4] then:
 ToVar('hello', target, SizeOf(target)) > 'h','e','l','l','o'
 ToVar('hello', target, SizeOf(target), True) > 'h','e','l','l',#0

See also StrVar, System.Move
asciiz
-------------------------------------------------------------------------------}
procedure TStringManager.ToVar(const Source : String; var Target; TargetSize : Integer; EnsureAsciiZ : Boolean = False; ClearData : Boolean = True);
var p : PChar;
    i : Integer;
  Upto : Integer;
begin
  p := @Target;
  if TargetSize <= 0 then Exit;
  if ClearData then System.FillChar(Target, TargetSize, #0);
  if EnsureAsciiZ then
  begin
    if Length(Source)+1>TargetSize
      then Upto := TargetSize-1
      else Upto := Length(Source);
  end else
  begin
    if Length(Source)>TargetSize
      then Upto := TargetSize
      else Upto := Length(Source);
  end;
  for i := 1 to Upto do
  begin
    p^ := Source[i];
    Inc(p);
  end;
  if Upto<TargetSize then if EnsureAsciiZ then p^ := #0;
end;

{-------------------------------------------------------------------------------
*MakePChar - allocates a copy of s on the heap and returns a null terminated string pointer.
Note that eventually the string needs to be free'd from the heap with FreePChar or SysUtils.StrDispose
See also FreePChar
-------------------------------------------------------------------------------}
function TStringManager.MakePChar(const s: String): PChar;
begin
  Result := SysUtils.StrNew(PChar(s));
end;

{-------------------------------------------------------------------------------
*FreePChar - free PChar string from the heap.
See also MakePChar
-------------------------------------------------------------------------------}
procedure TStringManager.FreePChar(var p : PChar);
begin
  SysUtils.StrDispose(p);
  p := nil;
end;

{-------------------------------------------------------------------------------
*Str - Returns a formatted string assembled from a format string and an
array of arguments.
  Simply a call to SysUtils.Format
See also FormatSize and other Str overloaded functions
-------------------------------------------------------------------------------}
function TStringManager.Str(const aFormat: String; const Args: array of const) : String;
begin
  try
    Result := SysUtils.Format(aFormat, Args);
  except
    DoRaise(aFormat, 'Format'); //sometimes exception if Args missing
  end;
end;

{-------------------------------------------------------------------------------
*IIF - "Immediate If"  - returns a string depending on a condition.
Example: if amount=89 then IIF(amount<100,'Cheap','Expensive') > 'Cheap'
-------------------------------------------------------------------------------}
function TStringManager.IIF(const Condition : Boolean; const sTrue : String; const sFalse : String = ''): String;
begin
  if Condition then Result := sTrue else Result := sFalse;
end;

{-------------------------------------------------------------------------------
*IIF - "Immediate If"  - returns a CharSet depending on a condition.
Example: if DoUpper=True then IIF(DoUpper,['A'..'Z'],['a'..'z']) >'A'..'Z'
-------------------------------------------------------------------------------}
function TStringManager.IIF(const Condition : Boolean; const csTrue : TCharSet; const csFalse : TCharSet = []): TCharSet;
begin
  if Condition then Result := csTrue else Result := csFalse;
end;

{-------------------------------------------------------------------------------
*StrHeader - returns the negative offset AnsiString control header.
Explanation:
  r = RefCount int
  a = Allocated bytes int
  s = actual string characters
 Format:  rrrr|aaaa|sssss....
Example:  0001 0005 Hello#0
See also MakeUnique, RefCount
-------------------------------------------------------------------------------}
function TStringManager.StrHeader(const s : AnsiString) : PAnsiStrHeader;
begin
  Result := Pointer(s);
  if Result<>nil then Result := Pointer(Integer(Result)-8);
end;

{-------------------------------------------------------------------------------
*MakeUnique
From Borland Help file:
Following a call to SetString, S is guaranteed to reference a unique string;
that is, a string with a reference count of one.
If  s2:=s1 then s2 and s2 will occupy the exact same memory location.
Normally used an application casts a string to a PChar which needs to modify
a copy.
For normal string handling, there is no need to ever call UniqueString.
See also RefCount, AsUnique
-------------------------------------------------------------------------------}
procedure TStringManager.MakeUnique(var s : String);
begin
  System.UniqueString(s);
end;

{-------------------------------------------------------------------------------
*AsUnique - functional version of MakeUnique
Example s2:=AsUnique(s1)
Do not use "s2:=AsUnique(s2)" use MakeUnique(s2) instead.
See also RefCount, MakeUnique
-------------------------------------------------------------------------------}
function TStringManager.AsUnique(const s : String) : String;
begin
  Result := s;
  System.UniqueString(Result);
end;


{-------------------------------------------------------------------------------
*RefCount - returns the reference count of an AnsiString.
When a string is assigned for the first time the reference count will be 1.
Setting s1:=s2 will increase the count to 2.  (both strings use the same memory)
A call s2:='changed' will decrease s1's refcount and s2 will be a new string.
A value of -1 is used for constants.
See also MakeUnique
-------------------------------------------------------------------------------}
function TStringManager.RefCount(const s : String) : Integer;
begin
  if StrHeader(s)=nil
    then Result := 0
    else Result := StrHeader(s).RefCount;
end;

{-------------------------------------------------------------------------------
*NTLen - Null Terminated length
Returns number of characters in a string excluding the null terminator.
Note that a Delphi AnsiString as a special length integer at a negative offset.
If a #0 character is manually inserted into an AnsiString the Length function
will not be aware of this.
  Example:
    s := 'Hello';
    s[3] := #0;
     Length(s) > 5
     NTLen(s) > 2
See also EnsureNTLen, StrHeader
-------------------------------------------------------------------------------}
function TStringManager.NTLen(const s : AnsiString) : Integer;
begin
  Result := NTLen(Pointer(s));
end;

{-------------------------------------------------------------------------------
*NTLen - Null Terminated length - same as SysUtils.StrLen but a bit faster.
Returns number of characters in a string excluding the null terminator.
Uses a simple "Unrolling loop" technique as explained by Robert Lee.
-------------------------------------------------------------------------------}
function TStringManager.NTLen(const pc : PChar) : Integer;
begin
  Result := 0;
  if pc=nil then Exit;
  while pc[Result] <> #0 do
  begin
    if pc[Result+1] = #0 then begin Inc(Result);    Exit; end;
    if pc[Result+2] = #0 then begin Inc(Result, 2); Exit; end;
    if pc[Result+3] = #0 then begin Inc(Result, 3); Exit; end;
    if pc[Result+4] = #0 then begin Inc(Result, 4); Exit; end;
    Inc(Result, 5);
  end;
end;

{-------------------------------------------------------------------------------
*EnsureNTLen - Checks if the length of the string corresponds with the
null terminated length. If not it will correct the length.
  Example:
    s := 'Hello';
    s[3] := #0;
     Length(s) > 5
     EnsureLen(s) > 2
     Length(s) >2
See also EnsureNTLen
-------------------------------------------------------------------------------}
function TStringManager.EnsureNTLen(var s : AnsiString) : Integer;
begin
  Result := NTLen(s);
  if Result<>Length(s) then SetLength(s, Result);
end;

{-------------------------------------------------------------------------------
*Encrypt - Standard encryption routine (Borland).
Note that the encrypted result may contain control characters, which could
cause problems with Ansi strings, text files and database fields.
See Encrypt224 for an alternative.
-------------------------------------------------------------------------------}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
procedure TStringManager.Encrypt(const Keys : TOpenByteArray; var aText; const aTextLen : Integer);
const
  C1 = 52845;
  C2 = 22719;
type aTextArray = array[1..MaxInt] of Char;

var  i : Integer;
    ps : ^aTextArray;
  aKey : Word;
 nKeys : Byte;
begin
  if aTextLen=0 then Exit;
  ps := @aText;
  nKeys := High(Keys)+1;
  if nKeys=0 then DoRaise('Missing Key', 'Encrypt');
  aKey := Integer((Keys[0]+nKeys+aTextLen) shl 8)+aTextLen;
  for i := 1 to aTextLen do
  begin
    ps^[i] := Char(Byte(ps^[i]) xor (aKey shr 8));
    aKey := (Byte(ps^[i]) + aKey + Keys[i mod nKeys]) * C1+C2;
  end;
end;

{-------------------------------------------------------------------------------
*Decrypt - Decrypt's text encrypted with the Encrypt procedure.
Pass the same Key parameters as when the string was encrypted.
See the Encrypt procedure for Notes.
-------------------------------------------------------------------------------}
procedure TStringManager.Decrypt(const Keys : TOpenByteArray; var aText; const aTextLen : Integer);
const
  C1 = 52845;
  C2 = 22719;
type aTextArray = array[1..MaxInt] of Char;

var  i : Integer;
    ps : ^aTextArray;
     c : Char;
  aKey : Word;
 nKeys : Byte;
begin
  if aTextLen=0 then Exit;
  ps := @aText;
  nKeys := High(Keys)+1;
  if nKeys=0 then DoRaise('Missing Key', 'Decrypt');
  aKey := Integer((Keys[0]+nKeys+aTextLen) shl 8)+aTextLen;

  for i := 1 to aTextLen do
  begin
    c := ps^[i];
    ps^[i] := Char(Byte(ps^[i]) xor (aKey shr 8));
    aKey := (Byte(c) + aKey + Keys[i mod nKeys]) * C1+C2;
  end;
end;
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}


{-------------------------------------------------------------------------------
*Encrypt - String version of the Encrypt procedure.
Note that the encrypted result may contain control characters, which could
cause problems with database fields.
See Encrypt224 for an alternative.
-------------------------------------------------------------------------------}
function TStringManager.Encrypt(const s : ShortString; const aKey : ShortString) : ShortString;
var Keys : TOpenByteArray;
       i : Integer;
begin
  Result := s;
  if Result='' then Exit;
  SetLength(Keys, Length(aKey));
  for i := 1 to Length(aKey) do Keys[i-1] := Byte(aKey[i]);
  Encrypt(Keys, Result[1], Length(Result));
  Keys := nil;
end;

{-------------------------------------------------------------------------------
*Decrypt - Decrypt's text encrypted with the Encrypt function
Pass the same Key as when the string was encrypted.
See the Encrypt function for Notes.
-------------------------------------------------------------------------------}
function TStringManager.Decrypt(const s : ShortString; const aKey : ShortString) : ShortString;
var Keys : TOpenByteArray;
    i : Integer;
begin
  Result := s;
  if Result='' then Exit;  
  SetLength(Keys, Length(aKey));
  for i := 1 to Length(aKey) do Keys[i-1] := Byte(aKey[i]);
  Decrypt(Keys, Result[1], Length(Result));
  Keys := nil;
end;

{-------------------------------------------------------------------------------
*Encrypt224 - Encrypts only the first 224 characters in the EncryptTable.
If the table has these values:
  [#32,#33,#34....#255,  #0,#1,#2...#31]
  then characters #32..#255 in the text will be encrypted and #0..#31 will
  be saved as is.
  In other words the last 32 characters in the table will always encrypt
  to ordinal values 0 to 31 irrespective of any xor (Key) value.

The Encryption Table must be filled with all 256 characters, but can be
in any order.
The encrypted result will always be the same length as the original text.

A typical use would be to encrypt a text file while preserving the control
characters, or a database field where control characters will cause problems.

See the overloaded Encrypt224 function for a simple example.


MODIFIED XOR RESULT TABLE: (64 xor 32 > 160, 64 xor 33 > 161 *)
Key          0     32     64     96    128    160    192    224
---------------------------------------------------------------
  0- 31     32     96    160    224     32     96    160    224
 32- 63     64     64    192    192     64     64    192    192
 64- 95     96    160*    96    160     96    160     96    160
 96-127    128    128    128    128    128    128    128    128
128-159    160    224     32     96    160    224     32     96
160-191    192    192     64     64    192    192     64     64
192-223    224     32    224     32    224     32    224     32
224-255      0      0      0      0      0      0      0      0
-------------------------------------------------------------------------------}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
procedure TStringManager.Encrypt224(const Keys : TOpenByteArray; var aText; const aTextLen : Integer; const EncryptTable : TCharByteArray);
var  Inverse : TCharByteArray;
   procedure SetInverse;
   var i : Char;
   begin
     for i := #0 to #255 do Inverse[EncryptTable[i]] := i;
   end;
var
     i : Integer;
     b : Byte;
aBound : Byte;
  aKey : Integer;
 nKeys : Byte;
 xKey  : Byte;
    ps : ^T2GBCharArray;
     c : Char;
begin
  if aTextLen=0 then Exit;
  ps := @aText;
  SetInverse;
  nKeys := High(Keys)+1;
  if nKeys=0 then raise Exception.Create('Missing Key in Encrypt224');
  aKey := Integer((Keys[0]+nKeys+aTextLen) shl 8)+aTextLen;
  for i := 1 to aTextLen do
  begin
    c := ps^[i];
    if Byte(Inverse[ps^[i]])<224 then
    begin
      //encode:
      xKey := Byte(aKey shr 8);
      aBound := (xKey div 32)*32;
      b := Byte(Inverse[c]);
      b := b xor xKey;
      b := Byte(b+abound+32); //shift xor - avoid 0..31 for first 224 chars in array
      ps^[i] := Char(b);
      //change Key:
    end else
    begin
      b := Byte(Inverse[ps^[i]]);
      b := b +32;
      ps^[i] := Char(b);
    end;
    aKey := Integer((i + Byte(c) + aKey + Keys[i mod nKeys])*52845);
  end;
end;

{-------------------------------------------------------------------------------
*Decrypt224 - Decrypt's text encrypted with the Encrypt224 procedure.
Pass the same Key and EncryptTable parameters as when the string was encrypted.
See Encrypt224 procedure for notes.
-------------------------------------------------------------------------------}
procedure TStringManager.Decrypt224(const Keys : TOpenByteArray; var aText; const aTextLen : Integer; const EncryptTable : TCharByteArray);
var
     i : Integer;
     b : Byte;
aBound : Byte;
  aKey : Integer;
 nKeys : Byte;
 xKey  : Byte;
    ps : ^T2GBCharArray;
begin
  if aTextLen=0 then Exit;
  ps := @aText;
  nKeys := High(Keys)+1;
  if nKeys=0 then raise Exception.Create('Missing Key in Decrypt224');
  aKey := Integer((Keys[0]+nKeys+aTextLen) shl 8)+aTextLen;
  for i := 1 to aTextLen do
  begin
    if Byte(ps^[i])>31 then
    begin
      xKey := Byte(aKey shr 8);
      aBound := (xKey div 32)*32;
      //encode:
      b := Byte(ps^[i]);
      b := Byte(b-(abound+32));
      b := b xor xKey;
      ps^[i] := EncryptTable[Char(b)];
    end else
    begin
      b := Byte(ps^[i]);
      b := b-32;
      ps^[i] := EncryptTable[Char(b)];
    end;
    aKey := Integer((i + Byte(ps^[i]) + aKey + Keys[i mod nKeys])*52845);
  end;
end;
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

{-------------------------------------------------------------------------------
*Encrypt224 - Simple 224 character encryption routine.
Pass a string with a Key (like a password) and the function will return an
encrypted string.
Note that by default this function does not encrypt any control characters.
This enable you to save an encrypted text file to disk, without the file being
truncated because of #0 or #26 control characters.

Example:
  s := 'hello world'
  s := Encrypt224(s,'password')
  s := Decrypt224(s,'password')
See the overloaded Encrypt224 procedure for more details.
-------------------------------------------------------------------------------}
function TStringManager.Encrypt224(const s : String; const aKey : ShortString; const EncryptSet : TCharSet = [#32..#255]) : String;
var Keys : TOpenByteArray;
       i : Integer;
begin
  Result := s;
  if Result='' then Exit;
  SetLength(Keys, Length(aKey));
  for i := 1 to Length(aKey) do Keys[i-1] := Byte(aKey[i]);
  Encrypt224(Keys, Result[1], Length(Result), ToCharArray(EncryptSet, True));
  Keys := nil;
end;

{-------------------------------------------------------------------------------
*Decrypt224 - Decrypt's text encrypted with the Encrypt224 function.
Pass the same Key and EncryptSet parameters as when the string was encrypted.
See Encrypt224 for Notes.
-------------------------------------------------------------------------------}
function TStringManager.Decrypt224(const s : String; const aKey : ShortString; const EncryptSet : TCharSet = [#32..#255]) : String;
var Keys : TOpenByteArray;
       i : Integer;
begin
  Result := s;
  if Result='' then Exit;  
  SetLength(Keys, Length(aKey));
  for i := 1 to Length(aKey) do Keys[i-1] := Byte(aKey[i]);
  Decrypt224(Keys, Result[1], Length(Result), ToCharArray(EncryptSet, True));
  Keys := nil;
end;

{-------------------------------------------------------------------------------
*ToCharArray - Simply fills the array with characters from aCharSet.
Example:
  ToCharArray(['a'..'d','A']) > ['A','a','b','c','d',#0,#0,#0...]
  ToCharArray(['a'..'d','A'], True) > ['A','a','b','c','d',#0,#1,#2...]
-------------------------------------------------------------------------------}
function TStringManager.ToCharArray(const aCharSet : TCharSet; AppendExcludedChars : Boolean = False) : TCharByteArray;
var c : Char;
    i : Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  i := -1;
  for c := #0 to #255 do
  begin
    if c in aCharSet then
    begin
      Inc(i);
      Result[Char(i)] := c;
    end;
  end;
  if AppendExcludedChars then for c := #0 to #255 do
  begin
    if not (c in aCharSet) then
    begin
      Inc(i);
      Result[Char(i)] := c;
    end;
  end;
end;

{-------------------------------------------------------------------------------
*FileWrite - Writes aText to a Text file.
-------------------------------------------------------------------------------}
procedure TStringManager.FileWrite(const aFileName : string; const aText : string);
var f : TFileStream;
begin
  f := TFileStream.Create(aFileName, fmCreate);
  try
    if aText<>'' then f.Write(aText[1], Length(aText));
  finally
    f.Free;
  end;
end;

{-------------------------------------------------------------------------------
*FileRead - Returns a Text file as a string.
-------------------------------------------------------------------------------}
function TStringManager.FileRead(const aFileName : string; MaxLen : Integer = MaxInt) : String;
var f : TFileStream;
begin
  f := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    if f.Size>MaxLen
      then SetLength(Result, MaxLen)
      else SetLength(Result, f.Size);
    f.Read(Result[1], Length(Result));
  finally
    f.Free;
  end;
end;

{-------------------------------------------------------------------------------
*StreamWrite - Writes aText to a steam
-------------------------------------------------------------------------------}
procedure TStringManager.StreamWrite(const aStream : TStream; const aText : string);
var aLen : Integer;
begin
  aLen := Length(aText);
  aStream.WriteBuffer(aLen, SizeOf(aLen));
  aStream.WriteBuffer(aText[1], aLen);
end;

{-------------------------------------------------------------------------------
*StreamWrite - Reads aText from a steam (as written with StreamWrite)
-------------------------------------------------------------------------------}
function TStringManager.StreamRead(const aStream : TStream) : String;
var aLen : Integer;
begin
  Result := '';
  aStream.ReadBuffer(aLen, SizeOf(aLen));
  if aLen<1
    then Exit
    else SetLength(Result, aLen);
  aStream.ReadBuffer(Result[1], aLen);
end;

{-------------------------------------------------------------------------------
*FileReadEnd - Reads a Text file as a string. If the FileSize > MaxLen, then
the function returns the last MaxLen characters.
-------------------------------------------------------------------------------}
function TStringManager.FileReadEnd(const aFileName : string; MaxLen : Integer = MaxInt) : String;
var f : TFileStream;
begin
  f := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    if MaxLen>=f.Size then
    begin
      SetLength(Result, f.Size);
      f.Read(Result[1], Length(Result));
    end else
    begin
      f.Seek(-MaxLen, soFromEnd);
      SetLength(Result, MaxLen);
      f.Read(Result[1], Length(Result));
    end;
  finally
    f.Free;
  end;
end;

{-------------------------------------------------------------------------------
*FileAppend - Writes aText to the end of a text file.
-------------------------------------------------------------------------------}
procedure TStringManager.FileAppend(const aFileName : string; const aText : string; EnsureOnNewLine : Boolean = False);
var f : TFileStream;

    function GetChar : char;
    begin
      f.Read(Result, SizeOf(Result));
    end;

    procedure PutStr(const s  : String);
    begin
      f.Write(s[1], Length(s));
    end;
begin
  if not FileExists(aFileName) then
  begin
    FileWrite(aFileName, aText);
    Exit;
  end;
  if aText='' then Exit;
  f := TFileStream.Create(aFileName, fmOpenReadWrite);
  try
    if (EnsureOnNewLine) and (f.Size>0) then
    begin
      f.Seek(-1, soFromEnd);
      if (GetChar in [#13, #10])
        then f.Seek(0, soFromEnd)
        else PutStr(#13+#10);
    end else f.Seek(0, soFromEnd);
    PutStr(aText);
  finally
    f.Free;
  end;
end;


{-------------------------------------------------------------------------------
*PosBetween - Scans s for the start/end combination of SubStr1, SubStr2 and
 returns the StartPos and EndPos of the text between them.
 If a combination is found StartPos/Result will be the first character
 after SubStr1, and EndPos the character before SubStr2.
 If not found then StartPos will be unaffected and EndPos/Result will be -1
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.

 Example:
   PosBetween('(',')','call (proc(s))') > 7,13  "proc(s)"
   PosBetween('(',')','call (proc(s)') > 1,-1   missing bracket

See function Between for more examples.
See also PosBetweenRev, Between, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.PosBetween(const SubStr1, SubStr2 : String; const s : String; var StartPos, EndPos : Integer; IgnoreCase : Boolean = False) : Integer;
var    i,p1 : Integer;
          c : Integer;
  Identical : Boolean;
       SLen : Integer;
    SubLen1 : Integer;
    SubLen2 : Integer;
  Sub1, Sub2 : PString;

   function IPos(ps : PString; SubLen : Integer) : Boolean;
   var  j : Integer;
       si : Integer;
   begin
     Result := False;
     si := i;
     if IgnoreCase
      then begin if gaANSIUpperArray[s[si]] <> gaANSIUpperArray[ps^[1]] then Exit; end
      else begin if s[si] <> ps^[1] then Exit; end;
     if SubLen>1 then
     begin
       if si+SubLen > SLen+1 then
       begin
         Exit;
       end;
       if IgnoreCase then
       begin
         for j := 2 to SubLen do
         begin
           Inc(si);
           if gaANSIUpperArray[ps^[j]] <> gaANSIUpperArray[s[si]] then
           begin
             i := si-1;
             Exit;
           end;
         end;
       end else
       begin
         for j := 2 to SubLen do
         begin
           Inc(si);
           if ps^[j] <> s[si] then
           begin
             i := si-1;
             Exit;
           end;
         end;
       end;
     end;
     Result := True;
   end;

begin
  EndPos := -1;
  Result := -1;
  Slen := Length(s);
  SubLen1:=Length(SubStr1);
  if SubLen1=0 then Exit;
  if SubLen1>SLen then Exit;
  SubLen2:=Length(SubStr2);
  Identical := SubLen2=0;
  EndPos := -1;
  Sub1:=@SubStr1;
  if Identical then
  begin
    SubLen2:=SubLen1;
    Sub2:=@SubStr1;
  end else Sub2:=@SubStr2;
  if StartPos<1
    then i := 1
    else i := StartPos;
  c  := 0;
  p1 := 0;
  while i<=SLen do
  begin
    if IPos(Sub2, SubLen2) then
    begin
      if c=0 then
      begin
        if IPos(Sub1, SubLen1) then
        begin
          p1 := i;
          Inc(c);
        end else Break;
      end else
      begin
        Dec(c);
        if c=0 then
        begin
          StartPos := p1 + SubLen1;
          Result := StartPos;
          EndPos := i-1;
          Break;
        end;
      end;
    end else if IPos(Sub1, SubLen1) then
    begin
      if c=0 then p1 := i;
      Inc(c);
    end;
    Inc(i);
  end;
end;

{-------------------------------------------------------------------------------
*PosBetweenRev - Scans s backwards for the start/end combination of SubStr1, SubStr2 and
 returns the StartPos and EndPos of the text between them.
 If a combination is found StartPos/Result will be the first character
 after SubStr1, and EndPos the character before SubStr2.
 If not found then StartPos will be unaffected and EndPos/Result will be -1
 If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.

 Example:
   PosBetweenRev('(',')','x(a) (proc(s))') > 7,13  "proc(s)"
   PosBetweenRev('(',')','x(a) (proc(s)') > 1,-1   missing bracket

See function BetweenRev for more examples.
See also PosBetween, Between, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.PosBetweenRev(const SubStr1, SubStr2 : String; const s : String; var StartPos, EndPos : Integer; IgnoreCase : Boolean = False) : Integer;
var    i,p1 : Integer;
          c : Integer;
  Identical : Boolean;
       SLen : Integer;
    SubLen1 : Integer;
    SubLen2 : Integer;
  Sub1, Sub2 : PString;

   function IPos(ps : PString; SubLen : Integer) : Boolean;
   var  j : Integer;
       si : Integer;
   begin
     Result := False;
     si := i;
     if IgnoreCase
       then begin if gaANSIUpperArray[s[si]] <> gaANSIUpperArray[ps^[1]] then Exit; end
       else begin if s[si] <> ps^[1] then Exit; end;
     if SubLen>1 then
     begin
       if si+SubLen > SLen+1 then
       begin
         Exit;
       end;
       if IgnoreCase then
       begin
         for j := 2 to SubLen do
         begin
           Inc(si);
           if gaANSIUpperArray[ps^[j]] <> gaANSIUpperArray[s[si]] then
           begin
             i := si-1;
             Exit;
           end;
         end;
       end else
       begin
         for j := 2 to SubLen do
         begin
           Inc(si);
           if ps^[j] <> s[si] then
           begin
             i := si-1;
             Exit;
           end;
         end;
       end;
     end;
     Result := True;
   end;

begin
  Result := -1;
  EndPos := -1;
  Slen := Length(s);
  SubLen1 := Length(SubStr1);
  if SubLen1=0 then Exit;
  if SubLen1>SLen then Exit;
  SubLen2 := Length(SubStr2);
  Identical := SubLen2=0;
  Sub1 := @SubStr1;
  if Identical then
  begin
    SubLen2 := SubLen1;
    Sub2 := @SubStr1;
  end else Sub2 := @SubStr2;
  if StartPos<1
    then i := (SLen-SubLen2)+1
    else i := StartPos;
  c := 0;
  p1 := 0;
  while i>0 do
  begin
    if IPos(Sub1, SubLen1) then
    begin
      if c=0 then
      begin
        if IPos(Sub2, SubLen2) then
        begin
          p1 := i;
          Inc(c);
        end else Break;
      end else
      begin
        Dec(c);
        if c=0 then
        begin
          StartPos := i+1;
          Result := StartPos;
          EndPos := p1-1;
          Break;
        end;
      end;
    end else if IPos(Sub2, SubLen2) then
    begin
      if c=0 then p1 := i;
      Inc(c);
    end;
    Dec(i);
  end;
end;

{-------------------------------------------------------------------------------
*PosBetween - TCharSet - Scans s for the start/end combination of Delims and
 returns the StartPos and EndPos of the text between them.
 If a combination is found StartPos/Result will be the first character
 after Delim1, and EndPos the character before Delim2.
 If not found then StartPos will be unaffected and EndPos/Result will be -1
 If Delim2 is empty Delim2 will be regarded as identical to Delim1.

 Example:
   PosBetween('(',')','call (proc(s))') > 7,13  "proc(s)"
   PosBetween('(',')','call (proc(s)') > 1,-1   missing bracket

See function Between for more examples.
See also PosBetweenRev, Between, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.PosBetween(Delim1, Delim2 : Char; const s : String; var StartPos, EndPos : Integer) : Integer;
var   i,p1 : Integer;
      SLen : Integer;
         c : Integer;
begin
  Result := -1;
  EndPos := -1;
  if Delim1=#0 then Exit;
  if Delim2=#0 then Delim2 := Delim1;
  Slen := Length(s);
  if StartPos<1
  then i := 1
  else i := StartPos;
  c := 0;
  p1 := 0;
  while i<=SLen do
  begin
    if s[i] = Delim2 then
    begin
      if c=0 then
      begin
        if s[i] = Delim1 then
        begin
          p1 := i;
          Inc(c);
        end else Break;
      end else
      begin
        Dec(c);
        if c=0 then
        begin
          StartPos := p1 + 1;
          Result := StartPos;
          EndPos := i-1;
          Break;
        end;
      end;
    end else if s[i] = Delim1 then
    begin
      if c=0 then p1 := i;
      Inc(c);
    end;
    Inc(i);
  end;
end;

{-------------------------------------------------------------------------------
*PosBetweenRev - TCharSet - Scans s backwards for the start/end combination of
 Delims and returns the StartPos and EndPos of the text between them.
 If a combination is found StartPos/Result will be the first character
 after Delim1, and EndPos the character before Delim2.
 If not found then StartPos will be unaffected and EndPos/Result will be -1
 If Delim2 is empty Delim2 will be regarded as identical to Delim1.

 Example:
   PosBetweenRev('(',')','x(a) (proc(s))') > 7,13  "proc(s)"
   PosBetweenRev('(',')','x(a) (proc(s)') > 1,-1   missing bracket

See function BetweenRev for more examples.
See also PosBetween, Between, Mid, Before, After
-------------------------------------------------------------------------------}
function TStringManager.PosBetweenRev(Delim1, Delim2 : Char; const s : String; var StartPos, EndPos : Integer) : Integer;
var  i,p1 : Integer;
     SLen : Integer;
        c : Integer;
begin
  Result := -1;
  EndPos := -1;
  if Delim1=#0 then Exit;
  if Delim2=#0 then Delim2 := Delim1;

  Slen := Length(s);
  if StartPos<=0
    then i := SLen
    else i := StartPos;
  c := 0;
  p1 := 0;
  while i>0 do
  begin
    if s[i] = Delim1 then
    begin
      if c=0 then
      begin
        if s[i] = Delim2 then
        begin
          p1 := i;
          Inc(c);
        end else Break;
      end else
      begin
        Dec(c);
        if c=0 then
        begin
          StartPos := i+1;
          Result := StartPos;
          EndPos := p1-1;
          Break;
        end;
      end;
    end else if s[i] = Delim2 then
    begin
      if c=0 then p1 := i;
      Inc(c);
    end;
    Dec(i);
  end;
end;

{-------------------------------------------------------------------------------
Simple example function used by ProperCase to return which words should be uppercased.
-------------------------------------------------------------------------------}
function BasicUpCaseWords(const s : String) : Boolean;
begin
  Result := True;
  if s='i' then Exit;
  if s='c' then Exit;
  if s='pascal' then Exit;
  if s='delphi' then Exit;
  if s='monday' then Exit;
  if s='tuesday' then Exit;
  if s='wednesday' then Exit;
  if s='thursday' then Exit;
  if s='friday' then Exit;
  if s='saturday' then Exit;
  if s='sunday' then Exit;
  Result := False;
end;

procedure TStringManager.DoRaise(const aMsg : String; aFunc : String = '');
begin
  {$IFDEF StrManEnableERaise}
    eRaise(aMsg, aFunc, Self);
  {$ELSE}
    raise Exception.Create(aMsg+#13+'TStringManager.'+aFunc);
  {$ENDIF}
end;


{//////////////////////////////////////////////////////////////////////////////}
{  OBJECT TStrBufPtr - Buffered concatenation.                                 }
{//////////////////////////////////////////////////////////////////////////////}

{TStrBufPtr Notes------------------------------------------------------------
If you append a string repeatedly with a syntax like "Result := Result+s[i]",
then memory is re-allocated with every concatenation which can be a
severe performance penalty.
This is especially true with the memory manager in Delphi5.
Use this object to append chars to a string, which requests memory in chunks
of Delta bytes.
In the DelSpace example below, the second function will be about 4 times faster
if the result is around 10 characters (Delphi5) and 8 times around 30 characters.
The amount of memory objects in your application will also effect this.
On large strings with a larger delta the difference will be huge.

  SLOW REPEATED MEMORY ALLOCATION:
    function DelSpace(const s : string) : string;
    var i : Integer;
    begin
      Result := '';
      for i :=1 to length(s) do if s[i] <> ' ' then Result := Result + s[i];
    end;

  FAST TStrBufPtr VERSION:
    function DelSpace(const s : string) : string;
    var i : Integer;
        r : TStrBufPtr;
    begin
      r.Init(Result);
      for i :=1 to Length(s) do if s[i] <> ' ' then r.Add(s[i]);
      r.Truncate;
    end;
-------------------------------------------------------------------------------}
procedure TStrBufPtr.Init(var AnsiStr : AnsiString);
begin
  AnsiStr := '';
  Fp := @AnsiStr;
  FCharLen := 0;
end;

function TStrBufPtr.GetCharPtr : PChar;
begin
  Result := Pointer(Fp^);
end;

{See notes above---------------------------------------------------------------}
procedure TStrBufPtr.Truncate;
begin
  SetLength(Fp^, FCharLen);
end;

{See notes above---------------------------------------------------------------}
function TStrBufPtr.GetMemLen : Integer;
begin
  Result := Length(Fp^);
end;

{See notes above---------------------------------------------------------------
If Value<Length(s) then it leaves memory for next concatenation
-------------------------------------------------------------------------------}
procedure TStrBufPtr.SetCharLen(Value : Integer);
begin
  FCharLen := Value;
  if FCharLen<0 then FCharLen := 0;
  SetLength(Fp^, FCharLen);
end;

procedure TStrBufPtr.SetMemLen(Value : Integer);
begin
  SetLength(Fp^, Value);
  FCharLen := Value;
end;

{See notes above---------------------------------------------------------------
If Value<Length(s) then it leaves memory for next  concatenation
-------------------------------------------------------------------------------}
procedure TStrBufPtr.DecLen(Count : Integer = 1);
begin
  SetCharLen(FCharLen-Count);
end;

{See notes above---------------------------------------------------------------
Just sets FCharLen to 0
-------------------------------------------------------------------------------}
procedure TStrBufPtr.Clear;
begin
  FCharLen := 0;
end;

{See notes above---------------------------------------------------------------}
procedure TStrBufPtr.Add(Value : Char; Delta : Integer = 8);
var Allocated : Integer;
begin
  if Fp=nil then raise Exception.Create('TStrBufPtr not initialzed');
  Allocated := Length(Fp^);
  Inc(FCharLen);
  if FCharLen>Allocated then
  begin
    if Delta<1 then Delta := 8;
    while FCharLen>Allocated do Inc(Allocated, Delta);
    SetLength(Fp^, Allocated);
  end;
  Fp^[FCharLen] := Value;
  if FCharLen<Allocated then Fp^[FCharLen+1] := #0;
end;

{See notes above---------------------------------------------------------------}
procedure TStrBufPtr.Add(const Value : String; Delta : Integer = 8);
var Allocated : Integer;
         SLen : Integer;
         TPos : Integer;
begin
  if Fp=nil then raise Exception.Create('TStrBufPtr not initialzed');
  SLen := Length(Value);
  if SLen=0 then Exit;
  Allocated := Length(Fp^);
  TPos := FCharLen+1;
  Inc(FCharLen, SLen);
  if FCharLen>Allocated then
  begin
    if Delta<1 then Delta := 8;
    while FCharLen>Allocated do Inc(Allocated, Delta);
    SetLength(Fp^, Allocated);
  end;
  System.Move(Value[1], Fp^[TPos], Slen);
  if FCharLen<Allocated then Fp^[FCharLen+1] := #0;
end;

function TStrBufPtr.LastChar : Char;
begin
  if FCharLen=0
    then Result := #0
    else Result := Fp^[FCharLen];
end;

function TStrBufPtr.FirstChar : Char;
begin
  if FCharLen=0
    then Result := #0
    else Result := Fp^[1];
end;

{True if no characters above #32                                               }
function TStrBufPtr.IsEmpty : Boolean;
var i : Integer;
begin
  Result := False;
  for i := 1 to FCharLen do if Fp^[i]>' ' then Exit;
  Result := True;
end;


{//////////////////////////////////////////////////////////////////////////////}
{  OBJECT TStrBuf                                                              }
{//////////////////////////////////////////////////////////////////////////////}
procedure TStrBuf.Init;
begin
  Fs := '';
  FCharLen := 0;
  FFixedLength := 0;
end;

procedure TStrBuf.Init(aFixedLength : Integer);
begin
  Fs := '';
  FCharLen := 0;
  FFixedLength := aFixedLength;
end;

procedure TStrBuf.Add(Value : Char; Delta : Integer = 8);
var Allocated : Integer;
begin
  Allocated := Length(Fs);
  if FFixedLength>0 then
  begin
    if Allocated = 0 then
    begin
      SetLength(Fs, FFixedLength);
      Allocated := FFixedLength;
    end;
    if FCharLen+1>Allocated then Exit;
    Inc(FCharLen);
  end else
  begin
    Inc(FCharLen);
    if FCharLen>Allocated then
    begin
      if Delta<1 then Delta := 8;
      while FCharLen>Allocated do Inc(Allocated, Delta);
      SetLength(Fs, Allocated);
    end;
  end;
  Fs[FCharLen] := Value;
  if FCharLen<Allocated then Fs[FCharLen+1] := #0;
end;

procedure TStrBuf.Add(const Value : String; Delta : Integer = 8);
var Allocated : Integer;
         SLen : Integer;
         TPos : Integer;
begin
  SLen := Length(Value);
  if SLen=0 then Exit;
  Allocated := Length(Fs);
  TPos := FCharLen+1;
  if FFixedLength>0 then
  begin
    if Allocated = 0 then
    begin
      SetLength(Fs, FFixedLength);
      Allocated := FFixedLength;
    end;

    if FCharLen+1>Allocated then Exit;
    if FCharLen+Slen>Allocated then
    begin
      Slen := Allocated - FCharLen;
      FCharLen := Allocated;
    end else Inc(FCharLen, SLen);
  end else
  begin
    Inc(FCharLen, SLen);
    if FCharLen>Allocated then
    begin
      if Delta<1 then Delta := 8;
      while FCharLen>Allocated do Inc(Allocated, Delta);
      SetLength(Fs, Allocated);
    end;
  end;
  System.Move(Value[1], Fs[TPos], Slen);
  if FCharLen<Allocated then Fs[FCharLen+1] := #0;
end;

function TStrBuf.GetCharPtr : PChar;
begin
  Result := nil;
  if @Fs<>nil
    then if Fs<>''
      then Result := Pointer(@Fs[1]);
end;

function TStrBuf.GetStrPtr : PString;
begin
  Result := @Fs;
end;

procedure TStrBuf.Clear;
begin
  FCharLen := 0;
  if FFixedLength=0 then Fs := '';
end;

procedure TStrBuf.Free;
begin
  FCharLen := 0;
  Fs := '';
end;

procedure TStrBuf.Truncate;
begin
  if FFixedLength>0 then
  begin
    if FCharLen<Length(Fs) then Fs[FCharLen+1] := #0;
  end else
  begin
    if FCharLen=Length(Fs) then Exit;
    SetLength(Fs, FCharLen);
  end;
end;

function TStrBuf.GetMemLen : Integer;
begin
  Result := Length(Fs);
end;

procedure TStrBuf.SetMemLen(Value : Integer);
begin
  if FFixedLength>0 then Exit;
  SetLength(Fs, Value);
  FCharLen := Value;
end;

procedure TStrBuf.SetCharLen(Value : Integer);
var Allocated : Integer;
begin
  if Value<0 then Value := 0;
  if FFixedLength=0 then
  begin
    FCharLen := Value;
    SetLength(Fs, FCharLen);
  end else
  begin
    Allocated := Length(Fs);
    if Allocated = 0 then
    begin
      SetLength(Fs, FFixedLength);
      Allocated := FFixedLength;
    end;
    if Value > Allocated then raise Exception.Create('Value>FixedLength - TStrBuf.SetCharLen');
    FCharLen := Value;
    if FCharLen<Allocated then Fs[FCharLen+1] := #0;
  end;
end;

procedure TStrBuf.DecLen(Count : Integer = 1);
begin
  SetCharLen(FCharLen-Count);
end;


function TStrBuf.LastChar : Char;
begin
  if FCharLen=0
    then Result := #0
    else Result := Fs[FCharLen];
end;

function TStrBuf.FirstChar : Char;
begin
  if FCharLen=0
    then Result := #0
    else Result := Fs[1];
end;

function TStrBuf.GetCharLenStr : String;
begin
  SetLength(Result, FCharLen);
  if FCharLen=0 then Exit;
  System.Move(Fs[1], Result[1], FCharLen);
end;

procedure TStrBuf.SetCharLenStr(const Value : String);
begin
  FCharLen := 0;
  Add(Value);
end;

procedure TStrBuf.UpCase;
var i : Integer;
begin
  for i := 1 to FCharLen do Fs[i] := gaANSIUpperArray[Fs[i]];
end;

procedure TStrBuf.LoCase;
var i : Integer;
begin
  for i := 1 to FCharLen do Fs[i] := gaANSILowerArray[Fs[i]];
end;

procedure TStrBuf.Trim(DoUpcase : Boolean = False);
var i, l: Integer;
begin
  l := FCharLen;
  i := 1;
  while (i <= l) and (Fs[i] <= ' ') do Inc(i);
  if i > l then FCharLen := 0 else
  begin
    while Fs[l] <= ' ' do System.Dec(l);
    if i=1
     then FCharLen := l else
     begin
       System.Move(Fs[i], Fs[1], l - i + 1);
       FCharLen := l - i + 1;
     end;
  end;
  Truncate;
  if DoUpcase then for i := 1 to FCharlen do Fs[i] := gaANSIUpperArray[Fs[i]];
end;

procedure TStrBuf.TrimRight(DoUpcase : Boolean = False);
var i : Integer;
begin
  while (FCharLen>0) and (Fs[FCharLen] <=  ' ') do System.Dec(FCharLen);
  Truncate;
  if DoUpcase then for i := 1 to FCharlen do Fs[i] := gaANSIUpperArray[Fs[i]];
end;

procedure TStrBuf.Mop(DoUpcase : Boolean = False);
var   i : Integer;
  aStop : Integer;
begin
  aStop := FCharLen;
  while (aStop>0) and (Fs[aStop] <= ' ') do System.Dec(aStop);
  FCharLen := aStop;
  if aStop=0 then Exit;
  i := 1;
  while (i<=aStop) and (Fs[i] <= ' ') do Inc(i);
  if i>1 then
  begin
    FCharLen := (aStop+1)-i;
    System.Move(Fs[i], Fs[1], FCharLen);
    i := 1;
  end;
  while i < FCharLen do
  begin
    if Fs[i] in [#1..#32] then Fs[i] := ' ';
    if (Fs[i] = ' ') and (Fs[i+1] = ' ') then
    begin
      System.Move(Fs[i+1], Fs[i], FCharLen-i);
      Dec(FCharLen);
    end else Inc(i);
  end;
  Truncate;
  if DoUpcase then for i := 1 to FCharlen do Fs[i] := gaANSIUpperArray[Fs[i]];
end;

procedure TStrBuf.Strip(DoUpcase : Boolean = False);
var   i : Integer;
  aStop : Integer;
begin
  aStop := FCharLen;
  while (aStop>0) and (Fs[aStop] = ' ') do System.Dec(aStop);
  FCharLen := aStop;
  if aStop=0 then Exit;
  i := 1;
  while (i<=aStop) and (Fs[i] = ' ') do Inc(i);
  if i>1 then
  begin
    FCharLen := (aStop+1)-i;
    System.Move(Fs[i], Fs[1], FCharLen);
    i := 1;
  end;
  while i < FCharLen do
  begin
    if (Fs[i] = ' ') and (Fs[i+1] = ' ') then
    begin
      System.Move(Fs[i+1], Fs[i], FCharLen-i);
      Dec(FCharLen);
    end else Inc(i);
  end;
  Truncate;
  if DoUpcase then for i := 1 to FCharlen do Fs[i] := gaANSIUpperArray[Fs[i]];
end;

function TStrBuf.GetChar(Index : Integer) : Char;
begin
  Result := Fs[Index];
end;

procedure TStrBuf.SetChar(Index : Integer; Value : Char);
begin
  Fs[Index] := Value;
end;

function TStrBuf.IsEmpty : Boolean;
var i : Integer;
begin
  Result := False;
  for i := 1 to FCharLen do if Fs[i]>' ' then Exit;
  Result := True;
end;

procedure TStrBuf.DelEndWhiteSpaces;
begin
  while (FCharLen>0) and (Fs[FCharLen] in [#0..#32]) do Dec(FCharLen);
  Truncate;
end;

function TStrBuf.HasChar(const aSet : TCharSet) : Boolean;
var i : Integer;
begin
  Result := True;
  for i := 1 to FCharLen do if Fs[i] in aSet then Exit;
  Result := False;
end;


{//////////////////////////////////////////////////////////////////////////////}
{                               ASM ROUTINES                                   }
{//////////////////////////////////////////////////////////////////////////////}

{$IFDEF StrManEnableASM}
{==============================================================================}
{                       FAST ASM CODE BY PETER MORRIS                          }
{------------------------------------------------------------------------------}
{ Peter Morris wrote an excellent article for the Delphi Developer:            }
{ "Optimizing String Searches in Delphi"                                       }
{ The url: http://www.borland.com/delphi/news/delphi_developer/optimizing.html }
{ He's code runs much faster than the Pos function in System.pas (Delphi 5)    }
{ which makes use of outdated assembler techniques according to the article.   }
{ I thank Peter for he's valueable contribution to the Delphi community.       }
{               ********************************************                   }
{ FastMemPos, FastMemPosNC is copyrighted by Peter Morris.                     }
{ Note that TStringManager makes use of these routines with permission by   }
{ the author, and that the author has no other association with this library.  }
{ If you would like to make use of the code outside this unit, please contact  }
{ Peter Morris at FastStrings@stuckindoors.com                                 }
{==============================================================================}


//Peter Morris - see note above
function FastMemPos(const aSource, aFind; const aSourceLen, aFindLen : Integer) : Pointer;
asm
          push ESI
          push EDI
          push EBX

          mov  ESI, aFind
          mov  EDI, aSource
          mov  ECX, aSourceLen

  //Quick exit code
          mov  Result, 0
  //SourceLen < FindLen
          cmp  ECX, aFindLen
          jl   @TheEnd
  //FindLen < 1
          cmp  aFindLen, 1
          jl   @TheEnd

  //Now DEC aSourceLen by aFindLen-1
          sub  ECX, aFindLen
          inc  ECX

  //Get the first Char of aFindString, note how it is done outside
  //of the main loop, as it never changes !
          Mov  Al, [ESI]
          jmp  @Scasb
  @FindNext:
          inc  EDI  //Done only when returning from CompareStrings
          dec  ECX
          jz   @NotFound

  //Now the FindFirstCharacter loop !
  @ScaSB:
  //Get the value of the current character in aSourceString
  //This is equal to ah := EDI^, that is what the [] are around [EDI]
  //compare this character with aDestString[1]
          cmp [EDI], al
  //If they are equal we compare the strings

          jz   @CompareStrings
          inc  EDI
          dec  ECX
          jnz  @ScaSB
          jmp  @NotFound

  @CompareStrings:
  //Put the length of aFindLen in EBX
          mov  EBX, aFindLen

  @CompareNext:
  //We DEC EBX to point to the end of the string, ie, we dont
  //want to add the whole length as this would point past the end of string
          dec  EBX
          jz   @FullMatch

  //here is another optimization tip !
  //People at this point usually PUSH ESI etc and then POP ESI etc
  //at the end, instead I opted not to change ESI etc at all.
  //This saves lots of pushing and popping !

  //Get aFindString character + aFindStringLength (the last Char)
          mov  Ah, [ESI+EBX]

  //Get aSourceString character (current position + aFindStringLength)
  //Compare them
          cmp  Ah, [EDI+EBX]
          Jnz  @FindNext

          Jmp  @CompareNext

    @FullMatch:
          //Move the address of the *current* character in EDI
          //note, we have not altered EDI since the first Char was found
          mov  Result, EDI
          jmp  @TheEnd
    @NotFound:
          //The substring was not found
          mov  Result, 0

  @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
end;

//Peter Morris - see note above
function FastMemPosNC(const aSource, aFind; const aSourceLen, aFindLen : Integer) : Pointer;
asm
          push ESI
          push EDI
          push EBX

          mov  ESI, aFind
          mov  EDI, aSource
          mov  ECX, aSourceLen

  //Quick exit code
          mov  Result, 0
  //SourceLen < FindLen
          cmp  ECX, aFindLen
          jl   @TheEnd
  //FindLen < 1
          cmp  aFindLen, 1
          jl   @TheEnd

  //Now DEC aSourceLen by aFindLen-1
          sub  ECX, aFindLen
          inc  ECX

  //Get the first Char of aFindString, note how it is done outside
  //of the main loop, as it never changes !
          mov  EDX, GUpCaseLUT
          xor  EBX, EBX
          jmp  @FindFirst
  @FindNext:
          inc  EDI  //Done only when returning from CompareStrings
          dec  ECX
          jz   @NotFound
  @FindFirst:
          mov  Bl, [ESI]
          mov  AL, [EDX+EBX]

  //Now the FindFirstCharacter loop !
  @ScaSB:
  //Get the value of the current character in aSourceString
  //This is equal to ah := EDI^, that is what the [] are around [EDI]
  //compare this character with aDestString[1]
          mov  Bl, [EDI]
          cmp  Al, [EDX+EBX]
  //If they are equal we compare the strings

          jz   @CompareStrings
          inc  EDI
          dec  ECX
          jnz  @ScaSB
          jmp  @NotFound

  @CompareStrings:
  //Save ECX
          push ECX
          mov  ECX, aFindLen

  @CompareNext:
          dec  ECX
          jz   @FullMatch

          mov  Bl, [ESI+ECX]
          mov  Al, [EDX+EBX]

          mov  Bl, [EDI+ECX]
          cmp  Al, [EDX+EBX]
          jz   @KeepChecking

          POP  ECX
          jmp  @FindNext

    @KeepChecking:
          Jmp  @CompareNext

    @FullMatch:
          pop  ECX
          mov  Result, EDI
          jmp  @TheEnd

    @NotFound:
          mov  Result, 0

  @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
end;

//Peter Morris - see note above
function FastPosBack(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  SourceLen : Integer;
begin
  if aFindLen < 1 then begin
    Result := 0;
    Exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    Exit;
  end;

  if (StartPos < 1) or  (StartPos + aFindLen >= aSourceLen) then
    SourceLen := aSourceLen - (aFindLen-1)
  else
    SourceLen := StartPos;

  asm
          push ESI
          push EDI
          push EBX

          mov EDI, aSourceString
          add EDI, SourceLen
          Dec EDI

          mov ESI, aFindString
          mov ECX, SourceLen
          Mov  Al, [ESI]

    @ScaSB:
          cmp  Al, [EDI]
          jne  @NextChar

    @CompareStrings:
          mov  EBX, aFindLen
          dec  EBX
          jz   @FullMatch

    @CompareNext:
          mov  Ah, [ESI+EBX]
          cmp  Ah, [EDI+EBX]
          Jnz  @NextChar

    @Matches:
          Dec  EBX
          Jnz  @CompareNext

    @FullMatch:
          mov  EAX, EDI
          sub  EAX, aSourceString
          inc  EAX
          mov  Result, EAX
          jmp  @TheEnd
    @NextChar:
          dec  EDI
          dec  ECX
          jnz  @ScaSB

          mov  Result,0

    @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
  end;
end;

//Peter Morris - see note above
function FastPosBackNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  SourceLen : Integer;
begin
  if aFindLen < 1 then begin
    Result := 0;
    Exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    Exit;
  end;

  if (StartPos < 1) or  (StartPos + aFindLen >= aSourceLen) then
    SourceLen := aSourceLen - (aFindLen-1)
  else
    SourceLen := StartPos;

  asm
          push ESI
          push EDI
          push EBX

          mov  EDI, aSourceString
          add  EDI, SourceLen
          Dec  EDI

          mov  ESI, aFindString
          mov  ECX, SourceLen

          mov  EDX, GUpCaseLUT
          xor  EBX, EBX

          mov  Bl, [ESI]
          mov  Al, [EDX+EBX]

    @ScaSB:
          mov  Bl, [EDI]
          cmp  Al, [EDX+EBX]
          jne  @NextChar

    @CompareStrings:
          PUSH ECX
          mov  ECX, aFindLen
          dec  ECX
          jz   @FullMatch

    @CompareNext:
          mov  Bl, [ESI+ECX]
          mov  Ah, [EDX+EBX]
          mov  Bl, [EDI+ECX]
          cmp  Ah, [EDX+EBX]
          Jz   @Matches

    //Go back to findind the first Char
          POP  ECX
          Jmp  @NextChar

    @Matches:
          Dec  ECX
          Jnz  @CompareNext

    @FullMatch:
          POP  ECX

          mov  EAX, EDI
          sub  EAX, aSourceString
          inc  EAX
          mov  Result, EAX
          jmp  @TheEnd
    @NextChar:
          dec  EDI
          dec  ECX
          jnz  @ScaSB

          mov  Result,0

    @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
  end;
end;

{//////////////////////////////////////////////////////////////////////////////}
//Eric Grobler
function TStringManager.Mop(const s : String; DoUpcase : Boolean = False) : String;
{Strip - asm version------------------------------------------------------------
Example String:                '  a   task   '
Char index:                     1234567890123
step1: find Stop                        *       (10)
step2: find Start                *              (3)
step3: SetLength minus outer     *********      (8)    'a   task'
step4: do result[i] := s[i]      *  *****
step5: Set final length                         (6)    'a task'
-------------------------------------------------------------------------------}
var  Stop  : Integer;
     Rlen  : Integer;
     Start : Integer;
begin
  {Step1}
  Stop := Length(s);
  while (Stop>0) and (s[Stop] <= ' ') do System.Dec(Stop); {too lazy to asm this}
  if Stop=0 then
  begin
    Result := '';
    Exit;
  end;
  {Step2}
  {Now we know we must have some chars}
  asm
      PUSH ESI
      {Reg changes: ECX, ESI}
      mov  ECX, Stop           {Max chars to search}
      mov  ESI, s              {point to s}
      mov  al,  32             {<=' '}

    @FindStart:                {while (i<=SLen) and (s[i] = c) do Inc(i);}
      cmp  al, [ESI]           {compare it against the SourceString}
      jb   @DoneStart
      inc  ESI
      dec  ECX
      jnz  @FindStart

    @DoneStart:
      mov  [Start], ESI         {Remember first non Space Char}
      POP  ESI
  end;

  {Step3}
  RLen := Stop-(Start-Integer(@s[1]));
  SetLength(Result, RLen);
  Stop := Integer(@Result[1]);   {Now we use Stop for the address of result!}
  {this does not work?  "mov EDI Result" or "mov EDI @Result[1]"}

  {Step4}
  asm
      PUSH ESI
      PUSH EDI
      PUSH EDX
      {Reg changes: EAX, ECX, ESI, EDX}
      mov  ESI, Start            {pointer to s[start]}
      mov  EDI, Stop             {pointer to Result[1]}
      mov  ECX, Rlen             {From (3-10) in this example}
      xor  EDX, EDX              {clear result len counter}
      mov  al,  32               {<=' '}

    @LoopMiddle:
      cmp  al, [ESI]             {compare it against the SourceString}
      jb   @IncludeChar          {is it a space?}

      @LoopSpaces:
        cmp  al, [ESI+1]         {compare it against the SourceString}
        jb  @PutSpace
        inc  ESI                 {next pos in s}
        dec  ECX                 {dec loop counter}
        jnz  @LoopSpaces

      @PutSpace:
        mov  Byte ptr [EDI], al  {put character in result index}
        inc  EDX                 {inc result counter}
        inc  EDI                 {move to text pos in result}
        jmp @GetNext

      @IncludeChar:
        mov  ah, Byte ptr [ESI]
        mov  Byte ptr [EDI], ah  {put character in result index}
        inc  EDX                 {inc result counter}
        inc  EDI                 {move to text pos in result}

      @GetNext:
        inc  ESI                 {next pos in s}
        dec  ECX                 {dec loop counter}
        jnz  @LoopMiddle


    @TheEnd:
      mov  [Rlen], EDX           {final number of chars in result}
      POP  EDX
      POP  EDI
      POP  ESI
  end;
  {Step5}
  SetLength(Result, RLen);
  if DoUpcase then
    for Start := 1 to RLen do Result[Start] := gaANSIUpperArray[Result[Start]];
end;

//Eric Grobler
function TStringManager.MopUp(const s : String) : String;
{StripUp asm version--------------------------------------------------------
Example String:              '  a   task   '
Char index:                   1234567890123
step1: find Stop                      *       (10)
step2: find Start              *              (3)
step3: count chars in result   *  *****       (6) ('a task') (RLen)
step4: SetLength to range
step5: do Result[i] := s[i]    *  *****
-------------------------------------------------------------------------------}
var  stop : Integer;
    start : Integer;
    Rlen  : Integer;
begin
  //Step1
  Stop := Length(s);
  while (Stop>0) and (s[Stop] <= ' ') do System.Dec(Stop);
  if Stop=0 then
  begin
    Result := '';
    Exit;
  end;
  //Step2
  //Now we know we must have some chars
  //First we calculate exactly how many chars the result will have
  asm
      PUSH ESI
      PUSH EDX
      //Will use regs: EAX, ECX, EDX, ESI
      mov  ECX, Stop           //Max chars to search
      mov  ESI, s              //point to s
      mov  al,  32             //<=' '

    @FindStart:                //while (i<=SLen) and (s[i] = c) do Inc(i);
      cmp  al, [ESI]           //compare it against the SourceString
      jb  @DoneStart
      inc  ESI
      dec  ECX
      jnz  @FindStart

    @DoneStart:
      mov  [Start], ESI            //Remember first non Space Char

   //----------------------------
   //Step3

    @CountMiddle:
      xor  EDX, EDX
      mov  al,  32             //<=' '

    @LoopMiddle:
      cmp  al, [ESI]           //compare it against the SourceString
      jb  @IncludeChar        //Include NonC

      @LoopSpaces:
        cmp  al, [ESI+1]         {compare it against the SourceString}
        jb   @IncludeChar
        inc  ESI                 {next pos in s}
        dec  ECX                 {dec loop counter}
        jnz  @LoopSpaces

      @IncludeChar:
        inc  EDX

      @ToNext:
        inc  ESI
        dec  ECX
        jnz  @LoopMiddle

    @MiddleEnd:
      mov RLen,   EDX
//--------------------------------------------------------------
    @TheEnd:
      POP  EDX
      POP  ESI
  end;

  //Step4
  SetLength(Result, Rlen);
  Stop := Integer(@Result[1]);   //Stop is now start of result

  //Now we just set the characters
  asm
      PUSH ESI
      PUSH EDI
      PUSH EDX
      PUSH EBX
      //Will use regs: EAX, ECX, ESI
      mov  ESI, Start          //point to s[start]
      mov  EDI, Stop           //point to Result[1]
      mov  EDX, GUpCaseLUT     //pointe to gaANSIUpperArray
      mov  ECX, Rlen           //We know how many chars
      xor  EBX, EBX
      mov  al,  32             //<=' '

    //Step5
    @LoopMiddle:
      cmp  al, [ESI]           //compare it against the SourceString
      jb  @IncludeChar

      @LoopSpaces:
        cmp  al, [ESI+1]         {compare it against the SourceString}
        jb  @PutSpace
        inc  ESI                 {next pos in s}
        jnz  @LoopSpaces         {No dec ecx - our initial CX already excluded the spaces}

      @PutSpace:
        mov  Byte ptr [EDI], al  {put character in result index}
        inc  EDI                 {move to text pos in result}
        jmp  @ToNext

      @IncludeChar:
        mov  bl, Byte ptr [ESI]   //ebx:=s[i]
        mov  ah, [EDX+EBX]        //upcase lookup

        mov  Byte ptr [EDI], ah
        inc  EDI

      @ToNext:
        inc  ESI
        dec  ECX
        jnz  @LoopMiddle
        jmp  @TheEnd

      @IgnoreChar:
        inc  ESI
        jnz  @LoopMiddle

    @TheEnd:
      POP  EBX
      POP  EDX
      POP  EDI
      POP  ESI
  end;
end;
{==============================================================================}
{$ENDIF} //if ASM



{$IFDEF StrManEnableERaise}
{//////////////////////////////////////////////////////////////////////////////}
{                                ERAISE                                        }
{//////////////////////////////////////////////////////////////////////////////}
{-------------------------------------------------------------------------------
Notes on the eRaise exception procedure
 You can use this procedure instead of the default call "raise exception.create("
 The eRaise procedure preserve nested exception error messages and is more consice.

 Delphi method:
     x := 0;
     try
       x := 2 div x;
     except
       on e:Exception do raise Exception.create('We have a problem'+#13+10+e.message);
     end;

  eRaise method:
     x := 0;
     try
       x := 2 div x;
     except
       eRaise('We have a problem'); //will add e.message "division by 0" automatically
     end;


You can also optionally pass a function name and class to the eRaise procedure.
 The example below will return the following error message:
 "Unable to read autoexec.bat
  Cannot open file: d:\autoexec.bat
  File not found
  Func: OpenAFile, ReadAutoExec
  Class: TForm1"

  procedure OpenAFile(var f : textfile; path : String);
  begin
    try
      AssignFile(f,path);
      Reset(f);
    except
      eRaise('Cannot open file: '+path,'OpenAFile'); //use ERaiseFunc for Delphi3
    end;
  end;

  procedure Tform1.ReadAutoExec;
  var f : textfile;
  begin
    try
      OpenAFile(f,'d:\autoexec.bat');
    except
      eRaise('Unable to read autoexec.bat','ReadAutoExec', Self); //use EraiseEx for Delphi3
    end;
  end;

  procedure TForm1.Button1Click(Sender: TObject);
  begin
    ReadAutoExec;
  end;
--------------------------------------------------------------------------------}
//Exception constants
const
  geIdFunc = 'Func: ';
  geIdClass = 'Class: ';


procedure RaiseException(aMessage, aFuncName : String; aObject : TObject);
var PrevMsg, PrevFunc, PrevClass : String;

  function Append(id : String; var IdText : String; SubStr : String) : Boolean;
  begin
    Result := sm.IsSameFirst(id, SubStr);
    if not Result then Exit;
    SubStr := sm.Mop(sm.After(id, SubStr));
    if IdText=''
    then IdText := SubStr else
    begin
      if sm.PosIC(SubStr, IdText)=0 then  IdText := IdText+','+SubStr;
    end;
  end;

  procedure GetPrevMsg;
   var e : TObject;
       m : String;
       s : String;
  begin
    e := ExceptObject; //SysUtils.pas in Delphi5, System.pas in Delphi6
    if e=nil then Exit;
    m := Exception(e).message;
    while m<>'' do
    begin
      s := sm.Before(gcCR, m);
      if sm.Mop(s)<>'' then
      begin
        if Append(geIdFunc, PrevFunc, s)=False then
        if Append(geIdClass, PrevClass, s)=False then
        PrevMsg := sm.DelIfFirst(gcCr, PrevMsg+gcCR+s);
      end;
      m := sm.After(gcCR, m);
    end;
  end;

  procedure Add(id : String; var Text : String; NewText : String);
  begin
    NewText := sm.Mop(NewText);
    if NewText='' then
    begin
      if Text<>'' then Text := id+Text;
    end else
    begin
      if Text=''
      then Text := id+NewText else
        if sm.PosIC(NewText, Text)=0
        then Text := id+Text+','+NewText
        else Text := id+Text
    end;
  end;

  function Clean(const s : String) : String;
  begin
    if s='' then Result := '' else Result := s+gcCR;
  end;
begin
  PrevMsg := '';
  PrevFunc := '';
  PrevClass := '';
  GetPrevMsg;
  Add(geIdFunc, PrevFunc, aFuncName);
  if aObject<>nil then Add(geIdClass, PrevClass, aObject.ClassName);
  raise Exception.Create(Clean(aMessage)+Clean(prevMsg)+Clean(PrevFunc)+PrevClass);
end;

procedure eRaise(aMessage : String; aFuncName : String= ''; aObject : TObject= nil);
begin
  RaiseException(aMessage, aFuncName, aObject);
end;

procedure eRaise(aMessage : String; aObject : TObject);
begin
  RaiseException(aMessage, '', aObject);
end;
{$ENDIF}


{//////////////////////////////////////////////////////////////////////////////}
{                         INITIALIZATION / FINALIZATION                        }
{//////////////////////////////////////////////////////////////////////////////}


{-------------------------------------------------------------------------------
Fills the upper and lower arrays with the current window locale case characters.
  Inspired by Christian Maas example to have faster case conversions.
Also fills the gaANSICharType with type flags.
-------------------------------------------------------------------------------}
procedure FillANSIArrays;
const
  //Windows ansi Char flags (call to GetStringTypeExA)
  //http://leb.net/wine/WinDoc/msdn/sdk/platforms/doc/sdk/win32/func/src/f39_11.htm
  C1_UPPER  = $0001;   // Uppercase
  C1_LOWER  = $0002;   // Lowercase - like: abcdefghijklmnopqrstuvwxyzƒšœžßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ
  C1_DIGIT  = $0004;   // Decimal digits (on my machine it returns funnies: "²³¹"??)
  C1_SPACE  = $0008;   // Space characters - #9..#13 ' ' and
  C1_PUNCT  = $0010;   // Punctuation Like: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~€‚ƒ„…†‡ˆ‰‹‘’“”•–—˜™›¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿×÷
  C1_CNTRL  = $0020;   // Control characters
  C1_BLANK  = $0040;   // Blank characters
  C1_XDIGIT = $0080;   // Hexadecimal digits
//C1_ALPHA  = $0100;   // Any linguistic character: alphabetic, syllabary, or ideographic

var   c : Char;
      w : Word;
      f : TCharTypeFlags;
begin
  for c := Low(Char) to High(Char) do gaANSIUpperArray[c] := c;
  for c := Low(Char) to High(Char) do gaANSILowerArray[c] := c;
  Windows.CharUpperBuff(@gaANSIUpperArray, SizeOf(gaANSIUpperArray));
  Windows.CharLowerBuff(@gaANSILowerArray, SizeOf(gaANSILowerArray));

  for c := Low(Char) to High(Char) do
  begin
    f := [];
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @c, SizeOf(c), w);
    if w and C1_UPPER <> 0     then include(f, cfUpper);
    if w and C1_LOWER <> 0     then include(f, cfLower);
    if c in ['A'..'Z']         then include(f, cfAZUpper);
    if c in ['a'..'z']         then include(f, cfAZLower);
    if w and C1_PUNCT <> 0     then include(f, cfPunct);
    if c in ['0'..'9']         then include(f, cfDigit);
    if c in gcsInt             then include(f, cfInt);
    if c in gcsNumeric         then include(f, cfNumeric);
    if c in gcsFloat           then include(f, cfFloat);
    if c in gcWordDelims       then include(f, cfDelim);
    if c in [#0..#31]          then include(f, cfControl);
    gaANSICharType[c] := f;
  end;

 {$IFDEF StrManEnableASM}
   GUpCaseLUT := @gaANSIUpperArray;
 {$ENDIF}
end;


initialization
  gcDefaultCurFormat := SysUtils.CurrencyString+'###,##0.00';
  FillANSIArrays;

{$IFDEF StrManEnableSMinstance}
  sm := TStringManager.Create;
{$ENDIF}

finalization

{$IFDEF StrManEnableSMinstance}
  sm.Free;
{$ENDIF}

end.

