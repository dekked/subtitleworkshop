//==================================================
//All code herein is copyrighted by
//Peter Morris
//-----
//No copying, alteration, or use is permitted without
//prior permission from myself.
//------
//Do not alter / remove this copyright notice
//Email me at : support@droopyeyes.com
//
//The homepage for this library is http://www.droopyeyes.com
//
// CURRENT VERSION V3.2
//
//(Check out www.HowToDoThings.com for Delphi articles !)
//(Check out www.stuckindoors.com if you need a free events page on your site !)
//==================================================
//Ps
//Permission can be obtained very easily, and there is no ££££ involved,
//please email me for permission, this way you can be included on the
//email list to be notififed of updates / fixes etc.

//(It just includes sending my kids a postcard, nothing more !)

//Modifications
//==============================================================================
//Date  : 17 Dec, 1999
//Found : VRP (on #Delphi EFNET)
//Fixed : VRP
//Change: Added SmartPos.  This will allow people to easily change POS to SmartPos
//        as the parameters are in the same order.  Clever use of default params
//        means that the extra functionality of FastStrings may be used by passing
//        some extra params.
//==============================================================================
//Date  : 17 Dec, 1999
//Found : Bob Richardson
//Fixed : Pete M
//Change: Oops a daisy.  FastPosBack (and NoCase) were not setting SearchLen
//        if a valid StartPos was passed.
//==============================================================================
//Date  : 10 Jan, 2000
//Found : Pete M
//Fixed : Pete M
//Change: Moved TFastPosProc into the interface section, so other routines
//        can use the same technique that I do in FastReplace
//==============================================================================
//Date  : 15 Jan, 2000
//Found : Pete M
//Fixed : Pete M
//Change: Created a FastCharPos and FastCharPosNoCase, if the code knows that
//        the FindString is only 1 char, it can use faster methods.
//==============================================================================
//Date  : 1 Mar, 2000
//Found : Pete M
//Fixed : Pete M
//Change: Changed the name of MyMove to FastCharMove, and added it to the
//        interface section.
//==============================================================================
//Date  : 5 Mar, 2000
//Found : Pete M
//Fixed : Pete M
//Change: Changed FastPosNoCase to implement the above changes AND to use a
//        lookup table for UpCase characters.
//==============================================================================
//Date  : 5 Mar, 2000
//Found : Pete M
//Fixed : Pete M
//Change: Realised that I was moving [EDI] into ah before comparing
//        with al, when I could have just compared al, [EDI].  doh !
//        Fastpos is now about 28% faster
//==============================================================================
//Date  : 12 Apr, 2000
//Found : hans gulo <hans@sangar.dhs.org>
//Fixed : Pete M
//Change: I was constantly converting to/from character indexes/pointers.
//        Considering we need pointers for MOVing data this was pointless +
//        Hans managed to write a quicker FastReplace in pure Object Pascal. (Nice job Hans)
//        Now I use pointers instead, this results in a much faster replace.
//        As I have always said, never assuming you have the fastest code :-)
//==============================================================================
//Date  : 02 May, 2000
//Found : hans gulo (again)
//Fixed : Pete M
//Change: In some (odd) circumstances FastMemPos(NC) would return a true result
//        for a substring that did not exist.
//==============================================================================
//Date  : 19 May, 2000
//Found : Dave Datta
//Fixed : Pete M
//Change: If the SOURCE was very small, and the REPLACE was very large, this
//        causes either an integer overflow or OutOfMemory.  In this case we
//        estimate the result size a lot lower and resize the result whenever
//        required (still not as often as StringReplace). See the const
//        cDeltaSize !!
//        You *may* still run out of memory, but that is a memory issue.
//==============================================================================
//Date  : 16 September, 2000
//Found : Lorenz Graf
//Fixed : Pete M
//Change: FastReplace had some EXIT statements before RESULT had been set.
//        I thought this would result in a Result of "", but it resulted in an
//        undetermined result (usually the same as the last valid result)
//        Set Result := '' in the first line of the code.
//==============================================================================
//Date  : 21 September, 2000
//Found : Chris Baldwin (TCrispy)
//Fixed : Pete M
//Change: NoCase routines were not working correctly with non-alphabetical
//        characters.  eg,   ) and #9 were thought to be the same
//        (Due to the UpCase routine simple ANDing the value eith $df)
//        Had to add lookup tables, which probably slows it down a little.
//==============================================================================
//Date  : 21 September, 2000
//Found : Pete M
//Fixed : Pete M
//Change: Forward searching routines could return errors if 0 was passed as the
//        StartPos.
//        This is actually an invalid value (1 is the first character)
//        So I inlcluded assert() statments.
//        Was *NOT* implemented in FastMEMPos as this is MEMORY and not a string
//==============================================================================
//Date  : 25 September, 2000
//Found : Lorenz Graf
//Fixed : Pete M
//Change: Incorrect value returned from FastMemPos if the SourceString and
//        FindString were the same values.
//        Also incorrect value returned from FastReplace if SourceString was ''
//==============================================================================
//Date  : 01 October, 2000
//Found : DJ (#delphi undernet)
//Fixed : Pete M
//Change: Uppercase table was incorrect for international alphabets.
//==============================================================================
//Date  : 23 November, 2000
//Found : DJ (#delphi undernet)
//Fixed : Pete M
//Change: CharUpperBuff(@GUpcaseTable[1], 256); should have been
//        CharUpperBuff(@GUpcaseTable[0], 256);
//==============================================================================
//Date  : 23 June, 2001
//Found : Lawrence Cheung <yllcheung@yahoo.com>
//Fixed : Pete M
//Change: FastPosBack ('bacdefga', 'a', 8, 1, 7);
//        The above example should return 2 but was returning 8
//==============================================================================
//Date  : 24 Aug, 2001
//Found : New development
//Fixed : Pete M
//Change: Removed FastMemPos, FastMemPosNoCase and replaced with BMPos and
//        BMPosNoCase.
//        These routines use my interpretation of a Boyer-Moore search routine.
//        If you call these routines directly you must first call
//        MakeBMTable or MakeBMTableNoCase, and you MUST call the correct routine !
//        Maybe I will create Boyer-Moore routines for backwards searching too.
//==============================================================================
//Date  : 06 Sept, 2001
//Found : Tim Frost <tim@roundhill.co.uk>
//Fixed : Pete M
//Change: Tim pointed out that using a global variable meant that the routines
//        were no longer thread safe.  I have had to change all POS type routines
//        so that they accept a JumpTable as an additional variable.  Sorry if
//        anyone calls these routines directly.
//==============================================================================
//Date  : 11 Sept, 2001
//Found : Misc
//Fixed : Pete M
//Change: MakeBMTable...... was not functioning correctly
//==============================================================================
//Date  : 10 January, 2002
//Found : Pete M
//Fixed : Pete M
//Change: A hideously small possibility that copying the remainder of the source
//        string to the end of Result when reaching the end of FastReplace
//        would run over the end of our buffer has been fixed. (No cases reported)
//==============================================================================
//Date  : 19 July, 2002
//Found : Robert Croshere <croshere@cns.nl>
//Fixed : Pete M
//Change: A bug when replacing a string with '' has been fixed.
//==============================================================================
//Date  : 14 August, 2002
//Found : Mark Derricutt <mark@talios.com>
//Fixed : Mark Derricutt <mark@talios.com>
//Change: Made compatible with Linux
//==============================================================================
//Date  : 23 October, 2002
//Fixed : Marc Bir <marc@delphihome.com>
//Change: Made compatible with Linux
//==============================================================================
//Date  : 02 November, 2002
//Fixed : Pete M
//Change: Added FastAnsiReplace.  Parameter compatible with StringReplace but
//        works with Multi-byte character sets (Japan, Korea, etc).
//==============================================================================
//Date  : 26 January, 2003
//Fixed : Pete M
//Change: Added FastTagReplace.  Lets you specify a TagStart and TagEnd, each
//        time text is encountered with these tags surrounding them, eg
//        <!UserName!> a callback procedure will be executed allowing you to
//        replace the tag with some specific text.
//==============================================================================
//Date  : 12 Febuary, 2003
//Fixed : Pete M
//Change: Added UserData: Integer to TFastTagReplaceProc so that a callback can
//        pass user data (such as an object instance)
//==============================================================================
//Date  : 15 Febuary, 2003
//Fixed : Pete M
//Change: It was possible for the procedure AddBuffer embedded within
//        FastTagReplace to not allocate a large enough buffer.
//==============================================================================
//Date  : 24 September, 2003
//Found : Michael Engesgaard <me@saxotech.com>
//Fixed : Pete M
//Change: FastAnsiReplace could overwrite the output buffer if the replace
//        string was much larger than the find string.
//==============================================================================


unit FastStrings;

interface

uses
   {$IFNDEF LINUX}
     Windows,
   {$ENDIF}
   SysUtils;

//This TYPE declaration will become apparent later
type
  TBMJumpTable = array[0..255] of Integer;
  TFastPosProc = function (const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
  TFastPosIndexProc = function (const aSourceString, aFindString: string; const aSourceLen, aFindLen, StartPos: Integer; var JumpTable: TBMJumpTable): Integer;
  TFastTagReplaceProc = procedure (var Tag: string; const UserData: Integer);


//Boyer-Moore routines
procedure MakeBMTable(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
procedure MakeBMTableNoCase(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
function BMPos(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
function BMPosNoCase(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;

function FastAnsiReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
procedure FastCharMove(const Source; var Dest; Count : Integer);
function FastCharPos(const aSource : string; const C: Char; StartPos : Integer): Integer;
function FastCharPosNoCase(const aSource : string; C: Char; StartPos : Integer): Integer;
function FastPos(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
function FastPosNoCase(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
function FastPosBack(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
function FastPosBackNoCase(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
function FastReplace(const aSourceString : string; const aFindString, aReplaceString : string;
  CaseSensitive : Boolean = False) : string;
function FastTagReplace(const SourceString, TagStart, TagEnd: string;
  FastTagReplaceProc: TFastTagReplaceProc; const UserData: Integer): string;
function SmartPos(const SearchStr,SourceStr : string;
                  const CaseSensitive : Boolean = TRUE;
                  const StartPos : Integer = 1;
                  const ForwardSearch : Boolean = TRUE) : Integer;

implementation

const
  cDeltaSize = 1.5;

var
  GUpcaseTable : array[0..255] of char;
  GUpcaseLUT: Pointer;

//MakeBMJumpTable takes a FindString and makes a JumpTable
procedure MakeBMTable(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
begin
  if BufferLen = 0 then raise Exception.Create('BufferLen is 0');
  asm
        push    EDI
        push    ESI

        mov     EDI, JumpTable
        mov     EAX, BufferLen
        mov     ECX, $100
        REPNE   STOSD

        mov     ECX, BufferLen
        mov     EDI, JumpTable
        mov     ESI, Buffer
        dec     ECX
        xor     EAX, EAX
@@loop:
        mov     AL, [ESI]
        lea     ESI, ESI + 1
        mov     [EDI + EAX * 4], ECX
        dec     ECX
        jg      @@loop

        pop     ESI
        pop     EDI
  end;
end;

procedure MakeBMTableNoCase(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
begin
  if BufferLen = 0 then raise Exception.Create('BufferLen is 0');
  asm
        push    EDI
        push    ESI

        mov     EDI, JumpTable
        mov     EAX, BufferLen
        mov     ECX, $100
        REPNE   STOSD

        mov     EDX, GUpcaseLUT
        mov     ECX, BufferLen
        mov     EDI, JumpTable
        mov     ESI, Buffer
        dec     ECX
        xor     EAX, EAX
@@loop:
        mov     AL, [ESI]
        lea     ESI, ESI + 1
        mov     AL, [EDX + EAX]
        mov     [EDI + EAX * 4], ECX
        dec     ECX
        jg      @@loop

        pop     ESI
        pop     EDI
  end;
end;

function BMPos(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
var
  LastPos: Pointer;
begin
  LastPos := Pointer(Integer(aSource) + aSourceLen - 1);
  asm
        push    ESI
        push    EDI
        push    EBX

        mov     EAX, aFindLen
        mov     ESI, aSource
        lea     ESI, ESI + EAX - 1
        std
        mov     EBX, JumpTable

@@comparetext:
        cmp     ESI, LastPos
        jg      @@NotFound
        mov     EAX, aFindLen
        mov     EDI, aFind
        mov     ECX, EAX
        push    ESI //Remember where we are
        lea     EDI, EDI + EAX - 1
        xor     EAX, EAX
@@CompareNext:
        mov     al, [ESI]
        cmp     al, [EDI]
        jne     @@LookAhead
        lea     ESI, ESI - 1
        lea     EDI, EDI - 1
        dec     ECX
        jz      @@Found
        jmp     @@CompareNext

@@LookAhead:
        //Look up the char in our Jump Table
        pop     ESI
        mov     al, [ESI]
        mov     EAX, [EBX + EAX * 4]
        lea     ESI, ESI + EAX
        jmp     @@CompareText

@@NotFound:
        mov     Result, 0
        jmp     @@TheEnd
@@Found:
        pop     EDI //We are just popping, we don't need the value
        inc     ESI
        mov     Result, ESI
@@TheEnd:
        cld
        pop     EBX
        pop     EDI
        pop     ESI
  end;
end;

function BMPosNoCase(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
var
  LastPos: Pointer;
begin
  LastPos := Pointer(Integer(aSource) + aSourceLen - 1);
  asm
        push    ESI
        push    EDI
        push    EBX

        mov     EAX, aFindLen
        mov     ESI, aSource
        lea     ESI, ESI + EAX - 1
        std
        mov     EDX, GUpcaseLUT

@@comparetext:
        cmp     ESI, LastPos
        jg      @@NotFound
        mov     EAX, aFindLen
        mov     EDI, aFind
        push    ESI //Remember where we are
        mov     ECX, EAX
        lea     EDI, EDI + EAX - 1
        xor     EAX, EAX
@@CompareNext:
        mov     al, [ESI]
        mov     bl, [EDX + EAX]
        mov     al, [EDI]
        cmp     bl, [EDX + EAX]
        jne     @@LookAhead
        lea     ESI, ESI - 1
        lea     EDI, EDI - 1
        dec     ECX
        jz      @@Found
        jmp     @@CompareNext

@@LookAhead:
        //Look up the char in our Jump Table
        pop     ESI
        mov     EBX, JumpTable
        mov     al, [ESI]
        mov     al, [EDX + EAX]
        mov     EAX, [EBX + EAX * 4]
        lea     ESI, ESI + EAX
        jmp     @@CompareText

@@NotFound:
        mov     Result, 0
        jmp     @@TheEnd
@@Found:
        pop     EDI //We are just popping, we don't need the value
        inc     ESI
        mov     Result, ESI
@@TheEnd:
        cld
        pop     EBX
        pop     EDI
        pop     ESI
  end;
end;


//NOTE : FastCharPos and FastCharPosNoCase do not require you to pass the length
//       of the string, this was only done in FastPos and FastPosNoCase because
//       they are used by FastReplace many times over, thus saving a LENGTH()
//       operation each time.  I can't see you using these two routines for the
//       same purposes so I didn't do that this time !
function FastCharPos(const aSource : string; const C: Char; StartPos : Integer) : Integer;
var
  L                           : Integer;
begin
  //If this assert failed, it is because you passed 0 for StartPos, lowest value is 1 !!
  Assert(StartPos > 0);

  Result := 0;
  L := Length(aSource);
  if L = 0 then exit;
  if StartPos > L then exit;
  Dec(StartPos);
  asm
      PUSH EDI                 //Preserve this register

      mov  EDI, aSource        //Point EDI at aSource
      add  EDI, StartPos
      mov  ECX, L              //Make a note of how many chars to search through
      sub  ECX, StartPos
      mov  AL,  C              //and which char we want
    @Loop:
      cmp  Al, [EDI]           //compare it against the SourceString
      jz   @Found
      inc  EDI
      dec  ECX
      jnz  @Loop
      jmp  @NotFound
    @Found:
      sub  EDI, aSource        //EDI has been incremented, so EDI-OrigAdress = Char pos !
      inc  EDI
      mov  Result,   EDI
    @NotFound:

      POP  EDI
  end;
end;

function FastCharPosNoCase(const aSource : string; C: Char; StartPos : Integer) : Integer;
var
  L                           : Integer;
begin
  Result := 0;
  L := Length(aSource);
  if L = 0 then exit;
  if StartPos > L then exit;
  Dec(StartPos);
  if StartPos < 0 then StartPos := 0;

  asm
      PUSH EDI                 //Preserve this register
      PUSH EBX
      mov  EDX, GUpcaseLUT

      mov  EDI, aSource        //Point EDI at aSource
      add  EDI, StartPos
      mov  ECX, L              //Make a note of how many chars to search through
      sub  ECX, StartPos

      xor  EBX, EBX
      mov  BL,  C
      mov  AL, [EDX+EBX]
    @Loop:
      mov  BL, [EDI]
      inc  EDI
      cmp  Al, [EDX+EBX]
      jz   @Found
      dec  ECX
      jnz  @Loop
      jmp  @NotFound
    @Found:
      sub  EDI, aSource        //EDI has been incremented, so EDI-OrigAdress = Char pos !
      mov  Result,   EDI
    @NotFound:

      POP  EBX
      POP  EDI
  end;
end;

//The first thing to note here is that I am passing the SourceLength and FindLength
//As neither Source or Find will alter at any point during FastReplace there is
//no need to call the LENGTH subroutine each time !
function FastPos(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  JumpTable: TBMJumpTable;
begin
  //If this assert failed, it is because you passed 0 for StartPos, lowest value is 1 !!
  Assert(StartPos > 0);
  if aFindLen < 1 then begin
    Result := 0;
    exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    exit;
  end;

  MakeBMTable(PChar(aFindString), aFindLen, JumpTable);
  Result := Integer(BMPos(PChar(aSourceString) + (StartPos - 1), PChar(aFindString),aSourceLen - (StartPos-1), aFindLen, JumpTable));
  if Result > 0 then
    Result := Result - Integer(@aSourceString[1]) +1;
end;

function FastPosNoCase(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  JumpTable: TBMJumpTable;
begin
  //If this assert failed, it is because you passed 0 for StartPos, lowest value is 1 !!
  Assert(StartPos > 0);
  if aFindLen < 1 then begin
    Result := 0;
    exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    exit;
  end;

  MakeBMTableNoCase(PChar(AFindString), aFindLen, JumpTable);
  Result := Integer(BMPosNoCase(PChar(aSourceString) + (StartPos - 1), PChar(aFindString),aSourceLen - (StartPos-1), aFindLen, JumpTable));
  if Result > 0 then
    Result := Result - Integer(@aSourceString[1]) +1;
end;

function FastPosBack(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  SourceLen : Integer;
begin
  if aFindLen < 1 then begin
    Result := 0;
    exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    exit;
  end;

  if (StartPos = 0) or  (StartPos + aFindLen > aSourceLen) then
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


function FastPosBackNoCase(const aSourceString, aFindString : string; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  SourceLen : Integer;
begin
  if aFindLen < 1 then begin
    Result := 0;
    exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    exit;
  end;

  if (StartPos = 0) or  (StartPos + aFindLen > aSourceLen) then
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

          mov  EDX, GUpcaseLUT
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

    //Go back to findind the first char
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

//My move is not as fast as MOVE when source and destination are both
//DWord aligned, but certainly faster when they are not.
//As we are moving characters in a string, it is not very likely at all that
//both source and destination are DWord aligned, so moving bytes avoids the
//cycle penality of reading/writing DWords across physical boundaries
procedure FastCharMove(const Source; var Dest; Count : Integer);
asm
//Note:  When this function is called, delphi passes the parameters as follows
//ECX = Count
//EAX = Const Source
//EDX = Var Dest

        //If no bytes to copy, just quit altogether, no point pushing registers
        cmp   ECX,0
        Je    @JustQuit

        //Preserve the critical delphi registers
        push  ESI
        push  EDI

        //move Source into ESI  (generally the SOURCE register)
        //move Dest into EDI (generally the DEST register for string commands)
        //This may not actually be neccessary, as I am not using MOVsb etc
        //I may be able just to use EAX and EDX, there may be a penalty for
        //not using ESI, EDI but I doubt it, this is another thing worth trying !
        mov   ESI, EAX
        mov   EDI, EDX

        //The following loop is the same as repNZ MovSB, but oddly quicker !
    @Loop:
        //Get the source byte
        Mov   AL, [ESI]
        //Point to next byte
        Inc   ESI
        //Put it into the Dest
        mov   [EDI], AL
        //Point dest to next position
        Inc   EDI
        //Dec ECX to note how many we have left to copy
        Dec   ECX
        //If ECX <> 0 then loop
        Jnz   @Loop

        //Another optimization note.
        //Many people like to do this

        //Mov AL, [ESI]
        //Mov [EDI], Al
        //Inc ESI
        //Inc ESI

        //There is a hidden problem here, I wont go into too much detail, but
        //the pentium can continue processing instructions while it is still
        //working out the result of INC ESI or INC EDI
        //(almost like a multithreaded CPU)
        //if, however, you go to use them while they are still being calculated
        //the processor will stop until they are calculated (a penalty)
        //Therefore I alter ESI and EDI as far in advance as possible of using them

        //Pop the critical Delphi registers that we have altered
        pop   EDI
        pop   ESI
    @JustQuit:
end;

function FastAnsiReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  BufferSize, BytesWritten: Integer;
  SourceString, FindString: string;
  ResultPChar: PChar;
  FindPChar, ReplacePChar: PChar;
  SPChar, SourceStringPChar, PrevSourceStringPChar: PChar;
  FinalSourceMarker: PChar;
  SourceLength, FindLength, ReplaceLength, CopySize: Integer;
  FinalSourcePosition: Integer;
begin
  //Set up string lengths
  BytesWritten := 0;
  SourceLength := Length(S);
  FindLength := Length(OldPattern);
  ReplaceLength := Length(NewPattern);
  //Quick exit
  if (SourceLength = 0) or (FindLength = 0) or
    (FindLength > SourceLength) then
  begin
    Result := S;
    Exit;
  end;

  //Set up the source string and find string
  if rfIgnoreCase in Flags then
  begin
    SourceString := AnsiUpperCase(S);
    FindString := AnsiUpperCase(OldPattern);
  end else
  begin
    SourceString := S;
    FindString := OldPattern;
  end;

  //Set up the result buffer size and pointers
  try
    if ReplaceLength <= FindLength then
      //Result cannot be larger, only same size or smaller
      BufferSize := SourceLength
    else
      //Assume a source string made entired of the sub string
      BufferSize := (SourceLength * ReplaceLength) div
    FindLength;

    //10 times is okay for starters. We don't want to
    //go allocating much more than we need.
    if BufferSize > (SourceLength * 10) then
      BufferSize := SourceLength * 10;
  except
    //Oops, integer overflow! Better start with a string
    //of the same size as the source.
    BufferSize := SourceLength;
  end;
  SetLength(Result, BufferSize);
  ResultPChar := @Result[1];

  //Set up the pointers to S and SourceString
  SPChar := @S[1];
  SourceStringPChar := @SourceString[1];
  PrevSourceStringPChar := SourceStringPChar;
  FinalSourceMarker := @SourceString[SourceLength - (FindLength - 1)];

  //Set up the pointer to FindString
  FindPChar := @FindString[1];

  //Set the pointer to ReplaceString
  if ReplaceLength > 0 then
    ReplacePChar := @NewPattern[1]
  else
    ReplacePChar := nil;

  //Replace routine
  repeat
    //Find the sub string
    SourceStringPChar := AnsiStrPos(PrevSourceStringPChar,
    FindPChar);
    if SourceStringPChar = nil then Break;
    //How many characters do we need to copy before
    //the string occurs
    CopySize := SourceStringPChar - PrevSourceStringPChar;

    //Check we have enough space in our Result buffer
    if CopySize + ReplaceLength > BufferSize - BytesWritten then
    begin
      BufferSize := Trunc((BytesWritten + CopySize + ReplaceLength) * cDeltaSize);
      SetLength(Result, BufferSize);
      ResultPChar := @Result[BytesWritten + 1];
    end;

    //Copy the preceeding characters to our result buffer
    Move(SPChar^, ResultPChar^, CopySize);
    Inc(BytesWritten, CopySize);
    //Advance the copy position of S
    Inc(SPChar, CopySize + FindLength);
    //Advance the Result pointer
    Inc(ResultPChar, CopySize);
    //Copy the replace string into the Result buffer
    if Assigned(ReplacePChar) then
    begin
      Move(ReplacePChar^, ResultPChar^, ReplaceLength);
      Inc(ResultPChar, ReplaceLength);
      Inc(BytesWritten, ReplaceLength);
    end;

    //Fake delete the start of the source string
    PrevSourceStringPChar := SourceStringPChar + FindLength;
  until (PrevSourceStringPChar > FinalSourceMarker) or
    not (rfReplaceAll in Flags);

  FinalSourcePosition := Integer(SPChar - @S[1]);
  CopySize := SourceLength - FinalSourcePosition;
  SetLength(Result, BytesWritten + CopySize);
  if CopySize > 0 then
    Move(SPChar^, Result[BytesWritten + 1], CopySize);
end;

function FastReplace(const aSourceString : string; const aFindString, aReplaceString : string;
   CaseSensitive : Boolean = False) : string;
var
  PResult                     : PChar;
  PReplace                    : PChar;
  PSource                     : PChar;
  PFind                       : PChar;
  PPosition                   : PChar;
  CurrentPos,
  BytesUsed,
  lResult,
  lReplace,
  lSource,
  lFind                       : Integer;
  Find                        : TFastPosProc;
  CopySize                    : Integer;
  JumpTable                   : TBMJumpTable;
begin
  LSource := Length(aSourceString);
  if LSource = 0 then begin
    Result := aSourceString;
    exit;
  end;
  PSource := @aSourceString[1];

  LFind := Length(aFindString);
  if LFind = 0 then exit;
  PFind := @aFindString[1];

  LReplace := Length(aReplaceString);

  //Here we may get an Integer Overflow, or OutOfMemory, if so, we use a Delta
  try
    if LReplace <= LFind then
      SetLength(Result,lSource)
    else
      SetLength(Result, (LSource *LReplace) div  LFind);
  except
    SetLength(Result,0);
  end;

  LResult := Length(Result);
  if LResult = 0 then begin
    LResult := Trunc((LSource + LReplace) * cDeltaSize);
    SetLength(Result, LResult);
  end;


  PResult := @Result[1];


  if CaseSensitive then
  begin
    MakeBMTable(PChar(AFindString), lFind, JumpTable);
    Find := BMPos;
  end else
  begin
    MakeBMTableNoCase(PChar(AFindString), lFind, JumpTable);
    Find := BMPosNoCase;
  end;


  BytesUsed := 0;
  if LReplace > 0 then begin
    PReplace := @aReplaceString[1];
    repeat
      PPosition := Find(PSource,PFind,lSource, lFind, JumpTable);
      if PPosition = nil then break;

      CopySize := PPosition - PSource;
      Inc(BytesUsed, CopySize + LReplace);

      if BytesUsed >= LResult then begin
        //We have run out of space
        CurrentPos := Integer(PResult) - Integer(@Result[1]) +1;
        LResult := Trunc(LResult * cDeltaSize);
        SetLength(Result,LResult);
        PResult := @Result[CurrentPos];
      end;

      FastCharMove(PSource^,PResult^,CopySize);
      Dec(lSource,CopySize + LFind);
      Inc(PSource,CopySize + LFind);
      Inc(PResult,CopySize);

      FastCharMove(PReplace^,PResult^,LReplace);
      Inc(PResult,LReplace);

    until lSource < lFind;
  end else begin
    repeat
      PPosition := Find(PSource,PFind,lSource, lFind, JumpTable);
      if PPosition = nil then break;

      CopySize := PPosition - PSource;
      FastCharMove(PSource^,PResult^,CopySize);
      Dec(lSource,CopySize + LFind);
      Inc(PSource,CopySize + LFind);
      Inc(PResult,CopySize);
      Inc(BytesUsed, CopySize);
    until lSource < lFind;
  end;

  SetLength(Result, (PResult+LSource) - @Result[1]);
  if LSource > 0 then
    FastCharMove(PSource^, Result[BytesUsed + 1], LSource);
end;

function FastTagReplace(const SourceString, TagStart, TagEnd: string;
  FastTagReplaceProc: TFastTagReplaceProc; const UserData: Integer): string;
var
  TagStartPChar: PChar;
  TagEndPChar: PChar;
  SourceStringPChar: PChar;
  TagStartFindPos: PChar;
  TagEndFindPos: PChar;
  TagStartLength: Integer;
  TagEndLength: Integer;
  DestPChar: PChar;
  FinalSourceMarkerStart: PChar;
  FinalSourceMarkerEnd: PChar;
  BytesWritten: Integer;
  BufferSize: Integer;
  CopySize: Integer;
  ReplaceString: string;

  procedure AddBuffer(const Buffer: Pointer; Size: Integer);
  begin
    if BytesWritten + Size > BufferSize then
    begin
      BufferSize := Trunc(BufferSize * cDeltaSize);
      if BufferSize <= (BytesWritten + Size) then
        BufferSize := Trunc((BytesWritten + Size) * cDeltaSize);
      SetLength(Result, BufferSize);
      DestPChar := @Result[BytesWritten + 1];
    end;
    Inc(BytesWritten, Size);
    FastCharMove(Buffer^, DestPChar^, Size);
    DestPChar := DestPChar + Size;
  end;

begin
  Assert(Assigned(@FastTagReplaceProc));
  TagStartPChar := PChar(TagStart);
  TagEndPChar := PChar(TagEnd);
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then
  begin
    Result := SourceString;
    Exit;
  end;

  SourceStringPChar := PChar(SourceString);
  TagStartLength := Length(TagStart);
  TagEndLength := Length(TagEnd);
  FinalSourceMarkerEnd := SourceStringPChar + Length(SourceString) - TagEndLength;
  FinalSourceMarkerStart := FinalSourceMarkerEnd - TagStartLength;

  BytesWritten := 0;
  BufferSize := Length(SourceString);
  SetLength(Result, BufferSize);
  DestPChar := @Result[1];

  repeat
    TagStartFindPos := AnsiStrPos(SourceStringPChar, TagStartPChar);
    if (TagStartFindPos = nil) or (TagStartFindPos > FinalSourceMarkerStart) then Break;
    TagEndFindPos := AnsiStrPos(TagStartFindPos + TagStartLength, TagEndPChar);
    if (TagEndFindPos = nil) or (TagEndFindPos > FinalSourceMarkerEnd) then Break;
    CopySize := TagStartFindPos - SourceStringPChar;
    AddBuffer(SourceStringPChar, CopySize);
    CopySize := TagEndFindPos - (TagStartFindPos + TagStartLength);
    SetLength(ReplaceString, CopySize);
    if CopySize > 0 then
      Move((TagStartFindPos + TagStartLength)^, ReplaceString[1], CopySize);
    FastTagReplaceProc(ReplaceString, UserData);
    if Length(ReplaceString) > 0 then
      AddBuffer(@ReplaceString[1], Length(ReplaceString));
    SourceStringPChar := TagEndFindPos + TagEndLength;
  until SourceStringPChar > FinalSourceMarkerStart;
  CopySize := PChar(@SourceString[Length(SourceString)]) - (SourceStringPChar - 1);
  if CopySize > 0 then
    AddBuffer(SourceStringPChar, CopySize);
  SetLength(Result, BytesWritten);
end;

function SmartPos(const SearchStr,SourceStr : string;
                  const CaseSensitive : Boolean = TRUE;
                  const StartPos : Integer = 1;
                  const ForwardSearch : Boolean = TRUE) : Integer;
begin
  // NOTE:  When using StartPos, the returned value is absolute!
  if (CaseSensitive) then
    if (ForwardSearch) then
      Result:=
        FastPos(SourceStr,SearchStr,Length(SourceStr),Length(SearchStr),StartPos)
    else
      Result:=
        FastPosBack(SourceStr,SearchStr,Length(SourceStr),Length(SearchStr),StartPos)
  else
    if (ForwardSearch) then
      Result:=
        FastPosNoCase(SourceStr,SearchStr,Length(SourceStr),Length(SearchStr),StartPos)
    else
      Result:=
        FastPosBackNoCase(SourceStr,SearchStr,Length(SourceStr),Length(SearchStr),StartPos)
end;

var
  I: Integer;
initialization
  {$IFNDEF LINUX}
    for I:=0 to 255 do GUpcaseTable[I] := Chr(I);
    CharUpperBuff(@GUpcaseTable[0], 256);
  {$ELSE}
    for I:=0 to 255 do GUpcaseTable[I] := UpCase(Chr(I));
  {$ENDIF}
  GUpcaseLUT := @GUpcaseTable[0];
end.
