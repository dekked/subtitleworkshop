// USubtitlesFunctions - Interesting functions
// Copyright ® 2001-2003 URUSoft.

unit USubtitlesFunctions;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Math, FastStrings;

// -----------------------------------------------------------------------------

function TimeToFrames   (Time: Integer; FPS: Single) : Integer;
function FramesToTime   (Frames, FPS: Single)        : Integer;
function StringCount    (const aFindString, aSourceString : string; Const CaseSensitive : Boolean = TRUE) : Integer;
function IsInteger      (const Str: String)   : Boolean;
function TimeToString   (Time: Integer; TimeFormat: String = {$IFDEF VIPLAY}'hh:mm:ss'{$ELSE}'hh:mm:ss,zzz'{$ENDIF}): String;
function StringToTime   (Time: String)        : Integer;
function TimeInFormat   (Time, Format: String): Boolean;
function PadLeft        (const S: String; const PadChar: AnsiChar; const Length: Integer; const Cut: Boolean = False): String;
function PadRight       (const S: AnsiString; const PadChar: AnsiChar; const Length: Integer; const Cut: Boolean): AnsiString;
function LimitDecimals  (Num: Real; Limit: Integer) : String;
function ReverseText    (Text: String; KeepLinesOrder: Boolean = True) : String;
function ReplaceEnters  (const S, NewPattern: String): String;
function ReplaceString  (const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
function InvertHTMLColor(const Color: String): String;
function RemoveRTFFormatting(Text: String): String;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function TimeToFrames(Time: Integer; FPS: Single): Integer;
begin
  Result := Round((Time / 1000) * FPS);
end;

// -----------------------------------------------------------------------------

function FramesToTime(Frames, FPS: Single): Integer;
begin
  Result := Round((Frames / FPS) * 1000);
end;

// -----------------------------------------------------------------------------

function StringCount(const aFindString, aSourceString : string; Const CaseSensitive : Boolean = TRUE) : Integer;
var
  Find,
  Source,
  NextPos                     : PChar;
  LSource,
  LFind                       : Integer;
  Next                        : TFastPosProc;
  JumpTable                   : TBMJumpTable;
begin
  Result := 0;
  LSource := Length(aSourceString);
  if LSource = 0 then exit;

  LFind := Length(aFindString);
  if LFind = 0 then exit;

  if CaseSensitive then
  begin
    Next := BMPos;
    MakeBMTable(PChar(aFindString), Length(aFindString), JumpTable);
  end
  else
  begin
    Next := BMPosNoCase;
    MakeBMTableNoCase(PChar(aFindString), Length(aFindString), JumpTable);
  end;

  Source := @aSourceString[1];
  Find := @aFindString[1];

  repeat
    NextPos := Next(Source, Find, LSource, LFind, JumpTable);
    if NextPos <> nil then
    begin
      Dec(LSource, (NextPos - Source) + LFind);
      Inc(Result);
      Source := NextPos + LFind;
    end;
  until NextPos = nil;
end;

// -----------------------------------------------------------------------------

function IsInteger(const Str: String): Boolean;
const
  Numbers = '0123456789';
var
  i : Integer;
begin
  if Str = '' then
  begin
    Result := False;
    exit;
  end;
  Result := True;
  for i := 1 to Length(Str) do
  begin
    if Pos(Str[i], Numbers) = 0 then
    begin
      Result := False;
      exit;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function StringToTime(Time: String): Integer;
var
  H, M, S, Z, i: Integer;
  PCount, PFirst, PSec, PThird: Integer;
begin
  Result := -1;
  if (Time = '') then exit;

  M      := 0;
  S      := 0;
  Z      := 0;
  PCount := 0;
  PFirst := 0;
  PSec   := 0;
  PThird := 0;

  for i := 1 to Length(Time) do
  begin
    if (Time[i] in ['0'..'9']) = False then
    begin
      if Time[i] in [':',',','.'] then
      begin
        if (i = 1) or (i = Length(Time)) then exit;
        case PCount of
          0: PFirst  := i;
          1: PSec    := i;
          2: PThird  := i;
        end;
        Inc(PCount);
      end
      else
        Exit;
    end;
  end;

  try
    if PFirst > 0 then
    begin
      H := StrToIntDef(Copy(Time, 0, PFirst - 1), 0);
      if PSec > 0 then
      begin
        M := StrToIntDef(Copy(Time, PFirst + 1, PSec - PFirst - 1), 0);
        if PThird > 0 then
        begin
          S := StrToIntDef(Copy(Time, PSec + 1, PThird - PSec - 1), 0);
          Z := StrToIntDef(PadRight(Copy(Time, PThird + 1, Length(Time)), '0', 3, False), 0);
        end
        else
          S := StrToIntDef(Copy(Time, PSec + 1, Length(Time)), 0);
      end;

      Result := ((H*3600)*1000) + ((M*60)*1000) + (S*1000) + Z;
    end;
  except
    Result := -1;
  end;
end;

// -----------------------------------------------------------------------------

function TimeInFormat(Time, Format: String): Boolean;
begin
  Result := False;
  if StringToTime(Time) > -1 then
  begin
    if (Pos(':', Time) = Pos(':', Format)) and
       (Pos('.', Time) = Pos('.', Format)) and
       (Pos(',', Time) = Pos(',', Format)) and
       (StringCount(':', Time) = StringCount(':', Format)) and
       (StringCount('.', Time) = StringCount('.', Format)) and
       (StringCount(',', Time) = StringCount(',', Format)) and
       (Length(Time) = Length(Format)) then
       Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function PadLeft(const S : String; const PadChar : Char; const Length : Integer; const Cut : Boolean = False) : String;
var
  F, L, P, M : Integer;
  I, J : PChar;
Begin
  if Length = 0 then
  begin
    if Cut then
      Result := ''
    else
      Result := S;
      
    exit;
  end;
  M := System.Length (S);
  if Length = M then
  begin
    Result := S;
    exit;
  end;
  if Cut then
    L := Length
  else
    L := Max(Length, M);
  P := Max(0, L - M);

  SetLength(Result, L);
  if P > 0 then
    FillChar(Pointer(Result)^, P, PadChar);
  if L > P then
  begin
    I := Pointer(Result);
    J := Pointer(S);
    Inc(I, P);
    for F := 1 to L - P do
    begin
      I^ := J^;
      Inc(I);
      Inc(J);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function PadRight(const S: AnsiString; const PadChar: AnsiChar; const Length: Integer; const Cut: Boolean): AnsiString;
var
  F, L, P, M : Integer;
  I, J       : PAnsiChar;
  function MaxI(const A, B: Integer): Integer;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;
begin
  if Length = 0 then
  begin
    if Cut then
      Result := ''
    else
      Result := S;

    Exit;
  end;
  M := System.Length(S);
  if Length = M then
  begin
    Result := S;
    exit;
  end;
  if Cut then
    L := Length
  else
    L := MaxI(Length, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if L > P then
  begin
    I := Pointer(Result);
    J := Pointer(S);
    for F := 1 to L - P do
    begin
      I^ := J^;
      Inc(I);
      Inc(J);
    end;
  end;
  if P > 0 then
    FillChar(Result[L - P + 1], P, PadChar);
end;

// -----------------------------------------------------------------------------

function TimeToString(Time: Integer; TimeFormat: String = {$IFDEF VIPLAY}'hh:mm:ss'{$ELSE}'hh:mm:ss,zzz'{$ENDIF}): String;
var
  Hour, Min, Sec, MSec : Word;
  tmpApariciones, tmp  : Byte;
begin
  if Time < 0 then Time := 0;
  Hour := Trunc(Time / 3600000);
  Min  := Trunc((Time-(Hour*3600000)) / 60000);
  Sec  := Trunc((Time-(Hour*3600000)-(Min*60000)) / 1000);
  MSec := Trunc((Time-(Hour*3600000)-(Min*60000)-(Sec*1000)));

  if TimeFormat = 'hh:mm:ss' then
    Result := Format('%.2d:%.2d:%.2d', [Hour, Min, Sec])
  else
  begin
    // ... La función StringOfChar crea una string de n caracteres ...
    tmpApariciones := StringCount('h', TimeFormat);
    if tmpApariciones > 0 then TimeFormat := ReplaceString(TimeFormat, StringOfChar('h',tmpApariciones), PadLeft(IntToStr(Hour), '0', tmpApariciones));
    tmpApariciones := StringCount('m', TimeFormat);
    if tmpApariciones > 0 then TimeFormat := ReplaceString(TimeFormat, StringOfChar('m',tmpApariciones), PadLeft(IntToStr(Min), '0', tmpApariciones));
    tmpApariciones := StringCount('s', TimeFormat);
    if tmpApariciones > 0 then TimeFormat := ReplaceString(TimeFormat, StringOfChar('s',tmpApariciones), PadLeft(IntToStr(Sec), '0', tmpApariciones));
    tmpApariciones := StringCount('z', TimeFormat);
    If tmpApariciones > 0 then
    begin
      tmp := Pos('z', TimeFormat);
      TimeFormat := Copy(ReplaceString(TimeFormat, StringOfChar('z', tmpApariciones), Copy(PadLeft(IntToStr(MSec), '0', 3), 0, tmpApariciones)), 0, tmp + tmpApariciones-1);
    end;
    Result := TimeFormat;
  end;
end;
{
// -----------------------------------------------------------------------------

function ReemplazaDecimalChar(Num: Real; Character: Char): String;
begin
  Result := FloatToStr(Num);
  Result := ReplaceString(Result,DecimalSeparator,Character, True, False);
end;   }

// -----------------------------------------------------------------------------

function LimitDecimals(Num: Real; Limit: Integer) : String;
begin
  Result := FloatToStr(Round(Num * Power(10, Limit)) / Power(10, Limit));
end;

// -----------------------------------------------------------------------------

Function ReverseText(Text: String; KeepLinesOrder: Boolean = True): String;
var
  x, TotalLines      : Integer;
  PosEnter, NewEnter : Integer;
begin
  try
    if KeepLinesOrder = True then
    begin
      SetLength(Result, Length(Text));
      TotalLines := 0;
      repeat
        NewEnter := 0;
        PosEnter   := Pos(#13#10, Text);

        if PosEnter = 0 then
          PosEnter := Length(Text)+1
        else
          NewEnter := PosEnter;

        for x := 1 to PosEnter-1 do
          Result[TotalLines+PosEnter-x] := Text[x];

        if NewEnter <> 0 then
        begin
          Result[TotalLines+NewEnter]   := #13;
          Result[TotalLines+NewEnter+1] := #10;
        end;

        Delete(Text, 1, PosEnter+1);
        Inc(TotalLines, PosEnter+1);
      until Text = '';
    end
    else
    begin
      Result := '';
      for x := 1 to Length(Text) do
        Result := Text[x] + Result;
      Result := ReplaceString(Result, #10#13, #13#10);
    end;
  except
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------

function ReplaceEnters(const S, NewPattern: String): String;
begin
  Result := FastReplace(S, #13#10, NewPattern);
end;

// -----------------------------------------------------------------------------

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
var
  Flags: TReplaceFlags;
begin
  Flags := [];
  If ReplaceAll = True Then Flags := Flags + [rfReplaceAll];
  If IgnoreCase = True Then Flags := Flags + [rfIgnoreCase];
  Result := FastAnsiReplace(S, OldPattern, NewPattern, Flags);
end;

// -----------------------------------------------------------------------------

function InvertHTMLColor(const Color: String): String;
begin
  If (Color = '') or (Length(Color) <> 6) Then Exit;
  Result := Copy(Color, 5, 2) + Copy(Color, 3, 2) + Copy(Color, 1, 2);
end;

// -----------------------------------------------------------------------------

function RemoveRTFFormatting(Text: String): String;
  function Resolve(c: Char): Integer;
  { Convert char to integer value - used to decode \'## to an ansi-value }
  begin
    c := UpperCase(c)[1];
    case Byte(c) of
      48..57 : Result := Byte(c) - 48;
      65..70 : Result := Byte(c) - 55;
      else Result := 0;
    end;
  end;
var
  i, c, a: Integer;
begin
(*  i := Pos('{', Text);
  c := Pos('}', Text);
  while (i > 0) and (c > 0) do
  begin
    if c < i then
      Delete(Text, c, 1) else
      Delete(Text, i, c-i+1);
    i := Pos('{', Text);
    c := Pos('}', Text);
  end;

  // Decode each \'## value to an ansi character
  c := Pos('\''', Text);
  while c > 0 do
  begin
    Delete(Text, c, 2);
    Text[c] := Char(Resolve(Text[c])*16 + Resolve(Text[c+1]));
    Delete(Text, c+1, 1);
    c := Pos('\''', Text);
  end; *)

  (*i := Pos('\', Text);
  if i > 0 then
  begin
    c := SmartPos(' ', Text, True, i);
    {a := SmartPos(#13#10, Text, True, i);
    if a < c then
      c := a;   }
    while (i > 0) and (c > 0) do
    begin
      Delete(Text, i, c-i+1);
      i := Pos('\', Text);
      if i > 0 then
      begin
        c := SmartPos(' ', Text, True, i);
        {a := SmartPos(#13#10, Text, True, i);
        if a < c then
          c := a;}
      end else
        c := 0;
    end;
  end;    *)

  Result := Text;
end;

// -----------------------------------------------------------------------------

end.
