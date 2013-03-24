{*
 *  USTimeUtils
 *  Copyright (C) 2001-2003 URUSoft
 *
 *  Website : http://www.urusoft.net
 *
 *}

unit USTimeUtils;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, FastStrings, USStringsUtils;

// -----------------------------------------------------------------------------

function TimeToFrames(const Time: Cardinal; const FPS: Single): Cardinal;
function FramesToTime(const Frames, FPS: Single): Cardinal;
function TimeToString(const Time: Cardinal; TimeFormat: String = {$IFDEF VIPLAY}'hh:mm:ss'{$ELSE}'hh:mm:ss,zzz'{$ENDIF}): String;
function StringToTime(const Time: String; const NoHours: Boolean = False): Integer;
function TimeInFormat(const Time, Format: String): Boolean;
function RefTimeToMSecs(RefTime: Int64): Cardinal;
function MSecsToRefTime(MSecs: Cardinal): Int64;
function MSToHHMMSSFFTime(const Time: Integer; const FPS: Single; const FramesSeparator: Char = ':'): String;
function HHMMSSFFTimeToMS(const Time: String; const FPS: Single): Integer;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function TimeToFrames(const Time: Cardinal; const FPS: Single): Cardinal;
begin
  Result := Round((Time / 1000) * FPS);
end;

// -----------------------------------------------------------------------------

function FramesToTime(const Frames, FPS: Single): Cardinal;
begin
  if FPS <> 0 then
    Result := Round((Frames / FPS) * 1000) else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TimeToString(const Time: Cardinal; TimeFormat: String = {$IFDEF VIPLAY}'hh:mm:ss'{$ELSE}'hh:mm:ss,zzz'{$ENDIF}): String;
var
  Hour, Min, Sec, MSec : Word;
  Count, tmp           : Byte;
begin
  Hour := Trunc(Time / 3600000);
  Min  := Trunc((Time-(Hour*3600000)) / 60000);
  Sec  := Trunc((Time-(Hour*3600000)-(Min*60000)) / 1000);
  MSec := Trunc((Time-(Hour*3600000)-(Min*60000)-(Sec*1000)));

  if Hour > 23  then Hour := 23;
  if Min > 59   then Min  := 59;
  if Sec > 59   then Sec  := 59;
  if MSec > 999 then MSec := 999;

  if TimeFormat = 'hh:mm:ss' then
    Result := Format('%.2d:%.2d:%.2d', [Hour, Min, Sec])
  else
  begin
    Count := StringCount('h', TimeFormat);
    if Count > 0 then
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('h', Count), PadLeft(IntToStr(Hour), '0', Count))
    else
      Min := Min + Hour * 60;

    Count := StringCount('m', TimeFormat);
    if Count > 0 then
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('m', Count), PadLeft(IntToStr(Min), '0', Count))
    else
      Sec := Sec + Min * 60;

    Count := StringCount('s', TimeFormat);
    if Count > 0 then
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('s', Count), PadLeft(IntToStr(Sec), '0', Count));

    Count := StringCount('z', TimeFormat);
    if Count > 0 then
    begin
      tmp := Pos('z', TimeFormat);
      TimeFormat := Copy(ReplaceString(TimeFormat, StringOfChar('z', Count), Copy(PadLeft(IntToStr(MSec), '0', 3), 0, Count)), 0, tmp + Count-1);
    end;

    Result := TimeFormat;
  end;
end;

// -----------------------------------------------------------------------------

function StringToTime(const Time: String; const NoHours: Boolean = False): Integer;
var
  H, M, S, Z, i                : Integer;
  PCount, PFirst, PSec, PThird : Integer;
begin
  Result := -1;
  if (Time = '') then Exit;

  H      := 0;
  M      := 0;
  S      := 0;
  Z      := 0;
  PCount := 0;
  PFirst := 0;
  PSec   := 0;
  PThird := 0;

  for i := 1 to Length(Time) do
  begin
    if not (Time[i] in ['0'..'9']) then
      if (Time[i] in [':', ',', '.']) then
      begin
        if (i = 1) or (i = Length(Time)) then Exit;

        case PCount of
          0 : PFirst := i;
          1 : PSec   := i;
          2 : PThird := i;
        end;

        Inc(PCount);
      end
      else
        Exit;
  end;

  try
    if PFirst > 0 then
    begin
      if NoHours then
      begin
        M := StringToInt(Copy(Time, 0, PFirst - 1));
        if PSec = 0 then
          S := StringToInt(Copy(Time, PFirst + 1, Length(Time)-PFirst));
      end
      else
      begin
        H := StringToInt(Copy(Time, 0, PFirst - 1));
        if PSec = 0 then
          M := StringToInt(Copy(Time, PFirst + 1, Length(Time)-PFirst));
      end;

      if PSec > 0 then
      begin
        if NoHours then
        begin
          S := StringToInt(Copy(Time, PFirst + 1, PSec - PFirst - 1));
          Z := StringToInt(PadRight(Copy(Time, PSec + 1, Length(Time)), '0', 3, False));
        end
        else
          M := StringToInt(Copy(Time, PFirst + 1, PSec - PFirst - 1));

        if PThird > 0 then
        begin
          S := StringToInt(Copy(Time, PSec + 1, PThird - PSec - 1));
          Z := StringToInt(PadRight(Copy(Time, PThird + 1, Length(Time)), '0', 3, False));
        end
        else
          if not NoHours then
            S := StringToInt(Copy(Time, PSec + 1, Length(Time)));
      end;

      Result := ((H*3600)*1000) + ((M*60)*1000) + (S*1000) + Z;
    end;
  except
    Result := -1;
  end;
end;

// -----------------------------------------------------------------------------

function TimeInFormat(const Time, Format: String): Boolean;
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

function RefTimeToMSecs(RefTime: Int64): Cardinal;
begin
  Result := Cardinal(RefTime div 10000);
end;

// -----------------------------------------------------------------------------

function MSecsToRefTime(MSecs: Cardinal): Int64;
begin
  Result := MSecs * 10000;
end;

// -----------------------------------------------------------------------------

function MSToHHMMSSFFTime(const Time: Integer; const FPS: Single; const FramesSeparator: Char = ':'): String;
var
  tmp: Integer;
begin
  Result := TimeToString(Time, 'hh:mm:ss');
  tmp := TimeToFrames(Time - StringToTime(Result), FPS);
  if tmp = 25 then
  begin
    Result := TimeToString(StringToTime(Result) + 1000, 'hh:mm:ss');
    tmp := 0;
  end;
  Result := Result + FramesSeparator + PadLeft(IntToStr(tmp), '0', 2);
end;

// -----------------------------------------------------------------------------

function HHMMSSFFTimeToMS(const Time: String; const FPS: Single): Integer;
begin
  if StringToTime(Time) = -1 then Result := -1 else
  Result := StringToTime(Copy(Time, 1, 8)) + Integer(FramesToTime(StrToIntDef(Copy(Time, 10, 2), 0), FPS));
end;

// -----------------------------------------------------------------------------

end.
