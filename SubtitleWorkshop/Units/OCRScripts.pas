unit OCRScripts;

// -----------------------------------------------------------------------------

interface

// -----------------------------------------------------------------------------

uses SysUtils, HTMLPars, FastStrings, Functions, RegExpr;

// -----------------------------------------------------------------------------
type
  TOCRError = record
    Find                 : String;
    ReplaceBy            : String;
    UseRE                : Boolean;
    UseREOnlyToFind      : Boolean;
    // Only if UseRE is false
    CaseSensitive        : Boolean;
    WholeWord            : Boolean;
    PreserveCase         : Boolean;
  end;

// -----------------------------------------------------------------------------

function ParseOCRErrors(const FileName: String): Boolean;
function FixOCRErrors(Text: String): String;
function HasOCRErrors(const Text: String): Boolean;

// -----------------------------------------------------------------------------

var
  OCRErrors    : array of TOCRError;
  OCRMax       : Integer;
  OCRWordChars : String;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------
                        
function ParseOCRErrors(const FileName: String): Boolean;
var
  Parser : THTMLParser;
  Obj    : TObject;
  Tag    : THTMLTag;
  Param  : THTMLParam;
  i, a   : Integer;
  AMax   : Integer;
begin
  Result := False;
  SetLength(OCRErrors, 0);
  AMax := 0;
  if FileExists(FileName) then
  begin
    Parser := THTMLParser.Create;
    Parser.Lines.LoadFromFile(FileName);
    Parser.Execute;
    for i := 0 to Parser.Parsed.Count-1 do
    begin
      Obj := Parser.Parsed[i];
      if Obj.ClassType = THTMLTag then
      begin
        Tag := THTMLTag(Obj);
        if (i = 0) and (Tag.Name <> 'SWOCR') then
          exit else
        begin
          if Tag.Params.Count > 0 then
          begin
            Param := Tag.Params[0];
            if Param.Key = 'WORDCHARS' then
            begin
              OCRWordChars := Param.Value;
              if OCRWordChars = '' then
                OCRWordChars := '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_Ò—·ÈÌÛ˙¡…Õ”⁄‰ÎÔˆ¸';
            end;
          end else
            OCRWordChars := '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_Ò—·ÈÌÛ˙¡…Õ”⁄‰ÎÔˆ¸';

          Result := True;
          if Tag.Name = 'ERROR' then
          begin
            SetLength(OCRErrors, AMax + 1);
            OCRErrors[AMax].UseRE           := True;
            OCRErrors[AMax].UseREOnlyToFind := True;
            OCRErrors[AMax].CaseSensitive   := False;
            OCRErrors[AMax].WholeWord       := False;
            OCRErrors[AMax].PreserveCase    := False;
            for a := 0 to Tag.Params.Count-1 do
            begin
              Param := Tag.Params[a];
              if Param.Key = 'FIND' then
                OCRErrors[AMax].Find := Param.Value;
              if Param.Key = 'REPLACEBY' then
                OCRErrors[AMax].ReplaceBy := Param.Value;
              // Read values from tag parameters
              if Param.Key = 'USERE' then
                OCRErrors[AMax].UseRE := StrToBool(Param.Value);
              if Param.Key = 'USEREONLYTOFIND' then
                OCRErrors[AMax].UseREOnlyToFind := StrToBool(Param.Value);
              if Param.Key = 'CASESENSITIVE' then
                OCRErrors[AMax].CaseSensitive := StrToBool(Param.Value);
              if Param.Key = 'WHOLEWORD' then
                OCRErrors[AMax].WholeWord := StrToBool(Param.Value);
              if Param.Key = 'PRESERVECASE' then
                OCRErrors[AMax].PreserveCase := StrToBool(Param.Value);
            end;
            Inc(AMax);
          end;
        end;
      end;
    end;
    Parser.Free;
    OCRMax := AMax;
  end;
end;

// -----------------------------------------------------------------------------

function RepRE(RE: String; AInputString, AReplaceString: String): String;
var
  r: TRegExpr;
  i: integer;
begin
  r := TRegExpr.Create;
  r.WordChars := OCRWordChars;
  Result := AInputString;
  i := 0;
  try
    r.Expression := RE;
    if r.Exec(AInputString) then
      repeat
        if (Length(r.Match[0]) = Length(AReplaceString)) and (Length(r.Substitute('$1')) = Length(AReplaceString)) then
          Result := Copy(Result, 1, r.MatchPos[0]-1 + i) +
                    AReplaceString +
                    Copy(AInputString, r.MatchPos[0] + r.MatchLen[0], Length(AInputString)) else
          Result := Copy(Result, 1, r.MatchPos[0]-1 + i) +
                    FastReplace(r.Match[0], r.Substitute('$1'), AReplaceString, True) +
                    Copy(AInputString, r.MatchPos[0] + r.MatchLen[0], Length(AInputString));
        i := i + Length(AReplaceString) - Length(r.Substitute('$1'));
      until not r.ExecNext;
  finally
    r.Free;
  end;
end;

// -----------------------------------------------------------------------------

function FixOCRErrors(Text: String): String;
var
  i: Integer;
begin
  if OCRMax < 1 then exit;
  for i := 0 to OCRMax-1 do
  begin
    if OCRErrors[i].UseRE then
    begin
      if OCRErrors[i].UseREOnlyToFind = True then
        Text := RepRE(OCRErrors[i].Find, Text, OCRErrors[i].ReplaceBy) else
        Text := ReplaceRegExpr(OCRErrors[i].Find, Text, OCRErrors[i].ReplaceBy);
    end else
      Text := Replace(Text, OCRErrors[i].Find, OCRErrors[i].ReplaceBy, OCRErrors[i].CaseSensitive, OCRErrors[i].WholeWord, OCRErrors[i].PreserveCase);
  end;
  Result := Text;
end;

// -----------------------------------------------------------------------------

function HasOCRErrors(const Text: String): Boolean;
begin
  Result := FixOCRErrors(Text) <> Text;
end;

// -----------------------------------------------------------------------------

end.
