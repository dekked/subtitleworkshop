unit InfoErrorsFunctions;

interface

uses Forms, SysUtils, Windows, TreeViewHandle, VirtualTrees, StrMan, RegExpr,  
     USubtitlesFunctions, HTMLPars, Functions, General, FastStrings, OCRScripts,
     Undo;

// -----------------------------------------------------------------------------

type
  TUnnecessarySpaces = set of (EntersAndSpacesBeginningEnd,
                               SpacesBetweenEnters,
                               DoubleSpacesAndEnters,
                               SpacesFrontPunctuation,
                               SpacesAfterQuestionAndExclamation,
                               SpacesBeforeQuestionAndExclamation,
                               SpacesBetweenNumbers);
  TErrors = record
    eLinesWithoutLetters   : Boolean;
    eEmptySubtitle         : Boolean;
    // ------------------------------
    eOverlapping           : Boolean;
    eBadValues             : Boolean;
    eTooLongDurations      : Boolean;
    eTooShortDurations     : Boolean;
    eTooLongLines          : Boolean;
    eOverTwoLines          : Boolean;
    // ------------------------------
    eHearingImpaired       : Boolean;
    eTextBeforeColon       : Boolean;
     eOnlyIfCapitalLetters : Boolean;
    eUnnecessaryDots       : Boolean;
    eProhibitedCharacter   : Boolean;
    eRepeatedCharacter     : Boolean;
    eRepeatedSubtitle      : Boolean;
    eOCRErrors             : Boolean;
    // ------------------------------
    eOpnDlgSubsOneLine     : Boolean;
    eSpaceAfterCustChars   : Boolean;
    eSpaceBeforeCustChars  : Boolean;
    eUnnecessarySpaces     : Boolean;
    eWhatUnnecessarySpaces : TUnnecessarySpaces;
  end;
  TFixTypes = (ftLinesWithoutLettersDeleted,
               ftEmptySubtitleDeleted,
               // ------------------------------
               ftOverlapping,
               ftBadValues,
               ftOverTwoLinesAdjusted,
               // ------------------------------
               ftHearingImpairedDeleted,
               ftHearingImpairedPartDeleted,
               ftTextBeforeColon,
               ftUnnecessaryDots,
               ftSubDeletedProhibitedCharacter,
               ftRepeatedCharacter,
               ftRepeatedSubtitle,
               ftOCRErrors,
               // ------------------------------
               ftOpnDlgOneLineSubDeleted,
               ftSpaceAfterCustChars,
               ftSpaceBeforeCustChars,
               ftUnnecessarySpaces
               );
  TFixTypesSet = set of TFixTypes;

// -----------------------------------------------------------------------------

function IsUpperCase(Str: String): Boolean;
function IsEmpty(const Text: String): Boolean;
function HasLinesWithoutLetters(Text: String): Boolean;
function IsHearingImpaired(Text: String): Boolean;
function HasTextBeforeColon(Text: String; CapitalLetters: Boolean): Boolean;
function HasUnnecessaryDots(Text: String): Boolean;
function CountLines(Text: String): Integer;
function HasProhibitedChar(Text: String): Boolean;
function HasRepeatedChar(Text: String): Boolean;
function HasSpaceAfterOrBeforeCustomChar(const Text: String; const After: Boolean): Boolean;
function HasUnnecessarySpaces(Text: String): Boolean;
function HasTooLongLine(Text: String): Boolean;
// --
function GetError(Node, Previous: PVirtualNode): TErrorTypeSet;
procedure CheckMarkErrors;
// --
function RemoveHearingImpaired(Text: String): String;
function RemoveTextBeforeColon(Text: String; CapitalLetters: Boolean): String;
function RemoveUnnecessaryDots(Text: String): String;
function RemoveRepeatedChar(Text: String): String;
function FixSpaceAfterOrBeforeCustomChar(Text: String; After: Boolean): String;
function RemoveUnnecessarySpaces(Text: String): String;
// --
function FixError(Node, Previous: PVirtualNode; ConfirmDeletions: Boolean; FormParent: TForm): TFixTypesSet;
procedure FixErrors(const OnlySelected: Boolean = False);
// --

// -----------------------------------------------------------------------------

var
  ErrorsToCheck : TErrors;
  ErrorsToFix   : TErrors;
  // -------------------------//
  //  Information and errors  //
  // -------------------------//
  ShowConfMainForm     : Boolean;
  MarkErrorsInList     : Boolean;
  MarkWithColor        : Integer;
  MarkBold             : Boolean;
  MarkItalic           : Boolean;
  MarkUnderline        : Boolean;
  MarkOnLoad           : Boolean;
  FixOnLoad            : Boolean;
  FixOneUnitOverlap    : Boolean;
  OCRDefFile           : String;
  RepeatableChars      : String;
  ProhibitedChars      : String;
  SpaceAfterChars      : String;
  SpaceBeforeChars     : String;
  ToleranceForRepeated : Integer;
  TooLongDuration      : Integer;
  TooShortDuration     : Integer;
  TooLongLine          : Integer;
  // -----------
  CancelProcess: Boolean;

implementation

// -----------------------------------------------------------------------------

uses formMain, formInfoErrors;

// -----------------------------------------------------------------------------

function IsUpperCase(Str: String): Boolean;
begin
  Result := False;
  if AnsiUpperCase(Str) = Str then
    Result := True;
end;

// -----------------------------------------------------------------------------

function DeleteSpecialChars(const Str: String): String;
var
  i      : Integer;
  tmpStr : String;
begin
  tmpStr := Str;
  if tmpStr <> '' then
  begin
    i := 1;
    while i <= Length(tmpStr) do
    begin
      if (tmpStr[i] in SpecialChars) then
        Delete(tmpStr, i, 1) else
        Inc(i);
    end;
  end;
  Result := Trim(tmpStr);
end;

// -----------------------------------------------------------------------------

function IsEmpty(const Text: String): Boolean;
begin
  Result := False;
  if DeleteSpecialChars(Text) = '' then
    Result := True;
end;

// -----------------------------------------------------------------------------

function HasLinesWithoutLetters(Text: String): Boolean;
  // We need to check every line separately...
  function CheckLine(Line: String): Boolean;
  begin
    Result := False;
    if DeleteSpecialChars(Line) = '' then
      Result := True;
  end;
var
  CurrLine : String;
  PosEnter : Integer;
begin
  Result   := False;
  CurrLine := '';
  PosEnter := Pos(#13#10, Text);
  // For multi line subtitles...
  while PosEnter > 0 do
  begin
    if CheckLine(Copy(Text, 1, PosEnter-1)) then
    begin
      Result := True;
      exit;
    end;
    Text     := Copy(Text, PosEnter + 2, Length(Text) - PosEnter);
    PosEnter := Pos(#13#10, Text);
  end;
  if CheckLine(Text) then
    Result := True;
end;

// -----------------------------------------------------------------------------

function IsHearingImpaired(Text: String): Boolean;
begin
  Result := False;
  if ((Pos('(', Text) > 0) and (Pos(')', Text) > Pos('(', Text))) or
     ((Pos('[', Text) > 0) and (Pos(']', Text) > Pos('[', Text))) or
     ((Pos('<', Text) > 0) and (Pos('>', Text) > Pos('<', Text))) then
  Result := True;
end;

// -----------------------------------------------------------------------------

function HasTextBeforeColon(Text: String; CapitalLetters: Boolean): Boolean;
  // We need to check every line separately...
  function CheckLine(Line: String): Boolean;
  var
    a: Integer;
  begin
    Result := False;
    a := Pos(':', Line);
    if a > 1 then
    begin
      if CapitalLetters then
      begin
        if IsUpperCase(Copy(Line, 1, a-1)) then
          Result := True;
      end else
        Result := True;
      // If colon is between two numbers
      if IsInteger(Copy(Line, a-1, 1)) and (IsInteger(Copy(Line, a+1,1))) then
        Result := False;
    end;
  end;
var
  PosEnter : Integer;
begin
  Result   := False;
  if Pos(':', Text) = Length(Text) then exit;
  PosEnter := Pos(#13#10, Text);
  // For multi line subtitles...
  while PosEnter > 0 do
  begin
    if CheckLine(Copy(Text, 1, PosEnter-1)) then
    begin
      Result := True;
      exit;
    end;
    Text     := Copy(Text, PosEnter + 2, Length(Text) - PosEnter);
    PosEnter := Pos(#13#10, Text);
  end;
  // If subtitle is one line or contains the text in the last line
  if CheckLine(Text) then
    Result := True;
end;

// -----------------------------------------------------------------------------

function HasUnnecessaryDots(Text: String): Boolean;
begin
  Result := False;
  if Pos('....', Text) > 0 then
    Result := True;
end;

// -----------------------------------------------------------------------------

function CountLines(Text: String): Integer;
begin
  Result := StringCount(#13#10, Text) + 1;
end;

// -----------------------------------------------------------------------------

function HasProhibitedChar(Text: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(Text) = 0 then exit;
  for i := 0 to Length(Text) do
  begin
    if (Text[i] <> ',') and (Pos(Text[i], ProhibitedChars) > 0) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HasRepeatedChar(Text: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(Text)-1 do
  begin
    if (Pos(Text[i], RepeatableChars) > 0) and (Text[i+1] = Text[i]) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HasSpaceAfterOrBeforeCustomChar(const Text: String; const After: Boolean): Boolean;
begin
  Result := not(FixSpaceAfterOrBeforeCustomChar(Text, After) = Text);
end;

// -----------------------------------------------------------------------------

function HasUnnecessarySpaces(Text: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Length(Text) = 0) then exit;
  if (sm.Trim(Text) = '') then
  begin
    Result := True;
    exit;
  end;
  if EntersAndSpacesBeginningEnd in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    if ((Pos(#13#10, Text) = 1) or ((Length(Text) > 1) and (SmartPos(#13#10, Text, True, Length(Text), False) = Length(Text)-1))) or
       (Text[1] = ' ') or (SmartPos(' ', Text, True, Length(Text), False) = Length(Text)) then
    begin
      Result := True;
      exit;
    end;
  end;
  if SpacesBetweenEnters in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    if (Pos(' ' + #13#10, Text) > 0) or (Pos(#13#10 + ' ', Text) > 0) then
    begin
      Result := True;
      exit;
    end;
  end;
  if DoubleSpacesAndEnters in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    if (Pos('  ', Text) > 0) or (Pos(#13#10#13#10, Text) > 0) then
    begin
      Result := True;
      exit;
    end;
  end;
  if SpacesFrontPunctuation in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    if (StringCount(' ,', Text) > 0) or
       ((StringCount(' .', Text) - StringCount('- ...', Text)) > 0) or
       (StringCount(' :', Text) > 0) or
       (StringCount(' ;', Text) > 0) then
    begin
      Result := True;
      exit;
    end;
  end;
  if SpacesAfterQuestionAndExclamation in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    if (Pos('¡ ', Text) > 0) or (Pos('¿ ', Text) > 0) then
    begin
      Result := True;
      exit;
    end;
  end;
  if SpacesBeforeQuestionAndExclamation in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    if (Pos(' ?', Text) > 0) or (Pos(' !', Text) > 0) then
    begin
      Result := True;
      exit;
    end;
  end;
  if SpacesBetweenNumbers in ErrorsToCheck.eWhatUnnecessarySpaces then
  begin
    for i := 0 to Length(Text)-2 do
    begin
      if (Text[i] in ['0'..'9', '/']) and
         (Text[i+1] = ' ') and
         (Text[i+2] in ['0'..'9', ',', '.', '-', ':', '/']) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function HasTooLongLine(Text: String): Boolean;
var
  PosEnter : Integer;
begin
  Result   := False;
  PosEnter := Pos(#13#10, Text);
  // For multi line subtitles...
  while PosEnter > 0 do
  begin
    if PosEnter-1 >= TooLongLine then
    begin
      Result := True;
      exit;
    end;
    Text     := Copy(Text, PosEnter + 2, Length(Text) - PosEnter);
    PosEnter := Pos(#13#10, Text);
  end;
  if Length(Text) >= TooLongLine then
    Result := True;
end;

// -----------------------------------------------------------------------------

function GetError(Node, Previous: PVirtualNode): TErrorTypeSet;
var
  Data    : PSubtitleItem; // Node
  DataP   : PSubtitleItem; // Previous
  SubText : String;
begin
  DataP := nil;
  Data  := frmMain.lstSubtitles.GetNodeData(Node);
  if Assigned(Previous) then
    DataP := frmMain.lstSubtitles.GetNodeData(Previous);
  SubText := RemoveSWTags(Data.Text, True, True, True, True);

  Data.ErrorType := [];

  // --------------------- //
  // Lines without letters //
  // --------------------- //
  if ErrorsToCheck.eLinesWithoutLetters then
  begin
    if HasLinesWithoutLetters(SubText) then
      Data.ErrorType := Data.ErrorType + [etLinesWithoutLetters];
  end;

  // --------------- //
  // Empty subtitles //
  // --------------- //
  if ErrorsToCheck.eEmptySubtitle then
  begin
    if IsEmpty(SubText) then
      Data.ErrorType := Data.ErrorType + [etEmptySubtitle];
  end;    

  // ----------- //
  // Overlapping //
  // ----------- //
  if ErrorsToCheck.eOverlapping then
  begin
    if Node <> frmMain.lstSubtitles.GetFirst then
    begin
      if Data.InitialTime <= DataP.FinalTime then
        Data.ErrorType := Data.ErrorType + [etOverlapping];
    end;
  end;

  // ---------- //
  // Bad values //
  // ---------- //
  if ErrorsToCheck.eBadValues then
  begin
    if Data.InitialTime > Data.FinalTime then
      Data.ErrorType := Data.ErrorType + [etBadValues];
  end;

  // ---------------- //
  // Hearing impaired //
  // ---------------- //
  if ErrorsToCheck.eHearingImpaired then
  begin
    if IsHearingImpaired(SubText) then
      Data.ErrorType := Data.ErrorType + [etHearingImpaired];
  end;

  // ---------------------------- //
  // Something before colon (":") //
  // ---------------------------- //
  if ErrorsToCheck.eTextBeforeColon then
  begin
    if HasTextBeforeColon(SubText, ErrorsToCheck.eOnlyIfCapitalLetters) then
      Data.ErrorType := Data.ErrorType + [etTextBeforeColon];
  end;

  // ---------------- //
  // Unnecessary dots //
  // ---------------- //
  if ErrorsToCheck.eUnnecessaryDots then
  begin
    if HasUnnecessaryDots(SubText) then
      Data.ErrorType := Data.ErrorType + [etUnnecessaryDots];
  end;

  // ----------------------- //
  // Subtitle over two lines //
  // ----------------------- //
  if ErrorsToCheck.eOverTwoLines then
  begin
    if CountLines(SubText) > 2 then
      Data.ErrorType := Data.ErrorType + [etOverTwoLines];
  end;

  // -------------------- //
  // Prohibited character //
  // -------------------- //
  if ErrorsToCheck.eProhibitedCharacter then
  begin
    if HasProhibitedChar(SubText) then
      Data.ErrorType := Data.ErrorType + [etProhibitedCharacter];
  end;

  // ------------------ //
  // Repeated character //
  // ------------------ //
  if ErrorsToCheck.eRepeatedCharacter then
  begin
    if HasRepeatedChar(SubText) then
      Data.ErrorType := Data.ErrorType + [etRepeatedCharacter];
  end;

  // ----------------- //
  // Repeated subtitle //
  // ----------------- //
  if ErrorsToCheck.eRepeatedSubtitle then
  begin
    if Node <> frmMain.lstSubtitles.GetFirst then
    begin
      if ((DataP.Text = Data.Text) and (Data.Text <> '')) and // If the text is the same (we don't use SubText because Data.Text contains the tags)
         ((Data.InitialTime - DataP.FinalTime) < ToleranceForRepeated) then // And the difference of time is less than the tolerance
        Data.ErrorType := Data.ErrorType + [etRepeatedSubtitle];
    end;
  end;

  // ---------- //
  // OCR Errors //
  // ---------- //
  if (ErrorsToCheck.eOCRErrors) and (OCRMax > 0) then
  begin
    if HasOCRErrors(SubText) then
      Data.ErrorType := Data.ErrorType + [etOCRErrors];
  end;

  // ------------------------------- //
  // "- " in subtitles with one line //
  // ------------------------------- //
  if ErrorsToCheck.eOpnDlgSubsOneLine then
  begin
    if CountLines(SubText) = 1 then
    begin
      if Copy(SubText, 1, 1) = '-' then
        Data.ErrorType := Data.ErrorType + [etOpnDlgSubsOneLine];
    end;
  end;

  // ----------------------------- //
  // Space after custom characters //
  // ----------------------------- //
  if ErrorsToCheck.eSpaceAfterCustChars then
  begin
    if HasSpaceAfterOrBeforeCustomChar(SubText, True) then
      Data.ErrorType := Data.ErrorType + [etSpaceAfterCustChars];
  end;

  // ------------------------------ //
  // Space before custom characters //
  // ------------------------------ //
  if ErrorsToCheck.eSpaceBeforeCustChars then
  begin
    if HasSpaceAfterOrBeforeCustomChar(SubText, False) then
      Data.ErrorType := Data.ErrorType + [etSpaceAfterCustChars];
  end;

  // ------------------ //
  // Unnecessary spaces //
  // ------------------ //
  if ErrorsToCheck.eUnnecessarySpaces then
  begin
    if HasUnnecessarySpaces(SubText) then
      Data.ErrorType := Data.ErrorType + [etUnnecessarySpaces];
  end;

  // ----------------- //
  // Too long duration //
  // ----------------- //
  if ErrorsToCheck.eTooLongDurations then
  begin
    if (Data.FinalTime-Data.InitialTime) > TooLongDuration then
      Data.ErrorType := Data.ErrorType + [etTooLongDuration];
  end;

  // ------------------ //
  // Too short duration //
  // ------------------ //
  if ErrorsToCheck.eTooShortDurations then
  begin
    if (Data.FinalTime-Data.InitialTime) < TooShortDuration then
      Data.ErrorType := Data.ErrorType + [etTooShortDuration];
  end;

  // -------------- //
  // Too long lines //
  // -------------- //
  if ErrorsToCheck.eTooLongLines then
  begin
    if HasTooLongLine(SubText) then
      Data.ErrorType := Data.ErrorType + [etTooLongLine];
  end;

  Result := Data.ErrorType;
end;

// -----------------------------------------------------------------------------

procedure CheckMarkErrors;
var
  Node: PVirtualNode;
begin
  if (ErrorsToCheck.eOCRErrors) and (FileExists(OCRDefFile)) then
    ParseOCRErrors(OCRDefFile);
  with frmMain do
  begin
    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      GetError(Node, Node.PrevSibling);
      Node := Node.NextSibling;
    end;
    lstSubtitles.Refresh;
  end;
end;

// -----------------------------------------------------------------------------

function DeleteLinesWithoutLetters(Text: String): String;
  function CheckLine(Line: String): Boolean;
  begin
    Result := False;
    if DeleteSpecialChars(Line) = '' then
      Result := True;
  end;
var
  NewText  : String;
  PosEnter : Integer;
begin
  Result   := Text;
  NewText  := '';
  PosEnter := Pos(#13#10, Text);
  // For multi line subtitles...
  while PosEnter > 0 do
  begin
    if CheckLine(Copy(Text, 1, PosEnter-1)) = False then
      NewText := NewText + #13#10 + Copy(Text, 1, PosEnter-1);
    Text     := Copy(Text, PosEnter + 2, Length(Text) - PosEnter);
    PosEnter := Pos(#13#10, Text);
  end;
  if CheckLine(Text) = False then
    NewText := NewText + #13#10 + Text;
  // We remove the #13#10 at the beginning
  Result := Copy(NewText, 3, Length(NewText));
end;

// -----------------------------------------------------------------------------

function RemoveHearingImpaired(Text: String): String;
  function RHearingImpaired(Line: String): String;
  begin
    while (Pos('(', Line) > 0) and (Pos(')', Line) > Pos('(', Line)) do
    begin
      if Copy(Line, Pos(')', Line) + 1, 1) = ':' then
        Delete(Line, Pos(')', Line) + 1, 1);
      Delete(Line, Pos('(', Line), Pos(')', Line) - Pos('(', Line) + 1);
    end;
    while (Pos('[', Line) > 0) and (Pos(']', Line) > Pos('[', Line)) do
    begin
      if Copy(Line, Pos(']', Line) + 1, 1) = ':' then
        Delete(Line, Pos(']', Line) + 1, 1);
      Delete(Line, Pos('[', Line), Pos(']', Line) - Pos('[', Line) + 1);
    end;
    if IsEmpty(Line) then Line := '';
    Result := Line;
  end;
var
  PosEnter : Integer;
  A, B     : String;
begin
  Result := '';
  if Text <> '' then
  begin
    A := Text;
    B := '';
    PosEnter := Pos(#13#10, A);
    while PosEnter > 0 do
    begin
      B     := B + RHearingImpaired(Copy(A, 1, PosEnter-1)) + #13#10;
      A     := Copy(A, PosEnter + 2, Length(A) - PosEnter);
      PosEnter := Pos(#13#10, A);
    end;
    B := RemoveUnnecessarySpaces(RHearingImpaired(B + RHearingImpaired(A)));
    if (Pos(#13#10, B) = 0) and (Copy(B, 1, 1) = '-') then
    begin
      B := Copy(B, 2, Length(B));
      Result := RemoveUnnecessarySpaces(B);
    end else
      Result := B;
  end;
end;

// -----------------------------------------------------------------------------

function RemoveTextBeforeColon(Text: String; CapitalLetters: Boolean): String;
  // We need to delete text before colon in every line, separately...
  function RTextBeforeColon(Line: String): String;
  var
    a: Integer;
  begin
    a := Pos(':', Line);
    if (a > 1) then
    begin
      if (IsInteger(Copy(Line, a-1, 1)) and (IsInteger(Copy(Line, a+1,1)))) = False then
      begin
        if CapitalLetters then
        begin
          if IsUpperCase(Copy(Line, 1, a-1)) then
            Delete(Line, 1, a);
        end else
          Delete(Line, 1, a);
      end;
    end;
    Result := sm.TrimRight(Line);
  end;
var
  PosEnter : Integer;
  A, B     : String;
begin
  A := Text;
  B := '';
  if Pos(':', Text) <> Length(Text) then
  begin
    PosEnter := Pos(#13#10, A);
    while PosEnter > 0 do
    begin
      B     := B + RTextBeforeColon(Copy(A, 1, PosEnter-1)) + #13#10;
      A     := Copy(A, PosEnter + 2, Length(A) - PosEnter);
      PosEnter := Pos(#13#10, A);
    end;
    B      := RTextBeforeColon(B + RTextBeforeColon(A));
    Result := RemoveUnnecessarySpaces(B);
  end else
  Result := Text;
end;

// -----------------------------------------------------------------------------

function RemoveUnnecessaryDots(Text: String): String;
begin
  while Pos('....', Text) > 0 do
    Delete(Text, Pos('....', Text), 1);
  Result := Text;
end;

// -----------------------------------------------------------------------------

function RemoveRepeatedChar(Text: String): String;
var
  i: Integer;
begin
  for i := Length(Text) downto 1 do
  begin
    if (Pos(Text[i], RepeatableChars) > 0)and (Text[i-1] = Text[i]) then
      Delete(Text, i, 1);
  end;
  Result := Text;
end;

// -----------------------------------------------------------------------------

function FixSpaceAfterOrBeforeCustomChar(Text: String; After: Boolean): String;
  function FixSpaceAfterOrBeforeCustChar(Line: String): String;
  var
    i: Integer;
  begin
    if After then
    begin
      // ---------------------------- //
      // Space after custom character //
      // ---------------------------- //
      i := 1;
      while i < Length(Line) do
      begin
        if Pos(Line[i], SpaceAfterChars) > 0 then
        begin
          if ((Line[i+1] <> ' ') and (Line[i+1] in ['0'..'9'] = False)) and // If it's not a space nor a number
             (Pos(Line[i+1], SpaceAfterChars) = 0) and  // nor another character after which we need to add a space
             (
             (i > 2) and (
                         ((Line[i] <> '-') and (Line[i-1] in (SpecialChars - [' ']) = False) and (Line[i+1] in SpecialChars = False)) or
                         ((Line[i] = '-') and (Line[i-1] in (SpecialChars - [' '])))
                         ) or
             (i <= 2)

             ) then
          begin
            Insert(' ', Line, i+1);
            Inc(i);
          end;
        end;  
        Inc(i);
      end;
    end else
    begin
      // ----------------------------- //
      // Space before custom character //
      // ----------------------------- //
      i := 2;
      while i <= Length(Line) do
      begin
        if Pos(Line[i], SpaceBeforeChars) > 0 then
        begin
          if (Line[i-1] <> ' ') and                     // If it's not a space
             (Pos(Line[i-1], SpaceBeforeChars) = 0) and  // nor another character after which we need to add a space
             (
               ((Line[i] = '-') and (Line[i-1] in SpecialChars)) or
               (Line[i] <> '-')
             ) then
          begin
            Insert(' ', Line, i);
            Inc(i);
          end;
        end;  
        Inc(i);
      end;
    end;
    
    Result := Line;
  end;
var
  PosEnter : Integer;
  A, B     : String;
begin
  A := Text;
  B := '';
  PosEnter := Pos(#13#10, A);
  while PosEnter > 0 do
  begin
    B     := B + FixSpaceAfterOrBeforeCustChar(Copy(A, 1, PosEnter-1)) + #13#10;
    A     := Copy(A, PosEnter + 2, Length(A) - PosEnter);
    PosEnter := Pos(#13#10, A);
  end;
  Result := B + FixSpaceAfterOrBeforeCustChar(A);
end;

// -----------------------------------------------------------------------------

function RemoveUnnecessarySpaces(Text: String): String;
  // Delete unnecessary spaces in each line
  function RUnnecessarySpaces(Line: String): String;
  var
    i: Integer;
  begin
    if (Length(Line) > 0) then
    begin
      if EntersAndSpacesBeginningEnd in ErrorsToFix.eWhatUnnecessarySpaces then
        Line := sm.Trim(Line);
      if SpacesBetweenEnters in ErrorsToFix.eWhatUnnecessarySpaces then
      begin
        while (Pos(' ' + #13#10, Line) > 0) do
          Delete(Line, Pos(' ' + #13#10, Line), 1);
        while (Pos(#13#10 + ' ', Line) > 0) do
          Delete(Line, Pos(#13#10 + ' ', Line) + 2, 1);
      end;
      if DoubleSpacesAndEnters in ErrorsToFix.eWhatUnnecessarySpaces then
      begin
        while (Pos('  ', Line) > 0) do
          Delete(Line, Pos('  ', Line), 1);
        while (Pos(#13#10#13#10, Line) > 0) do
          Delete(Line, Pos(#13#10#13#10, Line), 2);
      end;
      if SpacesFrontPunctuation in ErrorsToFix.eWhatUnnecessarySpaces then
      begin
        while (Pos(' ,', Line) > 0) do
          Delete(Line, Pos(' ,', Line), 1);
        i := Pos(' .', Line);
        while (i > 0) do
        begin
          if Copy(Line, i-1, 5) <> '- ...' then
            Delete(Line, i, 1);
          i := SmartPos(' .', Line, True, i+1);
        end;
        while (Pos(' :', Line) > 0) do
          Delete(Line, Pos(' :', Line), 1);
        while (Pos(' ;', Line) > 0) do
          Delete(Line, Pos(' ;', Line), 1);
      end;
      if SpacesAfterQuestionAndExclamation in ErrorsToFix.eWhatUnnecessarySpaces then
      begin
        while (Pos('¡ ', Line) > 0) do
          Delete(Line, Pos('¡ ', Line) + 1, 1);
        while (Pos('¿ ', Line) > 0) do
          Delete(Line, Pos('¿ ', Line) + 1, 1);
      end;
      if SpacesBeforeQuestionAndExclamation in ErrorsToFix.eWhatUnnecessarySpaces then
      begin
        while (Pos(' !', Line) > 0) do
          Delete(Line, Pos(' !', Line), 1);
        while (Pos(' ?', Line) > 0) do
          Delete(Line, Pos(' ?', Line), 1);
      end;
      if SpacesBetweenNumbers in ErrorsToFix.eWhatUnnecessarySpaces then
      begin
        for i := 0 to Length(Line)-2 do
        begin
          if (Line[i] in ['0'..'9', '/']) and
             (Line[i+1] = ' ') and
             (Line[i+2] in ['0'..'9', ',', '.', '-', ':', '/']) then
            Delete(Line, i+1, 1);
        end;
      end;
    end;
    Result := Line;
  end;
var
  PosEnter : Integer;
  A, B     : String;
begin
  A := Text;
  B := '';
  if Length(Text) > 0 then
  begin
    PosEnter := Pos(#13#10, A);
    while PosEnter > 0 do
    begin
      B     := B + RUnnecessarySpaces(Copy(A, 1, PosEnter-1)) + #13#10;
      A     := Copy(A, PosEnter + 2, Length(A) - PosEnter);
      PosEnter := Pos(#13#10, A);
    end;
    B      := RUnnecessarySpaces(B + RUnnecessarySpaces(A));
    Result := B;
  end else
    Result := '';
end;

// -----------------------------------------------------------------------------

function FixError(Node, Previous: PVirtualNode; ConfirmDeletions: Boolean; FormParent: TForm): TFixTypesSet;
var
  Data    : PSubtitleItem; // Node
  DataP   : PSubtitleItem; // Previous
  SubText : String;
  tmpText : String;
  // Variables to store tags
  Bold      : Boolean;
  Italic    : Boolean;
  Underline : Boolean;
  SubColor  : Integer;
  // -
  i: Integer;
  OVERLAPPRECISION: Integer;
begin
  Result := [];
  DataP := nil;
  Data  := frmMain.lstSubtitles.GetNodeData(Node);
  if Assigned(Previous) then
    DataP := frmMain.lstSubtitles.GetNodeData(Previous);

  // Store tags
  Bold      := Pos('<b>', Data.Text) > 0;
  Italic    := Pos('<i>', Data.Text) > 0;
  Underline := Pos('<u>', Data.Text) > 0;
  SubColor  := GetSubColor(Data.Text);
  // Important!!!: Remove tags
  SubText := RemoveSWTags(Data.Text, True, True, True, True);

  // --------------------- //
  // Lines without letters //
  // --------------------- //
  if ErrorsToFix.eLinesWithoutLetters then
  begin
    tmpText := DeleteLinesWithoutLetters(SubText);
    if tmpText <> SubText then
    begin
      SubText := tmpText;
      Result  := Result + [ftLinesWithoutLettersDeleted];
    end;
  end;

  // --------------- //
  // Empty subtitles //
  // --------------- //
  if ErrorsToFix.eEmptySubtitle then
  begin
    if IsEmpty(SubText) then
    begin
      if ConfirmDeletions = False then
        Result := Result + [ftEmptySubtitleDeleted] else
      begin
        case MsgBox(Format(IEMsgBoxes[1], [Node.Index + 1, Data.Text]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, FormParent) of
          1: Result := Result + [ftEmptySubtitleDeleted];
          2: Result := [];
          3: CancelProcess := True;
        end;
      end;
      exit;
    end;
  end;

  // ----------------- //
  // Repeated subtitle //
  // ----------------- //
  if ErrorsToFix.eRepeatedSubtitle then
  begin
    if Node <> frmMain.lstSubtitles.GetFirst then
    begin
      if ((DataP.Text = Data.Text) and (Data.Text <> '')) and // If the text is the same (we don't use SubText because Data.Text contains the tags)
         ((Data.InitialTime - DataP.FinalTime) < ToleranceForRepeated) then // And the difference of time is less than the tolerance
      begin
        DataP.FinalTime := Data.FinalTime;
        Result          := Result + [ftRepeatedSubtitle];
        exit;
      end;
    end;
  end;

  // -------------------- //
  // Prohibited character //
  // -------------------- //
  if ErrorsToFix.eProhibitedCharacter then
  begin
    if HasProhibitedChar(SubText) then
    begin
      if ConfirmDeletions = False then
        Result := Result + [ftSubDeletedProhibitedCharacter] else
      begin
        case MsgBox(Format(IEMsgBoxes[2], [Node.Index + 1, Data.Text]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, FormParent) of
          1: Result := Result + [ftSubDeletedProhibitedCharacter];
          2: Result := [];
          3: CancelProcess := True;
        end;
      end;
      exit;
    end;
  end;

  // ----------------------- //
  // Text before colon (":") //
  // ----------------------- //
  if ErrorsToFix.eTextBeforeColon then
  begin
    if Pos(':', SubText) > 0 then
    begin
      tmpText := RemoveTextBeforeColon(SubText, ErrorsToFix.eOnlyIfCapitalLetters);
      if tmpText <> SubText then
      begin
        if ConfirmDeletions = False then
        begin
          SubText := tmpText;
          Result  := Result + [ftTextBeforeColon];
        end else
        begin
          case MsgBox(Format(IEMsgBoxes[4], [Node.Index + 1, SubText, tmpText]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, FormParent) of
            1: begin SubText := tmpText; Result := Result + [ftTextBeforeColon]; end;
            2: Result := [];
            3: CancelProcess := True;
          end;
        end;
      end;
    end;
  end;

  // ---------------- //
  // Hearing impaired //
  // ---------------- //
  if ErrorsToFix.eHearingImpaired then
  begin
    if IsHearingImpaired(SubText) then
    begin
      tmpText := RemoveHearingImpaired(SubText);
      if tmpText <> SubText then
      begin
        // We have to delete whole subtitle
        if tmpText = '' then
        begin
          if ConfirmDeletions = False then
          begin
            Result := Result + [ftHearingImpairedDeleted];
            exit;
          end else
          begin
            case MsgBox(Format(IEMsgBoxes[3], [Node.Index + 1, Data.Text]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, FormParent) of
              1: begin Result := Result + [ftHearingImpairedDeleted]; exit; end;
              2: Result := [];
              3: begin CancelProcess := True; exit; end;
            end;
          end;
        end else
        begin
          // We only have to delete the hearing impaired part, not all
          if ConfirmDeletions = False then
          begin
            SubText := tmpText;
            Result  := Result + [ftHearingImpairedPartDeleted];
          end else
          begin
            case MsgBox(Format(IEMsgBoxes[4], [Node.Index + 1, Data.Text, tmpText]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, FormParent) of
              1: begin SubText := tmpText; Result := Result + [ftHearingImpairedPartDeleted]; end;
              2: Result := [];
              3: CancelProcess := True;
            end;
          end;
        end;
      end;
    end;
  end;

  // ----------- //
  // Overlapping //
  // ----------- //
  if (ErrorsToFix.eOverlapping) then
  begin
    if Node <> frmMain.lstSubtitles.GetFirst then
    begin
      if Data.InitialTime <= DataP.FinalTime then
      begin
        if frmMain.FormatType = ftTime then
          OVERLAPPRECISION := ID_TIMEOVERLAPPRECISION else
          OVERLAPPRECISION := FramesToTime(TimeToFrames(ID_TIMEOVERLAPPRECISION, GetFPS), GetFPS) + 10;
          
        tmpText := RemoveSWTags(DataP.Text, True, True, True, True);
        if Abs(Length(Data.Text) - Length(tmpText)) < 5 then // Set appropiate length for each subtitle
        begin
          i                := Data.FinalTime - DataP.InitialTime;
          DataP.FinalTime  := DataP.InitialTime + (i div 2);
          Data.InitialTime := DataP.FinalTime + OVERLAPPRECISION;
        end else
        begin
          // We cut smaller string
          if Length(Data.Text) < Length(tmpText) then
            Data.InitialTime := DataP.FinalTime + OVERLAPPRECISION else
        end;
        DataP.FinalTime := Data.InitialTime - OVERLAPPRECISION;

        Result := Result + [ftOverlapping];
      end;
    end;
  end;

  // ---------- //
  // Bad values //
  // ---------- //
  if ErrorsToFix.eBadValues then
  begin
    if Data.InitialTime > Data.FinalTime then
    begin
      i                := Data.InitialTime;
      Data.InitialTime := Data.FinalTime;
      Data.FinalTime   := i;
      Result           := Result + [ftBadValues];
    end;
  end;

  // ---------------- //
  // Unnecessary dots //
  // ---------------- //
  if ErrorsToFix.eUnnecessaryDots then
  begin
    if HasUnnecessaryDots(SubText) then
    begin
      tmpText := RemoveUnnecessaryDots(SubText);
      if tmpText <> SubText then
      begin
        SubText := tmpText;
        Result  := Result + [ftUnnecessaryDots];
      end;
    end;
  end;

  // ------------------ //
  // Repeated character //
  // ------------------ //
  if ErrorsToFix.eRepeatedCharacter then
  begin
    tmpText := RemoveRepeatedChar(SubText);
    if tmpText <> SubText then
    begin
      SubText := tmpText;
      Result  := Result + [ftRepeatedCharacter];
    end;
  end;

  // ---------- //
  // OCR Errors //
  // ---------- //
  if (ErrorsToFix.eOCRErrors) and (OCRMax > 0) then
  begin
    tmpText := FixOCRErrors(SubText);
    if tmpText <> SubText then
    begin
      SubText := tmpText;
      Result  := Result + [ftOCRErrors];
    end;
  end;

  // ------------------------------- //
  // "- " in subtitles with one line //
  // ------------------------------- //
  if ErrorsToFix.eOpnDlgSubsOneLine then
  begin
    if CountLines(SubText) = 1 then
    begin
      tmpText := SubText;
      while Copy(tmpText, 1, 1) = '-' do
      begin
        tmpText := TrimLeft(Copy(tmpText, 2, Length(tmpText)));
        if ftOpnDlgOneLineSubDeleted in Result = False then
          Result := Result + [ftOpnDlgOneLineSubDeleted];
      end;

      if Copy(SubText, 1, 1) = '-' then
      begin
        if ConfirmDeletions = True then
        begin
          case MsgBox(Format(IEMsgBoxes[5], [Node.Index + 1, Data.Text]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, FormParent) of
            1: begin SubText := tmpText; Result := Result + [ftHearingImpairedPartDeleted]; end;
            2: Result := Result - [ftOpnDlgOneLineSubDeleted];
            3: begin Result := Result - [ftOpnDlgOneLineSubDeleted]; CancelProcess := True; end;
          end;
        end else SubText := tmpText;
      end;
    end;
  end;

  // ------------------ //
  // Unnecessary spaces //
  // ------------------ //
  if ErrorsToFix.eUnnecessarySpaces then
  begin
    tmpText := RemoveUnnecessarySpaces(SubText);
    if tmpText <> SubText then
    begin
      SubText := tmpText;
      Result  := Result + [ftUnnecessarySpaces];
    end;
  end;


  // ----------------------------- //
  // Space after custom characters //
  // ----------------------------- //
  if ErrorsToFix.eSpaceAfterCustChars then
  begin
    tmpText := FixSpaceAfterOrBeforeCustomChar(SubText, True);
    if tmpText <> SubText then
    begin
      SubText := tmpText;
      Result  := Result + [ftSpaceAfterCustChars];
    end;
  end;

  // ------------------------------ //
  // Space before custom characters //
  // ------------------------------ //
  if ErrorsToFix.eSpaceBeforeCustChars then
  begin
    tmpText := FixSpaceAfterOrBeforeCustomChar(SubText, False);
    if tmpText <> SubText then
    begin
      SubText := tmpText;
      Result  := Result + [ftSpaceBeforeCustChars];
    end;
  end;

  // ----------------------- //
  // Subtitle over two lines //
  // ----------------------- //
  if ErrorsToFix.eOverTwoLines then
  begin
    if CountLines(SubText) > 2 then
    begin
      SubText := AdjustLines(SubText);
      Result  := Result + [ftOverTwoLinesAdjusted];
    end;
  end;

  if SubtitleAPI.NoInteractionWithTags = False then
  begin
    // Restore tags...
    if Underline = True then SubText := '<u>' + SubText;
    if Bold      = True then SubText := '<b>' + SubText;
    if Italic    = True then SubText := '<i>' + SubText;
    if SubColor > -1 then
      SubText := SetColorTag(SubText, SubColor);
  end;

  // Apply modifications done to the text
  Data.Text := SubText;  
  if ((etTooLongDuration  in Data.ErrorType) = False) and
     ((etTooShortDuration in Data.ErrorType) = False) and
     ((etTooLongLine      in Data.ErrorType) = False) then
    Data.ErrorType := [];
  //Data.ErrorType := GetError(Node, Previous);
end;

// -----------------------------------------------------------------------------

procedure FixErrors(const OnlySelected: Boolean = False);
var
  Data, Data2 : PSubtitleItem;
  Node        : PVirtualNode;
  Sib         : PVirtualNode;
  FTS         : TFixTypesSet;
  UndoAction  : PUndoAction;
begin
  if (ErrorsToFix.eOCRErrors) and (FileExists(OCRDefFile)) then
    ParseOCRErrors(OCRDefFile);
  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    
    if OnlySelected = False then
      Node := lstSubtitles.GetFirst else
      Node := lstSubtitles.GetFirstSelected;
    while Assigned(Node) do
    begin
      if OnlySelected = False then
        Sib := Node.NextSibling else
        Sib := lstSubtitles.GetNextSelected(Node);

      Data := lstSubtitles.GetNodeData(Node);

      New(UndoAction);
      UndoAction^.UndoActionType := uaFullSubChange;
      UndoAction^.BufferSize     := SizeOf(TLineChange);
      UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := Node.Index;
      UndoAction^.BindToNext     := True;
      PLineChange(UndoAction^.Buffer).SubtitleItem := Data^;

      // Delete:
      //   ftEmptySubtitleDeleted
      //   ftRepeatedSubtitle
      //   ftSubDeletedProhibitedCharacter
      //   ftHearingImpairedDeleted
      FTS := FixError(Node, Node.PrevSibling, ShowConfMainForm, frmMain);

      if (ftEmptySubtitleDeleted in FTS)          or
         (ftRepeatedSubtitle in FTS)              or
         (ftSubDeletedProhibitedCharacter in FTS) or
         (ftHearingImpairedDeleted in FTS)        then
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                    := uaDeleteLine;
        UndoAction^.BufferSize                        := SizeOf(TLineChange);
        UndoAction^.Buffer                            := AllocMem(UndoAction^.BufferSize);
        UndoAction^.BindToNext                        := True;
        UndoAction^.LineNumber                        := Node.Index;
        PLineChange(UndoAction^.Buffer)^.SubtitleItem := Data^;
        UndoList.Add(UndoAction);
        lstSubtitles.DeleteNode(Node);
      end else
      begin
        Data2 := lstSubtitles.GetNodeData(Node);
        if (PLineChange(UndoAction^.Buffer).SubtitleItem.InitialTime <> Data2^.InitialTime) or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.FinalTime   <> Data2^.FinalTime)   or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.Text        <> Data2^.Text)        or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.Translation <> Data2^.Translation) or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.ErrorType   <> Data2^.ErrorType)   then        
        UndoList.Add(UndoAction);
      end;

      if CancelProcess then
      begin
        CancelProcess        := False;
        OrgModified          := True;
        TransModified        := True;
        if UndoList.Count > 0 then
        begin
          PUndoAction(UndoList.Last)^.BindToNext := False;
          mnuUndo.Enabled := True;
        end;
        lstSubtitles.Refresh;
        RefreshTimes;
        exit;
      end;

      Node := Sib;
    end;

    if UndoList.Count > 0 then
    begin
      PUndoAction(UndoList.Last)^.BindToNext := False;
      mnuUndo.Enabled := True;
    end;
    RefreshTimes;
    lstSubtitles.Refresh;
    OrgModified   := True;
    TransModified := True;
  end; 
end;

// -----------------------------------------------------------------------------

end.
