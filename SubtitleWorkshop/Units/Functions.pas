unit Functions;

interface

uses Forms, Windows, Classes, SysUtils, StdCtrls, VirtualTrees, USubtitleApi,
    USubtitlesFunctions, TreeViewHandle, Mask, IniFiles, ComCtrls, ExtCtrls,
    ShellApi, Controls, General, FastStrings, StrMan, Math;

// -----------------------------------------------------------------------------

type
  TSyncPoint = record
    LineNum : Integer;
    OldTime : Integer;
    NewTime : Integer;
  end;
  TClassicSyncPoints = record
    Point1Sub, Point1Movie: Integer;
    Point2Sub, Point2Movie: Integer;
  end;

// -----------------------------------------------------------------------------

procedure AddCharsets(ComboBox: TComboBox);
function StrCharsetToInt(CharSet: String): Integer;
// --------------------------//
//   FPS-Related Functions   //
// --------------------------//
procedure AddFPS(ComboBox: TComboBox);
procedure SaveFPS(ComboBox: TComboBox);
procedure AddRecentFiles;
procedure SaveRecentFiles;
function GetInputFPS: Single;
function GetFPS: Single;
procedure AddFPSItem(FPS: Single; ModifyInputFPS, ModifyFPSTimes, ModifyInputFPSTimes: Boolean);
// --------------------------//
//   TAG-Related functions   //
// --------------------------//
function RemoveSWTags(Text: String; Bold, Italic, Underline, Color: Boolean; OverrideNoInterWithTags: Boolean = False): String;
function SetColorTag(Text: String; Color: Integer): String;
function GetSubColor(Text: String): Integer;
// ---------------------
function IsFloat(Texto : string): Boolean;
function StrSecToMS(Sec: String): Integer;
// ---------------------
function MsgBox(AMsg, BCap1, BCap2, BCap3: String; IconInd: Integer; MainForm: TForm; Charset: Integer = -1): Integer;
function QueryInput(const Caption, Prompt: String; var ResultStr: String; ParentForm: TForm): Integer;
// ---------------------
procedure MemoKeyPress(Sender: TObject; List: TVirtualStringTree; NextLine: Boolean);
function AdjustLines(Line: String; GoForwardAlso: Boolean = True; FindLessDifference: Boolean = True): String;
procedure AdjustSubtitles(Points: TClassicSyncPoints);
// ---------------------//
//   Search & Replace   //
// ---------------------//
function Replace(Text, This, ByThis: String; CaseSensitive, WholeWords, PreserveCase: Boolean): String;
function ContainsString(Text, This: String; CaseSensitive, WholeWords: Boolean): Boolean;
function ReplaceInNode(This, ByThis: String; CaseSensitive, WholeWords, PreserveCase, SelectedItemToEnd: Boolean): PVirtualNode;
function FindInNode(This: String; CaseSensitive, MatchWholeWords, SelectedItemToEnd: Boolean): PVirtualNode;
// ---------------------//
//    Formats related   //
// ---------------------//
function GetFormatExt(FormatIndex: Integer): String;
procedure GetFormatsList(Result: TStrings);
// ---------------------//
//  Video file related  //
// ---------------------//
function GetVideoInfo(const FileName: String; var FPS: Single; var Duration: Integer): Boolean;
// -------- //
//  Others  //
// -------- //
function GetLengthForEachLine(Text: String): String;
function FixRTLPunctuation(S: String): String;
// -------------- //
//  Divide lines  //
// -------------- //
procedure ProcessStringToDivide(const StringToDivide: String; var Breaks: TOpenIntegerArray; const AdjustAutomatically: Boolean; var Out1, Out2: String; var MaxBreaks: Integer);
procedure TrimParts(var Part1, Part2: String);

implementation

uses formMain, formSaveAs, Undo;

// -----------------------------------------------------------------------------

procedure AddCharsets(ComboBox: TComboBox);
begin
  ComboBox.Items.Add('ANSI');
  ComboBox.Items.Add('Default');
  ComboBox.Items.Add('Symbol');
//  ComboBox.Items.Add('MAC');
  ComboBox.Items.Add('Shiftjis');
  ComboBox.Items.Add('Hangeul');
  ComboBox.Items.Add('Johab');
  ComboBox.Items.Add('GB2312');
  ComboBox.Items.Add('Chinese BIG5');
  ComboBox.Items.Add('Greek');
  ComboBox.Items.Add('Turkish');
  ComboBox.Items.Add('Vietnamese');
  ComboBox.Items.Add('Hebrew');
  ComboBox.Items.Add('Arabic');
  ComboBox.Items.Add('Baltic');
  ComboBox.Items.Add('Cyrillic');
  ComboBox.Items.Add('Thai');
  ComboBox.Items.Add('EastEurope');
  //ComboBox.Items.Add('OEM');
  ComboBox.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

function StrCharsetToInt(CharSet: String): Integer;
begin
  Result := 1;
  if CharSet = 'ANSI'         then Result := 0 else
  if CharSet = 'Default'      then Result := 1 else
  if CharSet = 'Symbol'       then Result := 2 else
  if CharSet = 'Shiftjis'     then Result := 128 else
  if CharSet = 'Hangeul'      then Result := 129 else
  if CharSet = 'Johab'        then Result := 130 else
  if CharSet = 'GB2312'       then Result := 134 else
  if CharSet = 'Chinese BIG5' then Result := 136 else
  if CharSet = 'Greek'        then Result := 161 else
  if CharSet = 'Turkish'      then Result := 162 else
  if CharSet = 'Vietnamese'   then Result := 163 else
  if CharSet = 'Hebrew'       then Result := 177 else
  if CharSet = 'Arabic'       then Result := 178 else
  if CharSet = 'Baltic'       then Result := 186 else
  if CharSet = 'Cyrillic'     then Result := 204 else
  if CharSet = 'Thai'         then Result := 222 else
  if CharSet = 'EastEurope'   then Result := 238;
end;

// -----------------------------------------------------------------------------

procedure AddFPS(ComboBox: TComboBox);
var
  Ini  : TIniFile;
  i    : Integer;
  Item : String;
begin
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'FPS.ini');
  try
    for i := 1 to Ini.ReadInteger('FPS', 'Total', 0) do
    begin
      Item := FormatFloat('#.###', Ini.ReadFloat('FPS', IntToStr(i), 25));
      if ComboBox.Items.IndexOf(Item) = -1 then
        ComboBox.Items.Add(Item);
    end;
    if ComboBox.Items.Count > 0 then
    begin
      ComboBox.ItemIndex := Ini.ReadInteger('FPS', 'Active', 1) - 1;
      if ComboBox.Items[ComboBox.ItemIndex] = '' then
        ComboBox.ItemIndex := ComboBox.Items.IndexOf('25');
      if ComboBox.ItemIndex = -1 then
        ComboBox.ItemIndex := 0;
    end else
    begin
      ComboBox.Items.Add('15');
      ComboBox.Items.Add('20');
      ComboBox.Items.Add('23,976');
      ComboBox.Items.Add('23,978');
      ComboBox.Items.Add('24');
      ComboBox.Items.Add('25');
      ComboBox.Items.Add('29,97');
      ComboBox.Items.Add('30');
      ComboBox.ItemIndex := 5;
    end;
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveFPS(ComboBox: TComboBox);
var
  Ini : TIniFile;
  i   : Integer;
begin
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'FPS.ini');
  try
    Ini.WriteInteger('FPS', 'Total', ComboBox.Items.Count);
    for i := 0 to ComboBox.Items.Count-1 do
      Ini.WriteString('FPS', IntToStr(i + 1), ComboBox.Items[i]);
    Ini.WriteInteger('FPS', 'Active', ComboBox.ItemIndex + 1);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

function GetFPS: Single;
begin
  Result := StrToFloatDef(frmMain.cmbFPS.Items[frmMain.cmbFPS.ItemIndex], 25);
end;

// -----------------------------------------------------------------------------

function GetInputFPS: Single;
begin
  Result := StrToFloatDef(frmMain.cmbInputFPS.Items[frmMain.cmbInputFPS.ItemIndex], 25);
end;

// -----------------------------------------------------------------------------

procedure AddFPSItem(FPS: Single; ModifyInputFPS, ModifyFPSTimes, ModifyInputFPSTimes: Boolean);
var
  FPSStr : String;
  Index  : Integer;
begin
  if FPS <> 0 then
  begin
    FPSStr := FormatFloat('#.###', FPS);
    with frmMain do
    begin
      OldFPS := GetFPS;
      Index := cmbInputFPS.Items.IndexOf(FPSStr);
      if (Index = -1) and (FPSStr <> '') then
      begin
        cmbInputFPS.ItemIndex := cmbInputFPS.Items.Add(FPSStr);
        cmbFPS.ItemIndex      := cmbFPS.Items.Add(FPSStr);
      end else
      begin
        if ModifyInputFPS then
        begin
          OldInputFPS := FPS;
          cmbInputFPS.ItemIndex := Index;
          if ModifyInputFPSTimes then
            cmbInputFPSSelect(frmMain as TObject);
        end;
        OldFPS := FPS;
        cmbFPS.ItemIndex := Index;
        if ModifyFPSTimes then
          cmbInputFPSSelect(frmMain as TOBject);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function RemoveSWTags(Text: String; Bold, Italic, Underline, Color: Boolean; OverrideNoInterWithTags: Boolean = False): String;
var
  i: Integer;
begin
  if (SubtitleAPI.NoInteractionWithTags = False) or (OverrideNoInterWithTags = True) then
  begin
    if Bold      = True then Text := ReplaceString(Text, '<b>', '');
    if Italic    = True then Text := ReplaceString(Text, '<i>', '');
    if Underline = True then Text := ReplaceString(Text, '<u>', '');
    if Color = True then
    begin
      i := SmartPos('<c:#', Text, False);
      while (i > 0) and (SmartPos('>', Text, True, i + 1) > i) do
        Delete(Text, i, SmartPos('>', Text, True, i + 1));
    end;
  end;
  Result := Text;
end;

// -----------------------------------------------------------------------------

function SetColorTag(Text: String; Color: Integer): String;
  function ColorToHTML(Color: Integer): String;
  begin
    Result := Format('%.2x%.2x%.2x', [GetRValue(Color), GetGValue(Color), GetBValue(Color)]);
  end;
begin
  Text := RemoveSWTags(Text, False, False, False, True);
  Result := '<c:#' + ColorToHTML(Color) + '>' + Text;
end;

// -----------------------------------------------------------------------------

function GetSubColor(Text: String): Integer;
const
  HTMLChars: set of Char = ['A'..'F', '0'..'9'];
var
  i: Integer;
begin
  Result := -1;
  if (SmartPos('<c:#', Text, False) > 0) then
  begin
    Text := Copy(Text, SmartPos('<c:#', Text, False) + 4, 7);
    if Copy(Text, 7, 1) = '>' then
    begin
      Delete(Text, 7, 1);
      Text := AnsiUpperCase(Text);

      for i := 0 to Length(Text) do
        if (Text[i] <> '') and (Text[i] in HTMLChars = False) then exit;

      // convert hexadecimal values to RGB
      Result := Integer(StrToInt('$' + Copy(Text, 1, 2))) +
                Integer(StrToInt('$' + Copy(Text, 3, 2))) shl 8 +
                Integer(StrToInt('$' + Copy(Text, 5, 2))) shl 16;
    end;
  end;
end;
// -----------------------------------------------------------------------------

function IsFloat(Texto : String): Boolean;
var
  i: integer;
begin
  Texto  := Trim(Texto);
  Result := True;
  if Length(Texto) = 0 then
  begin
    Result := False;
    exit;
  end else
  begin
    if (Texto[1] in ['0'..'9']) = False then
    begin
      Result := False;
      exit;
    end;
    for i := 1 to Length(Texto) do
    begin
      if Pos(Texto[i],'0123456789+-E' + DecimalSeparator) = 0 then
      begin
        Result := False;
        Break;
      end;
    end;

  end;
end;

// -----------------------------------------------------------------------------

function StrSecToMS(Sec: String): Integer;
begin
  Result := (StrToInt(Copy(Sec, 1, Pos(',', Sec)-1)) * 1000) + StrToInt(Copy(Sec, Pos(',', Sec) +1, Length(Sec)));
end;

// -----------------------------------------------------------------------------

procedure AddRecentFiles;
var
  Ini : TIniFile;
  i   : Integer;
begin
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + ID_ININAME);
  try
    for i := 0 to frmMain.RFMaxCount-1 do
      if (Ini.ReadString('Recent',IntToStr(i),'') <> '') and
         (FileExists(Ini.ReadString('Recent',IntToStr(i),''))) and
         (RecentFiles.IndexOf(Ini.ReadString('Recent',IntToStr(i),'')) = -1) then
            RecentFiles.Add(Ini.ReadString('Recent',IntToStr(i),''));
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveRecentFiles;
var
  Ini : TIniFile;
  i   : Integer;
begin
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + ID_ININAME);
  try
    if RecentFiles.Count > 0 then
    begin
      for i := 0 to frmMain.RFMaxCount-1 do
      begin
        if i < RecentFiles.Count then
          Ini.WriteString('Recent',IntToStr(i),RecentFiles[i]) else
          Ini.WriteString('Recent',IntToStr(i),'');
      end;
    end else
      Ini.EraseSection('Recent');
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

function MsgBox(AMsg, BCap1, BCap2, BCap3: String; IconInd: Integer; MainForm: TForm; Charset: Integer = -1): Integer;
var
  W              : TForm;
  lCaption       : TLabel;
  But1,But2,But3 : TButton;
  i1             : integer;
  Image1         : TImage;
  IHandle        : THandle;
  P1             : array [byte] of Char;
  Textsize       : TSize;
  MDC            : hDC;
  CurMetrics     : TTextMetric;
  Curfont        : HFont;
  Msgrect        : TRect;
const UserExe: array [0..9] of Char = 'user.exe';
const
{$IFDEF Win32}
  BHeight = 23;
{$ELSE}
  BHeight = 25;
{$ENDIF}
  BWidth = 77;
begin
  W    := TForm.CreateNew(Application);
  But2 := nil;
  But3 := nil;
  AMsg := ReplaceString(AMsg, '|', #13#10);
  try
    // set up form
    W.BorderStyle  := bsDialog;
    W.Ctl3D        := True;
    W.Width        := 360;
    W.Height       := 160;
    W.Caption      := ID_PROGRAM;
    W.Font.Name    := Mainform.Font.Name;
    W.Font.Size    := Mainform.Font.Size;
    W.Font.Style   := Mainform.Font.Style;
    if Charset < 0 then
      W.Font.Charset := MainForm.Font.Charset else
      W.Font.Charset := Charset;

    // Get text extent
    for i1 := 0 to 25 do P1[i1] := Chr(i1 + Ord('A'));
    for i1 := 0 to 25 do P1[i1 + 26] := Chr(i1 + Ord('a'));
    GetTextExtentPoint(W.Canvas.Handle, P1, 52, TextSize);

    // Get line height
    MDC := GetDC(0);
    CurFont := SelectObject(MDC, W.Font.Handle);
    GetTextMetrics(MDC, CurMetrics);
    SelectObject(MDC, CurFont);
    ReleaseDC(0, MDC);

    // Set icon
    Image1 := TImage.Create(W);
    StrPCopy(P1,ParamStr(0));

    If Image1 <> nil then
    begin
      Image1.Width  := Image1.Picture.Icon.Width;
      Image1.Height := Image1.Picture.Icon.Height;
      Image1.Left   := 20;
      Image1.Top    := Textsize.CY  + (Textsize.CY div 2);
      Image1.Width  := 32;
      Image1.Height := 32;
      Image1.Parent := W;
      Image1.Name   := 'Image';
      // get icon index
      Case IconInd of
        16:  IHandle := ExtractIcon(hInstance,userexe,3);
        32:  IHandle := ExtractIcon(hInstance,userexe,2);
        48:  IHandle := ExtractIcon(hInstance,userexe,1);
        64:  IHandle := ExtractIcon(hInstance,userexe,4);
        128: IHandle := ExtractIcon(hInstance,userexe,0);
        256: IHandle := ExtractIcon(hInstance,userexe,5);
        512: IHandle := ExtractIcon(hInstance,userexe,6);
        else IHandle := ExtractIcon(hInstance,P1,IconInd);
      end;
      If IHandle <> 0 then
        Image1.Picture.Icon.Handle := IHandle else
        Image1.Picture.Icon        := Application.Icon;
    end;

    SetRect(MsgRect, 0, 0, Screen.Width div 2, 0);
    DrawText(W.Canvas.Handle, PChar(AMsg), -1, MsgRect, DT_CALCRECT or DT_WORDBREAK);

    // Set up label
    lCaption          := TLabel.Create(W);
    lCaption.Parent   := W;
    lCaption.Left     := 72;
    lCaption.Top      := Image1.Top;
    lCaption.Width    := Msgrect.Right;
    LCaption.Height   := Msgrect.Bottom;
    lCaption.Autosize := False;
    lCaption.WordWrap := True;

    // Adjust form width...must do here to accommodate buttons
    W.Width          := lCaption.Left + lCaption.Width + 30;
    lCaption.Caption := AMsg;

    // Buttons...
    But1             := TButton.Create(W);
    But1.Parent      := W;
    But1.Caption     := BCap1;
    But1.ModalResult := 1;

    If BCap2 <> '' then
    begin
      But2             := TButton.Create(W);
      But2.Parent      := W;
      But2.Caption     := BCap2;
      But2.ModalResult := 2;
      If BCap3 <> '' then
      begin
        But3 := TButton.Create(W);
        But3.Parent      := W;
        But3.Caption     := BCap3;
        But3.ModalResult := 3;
        But3.Cancel      := True;
      end else
      But2.Cancel := True;
    end else
    But1.Cancel := True;

    // Set button positions
    // set height depending on whether icon or message is tallest
    if lCaption.Height > Image1.Height then
      But1.Top := (lCaption.Top + lCaption.Height + 20) else
      But1.Top := (Image1.Top + Image1.Height + 20);

    But1.Width  := BWidth;
    But1.Height := BHeight;

    If But2 <> nil then
    begin
      But2.Height := BHeight;
      But2.Width  := BWidth;
      But2.Top    := But1.Top;
      If But3 <> nil then
      begin
        But3.Top    := But1.Top;
        But3.Width  := BWidth;
        But3.Height := BHeight;
        But3.Left   := (W.Width div 2) + ((BWidth div 2) + 8);
        But2.Left   := (W.Width div 2) - (BWidth div 2);
        But1.Left   := (W.Width div 2) - ((BWidth div 2) + BWidth + 8);
        But3.Cancel := True;
        if But3.Width + But3.Left > W.Width - 12 then
          W.Width := But3.Width + But3.Left + 12;
      end else
      begin
        But2.Left := (W.Width div 2) + 4;
        But1.Left := (W.Width div 2) - (BWidth + 4);
      end;
    end else
     But1.Left := (W.Width div 2) - (BWidth div 2);

    But1.Default := True;
    {
    case FocusInd of
      3: If BCap3 <> '' then But3.Default := True;
      2: If BCap2 <> '' then But2.Default := True;
      else But1.Default := True;
    end;   }

    // Set clientheight to proper height
    W.ClientHeight := But1.Top + But1.Height + TextSize.CY;

    // Show messagebox
    // Set position
    W.Position := poScreenCenter;
    {W.Left := Mainform.Left + ((Mainform.Width - W.Width) div 2);
    W.Top  := Mainform.Top + ((Mainform.Height - W.Height) div 2);   }
    W.ShowModal;
    Result := W.ModalResult;

  finally
    W.Free;
  end;
end;

// -----------------------------------------------------------------------------

function QueryInput(const Caption, Prompt: String; var ResultStr: String; ParentForm: TForm): Integer;
var
  frmInput     : TForm;
  Panel        : TPanel;
  LabelEdit    : TLabeledEdit;
  ButtonOk,
  ButtonCancel : TButton;
begin
  LabelEdit := nil;
  frmInput := TForm.CreateNew(Application);
  try
    frmInput.Ctl3D        := True;
    frmInput.BorderStyle  := bsSingle;
    frmInput.BorderIcons  := [];
    frmInput.Width        := 271;
    frmInput.Height       := 138;
    frmInput.Caption      := Caption;
    frmInput.ParentFont   := True;
    frmInput.Font.Name    := ParentForm.Font.Name;
    frmInput.Font.Size    := ParentForm.Font.Size;
    frmInput.Font.Style   := ParentForm.Font.Style;
    frmInput.Font.Charset := ParentForm.Font.Charset;
    frmInput.Position     := poScreenCenter;

    // We create panel...
    Panel             := TPanel.Create(frmInput);
    Panel.Parent      := frmInput;
    Panel.Caption     := '';
    Panel.Ctl3D       := True;
    Panel.ParentFont  := True;
    Panel.BorderStyle := bsNone;
    Panel.BevelInner  := bvNone;
    Panel.BevelOuter  := bvRaised;
    Panel.BevelWidth  := 1;
    Panel.Left        := 8;
    Panel.Top         := 8;
    Panel.Width       := 249;
    Panel.Height      := 65;
    Panel.Show;
    // Now we create LabelEdit...
    LabelEdit                      := TLabeledEdit.Create(frmInput);
    LabelEdit.Parent               := Panel;
    LabelEdit.ParentFont           := True;
    LabelEdit.EditLabel.ParentFont := True;
    LabelEdit.EditLabel.Caption    := Prompt;
    LabelEdit.ParentFont           := True;
    LabelEdit.Text                 := ResultStr;
    LabelEdit.Top                  := 32;
    LabelEdit.Left                 := 8;
    LabelEdit.Width                := 233;
    LabelEdit.Height               := 21;
    // Ok Button...
    ButtonOk             := TButton.Create(frmInput);
    ButtonOk.Parent      := frmInput;
    ButtonOk.ParentFont  := True;
    ButtonOk.Caption     := BTN_OK;
    ButtonOk.Top         := 80;
    ButtonOk.Left        := 48;
    ButtonOk.Width       := 81;
    ButtonOk.Height      := 25;
    ButtonOk.ModalResult := mrOk;
    ButtonOk.Default     := True;
    // Cancel Button...
    ButtonCancel             := TButton.Create(frmInput);
    ButtonCancel.Parent      := frmInput;
    ButtonCancel.ParentFont  := True;
    ButtonCancel.Caption     := BTN_CANCEL;
    ButtonCancel.Top         := 80;
    ButtonCancel.Left        := 136;
    ButtonCancel.Width       := 81;
    ButtonCancel.Height      := 25;
    ButtonCancel.ModalResult := mrCancel;
    ButtonCancel.Cancel      := True;

    Result := frmInput.ShowModal;
  finally
    if frmInput.ModalResult = mrOk then
      ResultStr := LabelEdit.Text;
    frmInput.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure MemoKeyPress(Sender: TObject; List: TVirtualStringTree; NextLine: Boolean);
begin
  if List.SelectedCount > 1 then
    exit;
  if NextLine then
  begin
    if List.FocusedNode.Index < List.RootNodeCount-1 then
    begin
      List.ScrollIntoView(List.FocusedNode.NextSibling, True);
      List.Selected[List.FocusedNode] := False;
      List.FocusedNode := List.FocusedNode.NextSibling;
      List.Selected[List.FocusedNode] := True;
      frmMain.RefreshTimes;
      (Sender as TWinControl).SetFocus;
      if (Sender.ClassType = TMemo) and (frmMain.SelTextNL = True) then
        (Sender as TMemo).SelectAll;
    end;
  end else
  begin
    List.ScrollIntoView(List.FocusedNode.PrevSibling, True);
    List.Selected[List.FocusedNode] := False;
    if List.FocusedNode <> List.GetFirst then
      List.FocusedNode := List.FocusedNode.PrevSibling;
    List.Selected[List.FocusedNode] := True;
    frmMain.RefreshTimes;
    (Sender as TWinControl).SetFocus;
    if (Sender.ClassType = TMemo) and (frmMain.SelTextPL = True) then
      (Sender as TMemo).SelectAll;
  end;
end;

// -----------------------------------------------------------------------------

function AdjustLines(Line: String; GoForwardAlso: Boolean = True; FindLessDifference: Boolean = True): String;
var
  // Tags
  Bold        : Boolean;
  Italic      : Boolean;
  Underline   : Boolean;
  Color       : Integer;
  // Points to break
  BreakPoint1 : Integer;
  BreakPoint2 : Integer;
  // Other
  tmpValue1  : Integer;
  tmpValue2  : Integer;
  LineLength : Integer;
begin
  if Line = '' then
  begin
    Result := '';
    exit;
  end;
  // Store tags
  Bold      := Pos('<b>', Line) > 0;
  Italic    := Pos('<i>', Line) > 0;
  Underline := Pos('<u>', Line) > 0;
  Color     := GetSubColor(Line);
  // Remove tags
  Line    := RemoveSWTags(Line, True, True, True, True);

  // Make one big line...
  while Pos(#13#10, Line) > 0 do
  begin
    if Line[Pos(#13#10, Line)-1] <> ' ' then
      Insert(' ', Line, Pos(#13#10, Line));
    Delete(Line, Pos(#13#10, Line), 2);
  end;

  Line := Trim(Line);

  if (Length(Line) > frmMain.TwoLinesIfLongerThan) or (Pos('-', Line) > 0) then
  begin
    // ----------------------------------------- //
    //              The way it works             //
    //                                           //
    // We are going to do two things:            //
    //   1. Length div 2 - Backwards_until_space //
    //   2. Length div 2 - Forwards_until_space  //
    //                                           //
    // After this we are going to see in which   //
    // break point there is less difference      //
    // between lines length, and we are going to //
    // stay with it.                             //
    // ----------------------------------------- //

    LineLength := Length(Line);
    if Pos('...', Line) > 0 then
      LineLength := LineLength - 2; // As "..." takes little space on the screen, we subtract a bit of length for better calculations...
    LineLength := LineLength div 2;

    BreakPoint1 := SmartPos(' ', Line, True, LineLength, False);
    if Copy(Line, BreakPoint1+1, 1)[1] in ['?', '!']  then
      BreakPoint1 := SmartPos(' ', Line, True, LineLength -1, False);

    if GoForwardAlso then
    begin
      if FindLessDifference = False then
      begin
        BreakPoint1 := SmartPos(' ', Line, True, LineLength);
        if Copy(Line, BreakPoint1+1, 1)[1] in ['?', '!']  then
          BreakPoint1 := SmartPos(' ', Line, True, LineLength -1, False);
      end else
      begin
        BreakPoint2 := SmartPos(' ', Line, True, LineLength);
        if Copy(Line, BreakPoint2+1, 1)[1] in ['?', '!']  then
          BreakPoint2 := SmartPos(' ', Line, True, LineLength+1);
        tmpValue1 := Abs(BreakPoint1 - (Length(Line) - BreakPoint1));
        tmpValue2 := Abs(BreakPoint2 - (Length(Line) - BreakPoint2));
        if tmpValue1 <> tmpValue2 then
        begin
          if Min(tmpValue1, tmpValue2) = tmpValue2 then
            BreakPoint1 := BreakPoint2;
        end;
        if tmpValue2 < tmpValue1 then
          BreakPoint1 := BreakPoint2;
      end;
    end;

    if (StringCount('-', Line) = 1) and (Pos('-', Line) > 1) then
    begin                //
      tmpValue1 := Pos('-', Line);
      // Only if it's not a word composed by "-"
      if (Line[tmpValue1+1] in SpecialChars = True) then
      begin
        if Pos(' -', Line) = 0 then
          Insert(' ', Line, tmpValue1); // Insert a space if there is not
        BreakPoint1 := Pos(' -', Line);
      end else
      begin
        if (Length(Line) < frmMain.TwoLinesIfLongerThan) then
          BreakPoint1 := -1;
      end;
    end else
    if (Pos('-', Line) = 1) and (StringCount('-', Line) = 2) then
    begin
      if SmartPos(' -', Line, True, Pos('-', Line)+1) = 0 then
        Insert(' ', Line, SmartPos('-', Line, True, Pos('-', Line)+1)); // Insert a space if there is not
      BreakPoint1 := SmartPos(' -', Line, True, Pos('-', Line)+1);
    end else
    if (Pos('-', Line) = 1) and (StringCount('-', Line) = 1) then
    begin
      if Length(Line) < frmMain.TwoLinesIfLongerThan then
        BreakPoint1 := -1;
    end;

    if BreakPoint1 > -1 then
    begin
      Delete(Line, BreakPoint1, 1);
      Insert(#13#10, Line, BreakPoint1);
    end;
  end;

  if SubtitleAPI.NoInteractionWithTags = False then
  begin
    // Restore tags
    if Underline = True then Line := '<u>' + Line;
    if Bold      = True then Line := '<b>' + Line;
    if Italic    = True then Line := '<i>' + Line;
    if Color > -1 then
      Line := SetColorTag(Line, Color);
  end;

  Result := Line;
end;

// -----------------------------------------------------------------------------

procedure AdjustSubtitles(Points: TClassicSyncPoints);
var
  Node       : PVirtualNode;
  a, b       : Extended;
  StartTime  : Integer;
  FinalTime  : Integer;
  UndoAction : PUndoAction;
begin

  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;

  ///////////////////////////////////////////////////////////////////////////
  //                                                                       //
  //                        Method 1 - More precise                        //
  //                                                                       //
  // This  is the easiest way of adjusting subtitles; what you do is watch //
  // the movie,  note  the  time at which the first subtitle line appears, //
  // move to the end of movie and note the time at which the last subtitle //
  // line appears then enter those values in Subtitle Workshop.            //
  // For  example, I watch the video and at 00:10:00,000 the first line is //
  // spoken  but  it appears at 00:05:00,000 in my subtitle file. The last //
  // subtitle  line  is  spoken at 02:10:00,000 but in my subtitle file it //
  // appears at 01:45:00,000. Let's do the math...                         //
  //                                                                       //
  // The scope in the movie is:                                            //
  //                                                                       //
  // 02:10:00,000-00:10:00,000=02:00:00,00=7.200,000 seconds               //
  //                                                                       //
  // The scope in the subtitle is:                                         //
  //                                                                       //
  // 01:45:00,000-00:05:00,000=01:40:00,000=6.000,000 seconds              //
  //                                                                       //
  // The coefficient is:                                                   //
  //                                                                       //
  // 7.200,000/6.000,000=1,2                                               //
  //                                                                       //
  // Ok,  let's  do  it...  You  need  to set the offset (delay) for every //
  // subtitle  line  (move  them) so the first one appears at 00:00:00,000 //
  // and the last one appears at 01:40:00,000. This is very important!.    //
  //                                                                       //
  // So,  now  that  you moved all the subtitles you simply multiply every //
  // time  with  the  coefficient and add 00:10:00,000 to multiplied times //
  // (the  time  when the first subtitle appears in the movie) and presto! //
  // instantly adjusted subtitles! It would take users a minute to do this //
  // and  subtitles  would  match  the first time around. Precision is the //
  // keyword  here  so  you would need to do all calculations unto a third //
  // decimal place.                                                        //
  ///////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////
  //                                                                       //
  //        Method 2 - Supports two random points in video/subtitle        //
  //                                                                       //
  // It is a simple 2x2 linear system of equations. I need to figure out   //
  // the parameters of a funtion of the type f(x) = ax+b being a and b     //
  // this unknown parámeters, so I need two pairs of f(x) and x values to  //
  // be able to determine a and b :                                        //
  //                                                                       //
  // (1)    f(x1) = a * x1 + b     2x2 system (2 equations and 2 unknown   //
  //                                           parameters a & b)           //
  // (2)    f(x2) = a * x2 + b                                             //
  //                                                                       //
  // so, isolating a from (1) :                                            //
  // (3)    a = [ f(x1) - b ] / x1                                         //
  // replacing a from (3) in (2) :                                         //
  // (4)    f(x2) = [ f(x1) - b ] / x1 * x2 + b                            //
  // isolating b from (4) :                                                //
  // (5)    b = [f(x2)*x1 - f(x1)*x2 ] / (x1 - x2)                         //
  // replacing b from (5) into (3) :                                       //
  // (6)    a = [ f(x1) - f(x2) ] / (x1 - x2)                              //
  //                                                                       //
  // This gives you a (6) and b (5), now, you take each frame number of    //
  // the subtitles and apply the formula : f(x) = a * x + b, being x this  //
  // frame number, and f(x) the new frame number. So x1 and f(x1) are the  //
  // values you select of SubFileFrame and WantedFrame for the FirstPoint  //
  // and x2 and f(x2) the values you select for SubFileFrame and           //
  // WantedFrame for LastPoint.                                            //
  ///////////////////////////////////////////////////////////////////////////

  // If the points in the subtitle are not the first & last then use method 2...
  if (Points.Point1Sub <> GetStartTime(frmMain.lstSubtitles.GetFirst)) and
     (Points.Point2Sub <> GetStartTime(frmMain.lstSubtitles.GetLast)) then
  begin
    if (Points.Point1Movie <> Points.Point2Movie) and
       (Points.Point1Sub <> Points.Point2Sub) then
    begin
      a := (Points.Point1Movie - Points.Point2Movie)  / (Points.Point1Sub - Points.Point2Sub);
      b := Trunc((Points.Point1Movie - a * Points.Point1Sub) / a);
    end else
    begin
      a := 1;
      b := Points.Point1Movie - Points.Point1Sub;
    end;

    Node  := frmMain.lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      StartTime := GetStartTime(Node);
      FinalTime := GetFinalTime(Node);

      New(UndoAction);
      UndoAction^.UndoActionType := uaTimeChange;
      UndoAction^.BufferSize     := SizeOf(TTimeChange);
      UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := Node.Index;
      UndoAction^.BindToNext     := True;
      PTimeChange(UndoAction^.Buffer)^.StartTime := StartTime;
      PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
      UndoList.Add(UndoAction);

      SetStartTime(Node, Round(a * StartTime + b));
      SetFinalTime(Node, Round(a * FinalTime + b));
      Node := Node.NextSibling;
    end;
  end else
  // Use the most precise method, very useful in this case
  begin
    if ((Points.Point2Movie - Points.Point1Movie) > 0) and ((Points.Point2Sub - Points.Point1Sub) > 0) then
    begin
      // a = coefficient
      a := (Points.Point2Movie - Points.Point1Movie) / (Points.Point2Sub - Points.Point1Sub);
      // b = delay
      b := GetStartTime(frmMain.lstSubtitles.GetFirst);

      Node  := frmMain.lstSubtitles.GetFirst;
      while Assigned(Node) do
      begin
        StartTime := GetStartTime(Node);
        FinalTime := GetFinalTime(Node);

        New(UndoAction);
        UndoAction^.UndoActionType := uaTimeChange;
        UndoAction^.BufferSize     := SizeOf(TTimeChange);
        UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
        UndoAction^.Node           := Node;
        UndoAction^.LineNumber     := Node.Index;
        UndoAction^.BindToNext     := True;
        PTimeChange(UndoAction^.Buffer)^.StartTime := StartTime;
        PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
        UndoList.Add(UndoAction);

        SetStartTime(Node, Round((StartTime - b) * a + Points.Point1Movie));
        SetFinalTime(Node, Round((FinalTime - b) * a + Points.Point1Movie));
        Node := Node.NextSibling;
      end;
    end;
  end;

  if UndoList.Count > 0 then
    PUndoAction(UndoList.Last)^.BindToNext := False;

  frmMain.mnuUndo.Enabled := True;
  frmMain.RefreshTimes;
  frmMain.OrgModified   := True;
  frmMain.TransModified := True;
  frmMain.lstSubtitles.Refresh;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                     Search & Replace related functions                     //
//                                                                            //
// ---------------------------------------------------------------------------//

function Replace(Text, This, ByThis: String; CaseSensitive, WholeWords, PreserveCase: Boolean): String;
begin
  if ByThis = '' then PreserveCase := False;
  Result := sm.Replace(This, ByThis, Text, not CaseSensitive, WholeWords, PreserveCase);
end;

// -----------------------------------------------------------------------------

function ContainsString(Text, This: String; CaseSensitive, WholeWords: Boolean): Boolean;
begin
  Result := Replace(Text, This, '', CaseSensitive, WholeWords, False) <> Text;
end;

// -----------------------------------------------------------------------------

function ReplaceInNode(This, ByThis: String; CaseSensitive, WholeWords, PreserveCase, SelectedItemToEnd: Boolean): PVirtualNode;
var
  Node   : PVirtualNode;
  RepStr : String;
  Text   : String;
  Bold, Italic, Underline: Boolean;
  Color : Integer;
  BreakNext: Boolean;
begin
  Result    := nil;
  BreakNext := False;
  if SelectedItemToEnd then
    Node := frmMain.lstSubtitles.GetFirstSelected.NextSibling else
    Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin

    Text      := GetSubText(Node);
    Bold      := Pos('<b>', Text) > 0;
    Italic    := Pos('<i>', Text) > 0;
    Underline := Pos('<u>', Text) > 0;
    Color     := GetSubColor(Text);
    Text      := RemoveSWTags(Text, True, True, True, True);

    RepStr := Replace(Text, This, ByThis, CaseSensitive, WholeWords, PreserveCase);
    if Text <> RepStr then
    begin
      if SubtitleAPI.NoInteractionWithTags = False then
      begin
        // Restore tags
        if Underline = True then RepStr := '<u>' + RepStr;
        if Bold      = True then RepStr := '<b>' + RepStr;
        if Italic    = True then RepStr := '<i>' + RepStr;
        if Color > -1 then
          RepStr := SetColorTag(RepStr, Color);
      end;
      SetText(Node, RepStr);
      BreakNext := True;
    end;

    if frmMain.mnuTranslatorMode.Checked then
    begin
      Text      := GetSubTranslation(Node);
      Bold      := Pos('<b>', Text) > 0;
      Italic    := Pos('<i>', Text) > 0;
      Underline := Pos('<u>', Text) > 0;
      Color     := GetSubColor(Text);
      Text      := RemoveSWTags(Text, True, True, True, True);

      RepStr := Replace(Text, This, ByThis, CaseSensitive, WholeWords, PreserveCase);
      if Text <> RepStr then
      begin
        if SubtitleAPI.NoInteractionWithTags = False then
        begin
          // Restore tags
          if Underline = True then RepStr := '<u>' + RepStr;
          if Bold      = True then RepStr := '<b>' + RepStr;
          if Italic    = True then RepStr := '<i>' + RepStr;
          if Color > -1 then
            RepStr := SetColorTag(RepStr, Color);
        end;
        SetTranslation(Node, RepStr);
        BreakNext := True;
      end;
    end;

    if BreakNext then
    begin
      UnSelectAll(frmMain.lstSubtitles);
      frmMain.lstSubtitles.ScrollIntoView(Node, True);
      frmMain.lstSubtitles.FocusedNode := Node;
      frmMain.lstSubtitles.Selected[Node] := True;
      Result := Node;
      Break;
    end;

    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

function FindInNode(This: String; CaseSensitive, MatchWholeWords, SelectedItemToEnd: Boolean): PVirtualNode;
var
  Node      : PVirtualNode;
  Text      : String;
  BreakNext : Boolean;
begin
  Result    := nil;
  BreakNext := False;
  if SelectedItemToEnd then
    Node := frmMain.lstSubtitles.GetFirstSelected.NextSibling else
    Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    Text := RemoveSWTags(GetSubText(Node), True, True, True, True);
    if ContainsString(Text, This, CaseSensitive, MatchWholeWords) then
      BreakNext := True;

    if frmMain.mnuTranslatorMode.Checked then
    begin
      Text := RemoveSWTags(GetSubTranslation(Node), True, True, True, True);
      if ContainsString(Text, This, CaseSensitive, MatchWholeWords) then
        BreakNext := True;
    end;

    if BreakNext then
    begin
      UnSelectAll(frmMain.lstSubtitles);
      frmMain.lstSubtitles.ScrollIntoView(Node, True);
      frmMain.lstSubtitles.FocusedNode := Node;
      frmMain.lstSubtitles.Selected[Node] := True;
      Result := Node;
      Break;
    end;

    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

function GetFormatExt(FormatIndex: Integer): String;
var
  FName : String;
begin
  SubtitleAPI.GetFormatInfo(FormatIndex, FName, Result);
  if Pos(';', Result) = 0 then
    Result := Copy(Result, 2, Length(Result)) else
    Result := Copy(Result, 2, Pos(';', Result)-2);
end;

// -----------------------------------------------------------------------------

procedure GetFormatsList(Result: TStrings);
var
  i, TotalFormats: Integer;
  Desc, Ext: String;
begin
  TotalFormats := SubtitleAPI.FormatsCount;
  if (Assigned(Result)) and (TotalFormats > 0) then
  begin
    Result.Clear;
    for i := 1 to TotalFormats do
    begin
      SubtitleAPI.GetFormatInfo(i, Desc, Ext);
      Result.Add(Desc);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetVideoInfo(const FileName: String; var FPS: Single; var Duration: Integer): Boolean;
const
  AviHeaderStart = 32; // Needed for positioning in the avi file
var
  f : File;
  // Temporary values
  TempFormatTest : array[0..2] of Char; // should be "AVI"
  TempMicroSec   : Integer;
  LengthInFrames : Integer;
begin
  // Get file size...
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  try
    {$I-}
    Reset(f, 1);
    {$I+}
    if IOResult = 0 then
    begin
      Seek(f, 8);
      BlockRead(f, TempFormatTest, SizeOf(TempFormatTest));
      if TempFormatTest <> 'AVI' then
      begin
        Result := False;
        Exit;
      end;
      Seek(f, AviHeaderStart);
      BlockRead(f, TempMicroSec, 4);
      // Length of movie in frames
      Seek(f, AviHeaderStart + 16);
      BlockRead(f, LengthInFrames, 4);
      // ---------------------- //
      //   Final calculations   //
      // ---------------------- //
      if TempMicroSec > 0 then
      begin
        FPS      := 1000000 / TempMicroSec; // Video FPS
        Duration := Trunc((LengthInFrames / FPS) * 1000); // Length in milliseconds
        Result   := (FPS > 0) and (Duration > 0);
      end else
        Result := False;
    end else
      Result := False;
  finally
    CloseFile(f);
  end;
end;

// -----------------------------------------------------------------------------

function GetLengthForEachLine(Text: String): String;
var
  TotLen  : Integer;
  PosEnter: Integer;
begin
  Result := '';
  TotLen := Length(Text) - StringCount(#13#10, Text) * 2;
  PosEnter := Pos(#13#10, Text);
  if PosEnter > 0 then
  begin
    while PosEnter > 0 do
    begin
      Result := Result + IntToStr(Length(RemoveSWTags(Copy(Text, 1, PosEnter-1), True, True, True, True))) + '/';
      Text := Copy(Text, PosEnter + 2, Length(Text));
      PosEnter := Pos(#13#10, Text);
    end;
    Result := Result + IntToStr(Length(RemoveSWTags(Text, True, True, True, True))) + '=' + IntToStr(TotLen);
  end else
  Result := IntToStr(Length(RemoveSWTags(Text, True, True, True, True)));
end;

// -----------------------------------------------------------------------------

procedure TrimParts(var Part1, Part2: String);
var
  i        : Integer;
  tmpLines : TStrings;
begin
  tmpLines := TStringList.Create;
  try
    tmpLines.Text := Part1;
    for i := 0 to tmpLines.Count-1 do
      tmpLines[i] := Trim(tmpLines[i]);
    Part1 := tmpLines.Text;
    tmpLines.Text := Part2;
    for i := 0 to tmpLines.Count-1 do
      tmpLines[i] := Trim(tmpLines[i]);
    Part2 := tmpLines.Text;
    if Copy(Part1, Length(Part1)-1, 2) = #13#10 then
      Part1 := Copy(Part1, 1, Length(Part1)-2);
    if Copy(Part2, Length(Part2)-1, 2) = #13#10 then
      Part2 := Copy(Part2, 1, Length(Part2)-2);
  finally
    tmpLines.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ProcessStringToDivide(const StringToDivide: String; var Breaks: TOpenIntegerArray; const AdjustAutomatically: Boolean; var Out1, Out2: String; var MaxBreaks: Integer);
var
  PosEnter : Integer;
  PrevI    : Integer;
  Temp     : String;
  InBreak  : Integer;
begin
  SetLength(Breaks, 0);
  PrevI    := 0;
  Temp     := StringToDivide;
  PosEnter := Pos(#13#10, Temp);
  if PosEnter = 0 then exit;
  while PosEnter <> 0 do
  begin
    SetLength(Breaks, Length(Breaks)+1);
    Breaks[Length(Breaks)-1] := PosEnter + PrevI;
    PrevI    := PrevI + PosEnter + Length(#13#10)-1;
    Temp     := Copy(Temp, PosEnter + 2, Length(Temp) - PosEnter - 1);
    PosEnter := Pos(#13#10, Temp);
  end;
  MaxBreaks := Length(Breaks);
  InBreak   := (StringCount(#13#10, StringToDivide) + 1) div 2;

  if Length(Breaks) > 0 then
  begin
    Out1 := Copy(StringToDivide, 1, Breaks[InBreak-1]-1);
    Out2 := Copy(StringToDivide, Breaks[InBreak-1] + 2, Length(StringToDivide) - Breaks[InBreak-1]);
    if AdjustAutomatically then
    begin
      Out1 := AdjustLines(Out1);
      Out2 := AdjustLines(Out2);
    end;
    TrimParts(Out1, Out2);
  end;
end;

// -----------------------------------------------------------------------------

function FixRTLPunctuation(S: String): String;
const
  SpecialChars = '.,:;''()-?!+=*&$^%#@~`" /';
  Delimiter    = #13#10;
var
  Posit : Integer;
  A,B   : String;

function FixSubString(Sub: String): String;
var
  Prefix : String;
  Suffix : String;
  Temp   : String;
  P,I    : Integer;
begin
  Temp   := Sub;
  Prefix := '';
  Suffix := '';
  I      := 1;
  if Temp = '' then
  begin
    Result := '';
    exit;
  end;

  P := Pos(Temp[i], SpecialChars);
  while P <> 0 do
  begin
    Prefix := Prefix + Temp[i];
    Temp   := Copy(Temp, 2, Length(Temp)-1);
    if Temp = '' then
      P := 0 else
      P := Pos(Temp[i], SpecialChars);
  end;
  if Suffix = ' -' then Suffix := '- ';

  I := Length(Temp);
  if Temp = '' then
    P := 0 else
    P := Pos(Temp[i], SpecialChars);
  while P <> 0 do
  begin
    Suffix := Suffix + Temp[I];
    Temp   := Copy(Temp, 1, Length(Temp)-1);
    i      := Length(Temp);
    if Temp = '' then
      P := 0 else
      P := Pos(Temp[i], SpecialChars);
    end;
  if Prefix = '- ' then Prefix := ' -';

  Result := Suffix + Temp + Prefix;
end;
var
  Bold      : Boolean;
  Italic    : Boolean;
  Underline : Boolean;
  Color     : Integer;
begin
  // Store tags
  Bold      := Pos('<b>', S) > 0;
  Italic    := Pos('<i>', S) > 0;
  Underline := Pos('<u>', S) > 0;
  Color     := GetSubColor(S);
  // Remove tags
  S := RemoveSWTags(S, True, True, True, True);

  A := S;
  B := '';
  Posit := Pos(Delimiter, A);
  while Posit > 0 do
  begin
    B     := B + FixSubString(Copy(A, 1, Posit-1)) + Delimiter;
    A     := Copy(A, Posit + Length(Delimiter), Length(A) - Posit);
    Posit := Pos(Delimiter, A);
  end;
  B := B + FixSubString(A);

  if SubtitleAPI.NoInteractionWithTags = False then
  begin
    // Restore tags
    if Underline = True then B := '<u>' + B;
    if Bold      = True then B := '<b>' + B;
    if Italic    = True then B := '<i>' + B;
    if Color > -1 then
      B := SetColorTag(B, Color);
  end;
  Result := B;
end;

// -----------------------------------------------------------------------------

end.



