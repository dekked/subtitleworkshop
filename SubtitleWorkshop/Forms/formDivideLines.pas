unit formDivideLines;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ExtCtrls, General, TreeViewHandle, USubtitlesFunctions,
  VirtualTrees, ComCtrls, Functions, StrMan, Math, FastStrings, IniFiles,
  Menus, TimeMaskEdit, Undo;

type
  TfrmDivideLines = class(TForm)
    btnDivide: TButton;
    btnCancel: TButton;
    pnlDivideLines: TPanel;
    lblDivideAfterLineNumber: TLabel;
    edtDivideAfterBreakNum: TEdit;
    udDivideAfterBreakNum: TUpDown;
    btn31: TButton;
    btn13: TButton;
    btn32: TButton;
    btn23: TButton;
    btn21: TButton;
    btn12: TButton;
    btn11: TButton;
    Bevel1: TBevel;
    lblShowSub1: TLabel;
    lblHideSub1: TLabel;
    lblShowSub2: TLabel;
    lblHideSub2: TLabel;
    chkContinueDirectly: TCheckBox;
    mmoSub1: TMemo;
    mmoSub2: TMemo;
    lblDuration1: TLabel;
    lblDuration2: TLabel;
    lblLength1: TLabel;
    lblLength2: TLabel;
    chkUseAutoDur: TCheckBox;
    tmeShowSub1: TTimeMaskEdit;
    tmeHideSub1: TTimeMaskEdit;
    tmeDuration1: TTimeMaskEdit;
    tmeShowSub2: TTimeMaskEdit;
    tmeHideSub2: TTimeMaskEdit;
    tmeDuration2: TTimeMaskEdit;
    procedure chkContinueDirectlyClick(Sender: TObject);
    procedure btnDivideClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure udDivideAfterBreakNumChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure mmoSub1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mmoSub2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btn11Click(Sender: TObject);
    procedure btn12Click(Sender: TObject);
    procedure btn21Click(Sender: TObject);
    procedure btn13Click(Sender: TObject);
    procedure btn31Click(Sender: TObject);
    procedure btn23Click(Sender: TObject);
    procedure btn32Click(Sender: TObject);
    procedure mmoSub1Change(Sender: TObject);
    procedure mmoSub2Change(Sender: TObject);
    procedure chkUseAutoDurClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmeShowSub2TimeChangeFromEditOnly(Sender: TObject;
      NewTime: Cardinal);
    procedure tmeHideSub1TimeChangeFromEditOnly(Sender: TObject;
      NewTime: Cardinal);
    procedure tmeDuration1TimeChangeFromEditOnly(Sender: TObject;
      NewTime: Cardinal);
  private
    procedure SetLanguage;
    procedure CalculateTimes(Prop1, Prop2: Integer; AutomaticDur: Boolean = False);
  public
    { Public declarations }
  end;

var
  frmDivideLines      : TfrmDivideLines;
  OriginalString      : String;
  BreaksArray         : TOpenIntegerArray;
  AdjustAutomatically : Boolean;
  LengthInChars       : String;
  TotalOrgLength      : Integer;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    with LF do
    begin
      Caption                          := ReadString('Divide lines', '01', 'Divide lines');
      lblDivideAfterLineNumber.Caption := ReadString('Divide lines', '02', 'Divide after line number:');
      chkUseAutoDur.Caption            := ReadString('Divide lines', '03', 'Use automatic duration');
      lblShowSub1.Caption              := ReadString('Divide lines', '04', 'Show:');
      lblShowSub2.Caption              := lblShowSub1.Caption;
      lblHideSub1.Caption              := ReadString('Divide lines', '05', 'Hide:');
      lblHideSub2.Caption              := lblHideSub1.Caption;
      lblDuration1.Caption             := ReadString('Divide lines', '06', 'Duration:');
      lblDuration2.Caption             := lblDuration1.Caption;
      chkContinueDirectly.Caption      := ReadString('Divide lines', '07', 'Continue directly');
      LengthInChars                    := ReadString('Divide lines', '08', '%s characters');

      // Buttons
      btnDivide.Caption := ReadString('Divide lines', '09', '&Divide!');
      btnCancel.Caption := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnDivide.ParentFont := True;
      Font                 := frmMain.Font;
      btnDivide.Font.Style := frmMain.Font.Style + [fsBold];
      mmoSub1.Font := frmMain.mmoSubtitleText.Font;
      mmoSub2.Font := frmMain.mmoSubtitleText.Font;
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

function ZeroIfMinor(Value: Integer): Integer;
begin
  Result := Value;
  if Value < 0 then Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.CalculateTimes(Prop1, Prop2: Integer; AutomaticDur: Boolean = False);
var
  InitialTime : Integer;
  FinalTime   : Integer;
  Time1       : Integer;
  DurPerChar  : Single;
begin
  with frmMain do
  begin
    InitialTime := GetStartTime(lstSubtitles.FocusedNode);
    FinalTime   := GetFinalTime(lstSubtitles.FocusedNode);
    if AutomaticDur = False then
    begin
      Time1 := Trunc(InitialTime + ((FinalTime - InitialTime) / (Prop1+Prop2) * Prop1));
      tmeShowSub1.Time  := InitialTime;
      tmeHideSub2.Time  := FinalTime;
      tmeHideSub1.Time  := Time1;
      tmeShowSub2.Time  := Time1 + 1;
      tmeDuration1.Time := Time1 - InitialTime;
    end else
    begin
      if TotalOrgLength <> 0 then
        DurPerChar := (FinalTime - InitialTime) / TotalOrgLength else // Milliseconds
        DurPerChar := 0;
      Time1 := Round(DurPerChar * Length(mmoSub1.Text));

      // We don't want to leave second subtitle with 0 duration!
      if FinalTime-InitialTime-Time1 > 100 then
      begin
        tmeShowSub1.Time  := InitialTime;
        tmeHideSub2.Time  := FinalTime;
        tmeHideSub1.Time  := Time1 + InitialTime;
        tmeShowSub2.Time  := InitialTime + Time1 + 1;
        tmeDuration1.Time := Time1;
        tmeDuration2.Time := FinalTime - (InitialTime + Time1 + 1);
      end;

    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.chkContinueDirectlyClick(Sender: TObject);
begin
  if chkContinueDirectly.Checked then
  begin
    tmeShowSub2.Enabled := False;
    if chkUseAutoDur.Checked = False then
    begin
      tmeShowSub2.Time  := StringToTime(tmeHideSub1.Text) + 1;
      tmeDuration2.Time := Cardinal(GetFinalTime(frmMain.lstSubtitles.FocusedNode)) - (tmeShowSub2.Time + 1);
    end else
      CalculateTimes(1, 1, True);
  end else
    tmeShowSub2.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btnDivideClick(Sender: TObject);
var
  Data       : PSubtitleItem;
  UndoAction : PUndoAction;
begin
  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    // ------------------ //
    // Set first subtitle //
    // ------------------ //
    Data := lstSubtitles.GetNodeData(lstSubtitles.FocusedNode);

    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.Node           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
    UndoAction^.BindToNext     := True;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
    PFullTextChange(UndoAction^.Buffer)^.OldText := Data.Text;
    UndoList.Add(UndoAction);

    New(UndoAction);
    UndoAction^.UndoActionType := uaTimeChange;
    UndoAction^.Node           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
    UndoAction^.BindToNext     := True;
    UndoAction^.BufferSize     := SizeOf(TTimeChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PTimeChange(UndoAction^.Buffer)^.StartTime := Data.InitialTime;
    PTimeChange(UndoAction^.Buffer)^.FinalTime := Data.FinalTime;
    UndoList.Add(UndoAction);

    Data.FinalTime := tmeHideSub1.Time;
    Data.Text := mmoSub1.Text;
    lstSubtitles.InsertNode(lstSubtitles.FocusedNode, amInsertAfter);

    // ------------------- //
    // Set second subtitle //
    // ------------------- //
    New(UndoAction);
    UndoAction^.UndoActionType := uaInsertLine;
    UndoAction^.Node           := lstSubtitles.GetNextSibling(lstSubtitles.FocusedNode);
    UndoAction^.LineNumber     := UndoAction^.Node.Index;
    UndoAction^.BindToNext     := False;
    UndoAction^.Buffer         := nil;
    UndoAction^.BufferSize     := 0;
    UndoList.Add(UndoAction);

    Data := lstSubtitles.GetNodeData(lstSubtitles.GetNextSibling(lstSubtitles.FocusedNode));
    Data.InitialTime := tmeShowSub2.Time;
    Data.FinalTime   := tmeHideSub2.Time;
    Data.Text := mmoSub2.Text;
    if mnuTranslatorMode.Checked then
      Data.Translation := UntranslatedSub;

    mnuUndo.Enabled := True;
    lstSubtitles.Refresh;
    RefreshTimes;
    OrgModified   := True;
    TransModified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.FormCreate(Sender: TObject);
var
  Ini        : TIniFile;
  Out1, Out2 : String;
  MaxBreaks  : Integer;
begin
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    AdjustAutomatically   := Ini.ReadBool('Advanced', 'Smart line adjust automatically', True);
    chkUseAutoDur.Checked := Ini.ReadBool('General', 'Use automatic durations (Divide lines)', False);
  finally
    Ini.Free;
  end;

  tmeShowSub1.FPS  := GetFPS;
  tmeHideSub1.FPS  := GetFPS;
  tmeShowSub2.FPS  := GetFPS;
  tmeHideSub2.FPS  := GetFPS;
  tmeDuration1.FPS := GetFPS;
  tmeDuration2.FPS := GetFPS;

  if frmMain.FormatType = ftTime then
  begin
    tmeShowSub1.TimeMode  := tmTime;
    tmeHideSub1.TimeMode  := tmTime;
    tmeShowSub2.TimeMode  := tmTime;
    tmeHideSub2.TimeMode  := tmTime;
    tmeDuration1.TimeMode := tmTime;
    tmeDuration2.TimeMode := tmTime;
  end else
  begin
    tmeShowSub1.TimeMode  := tmFrames;
    tmeHideSub1.TimeMode  := tmFrames;
    tmeShowSub2.TimeMode  := tmFrames;
    tmeHideSub2.TimeMode  := tmFrames;
    tmeDuration1.TimeMode := tmFrames;
    tmeDuration2.TimeMode := tmFrames;
  end;

  if frmMain.FormatType = ftFrames then
  begin
    tmeShowSub1.MaxLength  := 7;
    tmeShowSub2.MaxLength  := 7;
    tmeHideSub1.MaxLength  := 7;
    tmeHideSub2.MaxLength  := 7;
    tmeDuration1.MaxLength := 7;
    tmeDuration2.MaxLength := 7;
  end;
  chkContinueDirectly.Checked := True;
  with frmMain do
  begin
    mmoSub1.Alignment  := frmMain.mmoSubtitleText.Alignment;
    mmoSub2.Alignment  := frmMain.mmoSubtitleText.Alignment;
    OriginalString     := GetSubText(lstSubtitles.FocusedNode);
    TotalOrgLength     := Length(OriginalString) - (StringCount(#13#10, OriginalString)*2);
    if Pos(#13#10, OriginalString) = 0 then
      OriginalString := WrapText(OriginalString, frmMain.BreakLineAfter);
  end;
  ProcessStringToDivide(OriginalString, BreaksArray, AdjustAutomatically, Out1, Out2, MaxBreaks);
  mmoSub1.Text := Out1;
  mmoSub2.Text := Out2;
  udDivideAfterBreakNum.Max := MaxBreaks;

  if udDivideAfterBreakNum.Max = 1 then
  begin
    udDivideAfterBreakNum.Enabled  := False;
    edtDivideAfterBreakNum.Enabled := False;
  end;
  Application.ProcessMessages;

  udDivideAfterBreakNum.Position := (StringCount(#13#10, OriginalString) + 1) div 2;
  CalculateTimes(1, 1, chkUseAutoDur.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.udDivideAfterBreakNumChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
var
  Part1, Part2: String;
begin
  if (NewValue >= 1) and (NewValue <= Length(BreaksArray)) then
  begin
    if Length(BreaksArray) > 0 then
    begin
      Part1 := Copy(OriginalString, 1, BreaksArray[NewValue-1]-1);
      Part2 := Copy(OriginalString, BreaksArray[NewValue-1] + 2, Length(OriginalString) - BreaksArray[NewValue-1]);
      TrimParts(Part1, Part2);
      mmoSub1.Text := Part1;
      mmoSub2.Text := Part2;
    end;
  end else
    AllowChange := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.mmoSub1Change(Sender: TObject);
begin
  lblLength1.Caption := Format(LengthInChars, [GetLengthForEachLine(mmoSub1.Text)]);
  if chkUseAutoDur.Checked then CalculateTimes(1, 1, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.mmoSub2Change(Sender: TObject);
begin
  lblLength2.Caption := Format(LengthInChars, [GetLengthForEachLine(mmoSub2.Text)]);
  if chkUseAutoDur.Checked then CalculateTimes(1, 1, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.mmoSub1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = frmMain.mnuSmartLineAdjust.ShortCut then
    mmoSub1.Text := AdjustLines(mmoSub1.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.mmoSub2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = frmMain.mnuSmartLineAdjust.ShortCut then
    mmoSub2.Text := AdjustLines(mmoSub2.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn11Click(Sender: TObject);
begin
  CalculateTimes(1, 1);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn12Click(Sender: TObject);
begin
  CalculateTimes(1, 2);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn21Click(Sender: TObject);
begin
  CalculateTimes(2, 1);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn23Click(Sender: TObject);
begin
  CalculateTimes(2, 3);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn32Click(Sender: TObject);
begin
  CalculateTimes(3, 2);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn13Click(Sender: TObject);
begin
  CalculateTimes(1, 3);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.btn31Click(Sender: TObject);
begin
  CalculateTimes(3, 1);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.chkUseAutoDurClick(Sender: TObject);
  procedure EnableCtrls(const Value: Boolean);
  begin
    chkContinueDirectly.Enabled := Value;
    tmeHideSub1.Enabled         := Value;
    tmeDuration1.Enabled        := Value;
    if chkContinueDirectly.Checked then
      tmeShowSub2.Enabled := False else
      tmeShowSub2.Enabled := Value = True;
  end;
begin
  CalculateTimes(1, 1, chkUseAutoDur.Checked);
  EnableCtrls(not chkUseAutoDur.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('General', 'Use automatic durations (Divide lines)', chkUseAutoDur.Checked);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.tmeShowSub2TimeChangeFromEditOnly(
  Sender: TObject; NewTime: Cardinal);
begin
  tmeDuration2.Time := Cardinal(GetFinalTime(frmMain.lstSubtitles.FocusedNode)) - tmeShowSub2.Time;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.tmeHideSub1TimeChangeFromEditOnly(
  Sender: TObject; NewTime: Cardinal);
var
  Time: Integer;
begin
  if (chkContinueDirectly.Checked) then
  begin
    Time := tmeHideSub1.Time;
    if (Time > -1) then
    begin
      tmeShowSub2.Time  := Time + 1;
      tmeDuration1.Time := Time - GetStartTime(frmMain.lstSubtitles.FocusedNode);
      tmeDuration2.Time := GetFinalTime(frmMain.lstSubtitles.FocusedNode) - (StringToTime(tmeShowSub2.Text));
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDivideLines.tmeDuration1TimeChangeFromEditOnly(
  Sender: TObject; NewTime: Cardinal);
var
  Time: Integer;
begin
  if (chkContinueDirectly.Checked) then
  begin
    Time := tmeDuration1.Time;
    if (Time > -1) then
    begin
      tmeShowSub2.Time  := GetStartTime(frmMain.lstSubtitles.FocusedNode) + Time + 1;
      if GetFocus <> tmeHideSub1.Handle then
        tmeHideSub1.Time  := GetStartTime(frmMain.lstSubtitles.FocusedNode) + Time;
      tmeDuration2.Time := GetFinalTime(frmMain.lstSubtitles.FocusedNode) - (StringToTime(tmeShowSub2.Text));
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
