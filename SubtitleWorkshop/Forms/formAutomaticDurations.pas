unit formAutomaticDurations;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, FastStringFuncs, TreeViewHandle, Functions, StrMan,
  General, ExtCtrls, IniFiles, ComCtrls, Undo;

type
  TfrmAutomaticDurations = class(TForm)
    btnOk: TButton;
    gbMilliseconds: TGroupBox;
    lblPerCharacter: TLabel;
    edtMSPerCharacter: TEdit;
    udMSPerCharacter: TUpDown;
    lblPerWord: TLabel;
    edtMSPerWord: TEdit;
    udMSPerWord: TUpDown;
    lblPerLine: TLabel;
    edtMSPerLine: TEdit;
    udMSPerLine: TUpDown;
    btnCancel: TButton;
    pnlWhere: TPanel;
    rdoSelectedSubs: TRadioButton;
    rdoAllSubs: TRadioButton;
    Panel1: TPanel;
    rdoNewDurGreaterOrg: TRadioButton;
    rdoAllCases: TRadioButton;
    rdoNewDurSmallerOrg: TRadioButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmAutomaticDurations: TfrmAutomaticDurations;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                     := ReadString('Automatic durations', '01', 'Automatic durations');
      gbMilliseconds.Caption      := ReadString('Automatic durations', '02', 'Milliseconds');
      lblPerCharacter.Caption     := ReadString('Automatic durations', '03', 'per character');
      lblPerWord.Caption          := ReadString('Automatic durations', '04', 'per word');
      lblPerLine.Caption          := ReadString('Automatic durations', '05', 'per line');
      rdoAllCases.Caption         := ReadString('Automatic durations', '06', 'Apply new duration in all cases');
      rdoNewDurGreaterOrg.Caption := ReadString('Automatic durations', '07', 'Only if new duration is greater than original');
      rdoNewDurSmallerOrg.Caption := ReadString('Automatic durations', '08', 'Only if new duration is smaller than original');
      rdoAllSubs.Caption          := ReadString('Automatic durations', '09', 'All subtitles');
      rdoSelectedSubs.Caption     := ReadString('Automatic durations', '10', 'Selected subtitles only');
      btnOk.Caption               := BTN_OK;
      btnCancel.Caption           := BTN_CANCEL;
      
      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnOk.ParentFont := True;
      Font             := frmMain.Font;
      btnOk.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.btnOkClick(Sender: TObject);
var
  Node        : PVirtualNode;
  InitialTime : Integer;
  OldDuration : Integer;
  NewDuration : Integer;
  Text        : String;
  UndoAction  : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;

  if rdoAllSubs.Checked then
    Node := frmMain.lstSubtitles.GetFirst else
    Node := frmMain.lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    // Duration = Tcharacter x Ncharacters + Tword x Nwords + Tline x Nlines
    Text := RemoveSWTags(GetSubText(Node), True, True, True, True);
    NewDuration := (
                    udMSPerCharacter.Position * Length(Text)-StringCount(' ', Text)-StringCount(#13#10, Text)*2 +
                    udMSPerWord.Position      * sm.CountWords(Text) +
                    udMSPerLine.Position      * (StringCount(#13#10, Text)+1)
                    );
    OldDuration := GetFinalTime(Node) - GetStartTime(Node);

    InitialTime := GetStartTime(Node);
    if Node <> frmMain.lstSubtitles.GetLast then
    begin
      // Avoid overlapping
      if (InitialTime + NewDuration) > GetStartTime(Node.NextSibling) then
        NewDuration := GetStartTime(Node.NextSibling) - 1 - InitialTime;
    end;

    if (rdoAllCases.Checked) or
       ((rdoNewDurGreaterOrg.Checked) and (NewDuration > OldDuration)) or
       ((rdoNewDurSmallerOrg.Checked) and (NewDuration < OldDuration)) then
    begin
      New(UndoAction);
      UndoAction^.UndoActionType                 := uaTimeChange;
      UndoAction^.BufferSize                     := SizeOf(TTimeChange);
      UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
      UndoAction^.Node                           := Node;
      UndoAction^.LineNumber                     := Node.Index;
      UndoAction^.BindToNext                     := ((rdoAllSubs.Checked) and (Assigned(Node.NextSibling))) or ((rdoAllSubs.Checked = False) and (Assigned(frmMain.lstSubtitles.GetNextSelected(Node))));
      PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
      PTimeChange(UndoAction^.Buffer)^.FinalTime := GetFinalTime(Node);
      UndoList.Add(UndoAction);

      SetFinalTime(Node, InitialTime + NewDuration);
    end;

    if rdoAllSubs.Checked then
      Node := Node.NextSibling else
      Node := frmMain.lstSubtitles.GetNextSelected(Node);
  end;
  
  frmMain.mnuUndo.Enabled := True;
  frmMain.lstSubtitles.Refresh;
  frmMain.RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    udMSPerCharacter.Position := Ini.ReadInteger('Automatic durations', 'Per character', 60);
    udMSPerWord.Position      := Ini.ReadInteger('Automatic durations', 'Per word', 50);
    udMSPerLine.Position      := Ini.ReadInteger('Automatic durations', 'Per line', 50);
    rdoAllSubs.Checked        := Ini.ReadBool('Automatic durations', 'All subtitles', True);
    rdoSelectedSubs.Checked   := not Ini.ReadBool('Automatic durations', 'All subtitles', True);
    case Ini.ReadInteger('Automatic durations', 'Apply if', 1) of
      2: rdoNewDurGreaterOrg.Checked := True;
      3: rdoNewDurSmallerOrg.Checked := True else
      rdoAllCases.Checked := True;
    end;
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteInteger('Automatic durations', 'Per character', udMSPerCharacter.Position);
    Ini.WriteInteger('Automatic durations', 'Per word', udMSPerWord.Position);
    Ini.WriteInteger('Automatic durations', 'Per line', udMSPerLine.Position);
    Ini.WriteBool('Automatic durations', 'All subtitles', rdoAllSubs.Checked);
    if rdoAllCases.Checked         then Ini.WriteInteger('Automatic durations', 'Apply if', 1) else
    if rdoNewDurGreaterOrg.Checked then Ini.WriteInteger('Automatic durations', 'Apply if', 2) else
    if rdoNewDurSmallerOrg.Checked then Ini.WriteInteger('Automatic durations', 'Apply if', 3);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
