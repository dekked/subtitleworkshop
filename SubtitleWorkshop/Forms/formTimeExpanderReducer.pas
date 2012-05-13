unit formTimeExpanderReducer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ExtCtrls, ComCtrls, General, IniFiles, VirtualTrees,
  Functions, USubtitlesFunctions, TreeViewHandle, Undo;

type
  TfrmTimeExpanderReducer = class(TForm)
    pnlTimeExpander: TPanel;
    lblModifyDuration: TLabel;
    edtTimeToExpand: TMaskEdit;
    lblSecOrFrames: TLabel;
    chkOnlyIfLongerThan: TCheckBox;
    edtChars: TEdit;
    udChars: TUpDown;
    lblChars: TLabel;
    rdoSelSubs: TRadioButton;
    bvlSeparator: TBevel;
    btnApply: TButton;
    btnCancel: TButton;
    chkOnlyIfDuration: TCheckBox;
    lblSecOrFrames2: TLabel;
    edtMinMaxDuration: TMaskEdit;
    rdoAllSubs: TRadioButton;
    chkPreventOverlapping: TCheckBox;
    Panel1: TPanel;
    rdoExpandDuration: TRadioButton;
    rdoReduceDuration: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkOnlyIfLongerThanClick(Sender: TObject);
    procedure chkOnlyIfDurationClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edtTimeToExpandKeyPress(Sender: TObject; var Key: Char);
    procedure edtMinMaxDurationKeyPress(Sender: TObject; var Key: Char);
    procedure rdoExpandDurationClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmTimeExpanderReducer: TfrmTimeExpanderReducer;
  ShorterThan : String;
  LongerThan  : String;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                   := ReadString('Time expander/reducer', '01', 'Time expander/reducer');
      rdoExpandDuration.Caption := ReadString('Time expander/reducer', '02', 'Expand duration');
      rdoReduceDuration.Caption := ReadString('Time expander/reducer', '03', 'Reduce duration');
      lblModifyDuration.Caption := ReadString('Time expander/reducer', '04', 'Expand/reduce duration:');
      if frmMain.FormatType = ftTime then
        lblSecOrFrames.Caption  := ReadString('Time expander/reducer', '05', 'Seconds') else
        lblSecOrFrames.Caption  := ReadString('Time expander/reducer', '06', 'Frames');

      lblSecOrFrames2.Caption       := lblSecOrFrames.Caption;
      chkOnlyIfLongerThan.Caption   := ReadString('Time expander/reducer', '07', 'Only if subtitle is longer than:');
      lblChars.Caption              := ReadString('Time expander/reducer', '09', 'characters');
      LongerThan                    := ReadString('Time expander/reducer', '10', 'Only if duration is longer than:');
      ShorterThan                   := ReadString('Time expander/reducer', '11', 'Only if duration is shorter than:');
      chkPreventOverlapping.Caption := ReadString('Time expander/reducer', '12', 'Prevent overlapping');

      rdoAllSubs.Caption := ReadString('Time expander/reducer', '13', 'For all the subtitles');
      rdoSelSubs.Caption := ReadString('Time expander/reducer', '14', 'For selected subtitles');
                                                                                     
      btnApply.Caption   := BTN_APPLY;
      btnCancel.Caption  := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnApply.ParentFont := True;
      Font                := frmMain.Font;
      btnApply.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    rdoExpandDuration.Checked := Ini.ReadBool('Time expander', 'Expand', True);
    rdoReduceDuration.Checked := not rdoExpandDuration.Checked;
    if frmMain.FormatType = ftTime then
    begin
      edtTimeToExpand.EditMask   := '00,000;1;_';
      edtMinMaxDuration.EditMask := '00,000;1;_';
      edtTimeToExpand.Text       := Ini.ReadString('Time expander', 'Expand time', '01,500');
      edtMinMaxDuration.Text     := Ini.ReadString('Time expander', 'Minimum duration time', '01,000');
    end else
    begin
      edtTimeToExpand.EditMask   := '';
      edtMinMaxDuration.EditMask := '';
      edtTimeToExpand.Text       := Ini.ReadString('Time expander', 'Expand frames', '40');
      edtMinMaxDuration.Text     := Ini.ReadString('Time expander', 'Minimum duration frames', '28');
    end;
    chkOnlyIfLongerThan.Checked     := Ini.ReadBool('Time expander', 'Only if bigger than', True);
    udChars.Position                := Ini.ReadInteger('Time expander', 'Chars', 40);
    chkOnlyIfDuration.Checked       := Ini.ReadBool('Time expander', 'Only if duration is longer/shorter than', True);
    chkPreventOverlapping.Checked   := Ini.ReadBool('Time expander', 'Prevent overlapping', True);
    rdoAllSubs.Checked              := Ini.ReadBool('Time expander', 'For all subtitles', True);
    rdoSelSubs.Checked              := not Ini.ReadBool('Time expander', 'For all subtitles', True);
    chkOnlyIfLongerThanClick(Sender);
    chkOnlyIfDurationClick(Sender);
    if rdoExpandDuration.Checked then
    begin
      chkOnlyIfDuration.Caption     := ShorterThan;
      chkPreventOverlapping.Enabled := True;
    end else
    begin
      chkOnlyIfDuration.Caption     := LongerThan;
      chkPreventOverlapping.Enabled := False;
    end;
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Time expander', 'Expand', rdoExpandDuration.Checked);
    if frmMain.FormatType = ftTime then
    begin
      Ini.WriteString('Time expander', 'Expand time', edtTimeToExpand.Text);
      Ini.WriteString('Time expander', 'Minimum duration time', edtMinMaxDuration.Text);
    end else
    begin
      Ini.WriteString('Time expander', 'Expand frames', edtTimeToExpand.Text);
      Ini.WriteString('Time expander', 'Minimum duration frames', edtMinMaxDuration.Text);
    end;
    Ini.WriteBool('Time expander', 'Only if bigger than', chkOnlyIfLongerThan.Checked);
    Ini.WriteInteger('Time expander', 'Chars', udChars.Position);
    Ini.WriteBool('Time expander', 'Only if duration is longer/shorter than', chkOnlyIfDuration.Checked);
    Ini.WriteBool('Time expander', 'Prevent overlapping', chkPreventOverlapping.Checked);
    Ini.WriteBool('Time expander', 'For all subtitles', rdoAllSubs.Checked);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.chkOnlyIfLongerThanClick(Sender: TObject);
begin
  if chkOnlyIfLongerThan.Checked then
  begin
    edtChars.Enabled := True;
    udChars.Enabled  := True;
    lblChars.Enabled := True;
  end else
  begin
    edtChars.Enabled := False;
    udChars.Enabled  := False;
    lblChars.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.chkOnlyIfDurationClick(Sender: TObject);
begin
  if chkOnlyIfDuration.Checked then
  begin
    edtMinMaxDuration.Enabled := True;
    lblSecOrFrames2.Enabled   := True;
  end else
  begin
    edtMinMaxDuration.Enabled := False;
    lblSecOrFrames2.Enabled   := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.btnApplyClick(Sender: TObject);
var
  Node           : PVirtualNode;
  ExpandOrReduce : Integer;
  MinMaxDur      : Integer;
  Flag           : Boolean;
  FinalTime      : Integer;
  UndoAction     : PUndoAction;
begin
  if frmMain.FormatType = ftTime then
  begin
    ExpandOrReduce := StrSecToMS(edtTimeToExpand.Text);
    MinMaxDur      := StrSecToMS(edtMinMaxDuration.Text);
  end else
  begin
    ExpandOrReduce := FramesToTime(StrToIntDef(edtTimeToExpand.Text, 0), GetFPS);
    MinMaxDur      := FramesToTime(StrToIntDef(edtMinMaxDuration.Text, 0), GetFPS);
  end;

  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    if rdoSelSubs.Checked then
      Node := lstSubtitles.GetFirstSelected else
      Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      Flag := True;
      if chkOnlyIfLongerThan.Checked then
      begin
        if Length(GetSubText(Node)) <= udChars.Position then
          Flag := False;
      end;
      FinalTime := GetFinalTime(Node);
      if Flag = True then
      begin
        if chkOnlyIfDuration.Checked then
        begin
          if rdoExpandDuration.Checked then
          begin
            if (FinalTime - GetStartTime(Node)) > MinMaxDur then
              Flag := False;
          end else
          begin
            if (FinalTime - GetStartTime(Node)) < MinMaxDur then
              Flag := False;
          end;
        end;
        if Flag = True then
        begin
          New(UndoAction);
          UndoAction^.UndoActionType                 := uaTimeChange;
          UndoAction^.BufferSize                     := SizeOf(TTimeChange);
          UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
          UndoAction^.Node                           := Node;
          UndoAction^.LineNumber                     := Node.Index;
          UndoAction^.BindToNext                     := ((rdoSelSubs.Checked) and (Assigned(lstSubtitles.GetNextSelected(Node)))) or ((rdoSelSubs.Checked = False) and (Assigned(Node.NextSibling)));
          PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
          PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
          UndoList.Add(UndoAction);

          if rdoExpandDuration.Checked then
          begin
            FinalTime := FinalTime + ExpandOrReduce;
            if chkPreventOverlapping.Checked then
            begin
              if (FinalTime > GetStartTime(Node.NextSibling)) and (Node <> lstSubtitles.GetLast) then
                FinalTime := GetStartTime(Node.NextSibling) - 1;
            end;
          end else
          begin
            FinalTime := FinalTime - ExpandOrReduce;
            if (FinalTime - GetStartTime(Node)) < 600 then
              FinalTime := GetStartTime(Node) + 600;
          end;

          SetFinalTime(Node, FinalTime);
        end;
      end;
      if rdoSelSubs.Checked then
        Node := lstSubtitles.GetNextSelected(Node) else
        Node := Node.NextSibling;
    end;

    mnuUndo.Enabled := True;
    RefreshTimes;
    frmTimeExpanderReducer.Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.edtTimeToExpandKeyPress(Sender: TObject;
  var Key: Char);
begin
  if frmMain.FormatType = ftFrames then
  begin
    if not (Key in['0'..'9', Chr(VK_RETURN), Chr(VK_BACK), Chr(VK_ESCAPE)]) then
      Key := #0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.edtMinMaxDurationKeyPress(Sender: TObject;
  var Key: Char);
begin
  if frmMain.FormatType = ftFrames then
  begin
    if not (Key in['0'..'9', Chr(VK_RETURN), Chr(VK_BACK), Chr(VK_ESCAPE)]) then
      Key := #0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpanderReducer.rdoExpandDurationClick(Sender: TObject);
begin
  if rdoExpandDuration.Checked then
  begin
    chkOnlyIfDuration.Caption     := ShorterThan;
    chkPreventOverlapping.Enabled := True;
  end else
  begin
    chkOnlyIfDuration.Caption     := LongerThan;
    chkPreventOverlapping.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

end.
