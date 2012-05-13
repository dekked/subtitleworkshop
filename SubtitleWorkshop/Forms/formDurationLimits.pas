unit formDurationLimits;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, General, VirtualTrees, TreeViewHandle, IniFiles,
  Undo;

type
  TfrmDurationLimits = class(TForm)
    btnApply: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    edtMaxDur: TLabeledEdit;
    chkSetMaxDur: TCheckBox;
    lblNoOverlapping: TLabel;
    edtMinDur: TLabeledEdit;
    chkSetMinDur: TCheckBox;
    procedure edtMaxDurKeyPress(Sender: TObject; var Key: Char);
    procedure edtMinDurKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmDurationLimits: TfrmDurationLimits;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                     := ReadString('Duration limits', '01', 'Duration limits');
      chkSetMaxDur.Caption        := ReadString('Duration limits', '02', 'Set maximum duration');
      edtMaxDur.EditLabel.Caption := ReadString('Duration limits', '03', 'Milliseconds');
      chkSetMinDur.Caption        := ReadString('Duration limits', '04', 'Set minimum duration');
      edtMinDur.EditLabel.Caption := edtMaxDur.EditLabel.Caption;
      lblNoOverlapping.Caption    := ReadString('Duration limits', '05', '* Increasing the time will not cause overlapping');
      btnApply.Caption            := BTN_APPLY;
      btnCancel.Caption           := BTN_CANCEL;

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

procedure TfrmDurationLimits.edtMaxDurKeyPress(Sender: TObject;
  var Key: Char);
begin
 if Not (Key in['0'..'9', Chr(VK_RETURN), Chr(VK_BACK)]) then
    Key := #0;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.edtMinDurKeyPress(Sender: TObject;
  var Key: Char);
begin
 if Not (Key in['0'..'9', Chr(VK_RETURN), Chr(VK_BACK)]) then
    Key := #0;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    chkSetMaxDur.Checked   := Ini.ReadBool('Duration limits','Set maximum duration',True);
    chkSetMinDur.Checked   := Ini.ReadBool('Duration limits','Set minimum duration',True);
    edtMaxDur.Text         := IntToStr(Ini.ReadInteger('Duration limits', 'Maximum duration', 4000));
    edtMinDur.Text         := IntToStr(Ini.ReadInteger('Duration limits', 'Minimum duration', 800));
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Duration limits','Set maximum duration',chkSetMaxDur.Checked);
    Ini.WriteBool('Duration limits','Set minimum duration',chkSetMinDur.Checked);
    Ini.WriteInteger('Duration limits', 'Maximum duration', StrToInt(edtMaxDur.Text));
    Ini.WriteInteger('Duration limits', 'Minimum duration', StrToInt(edtMinDur.Text));
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmDurationLimits.btnApplyClick(Sender: TObject);
var
  InitialTime : Integer;
  FinalTime   : Integer;
  Duration    : Integer;
  Node        : PVirtualNode;
  UndoAction  : PUndoAction;
begin
  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    Node := lstSubtitles.GetFirst;

    while Assigned(Node) do
    begin
      InitialTime := GetStartTime(Node);
      FinalTime   := GetFinalTime(Node);
      Duration    := FinalTime - InitialTime;

      // ---------------- //
      // Maximum duration //
      // ---------------- //
      if (chkSetMaxDur.Checked) and (Duration > StrToInt(edtMaxDur.Text)) and (StrToInt(edtMaxDur.Text) > 0) then
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                 := uaTimeChange;
        UndoAction^.BufferSize                     := SizeOf(TTimeChange);
        UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
        UndoAction^.Node                           := Node;
        UndoAction^.LineNumber                     := Node.Index;
        UndoAction^.BindToNext                     := Assigned(Node.NextSibling);
        PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
        PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
        UndoList.Add(UndoAction);

        FinalTime := InitialTime + StrToInt(edtMaxDur.Text);
        SetFinalTime(Node, FinalTime);
        Duration := FinalTime - InitialTime;
      end;

      // ---------------- //
      // Mimimum duration //
      // ---------------- //
      if (chkSetMinDur.Checked) and (Duration < StrToInt(edtMinDur.Text)) then
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                 := uaTimeChange;
        UndoAction^.BufferSize                     := SizeOf(TTimeChange);
        UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
        UndoAction^.Node                           := Node;
        UndoAction^.LineNumber                     := Node.Index;
        UndoAction^.BindToNext                     := Assigned(Node.NextSibling);
        PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
        PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
        UndoList.Add(UndoAction);

        FinalTime := InitialTime + StrToInt(edtMinDur.Text);
        if (Node <> lstSubtitles.GetLast) and (FinalTime > GetStartTime(Node.NextSibling)) then
          FinalTime := GetStartTime(Node.NextSibling) - 1;
        SetFinalTime(Node, FinalTime);
      end;

      Node := Node.NextSibling;
    end;
    
    mnuUndo.Enabled := True;
    OrgModified   := True;
    TransModified := True;
    RefreshTimes;
  end;
  Close;
end;

// -----------------------------------------------------------------------------

end.
