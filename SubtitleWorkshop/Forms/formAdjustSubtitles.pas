unit formAdjustSubtitles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ExtCtrls, USubtitlesFunctions, TreeViewHandle, Functions,
  IniFiles, General, VirtualTrees, USubtitleAdjust, VideoPreview, Undo,
  ComCtrls, TimeMaskEdit, FastStrings;

type
  PSyncPoint = ^TSyncPoint;
  TfrmAdjustSubtitles = class(TForm)
    btnAdjust: TButton;
    btnCancel: TButton;
    pgeMode: TPageControl;
    pgeSimple: TTabSheet;
    pgeAdvanced: TTabSheet;
    lstPoints: TVirtualStringTree;
    btnAdd: TButton;
    btnRemove: TButton;
    lblIfTimeOutsideScope: TLabel;
    rdoExtrapolate: TRadioButton;
    rdoReturnOrgTime: TRadioButton;
    rdoNeighbour: TRadioButton;
    lblFirstSpokenLine: TLabel;
    lblLastSpokenLine: TLabel;
    tmeFirstSpokenLine: TTimeMaskEdit;
    tmeLastSpokenLine: TTimeMaskEdit;
    btnLoadFromFile: TButton;
    btnSaveToFile: TButton;
    btnAddFromMedia: TButton;
    dlgSaveToFile: TSaveDialog;
    dlgLoadFromFile: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnAdjustClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstPointsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure lstPointsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure lstPointsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure lstPointsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstPointsEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure lstPointsNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure btnAddFromMediaClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    procedure AddSyncPoint(const OldTime, NewTime: Integer; const LineNum: Integer = -1);
  end;

var
  frmAdjustSubtitles: TfrmAdjustSubtitles;

implementation

uses formMain, formAdjustSubsEnterNewSyncPoint;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                    := ReadString('Adjust subtitles', '01', 'Adjust subtitles');
      pgeSimple.Caption          := ReadString('Adjust subtitles', '02', 'Simple');
      pgeAdvanced.Caption        := ReadString('Adjust subtitles', '03', 'Advanced');
      lblFirstSpokenLine.Caption := ReadString('Adjust subtitles', '04', 'First spoken line:');
      lblLastSpokenLine.Caption  := ReadString('Adjust subtitles', '05', 'Last spoken line:');
      btnLoadFromFile.Caption    := ReadString('Adjust subtitles', '06', 'Load from file');
      btnSaveToFile.Caption      := ReadString('Adjust subtitles', '07', 'Save to file');

      lstPoints.Header.Columns[1].Text := StringToWideStringEx(ReadString('Adjust subtitles', '08', 'Line #'), CharSetToCodePage(frmMain.Font.Charset));
      lstPoints.Header.Columns[2].Text := StringToWideStringEx(ReadString('Adjust subtitles', '09', 'Old time'), CharSetToCodePage(frmMain.Font.Charset));
      lstPoints.Header.Columns[3].Text := StringToWideStringEx(ReadString('Adjust subtitles', '10', 'New time'), CharSetToCodePage(frmMain.Font.Charset));

      btnAdd.Caption                := ReadString('Adjust subtitles', '11', '&Add');
      btnAddFromMedia.Caption       := ReadString('Adjust subtitles', '12', 'Add from &media');
      btnRemove.Caption             := ReadString('Adjust subtitles', '13', '&Remove');
      lblIfTimeOutsideScope.Caption := ReadString('Adjust subtitles', '14', 'If time is outside points scope:');
      rdoExtrapolate.Caption        := ReadString('Adjust subtitles', '15', 'Extrapolate');
      rdoReturnOrgTime.Caption      := ReadString('Adjust subtitles', '16', 'Return original time (no changes)');
      rdoNeighbour.Caption          := ReadString('Adjust subtitles', '17', 'Return displacement of neighbour point');

      btnAdjust.Caption          := ReadString('Adjust subtitles', '18', '&Adjust!');
      btnCancel.Caption          := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnAdjust.ParentFont := True;
      Font                 := frmMain.Font;
      btnAdjust.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.AddSyncPoint(const OldTime, NewTime: Integer; const LineNum: Integer = -1);
var
  Data: PSyncPoint;
begin
  lstPoints.RootNodeCount := lstPoints.RootNodeCount + 1;
  Data         := lstPoints.GetNodeData(lstPoints.GetLast);
  Data.OldTime := OldTime;
  Data.NewTime := NewTime;
  Data.LineNum := LineNum;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.FormCreate(Sender: TObject);
var
  Ini : TIniFile;
  i   : Integer;
begin
  SetLanguage;
  tmeFirstSpokenLine.FPS := GetFPS;
  tmeLastSpokenLine.FPS  := GetFPS;
  if frmMain.FormatType = ftTime then
  begin
    tmeFirstSpokenLine.TimeMode := tmTime;
    tmeLastSpokenLine.TimeMode  := tmTime;
  end else
  begin
    tmeFirstSpokenLine.TimeMode  := tmFrames;
    tmeLastSpokenLine.TimeMode   := tmFrames;
    tmeFirstSpokenLine.MaxLength := 7;
    tmeLastSpokenLine.MaxLength  := 7;
  end;
  tmeFirstSpokenLine.Time := GetStartTime(frmMain.lstSubtitles.GetFirst);
  tmeLastSpokenLine.Time  := GetStartTime(frmMain.lstSubtitles.GetLast);

  for i := 0 to High(frmMain.SyncPointsArray) do
  begin
    AddSyncPoint(frmMain.SyncPointsArray[i].OldTime,
                 frmMain.SyncPointsArray[i].NewTime,
                 frmMain.SyncPointsArray[i].LineNum);
  end;

  Ini := TIniFile.Create(IniRoot);
  try
    case Ini.ReadInteger('Adjust subtitles', 'Mode', 0) of
      1: rdoReturnOrgTime.Checked := True;
      2: rdoNeighbour.Checked := True else
      rdoExtrapolate.Checked := True;
    end;    
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnAdjustClick(Sender: TObject);
var
  ta         : TAdjust;
  Node       : PVirtualNode;
  Data       : PSyncPoint;
  Data2      : PSubtitleItem;
  UndoAction : PUndoAction;
  Points     : TClassicSyncPoints;
begin
  if pgeMode.ActivePageIndex = 0 then
  begin
    Points.Point1Sub   := GetStartTime(frmMain.lstSubtitles.GetFirst);
    Points.Point2Sub   := GetStartTime(frmMain.lstSubtitles.GetLast);
    Points.Point1Movie := tmeFirstSpokenLine.Time;
    Points.Point2Movie := tmeLastSpokenLine.Time;
    AdjustSubtitles(Points);
    frmAdjustSubtitles.Close;
  end else
  begin
    if lstPoints.RootNodeCount < 2 then
      MsgBox('', BTN_OK, '', '', MB_ICONERROR, Self) else
    begin
      ta := TAdjust.Create;
      ta.Init;
      Node := lstPoints.GetFirst;
      while Assigned(Node) do
      begin
        Data := lstPoints.GetNodeData(Node);
        ta.Add(Data.OldTime, Data.NewTime-Data.OldTime);
        Node := Node.NextSibling;
      end;

      ClearUndoList(RedoList);
      frmMain.mnuRedo.Enabled := False;

      Node := frmMain.lstSubtitles.GetFirst;
      while Assigned(Node) do
      begin
        Data2 := frmMain.lstSubtitles.GetNodeData(Node);
        if Assigned(Data2) then
        begin

          New(UndoAction);
          UndoAction^.UndoActionType := uaTimeChange;
          UndoAction^.BufferSize     := SizeOf(TTimeChange);
          UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
          UndoAction^.Node           := Node;
          UndoAction^.LineNumber     := Node.Index;
          UndoAction^.BindToNext     := True;
          PTimeChange(UndoAction^.Buffer)^.StartTime := Data2.InitialTime;
          PTimeChange(UndoAction^.Buffer)^.FinalTime := Data2.FinalTime;
          UndoList.Add(UndoAction);

          if rdoExtrapolate.Checked then
          begin
            Data2.InitialTime := ta.CalcLineValue(Data2.InitialTime, EXTRAPOLATE);
            Data2.FinalTime   := ta.CalcLineValue(Data2.FinalTime, EXTRAPOLATE);
          end else
          if rdoReturnOrgTime.Checked then
          begin
            Data2.InitialTime := ta.CalcLineValue(Data2.InitialTime, ZEROFILL);
            if Node = frmMain.lstSubtitles.GetLast then
              Data2.FinalTime   := ta.CalcLineValue(Data2.FinalTime, EXTRAPOLATE) else // Avoid problem calculating final time of last subtitle (because time is outside the scope)
              Data2.FinalTime   := ta.CalcLineValue(Data2.FinalTime, ZEROFILL);
          end else
          if rdoNeighbour.Checked then
          begin
            Data2.InitialTime := ta.CalcLineValue(Data2.InitialTime, NEIGHBOUR);
            Data2.FinalTime   := ta.CalcLineValue(Data2.FinalTime, NEIGHBOUR);
          end;
        end;

        Node := frmMain.lstSubtitles.GetNextSibling(Node);
      end;

      if UndoList.Count > 0 then
        PUndoAction(UndoList.Last)^.BindToNext := False;    

      frmMain.mnuUndo.Enabled := True;
      frmMain.OrgModified     := True;
      frmMain.TransModified   := True;
      SetLength(frmMain.SyncPointsArray, 0);
      ta.Free;
      frmAdjustSubtitles.Close;
      frmMain.RefreshTimes;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnCancelClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frmMain.AdjustFormOpened := False;
  Action := caFree;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.lstPointsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSyncPoint);
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.lstPointsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PSyncPoint;
begin
  Data         := Sender.GetNodeData(Node);
  Data.OldTime := 0;
  Data.NewTime := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.lstPointsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PSyncPoint;
begin
  Data         := Sender.GetNodeData(Node);
  Data.OldTime := 0;
  Data.NewTime := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.lstPointsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PSyncPoint;
begin
  Data := lstPoints.GetNodeData(Node);
  case Column of
    0: CellText := IntToStr(Node.Index + 1);
    1:
      begin
        if Data.LineNum > -1 then
          CellText := IntToStr(Data.LineNum) else
          CellText := '-';
      end;
    2:
      begin
        if frmMain.FormatType = ftTime then
          CellText := TimeToString(Data.OldTime) else
          CellText := IntToStr(TimeToFrames(Data.OldTime, GetFPS));
      end;
    3:
      begin
        if frmMain.FormatType = ftTime then
          CellText := TimeToString(Data.NewTime) else
          CellText := IntToStr(TimeToFrames(Data.NewTime, GetFPS));
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnAddFromMediaClick(Sender: TObject);
begin
  with frmMain do
  begin
    if (MovieFile <> '') then
    begin
      if (lstSubtitles.RootNodeCount > 1) and (Assigned(lstSubtitles.FocusedNode)) and (lstSubtitles.SelectedCount = 1) then
        AddSyncPoint(GetStartTime(lstSubtitles.GetFirstSelected), GetCurrentPos, lstSubtitles.GetFirstSelected.Index + 1);
      lstPoints.FocusedNode := lstPoints.GetLast;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnRemoveClick(Sender: TObject);
begin
  lstPoints.DeleteSelectedNodes;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.FormDestroy(Sender: TObject);
var
  Ini : TIniFile;
  i   : Byte;
  Data : PSyncPoint;
  Node : PVirtualNode;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    i := 0;
    if rdoReturnOrgTime.Checked then i := 1 else
    if rdoNeighbour.Checked then i := 2;
    Ini.WriteInteger('Adjust subtitles', 'Mode', i);

    // Save sync points to array
    SetLength(frmMain.SyncPointsArray, 0);
    Node := lstPoints.GetFirst;
    while Assigned(Node) do
    begin
      SetLength(frmMain.SyncPointsArray, Length(frmMain.SyncPointsArray)+1);
      Data := lstPoints.GetNodeData(Node);
      i := High(frmMain.SyncPointsArray);
      frmMain.SyncPointsArray[i].LineNum := Data^.LineNum;
      frmMain.SyncPointsArray[i].OldTime := Data^.OldTime;
      frmMain.SyncPointsArray[i].NewTime := Data^.NewTime;

      Node := Node.NextSibling;
    end;

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.lstPointsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  case Column of
    0,1: Allowed := False else
    Allowed := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.lstPointsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  Data : PSyncPoint;
  tmp  : Integer;
begin
  Data := lstPoints.GetNodeData(Node);
  if frmMain.FormatType = ftTime then
    tmp := StringToTime(NewText) else
  begin
    tmp := StrToIntDef(NewText, -1);
    if tmp > -1 then
      tmp := FramesToTime(tmp, GetFPS);
  end;

  if (Column = 2) then
  begin
    if tmp > -1 then
      Data.OldTime := tmp;
  end else
  if (Column = 3) then
  begin
    if tmp > -1 then
      Data.NewTime := tmp;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnAddClick(Sender: TObject);
begin
  frmEnterNewSyncPoint := TfrmEnterNewSyncPoint.Create(Application);
  frmEnterNewSyncPoint.ShowModal;
  frmEnterNewSyncPoint.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnLoadFromFileClick(Sender: TObject);
var
  s   : TStringList;
  i,a : Integer;
begin
  if (dlgLoadFromFile.Execute) and (dlgLoadFromFile.FileName <> '') then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(dlgLoadFromFile.FileName);
      for i := 0 to s.Count-1 do
      begin
        a := Pos('|', s[i]);
        AddSyncPoint(StrToIntDef(Copy(s[i], a+1, SmartPos('|', s[i], True, a+1)-(a+1)), -1),
                     StrToIntDef(Copy(s[i], SmartPos('|', s[i], True, a+1) + 1, Length(s[i])), 0),
                     StrToIntDef(Copy(s[i], 1, a-1), 0)
                     );
      end;
    finally
      s.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAdjustSubtitles.btnSaveToFileClick(Sender: TObject);
var
  Node : PVirtualNode;
  Data : PSyncPoint;
  s    : TStringList;
begin
  if (lstPoints.RootNodeCount > 0) and (dlgSaveToFile.Execute) and (dlgSaveToFile.FileName <> '') then
  begin
    if LowerCase(ExtractFileExt(dlgSaveToFile.FileName)) <> '.spf' then
      dlgSaveToFile.FileName := dlgSaveToFile.FileName + '.spf';
    s := TStringList.Create;
    try
      Node := lstPoints.GetFirst;
      while Assigned(Node) do
      begin
        Data := lstPoints.GetNodeData(Node);
        if Assigned(Data) then
          s.Add(Format('%d|%d|%d', [Data.LineNum, Data.OldTime, Data.NewTime]));
        Node := Node.NextSibling;
      end;
    finally
      s.SaveToFile(dlgSaveToFile.FileName);
      s.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
