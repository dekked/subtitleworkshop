unit formJoin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, General, TreeViewHandle, Functions,
  USubtitleAPI, IniFiles;

type
  TfrmJoin = class(TForm)
    btnJoin: TButton;
    btnCancel: TButton;
    pnlJoin: TPanel;
    lblOutputFormat: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    chkLoadFile: TCheckBox;
    cmbOutputFormat: TComboBox;
    chkRecalculate: TCheckBox;
    opnDlg: TOpenDialog;
    dlgSave: TSaveDialog;
    lstFiles: TListView;
    lblAddFiles: TLabel;
    cmbOutputFPS: TComboBox;
    btnClear: TButton;
    cmbFPS: TComboBox;
    lblOutputFPS: TLabel;
    btnSetMovieFrag: TButton;
    btnClearMovieFrag: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure dlgSaveClose(Sender: TObject);
    procedure cmbOutputFPSChange(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
    procedure cmbOutputFormatChange(Sender: TObject);
    procedure lstFilesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lstFilesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btnSetMovieFragClick(Sender: TObject);
    procedure btnClearMovieFragClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmJoin: TfrmJoin;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmJoin.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                     := ReadString('Join','01','Join subtitles...');
      lblAddFiles.Caption         := ReadString('Join','02','Add the subtitle files you wish to join (in order):');
      lstFiles.Columns[0].Caption := ReadString('Join','03','File name');
      lstFiles.Columns[1].Caption := ReadString('Join','04','Format');
      lstFiles.Columns[2].Caption := ReadString('Join','05','FPS');
      lstFiles.Columns[3].Caption := ReadString('Join','06','Size');
      lstFiles.Columns[4].Caption := ReadString('Join','07','Movie fragment (Optional)');
      btnAdd.Caption              := ReadString('Join','08','&Add');
      btnRemove.Caption           := ReadString('Join','09','&Remove');
      btnClear.Caption            := ReadString('Join','10','C&lear');
      btnSetMovieFrag.Caption     := ReadString('Join','11','&Set movie fragment');
      btnClearMovieFrag.Caption   := ReadString('Join','12','C&lear movie fragment');
      lblOutputFormat.Caption     := ReadString('Join','13','Output format:');
      lblOutputFPS.Caption        := ReadString('Join','14','Output FPS:');
      chkLoadFile.Caption         := ReadString('Join','15','Load file after joining and saving');
      chkRecalculate.Caption      := ReadString('Join','16','Recalculate time values');
      btnJoin.Caption             := ReadString('Join','17','&Join!');
      btnCancel.Caption           := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnJoin.ParentFont := True;
      Font               := frmMain.Font;
      btnJoin.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.btnAddClick(Sender: TObject);
var
  i       : Integer;
  FFormat : Integer;
  Item    : TListItem;
  f       : File;
begin
  opnDlg.Filter := frmMain.dlgLoadFile.Filter;
  if (opnDlg.Execute) and (opnDlg.Files.Count > 0) then
  begin
    frmJoin.Enabled := False;
    for i := 0 to opnDlg.Files.Count-1 do
    begin
      Application.ProcessMessages;
      if SubtitleAPI.LoadSubtitle(opnDlg.Files[i], 25) then
        FFormat := SubtitleAPI.CurrentFormatIndex else
        FFormat := 0;
      if (FFormat > 0) then
      begin
        Item := lstFiles.Items.Add;
        Item.Caption := opnDlg.Files[i];
        Item.SubItems.Add(SubtitleAPI.GetFormatName(FFormat));
        if SubtitleAPI.IsFrameBased(FFormat) then
          Item.SubItems.Add(FloatToStr(GetInputFPS)) else
          Item.SubItems.Add('-');

        // Get file size...
        FileMode := fmOpenRead;
        AssignFile(f, opnDlg.Files[i]);
        try
          {$I-}
          Reset(f,1);
          {$I+}
          if IOResult = 0 then
            Item.SubItems.Add(IntToStr(FileSize(f) div 1024) + ' Kb');
        finally
          CloseFile(f);
        end;
      end;
    end;
    frmJoin.Enabled := True;
  end;
end;


// -----------------------------------------------------------------------------

procedure TfrmJoin.btnRemoveClick(Sender: TObject);
begin
  lstFiles.DeleteSelected;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.btnClearClick(Sender: TObject);
begin
  lstFiles.Clear;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.lstFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (lstFiles.ItemIndex > -1) then
    lstFiles.DeleteSelected;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  cmbFPS.Items           := frmMain.cmbInputFPS.Items;
  cmbFPS.ItemIndex       := frmMain.cmbInputFPS.ItemIndex;
  cmbOutputFPS.Items     := frmMain.cmbInputFPS.Items;
  cmbOutputFPS.ItemIndex := frmMain.cmbInputFPS.ItemIndex;

  GetFormatsList(cmbOutputFormat.Items);
  Ini := TIniFile.Create(IniRoot);
  try
    cmbOutputFormat.ItemIndex := SubtitleAPI.GetFormatIndex(Ini.ReadString('Formats','Default format', 'SubRip')) - 1;
    cmbOutputFormatChange(Sender);
    chkLoadFile.Checked       := Ini.ReadBool('Join', 'Load file after joining and saving', True);
    chkRecalculate.Checked    := Ini.ReadBool('Join', 'Recalculate times', False);
    dlgSave.InitialDir        := Ini.ReadString('Join', 'Output directory', '');
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Join', 'Load file after joining and saving', chkLoadFile.Checked);
    Ini.WriteBool('Join', 'Recalculate times', chkRecalculate.Checked);
    Ini.WriteString('Join', 'Output FPS', cmbOutputFPS.Items[cmbOutputFPS.ItemIndex]);
    Ini.WriteString('Join', 'Output directory', dlgSave.InitialDir);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.dlgSaveClose(Sender: TObject);
begin
  if dlgSave.FileName <> '' then
    dlgSave.InitialDir := ExtractFilePath(dlgSave.FileName);
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.cmbOutputFPSChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lstFiles.Items.Count-1 do
  begin
    if (lstFiles.Items[i].Selected) and (lstFiles.Items[i].SubItems[1] <> '-') then
      lstFiles.Items[i].SubItems[1] := cmbFPS.Items[cmbFPS.ItemIndex];
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.btnJoinClick(Sender: TObject);
var
  i,m,z,a    : Integer;
  Delay      : Integer;
  SubFormat  : Integer;
  CurrFPS    : Single;
  OutputFPS  : Single;
  tempFPS    : Single;
  FormatName : String;
  Ext        : String;
begin
  if (lstFiles.Items.Count < 2) then
  begin
    MsgBox(ErrorMsg[10], BTN_OK, '', '', MB_ICONERROR, frmJoin);
    exit;
  end;

  m := 0;
  for i := 0 to lstFiles.Items.Count-1 do
  begin
    if lstFiles.Items[i].SubItems.Count = 4 then
      Inc(m);
  end;
  if (m > 0) and (m <> lstFiles.Items.Count-1) then
  begin
    MsgBox(ErrorMsg[19], BTN_OK, '', '', MB_ICONERROR, Self);
    exit;
  end;

  SubFormat := SubtitleAPI.GetFormatIndex(cmbOutputFormat.Items[cmbOutputFormat.ItemIndex]);
  SubtitleAPI.GetFormatInfo(SubFormat, FormatName, Ext);
  dlgSave.Filter := FormatName + ' (' + Ext + ')|' + Ext;
  if (dlgSave.Execute) and (dlgSave.FileName <> '') then
  begin
    OutputFPS := StrToFloat(cmbOutputFPS.Items[cmbOutputFPS.ItemIndex]);
    SubtitleAPI.CloseSubtitle;
    SubtitleAPI.CreateNewSubtitle;
    Delay := 0;
    for i := 0 to lstFiles.Items.Count-1 do
    begin
      if lstFiles.Items[i].SubItems[1] = '-' then
        CurrFPS := 25 else
        CurrFPS := StrToFloat(lstFiles.Items[i].SubItems[1]);

      if m = 0 then // No movie selected, we use "Recalculate time values" checkbox value
        SubtitleAPI.LoadSubtitle(lstFiles.Items[i].Caption, CurrFPS, 0, True, chkRecalculate.Checked) else
      begin
        z := SubtitleAPI.SubtitleCount;
        SubtitleAPI.LoadSubtitle(lstFiles.Items[i].Caption, CurrFPS, 0, True, False);

        if i > 0 then
        begin
          GetVideoInfo(opnDlg.FileName, tempFPS, a);
          Delay := Delay + a;

          for a := z to SubtitleAPI.SubtitleCount-1 do
          begin
            SubtitleAPI.SetSubtitle(a,
                                    SubtitleAPI.GetInitialTime(a) + Delay,
                                    SubtitleAPI.GetFinalTime(a) + Delay,
                                    SubtitleAPI.GetText(a)
                                   );
          end;
        end;

      end;
    end;
    if ExtractFileExt(dlgSave.FileName) = '' then
      dlgSave.FileName := dlgSave.FileName + Copy(Ext, 2, Length(Ext));

    if FileExists(dlgSave.FileName) then
    begin
      case MsgBox(Format(QuestionMsg[02], [dlgSave.FileName]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, frmJoin) of
        2: begin SubtitleAPI.CloseSubtitle; Close; exit; end;
        3: begin SubtitleAPI.CloseSubtitle; exit; end;
      end;
    end;

    SubtitleAPI.SaveSubtitle(dlgSave.FileName, SubFormat, OutputFPS);
    SubtitleAPI.CloseSubtitle;

    if chkLoadFile.Checked then
    begin
      Close;
      LoadSubtitle(dlgSave.FileName, OutputFPS);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.cmbOutputFormatChange(Sender: TObject);
begin
  if SubtitleAPI.IsFrameBased(cmbOutputFormat.ItemIndex + 1) then
  begin
    lblOutputFPS.Enabled := True;
    cmbOutputFPS.Enabled := True;
  end else
  begin
    lblOutputFPS.Enabled := False;
    cmbOutputFPS.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.lstFilesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
  type
    MyItems = Record
    FileName, Format, FPS, Size: String;
  end;
var
  IT: TListItem;
  i, Ind: Integer;
  vItems: Array of MyItems;
  Total: Integer;
begin
  SetLength(vItems, 0);
  Total := 0;

  for i := TListView(Source).Items.Count-1 downto 0 do
  begin
    if TListView(Source).Items[i].Selected = True then
    begin
      Inc(Total);
      SetLength(vItems, Total);
      vItems[total-1].FileName := TListView(Source).Items[i].Caption;
      vItems[total-1].Format   := TListView(Source).Items[i].SubItems[0];
      vItems[total-1].FPS      := TListView(Source).Items[i].SubItems.Strings[1];
      vItems[total-1].Size     := TListView(Source).Items[i].SubItems.Strings[2];
      TListView(Source).Items.Delete(i);
    end;
  end;

  IT := TListView(Source).GetItemAt(X, Y);
  if IT <> nil then
    Ind := It.Index else
    Ind := TListView(Source).Items.Count;

  TListView(Source).Items.BeginUpdate;
  for i := 0 to Total-1 do
  begin
    IT := TListView(Source).Items.Insert(ind);
    IT.Caption := vItems[i].FileName;
    IT.SubItems.Add(vItems[i].Format);
    IT.SubItems.Add(vItems[i].FPS);
    IT.SubItems.Add(vItems[i].Size);
  end;
  TListView(Source).Items.EndUpdate;

  SetLength(vItems, 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.lstFilesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TListView;
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.btnSetMovieFragClick(Sender: TObject);
var
  FPS : Single;
  Dur : Integer;
begin
  opnDlg.Filter := 'AVI Files (*.avi)|*.avi';
  if lstFiles.ItemIndex > -1 then
  begin
    if lstFiles.ItemIndex = lstFiles.Items.Count-1 then
      MsgBox(InfoMsg[11], BTN_OK, '', '', MB_ICONINFORMATION, Self) else
    begin
      if (opnDlg.Execute) then
      begin
        if GetVideoInfo(opnDlg.FileName, FPS, Dur) = True then
          lstFiles.Items[lstFiles.ItemIndex].SubItems.Add(opnDlg.FileName);
      end;
    end;
  end else
    MsgBox(ErrorMsg[18], BTN_OK, '', '', MB_ICONERROR, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmJoin.btnClearMovieFragClick(Sender: TObject);
begin
  if lstFiles.ItemIndex > -1 then
  begin
    if lstFiles.Items[lstFiles.ItemIndex].SubItems.Count = 4 then
      lstFiles.Items[lstFiles.ItemIndex].SubItems.Delete(3);
  end else
    MsgBox(ErrorMsg[18], BTN_OK, '', '', MB_ICONERROR, Self);
end;

// -----------------------------------------------------------------------------

end.
