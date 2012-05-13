unit formBatchConvert;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ComCtrls, ExtCtrls, Functions, General, IniFiles,
  FileTypes, FileCtrl, USubtitleAPI, USubtitlesFunctions;

type                                 
  TfrmBatchConvert = class(TForm)
    pnlHeading: TPanel;
    imgDrawing: TImage;
    lblTitle: TLabel;
    lblDescription: TLabel;
    bvlSeparator: TBevel;
    btnClose: TButton;
    btnNext: TButton;
    btnBack: TButton;
    bvlPgeBtnSeparator: TBevel;
    pnlBorder: TPanel;
    NoteBook: TNotebook;
    chkIncludeSubFolders: TCheckBox;
    chklstExtensions: TCheckListBox;
    lblExtensions: TLabel;
    edtSearchPath: TLabeledEdit;
    btnBrowse: TButton;
    cmbDefaultFPS: TComboBox;
    lblDefaultFPS: TLabel;
    cmbOutputFormat: TComboBox;
    lblOutputFormat: TLabel;
    lstFiles: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnClear: TButton;
    cmbFPS: TComboBox;
    pnlPleaseWait: TPanel;
    lblFolder: TLabel;
    aviSearching: TAnimate;
    btnCancel: TButton;
    chkExhaustiveCheck: TCheckBox;
    btnBrowse2: TButton;
    edtOutputDirectory: TLabeledEdit;
    bvlSeparator2: TBevel;
    dlgAdd: TOpenDialog;
    mmoLog: TMemo;
    lblDone: TLabel;
    ProgressBar: TProgressBar;
    dlgSaveLog: TSaveDialog;
    btnSaveLog: TButton;
    btnAll: TButton;
    btnNone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnBrowse2Click(Sender: TObject);
    procedure cmbFPSChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure lstFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnBackClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
  private
    procedure SearchForFiles(Path: String; Extensions: TStrings; IncludeSubFolders: Boolean);
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmBatchConvert: TfrmBatchConvert;
  CancelSearching: Boolean;
  capNext, capConvert, capExit: String;
  CheckingFile: String;
  LogStr: array[1..14] of String;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                := ReadString('Batch convert', '01', 'Batch convert');
      lblTitle.Caption       := ReadString('Batch convert', '01', 'Batch convert');
      lblDescription.Caption := ReadString('Batch convert', '02', 'Convert multiple files to a single format in a few steps.');

      // --------- //
      //  Buttons  //
      // --------- //
      btnBack.Caption   := ReadString('Batch convert', '03', '< &Back');
      capNext           := ReadString('Batch convert', '04', '&Next >');
      capConvert        := ReadString('Batch convert', '05', 'C&onvert');
      capExit           := ReadString('Batch convert', '06', '&Exit');
      btnClose.Caption  := ReadString('Batch convert', '07', '&Close');
      btnNext.Caption   := capNext;
      btnAll.Caption    := ReadString('Batch convert', '08', '&All');
      btnNone.Caption   := ReadString('Batch convert', '09', '&None');

      // -------- //
      //  Page 1  //
      // -------- //
      lblExtensions.Caption                := ReadString('Batch convert', '10', 'Extensions:');
      edtSearchPath.EditLabel.Caption      := ReadString('Batch convert', '11', 'Search path:');
      chkIncludeSubFolders.Caption         := ReadString('Batch convert', '12', 'Include subfolders');
      chkExhaustiveCheck.Caption           := ReadString('Batch convert', '13', 'Exhaustive format check');
      btnBrowse.Caption                    := BTN_BROWSE;
      btnBrowse2.Caption                   := BTN_BROWSE;
      edtOutputDirectory.EditLabel.Caption := ReadString('Batch convert', '14', 'Output directory:');
      lblOutputFormat.Caption              := ReadString('Batch convert', '15', 'Output format:');
      lblDefaultFPS.Caption                := ReadString('Batch convert', '16', 'Default FPS:');

      // ------------ //
      // List columns //
      // ------------ //
      lstFiles.Columns[0].Caption := ReadString('Batch convert', '17', 'File name');
      lstFiles.Columns[1].Caption := ReadString('Batch convert', '18', 'Format');
      lstFiles.Columns[2].Caption := ReadString('Batch convert', '19', 'FPS');
      lstFiles.Columns[3].Caption := ReadString('Batch convert', '20', 'Size');

      // -------- //
      //  Page 2  //
      // -------- //
      btnAdd.Caption    := ReadString('Batch convert', '21', '&Add');
      btnRemove.Caption := ReadString('Batch convert', '22', '&Remove');
      btnClear.Caption  := ReadString('Batch convert', '23', 'C&lear');

      // -------- //
      //  Page 3  //
      // -------- //
      lblDone.Caption    := ReadString('Batch convert', '24', 'Done!. See log for details:');
      btnSaveLog.Caption := ReadString('Batch convert', '25', 'Save...');

      // ----------- //
      //  Searching  //
      // ----------- //
      CheckingFile := ReadString('Batch convert', '26', 'Checking file %s...');
      btnCancel.Caption := BTN_CANCEL;

      // ------------- //
      //  Log strings  //
      // ------------- //
      LogStr[1]  := '// ' + ReadString('Batch convert log', '01', '%s %s - Batch Conversion Log');
      LogStr[2]  := '// ' + ReadString('Batch convert log', '02', 'Generated on %s');
      LogStr[3]  := '// ' + ReadString('Batch convert log', '03', 'Output format: %s');
      LogStr[4]  := '// ' + ReadString('Batch convert log', '04', 'Total files to convert: %d');
      LogStr[5]  := ReadString('Batch convert log', '05', 'Trying to load "%s"...');
      LogStr[6]  := ReadString('Batch convert log', '06', 'File succesfully loaded!');
      LogStr[7]  := ReadString('Batch convert log', '07', 'File "%s" already existed, replacing...');
      LogStr[8]  := ReadString('Batch convert log', '08', 'File succesfully saved to "%s"');
      LogStr[9]  := ReadString('Batch convert log', '09', 'File "%s" already existed and you didn''t enable the program to replace it');
      LogStr[10] := ReadString('Batch convert log', '10', 'Error while loading file, can''t save!');
      LogStr[11] := '// ' + ReadString('Batch convert log', '11', 'Total conversions done: %u');
      LogStr[12] := '// ' + ReadString('Batch convert log', '12', 'Total time: %s');
      LogStr[13] := '// ' + ReadString('Batch convert log', '13', 'All the files were converted succesfully');
      LogStr[14] := '// ' + ReadString('Batch convert log', '14', 'There were errors while converting some files, see below for details');

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnNext.ParentFont  := True;
      lblTitle.ParentFont := True;
      Font                := frmMain.Font;
      btnNext.Font.Style  := frmMain.Font.Style + [fsBold];
      lblTitle.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.SearchForFiles(Path: String; Extensions: TStrings; IncludeSubFolders: Boolean);
var
  srRes   : TSearchRec;
  iFound  : Integer;
  i       : Integer;
  FFormat : Integer;
  Item    : TListItem;
  f       : File;
begin
  if CancelSearching = True then exit;

  if IncludeSubFolders then
  begin
    if Path[Length(Path)] <> '\' then Path := Path + '\';
    iFound := FindFirst(Path + '*.*', faAnyfile, srRes);
    while iFound = 0 do // While we find something
    begin
      if CancelSearching = True then exit;
      Application.ProcessMessages;
      if (srRes.Name <> '.') and (srRes.Name <> '..') then
        if (srRes.Attr and faDirectory) > 0 then
          SearchForFiles( path + srRes.Name, Extensions, IncludeSubFolders);
      iFound := FindNext(srRes);
    end;
    FindClose(srRes);
  end;

  if Path[Length(Path)] <> '\' then Path := Path + '\';

  iFound := FindFirst(path + '*.*', faAnyFile - faDirectory, srRes);
  while iFound = 0 do 
  begin
    if CancelSearching = True then exit;
    Application.ProcessMessages;
    if (srRes.Name <> '.') and (srRes.Name <> '..') and (srRes.Name <> '') then
    begin
      lblFolder.Caption := Format(CheckingFile, [srRes.Name]);
      for i := 0 to Extensions.Count-1 do
      begin
        if (ExtractFileExt(srRes.Name) = Copy(AnsiLowerCase(Extensions[i]), 2, Length(Extensions[i]))) then
        begin
          if chkExhaustiveCheck.Checked then
          begin
            if SubtitleAPI.LoadSubtitle(Path + srRes.Name, 25) then
              FFormat := SubtitleAPI.CurrentFormatIndex else
              FFormat := 0;
            SubtitleAPI.CloseSubtitle;
          end else
            FFormat := SubtitleAPI.GetFileFormat(Path + srRes.Name);
          if (FFormat > 0) then
          begin
            Item := lstFiles.Items.Add;
            Item.Caption := Path + srRes.Name;
            Item.SubItems.Add(SubtitleAPI.GetFormatName(FFormat));
            if SubtitleAPI.IsFrameBased(FFormat) then
              Item.SubItems.Add(cmbDefaultFPS.Items[cmbDefaultFPS.ItemIndex]) else
              Item.SubItems.Add('-');

            // Get file size...
            FileMode := fmOpenRead;
            AssignFile(f, Path + srRes.Name);
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
      end;
    end;
    iFound := FindNext(srRes);
  end;
  FindClose(srRes);
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.FormCreate(Sender: TObject);
var
  Ini      : TIniFile;
  ExtItems : TStrings;
  i        : Integer;
begin
  SetLanguage;
  CancelSearching := False;
  GetFormatsList(cmbOutputFormat.Items);
  ExtItems := TStringList.Create;
  try
    SeparateExtensions(ExtItems, GetExtensionsList, True);
    chkLstExtensions.Items := ExtItems;
    for i := 0 to chkLstExtensions.Items.Count-1 do
      chkLstExtensions.Checked[i] := True;
  finally
    ExtItems.Free;
  end;
  cmbDefaultFPS.Items     := frmMain.cmbInputFPS.Items;
  cmbDefaultFPS.ItemIndex := frmMain.cmbInputFPS.ItemIndex;
  cmbFPS.Items            := frmMain.cmbInputFPS.Items;
  cmbFPS.ItemIndex        := frmMain.cmbInputFPS.ItemIndex;
  edtSearchPath.Text := frmMain.dlgLoadFile.InitialDir;
  NoteBook.PageIndex := 0;
  Ini := TIniFile.Create(IniRoot);
  try
    cmbOutputFormat.ItemIndex    := SubtitleAPI.GetFormatIndex(Ini.ReadString('Formats','Default format', 'SubRip')) - 1;
    chkIncludeSubFolders.Checked := Ini.ReadBool('Batch convert', 'Include subfolders', True);
    chkExhaustiveCheck.Checked   := Ini.ReadBool('Batch convert', 'Exhaustive check', False);
    edtSearchPath.Text           := Ini.ReadString('Batch convert', 'Search path', frmMain.dlgLoadFile.InitialDir);
    edtOutputDirectory.Text      := Ini.ReadString('Batch convert', 'Output directory', 'C:\');
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Batch convert', 'Include subfolders', chkIncludeSubFolders.Checked);
    Ini.WriteBool('Batch convert', 'Exhaustive check', chkExhaustiveCheck.Checked);
    Ini.WriteString('Batch convert', 'Output directory', edtOutputDirectory.Text);
    Ini.WriteString('Batch convert', 'Search path', edtSearchPath.Text);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnBrowseClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := '';
  if SelectDirectory(edtSearchPath.EditLabel.Caption, '', Dir) = True then
    edtSearchPath.Text := Dir;
end;


// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnBackClick(Sender: TObject);
begin
  case NoteBook.PageIndex of
    1: begin btnBack.Enabled := False; NoteBook.PageIndex := 0; btnNext.Caption := capNext; CancelSearching := False; end;
    2: begin btnBack.Enabled := True; NoteBook.PageIndex := 1; btnNext.Caption := capConvert; ProgressBar.Position := 0; btnClose.Visible := True; end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnNextClick(Sender: TObject);
var
  i, TotConv : Integer;
  TotalTime  : Cardinal;
  ExtStr     : String;
  OutputPath : String;
  FName      : String;
  FPS        : Single;
  Extensions : TStrings;
begin
  ExtStr := '';
  if NoteBook.PageIndex = 0 then
  begin
    for i := 0 to chkLstExtensions.Items.Count-1 do
      if chkLstExtensions.Checked[i] then
        ExtStr := ExtStr + chkLstExtensions.Items[i] + ';';
    Delete(ExtStr, Length(ExtStr), 1);
    if ExtStr = '' then
    begin
      MsgBox(ErrorMsg[07], BTN_OK, '', '', MB_ICONERROR, frmBatchConvert);
      exit;
    end;
    if DirectoryExists(edtSearchPath.Text) = False then
    begin
      MsgBox(ErrorMsg[08], BTN_OK, '', '', MB_ICONERROR, frmBatchConvert);
      exit;
    end;
    if DirectoryExists(edtOutputDirectory.Text) = False then
      ForceDirectories(edtOutputDirectory.Text);

    lstFiles.Clear;
    ProgressBar.Position  := 0;
    pnlPleaseWait.Left    := (ClientWidth div 2) - (pnlPleaseWait.Width div 2);
    pnlPleaseWait.Top     := (ClientHeight div 2) - (pnlPleaseWait.Height div 2);
    NoteBook.Enabled      := False;
    pnlPleaseWait.Visible := True;
    aviSearching.Active   := True;
    try
      SeparateExtensions(Extensions, ExtStr, True);
      SearchForFiles(edtSearchPath.Text, Extensions, chkIncludeSubFolders.Checked);
    finally
      Extensions.Free;
      aviSearching.Active := False;
      pnlPleaseWait.Visible := False;
      NoteBook.Enabled := True;
      btnNext.Caption := capConvert;
      btnBack.Enabled := True;
      NoteBook.PageIndex := 1;
    end;
  end else
  if NoteBook.PageIndex = 1 then
  begin
    mmoLog.Clear;
    mmoLog.Lines.Add(Format(LogStr[1], [ID_PROGRAM, ID_VERSION]));
    mmoLog.Lines.Add(Format(LogStr[2], [DateToStr(Now) + ' ' + TimeToStr(Now)]));
    mmoLog.Lines.Add(Format(LogStr[3], [cmbOutputFormat.Items[cmbOutputFormat.ItemIndex]]));
    mmoLog.Lines.Add(Format(LogStr[4], [lstFiles.Items.Count]));
    mmoLog.Lines.Add('');

    if lstFiles.Items.Count > 0 then
    begin
      ProgressBar.Max      := lstFiles.Items.Count-1;
      ProgressBar.Position := 0;
      OutputPath := edtOutputDirectory.Text;
      if OutputPath[Length(OutputPath)] <> '\' then
        OutputPath := OutputPath + '\';

      ExtStr := GetFormatExt(cmbOutputFormat.ItemIndex + 1);
      TotConv := 0;
      TotalTime := GetTickCount;
      for i := 0 to lstFiles.Items.Count-1 do
      begin
        ProgressBar.Position := ProgressBar.Position + 1;
        if lstFiles.Items[i].SubItems[1] <> '-' then
          FPS := StrToFloat(lstFiles.Items[i].SubItems[1]) else
          FPS := StrToFloat(cmbDefaultFPS.Items[cmbDefaultFPS.ItemIndex]);
        mmoLog.Lines.Add(Format(LogStr[5], [lstFiles.Items[i].Caption]));

        if SubtitleAPI.LoadSubtitle(lstFiles.Items[i].Caption, FPS, SubtitleAPI.GetFormatIndex(lstFiles.Items[i].SubItems[0])) then
        begin
          mmoLog.Lines.Add(LogStr[6]);
          FName := ExtractFileName(lstFiles.Items[i].Caption);
          FName := Copy(FName, 0, LastDelimiter('.', FName)-1) + ExtStr;
          if FileExists(OutputPath +  FName) = True then
          begin
            if MsgBox(Format(QuestionMsg[02], [OutputPath +  FName]), BTN_YES, BTN_NO, '', MB_ICONQUESTION, frmBatchConvert) = 1 then
            begin
              Inc(TotConv);
              mmoLog.Lines.Add(Format(LogStr[7], [OutputPath +  FName]));
              if SubtitleAPI.SaveSubtitle(OutputPath +  FName, cmbOutputFormat.ItemIndex + 1, FPS) then
                mmoLog.Lines.Add(Format(LogStr[8], [OutputPath +  FName]));
            end else
              mmoLog.Lines.Add(Format(LogStr[9], [OutputPath +  FName]))
          end else
            if SubtitleAPI.SaveSubtitle(OutputPath +  FName, cmbOutputFormat.ItemIndex + 1, FPS) then
            begin
              Inc(TotConv);
              mmoLog.Lines.Add(Format(LogStr[8], [OutputPath +  FName]));
            end;
        end else
        begin
          mmoLog.Lines.Add(LogStr[10]);
        end;
        mmoLog.Lines.Add('');
      end;

      mmoLog.Lines.Insert(4, Format(LogStr[11], [TotConv]));
      mmoLog.Lines.Insert(5, Format(LogStr[12], [TimeToString(GetTickCount - TotalTime, 'hh:mm:ss,zzz')]));
      mmoLog.Lines.Insert(6, '//');
      if TotConv = lstFiles.Items.Count then
        mmoLog.Lines.Insert(7, LogStr[13]) else
        mmoLog.Lines.Insert(7, LogStr[14]);

      btnNext.Caption  := capExit;
      btnClose.Visible := False;
      btnBack.Enabled  := True;
      NoteBook.PageIndex := 2;
    end;
  end else
  if NoteBook.PageIndex = 2 then
  begin
    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnCancelClick(Sender: TObject);
begin
  CancelSearching := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnBrowse2Click(Sender: TObject);
var
  Dir: String;
begin
  Dir := '';
  if SelectDirectory(edtOutputDirectory.EditLabel.Caption, '', Dir) = True then
    edtOutputDirectory.Text := Dir;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.cmbFPSChange(Sender: TObject);
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

procedure TfrmBatchConvert.btnRemoveClick(Sender: TObject);
begin
  lstFiles.DeleteSelected;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnClearClick(Sender: TObject);
begin
  lstFiles.Clear;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnAddClick(Sender: TObject);
var
  i       : Integer;
  FFormat : Integer;
  Item    : TListItem;
  f       : File;
begin
  dlgAdd.Filter := frmMain.dlgLoadFile.Filter;
  if (dlgAdd.Execute) and (dlgAdd.Files.Count > 0) then
  begin
    NoteBook.Enabled := False;
    for i := 0 to dlgAdd.Files.Count-1 do
    begin
      Application.ProcessMessages;
      if chkExhaustiveCheck.Checked then
      begin
        if SubtitleAPI.LoadSubtitle(dlgAdd.Files[i], 25) then
          FFormat := SubtitleAPI.CurrentFormatIndex else
          FFormat := 0;
        SubtitleAPI.CloseSubtitle;
      end else
        FFormat := SubtitleAPI.GetFileFormat(dlgAdd.Files[i]);
      if (FFormat > 0) then
      begin
        Item := lstFiles.Items.Add;
        Item.Caption := dlgAdd.Files[i];
        Item.SubItems.Add(SubtitleAPI.GetFormatName(FFormat));
        if SubtitleAPI.IsFrameBased(FFormat) then
          Item.SubItems.Add(cmbDefaultFPS.Items[cmbDefaultFPS.ItemIndex]) else
          Item.SubItems.Add('-');

        // Get file size...
        FileMode := fmOpenRead;
        AssignFile(f, dlgAdd.Files[i]);
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
    NoteBook.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.lstFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (lstFiles.ItemIndex > -1) then
    lstFiles.DeleteSelected;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnSaveLogClick(Sender: TObject);
begin
  if (dlgSaveLog.Execute) and (dlgSaveLog.FileName <> '') then
  begin
    if ExtractFileExt(dlgSaveLog.FileName) = '' then
      dlgSaveLog.FileName := dlgSaveLog.FileName + '.log';
    mmoLog.Lines.SaveToFile(dlgSaveLog.FileName);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chklstExtensions.Items.Count-1 do
    chklstExtensions.Checked[i] := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chklstExtensions.Items.Count-1 do
    chklstExtensions.Checked[i] := False;
end;

// -----------------------------------------------------------------------------

end.
