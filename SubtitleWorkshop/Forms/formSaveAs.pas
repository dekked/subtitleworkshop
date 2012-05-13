unit formSaveAs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, ImgList, ComCtrls, General, Functions, USubtitleAPI, IniFiles,
  FastStrings, USubtitlesFunctions, TreeViewHandle;

// -----------------------------------------------------------------------------

type
  TfrmSaveAs = class(TForm)
    chkAllFormats: TCheckBox;
    lstFormats: TListView;
    lblDblClick: TLabel;
    dlgSave: TSaveDialog;
    btnCustomFormat: TButton;
    btnCancel: TButton;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure chkAllFormatsClick(Sender: TObject);
    procedure lstFormatsDblClick(Sender: TObject);
    procedure lstFormatsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCustomFormatClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure AddCustomFormats;
    procedure SetLanguage;
  public
    SaveTranslation: Boolean;
  end;

// -----------------------------------------------------------------------------

var
  frmSaveAs : TfrmSaveAs;

implementation

uses formMain, formCustomFormats;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                 := ReadString('Save as', '01', 'Save as');
      lblDblClick.Caption     := ReadString('Save as', '02', 'Double-click on the output format:');
      btnCustomFormat.Caption := ReadString('Save as', '03', 'Custom format');
      chkAllFormats.Caption   := ReadString('Save as', '04', 'All formats');
      btnCancel.Caption       := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      Font := frmMain.Font;
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.AddCustomFormats;
var
  Busca : TSearchRec;
  i     : Integer;
  A     : TListItem;
begin
  i := FindFirst(ExtractFilePath(Application.ExeName) + ID_CFPDIR + '\*.cfp', faAnyFile,Busca);
  while i = 0 do
  begin
    A            := lstFormats.Items.Add;
    A.Caption    := Copy(Busca.Name, 1, LastDelimiter('.', Busca.Name) - 1);
    A.StateIndex := 1;
    i := FindNext(Busca);
  end;
  FindClose(Busca);
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.FormCreate(Sender: TObject);
var
  i            : Integer;
  TotalFormats : Integer;
  Name, Ext    : String;
  Item         : TListItem;
  Ini          : TIniFile;
begin
//  frmSaveAsExecuting := True;   // removed by BDZL
  SetLanguage;
  SaveTranslation := False;
  TotalFormats := SubtitleApi.FormatsCount;
  if TotalFormats = 0 then Exit;
  lstFormats.Clear;

  Ini := TIniFile.Create(IniRoot);
  try
    for i := 1 to TotalFormats do
    begin
      SubtitleAPI.GetFormatInfo(i, Name, Ext);
      if Ini.ReadBool('Formats to show', SubtitleAPI.GetFormatName(i), True) = True then
      begin
        Item            := lstFormats.Items.Add;
        Item.Caption    := Name;
        Item.ImageIndex := 0;
        Item.StateIndex := 0;
      end;
    end;
    if Ini.ReadBool('Formats','Show custom formats', False) then
      AddCustomFormats;
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.chkAllFormatsClick(Sender: TObject);
var
  i            : Integer;
  TotalFormats : Integer;
  Name, Ext    : String;
  Item         : TListItem;
  Ini          : TIniFile;
begin
  TotalFormats := SubtitleAPI.FormatsCount;
  if TotalFormats = 0 then Exit;
  lstFormats.Clear;
  Ini := TIniFile.Create(IniRoot);
  for i := 1 to TotalFormats do
  begin
    SubtitleAPI.GetFormatInfo(i, Name, Ext);
    if chkAllFormats.Checked then
    begin
      Item            := lstFormats.Items.Add;
      Item.Caption    := Name;
      Item.ImageIndex := 0;
      Item.StateIndex := 0;
    end else
    begin
      if Ini.ReadBool('Formats to show', SubtitleAPI.GetFormatName(i),True) = True then
      begin
        Item            := lstFormats.Items.Add;
        Item.Caption    := Name;
        Item.ImageIndex := 0;
        Item.StateIndex := 0;
      end;
    end;
  end;

  if Ini.ReadBool('Formats','Show custom formats', False) then
    AddCustomFormats;

  Ini.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.lstFormatsDblClick(Sender: TObject);
  label SaveSubFile;
  label SaveCustFile;
var
  FormatName : String;
  AllExts    : String;
  Ext        : String;
  SubFile    : String;
  SubFormat  : Integer;
  // Custom formats stuff
  // FormatName
  // Ext
  NewLineChar    : String;
  TimeStructure  : String;
  Time, Frames   : Boolean;
  FPS            : Single;
  Lines          : TStrings;
begin
  if lstFormats.ItemIndex = -1 then exit;
  UpdateArray(SaveTranslation);

  if lstFormats.Items[lstFormats.ItemIndex].StateIndex = 0 then
  begin
    SubFormat := SubtitleAPI.GetFormatIndex(lstFormats.Items[lstFormats.ItemIndex].Caption);
    SubtitleAPI.GetFormatInfo(SubFormat, FormatName, AllExts);
    dlgSave.Filter := FormatName + ' (' + AllExts + ')|' + AllExts;

    if SaveTranslation then
      SubFile := Copy(frmMain.TransFile, 1, LastDelimiter('.', frmMain.TransFile)) else
      SubFile := Copy(frmMain.OrgFile, 1, LastDelimiter('.', frmMain.OrgFile) - 1);
    if SubFile = '' then
      SubFile := lstFormats.Items[lstFormats.ItemIndex].Caption;
    Ext := GetFormatExt(SubFormat);
    if ExtractFileExt(SubFile) <> Ext then
      SubFile := SubFile + Ext;

    dlgSave.FileName := SubFile;

    if (dlgSave.Execute) and (dlgSave.FileName <> '') then
    begin
      SubFile := dlgSave.FileName;

      if AnsiLowerCase(ExtractFileExt(SubFile)) <> AnsiLowerCase(Ext) then
      begin
        if SmartPos(ExtractFileExt(SubFile), AllExts, False) = 0 then
          SubFile := SubFile + Ext;
      end;
      if FileExists(SubFile) then
      begin
        case MsgBox(Format(QuestionMsg[02], [SubFile]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, frmSaveAs) of
          1: GoTo SaveSubFile;
          2: begin Close; exit; end;
          3: exit;
        end;
      end;

      SaveSubFile:
      if SaveFile(SubFile, SubFormat, GetFPS) = False then
      begin
        Close;
        exit;
      end;
      if SaveTranslation then
      begin
        frmMain.TransFile     := SubFile;
        frmMain.TransFormat   := SubFormat;
        frmMain.TransModified := False;
      end else
      begin
        frmMain.OrgFile     := SubFile;
        frmMain.OrgFormat   := SubFormat;
        frmMain.OrgModified := False;
      end;

      SetFormCaption;
      frmMain.AddToRecent(SubFile);
      Close;
    end;
  end else
  if lstFormats.Items[lstFormats.ItemIndex].StateIndex = 1 then
  begin
    Lines := TStringList.Create;
    try
      GetCustomFormatInfo(ExtractFilePath(Application.ExeName) + ID_CFPDIR + '\' + lstFormats.Items[lstFormats.ItemIndex].Caption + '.cfp', FormatName, Ext, NewLineChar, TimeStructure, Time, Frames, FPS, Lines);
      Ext := Copy(Ext, 2, Length(Ext));
      if SaveTranslation then
        SubFile := Copy(frmMain.TransFile, 1, LastDelimiter('.', frmMain.TransFile)) else
        SubFile := Copy(frmMain.OrgFile, 1, LastDelimiter('.', frmMain.OrgFile) - 1);
      if ExtractFileExt(SubFile) = '' then
        SubFile := SubFile + Ext;

      dlgSave.Filter := FormatName + ' (' + Ext + ')|' + Ext;
      dlgSave.FileName := SubFile;

      if (dlgSave.Execute) and (dlgSave.FileName <> '') then
      begin
        SubFile := dlgSave.FileName;
        if ExtractFileExt(SubFile) = '' then
          SubFile := SubFile + Ext;
        if FileExists(SubFile) then
        begin
          case MsgBox(Format(QuestionMsg[02], [SubFile]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONQUESTION, frmSaveAs) of
            1: GoTo SaveCustFile;
            2: begin Close; exit; end;
            3: exit;
          end;
        end;
        SaveCustFile:
        SaveCustomFormat(SubFile, Lines, Time, Frames, TimeStructure, FPS, NewLineChar);
      end;
    finally
      Lines.Clear;
    end;
  end;
  SubtitleAPI.ClearSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.lstFormatsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    lstFormatsDblClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.btnCustomFormatClick(Sender: TObject);
begin
  frmCustomFormats := TfrmCustomFormats.Create(Application);
  frmCustomFormats.ShowModal;
  frmCustomFormats.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmSaveAs.FormDestroy(Sender: TObject);
begin
//  frmSaveAsExecuting := False;  // removed by BDZL
end;

// -----------------------------------------------------------------------------

end.
