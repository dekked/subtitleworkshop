unit formCustomFormats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, General, IniFiles, USubtitleAPI, TreeViewHandle;

type
  TfrmCustomFormats = class(TForm)
    gbInformation: TGroupBox;
    edtFormatName: TLabeledEdit;
    edtExtension: TLabeledEdit;
    edtTimeStructure: TLabeledEdit;
    rdoTime: TRadioButton;
    rdoFrames: TRadioButton;
    cmbFPS: TComboBox;
    lblFPS: TLabel;
    edtNewLineChar: TLabeledEdit;
    btnLoadProject: TButton;
    btnSaveProject: TButton;
    chkRemember: TCheckBox;
    btnCancel: TButton;
    btnSave: TButton;
    mmoCustomFormat: TMemo;
    dlgLoad: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadProjectClick(Sender: TObject);
    procedure btnSaveProjectClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmCustomFormats    : TfrmCustomFormats;
  CustomFormat        : String;
  CustomFormatProject : String;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmCustomFormats.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                            := ReadString('Custom formats','01','Custom formats');
      gbInformation.Caption              := ReadString('Custom formats','02','Information');
      edtFormatName.EditLabel.Caption    := ReadString('Custom formats','03','Name:');
      edtExtension.EditLabel.Caption     := ReadString('Custom formats','04','Extension:');
      edtNewLineChar.EditLabel.Caption   := ReadString('Custom formats','05','New line char:');
      rdoTime.Caption                    := ReadString('Custom formats','06','Time');
      rdoFrames.Caption                  := ReadString('Custom formats','07','Frames');
      edtTimeStructure.EditLabel.Caption := ReadString('Custom formats','08','Structure:');
      lblFPS.Caption                     := ReadString('Custom formats','09','FPS:');
      btnLoadProject.Caption             := ReadString('Custom formats','10','Load project');
      btnSaveProject.Caption             := ReadString('Custom formats','11','Save project');
      chkRemember.Caption                := ReadString('Custom formats','12','Remember last custom format');
      btnSave.Caption                    := ReadString('Custom formats','13','Save!');
      CustomFormatProject                := ReadString('Custom formats','14','Custom format project');
      btnCancel.Caption                  := BTN_CANCEL;
      
      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnSave.ParentFont := True;
      Font               := frmMain.Font;
      btnSave.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormats.FormCreate(Sender: TObject);
var
  Ini           : TIniFile;
  FormatName    : String;
  Extension     : String;
  NewLineChars  : String;
  TimeStructure : String;
  Time, Frames  : Boolean;
  FPS           : Single;
begin
  SetLanguage;
  rdoTime.Checked         := True;
  edtFormatName.Text      := 'Custom format'; // Después hay que leer esto del lenguaje
  cmbFPS.Items            := frmMain.cmbInputFPS.Items;
  cmbFPS.ItemIndex        := frmMain.cmbInputFPS.ItemIndex;
  edtFormatName.SelLength := 0;
  if not DirectoryExists(ExtractFilePath(Application.ExeName) + 'CustomFormats\') then
    CreateDir(ExtractFilePath(Application.ExeName) + 'CustomFormats\');
  dlgLoad.InitialDir := ExtractFilePath(Application.ExeName) + 'CustomFormats\';
  Ini := TIniFile.Create(IniRoot);
  try
    if Ini.ReadBool('Custom formats', 'Remember last custom format', True) then
    begin
      CustomFormat        := Ini.ReadString('Custom formats','Last custom format', '');
      if FileExists(CustomFormat) then
      begin
        chkRemember.Checked := True;
        GetCustomFormatInfo(CustomFormat, FormatName, Extension, NewLineChars, TimeStructure, Time, Frames, FPS, mmoCustomFormat.Lines);
        edtFormatName.Text    := FormatName;
        edtExtension.Text     := Extension;
        edtNewLineChar.Text   := NewLineChars;
        edtTimeStructure.Text := TimeStructure;
        rdoTime.Checked       := Time;
        rdoFrames.Checked     := not Time;
      end;
    end else
      chkRemember.Checked := False;
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormats.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Custom formats', 'Remember last custom format', chkRemember.Checked);
    Ini.WriteString('Custom formats', 'Last custom format', CustomFormat);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormats.btnLoadProjectClick(Sender: TObject);
var
  FormatName    : String;
  Extension     : String;
  NewLineChars  : String;
  TimeStructure : String;
  Time, Frames  : Boolean;
  FPS           : Single;
begin
  dlgLoad.Filter := CustomFormatProject + ' (*.cfp)|*.cfp';
  if (dlgLoad.Execute) and (dlgLoad.FileName <> '') then
  begin
    GetCustomFormatInfo(dlgLoad.FileName, FormatName, Extension, NewLineChars, TimeStructure, Time, Frames, FPS, mmoCustomFormat.Lines);
    edtFormatName.Text    := FormatName;
    edtExtension.Text     := Extension;
    edtNewLineChar.Text   := NewLineChars;
    edtTimeStructure.Text := TimeStructure;
    rdoTime.Checked       := Time;
    rdoFrames.Checked     := not Time;
    CustomFormat          := dlgLoad.FileName;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormats.btnSaveProjectClick(Sender: TObject);
var
  f: TextFile;
begin
  dlgSave.Filter := CustomFormatProject + ' (*.cfp)|*.cfp';
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'CustomFormats\';
  if (dlgSave.Execute) and (dlgSave.FileName <> '') then
  begin
    try
      if AnsiLowerCase(ExtractFileExt(dlgSave.FileName)) = '.cfp' then
      AssignFile(f, dlgSave.FileName) else
      AssignFile(f, dlgSave.FileName + '.cfp');
      ReWrite(f);
      WriteLn(f, '[Information]');
      WriteLn(f, 'Name=' + edtFormatName.Text);
      WriteLn(f, 'Extension=' + edtExtension.Text);
      WriteLn(f, 'Time structure=',edtTimeStructure.Text);
      WriteLn(f, 'Time=' + BoolToStr(rdoTime.Checked,True));
      WriteLn(f, 'FPS=' + cmbFPS.Items[cmbFPS.ItemIndex]);
      WriteLn(f, 'New line char=' + edtNewLineChar.Text);
      WriteLn(f, '');
      WriteLn(f, '[Format text]');
      WriteLn(f, mmoCustomFormat.Text);
    finally
      CloseFile(f);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormats.btnSaveClick(Sender: TObject);
begin
  UpdateArray;
  dlgSave.Filter     := edtFormatName.Text + ' (' + edtExtension.Text + ')|' + edtExtension.Text;
  dlgSave.InitialDir := frmMain.dlgLoadFile.InitialDir;
  if (dlgSave.Execute) and (dlgSave.FileName <> '') then
  begin
    if AnsiLowerCase(ExtractFileExt(dlgSave.FileName)) <> AnsiLowerCase(Copy(edtExtension.Text, 2, Length(edtExtension.Text))) then
      dlgSave.FileName := dlgSave.FileName + Copy(edtExtension.Text, LastDelimiter('.', edtExtension.Text), Length(edtExtension.Text));
    SaveCustomFormat(dlgSave.FileName,
                     mmoCustomFormat.Lines,
                     rdoTime.Checked,
                     rdoFrames.Checked,
                     edtTimeStructure.Text,
                     StrToFloat(cmbFPS.Items[cmbFPS.ItemIndex]),
                     edtNewLineChar.Text);
    dlgSave.FileName := '';
  end;
  SubtitleAPI.ClearSubtitles;
end;

// -----------------------------------------------------------------------------

end.
