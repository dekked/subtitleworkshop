unit formSetDelay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TreeViewHandle, General, ExtCtrls, StdCtrls, Mask, IniFiles, Functions,
  USubtitlesFunctions, TimeMaskEdit;

type
  TfrmSetDelay = class(TForm)
    btnApply: TButton;
    cmbDelayType: TComboBox;
    rdoAllSubs: TRadioButton;
    rdoSelSubs: TRadioButton;
    bvlDelay: TBevel;
    btnCancel: TButton;
    tmeDelay: TTimeMaskEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edtDelayKeyPress(Sender: TObject; var Key: Char);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmSetDelay: TfrmSetDelay;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmSetDelay.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption            := ReadString('Set delay', '01', 'Set delay');
      rdoAllSubs.Caption := ReadString('Set delay', '02', 'For all the subtitles');
      rdoSelSubs.Caption := ReadString('Set delay', '03', 'For selected subtitles');
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

procedure TfrmSetDelay.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  tmeDelay.FPS := GetFPS;
  if frmMain.FormatType = ftTime then
    tmeDelay.TimeMode := tmTime else
  begin
    tmeDelay.TimeMode  := tmFrames;
    tmeDelay.MaxLength := 7;
  end;
  Ini := TIniFile.Create(IniRoot);
  try
    cmbDelayType.ItemIndex := Ini.ReadInteger('Delay', 'Type', 0);
    rdoAllSubs.Checked := Ini.ReadBool('Delay', 'All subtitles', True);
    rdoSelSubs.Checked := not Ini.ReadBool('Delay', 'All subtitles', True);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSetDelay.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteInteger('Delay', 'Type', cmbDelayType.ItemIndex);
    Ini.WriteBool('Delay','All subtitles',rdoAllSubs.Checked);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSetDelay.btnApplyClick(Sender: TObject);
var
  DelayTime: Integer;
begin
  DelayTime := tmeDelay.Time;
  if cmbDelayType.ItemIndex = 1 then
    DelayTime := -DelayTime;
  SetDelay(DelayTime, rdoSelSubs.Checked);
  frmMain.RefreshTimes;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmSetDelay.edtDelayKeyPress(Sender: TObject; var Key: Char);
begin
  if frmMain.FormatType = ftFrames then
  begin
    if not (Key in['0'..'9', Chr(VK_RETURN), Chr(VK_BACK), Chr(VK_ESCAPE)]) then
      Key := #0;
  end;
end;

// -----------------------------------------------------------------------------

end.
