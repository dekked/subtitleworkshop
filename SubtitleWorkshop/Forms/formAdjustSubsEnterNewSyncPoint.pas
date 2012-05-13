unit formAdjustSubsEnterNewSyncPoint;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, TimeMaskEdit, ExtCtrls, IniFiles, General, Functions;

type
  TfrmEnterNewSyncPoint = class(TForm)
    pnlEnterTimes: TPanel;
    lblOldTime: TLabel;
    tmeOldTime: TTimeMaskEdit;
    lblNewTime: TLabel;
    tmeNewTime: TTimeMaskEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmEnterNewSyncPoint: TfrmEnterNewSyncPoint;

implementation

uses formAdjustSubtitles, formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmEnterNewSyncPoint.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption            := ReadString('Adjust subtitles', '19', 'Enter new sync point');
      lblOldTime.Caption := ReadString('Adjust subtitles', '20', 'Old time:');
      lblNewTime.Caption := ReadString('Adjust subtitles', '21', 'New time:');
      btnOk.Caption      := BTN_OK;
      btnCancel.Caption  := BTN_CANCEL;
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

procedure TfrmEnterNewSyncPoint.btnOkClick(Sender: TObject);
begin
  frmAdjustSubtitles.AddSyncPoint(tmeOldTime.Time, tmeNewTime.Time);
end;

// -----------------------------------------------------------------------------

procedure TfrmEnterNewSyncPoint.FormCreate(Sender: TObject);
begin
  tmeOldTime.FPS := GetFPS;
  tmeNewTime.FPS := GetFPS;
  if frmMain.FormatType = ftTime then
  begin
    tmeOldTime.TimeMode := tmTime;
    tmeNewTime.TimeMode := tmTime;
  end else
  begin
    tmeOldTime.TimeMode := tmFrames;
    tmeNewTime.TimeMode := tmFrames;
    tmeOldTime.MaxLength := 7;
    tmeNewTime.MaxLength := 7;
  end;
  SetLanguage;
end;

// -----------------------------------------------------------------------------

end.
