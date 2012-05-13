program Demo;

uses
  Forms,
  UMain in 'UMain.pas' {frmMain},
  USubtitleApi in 'USubtitleApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
