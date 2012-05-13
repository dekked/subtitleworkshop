unit formAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, ExtCtrls, General, IniFiles, ShellAPI,
  USubtitleAPI;

type
  TfrmAbout = class(TForm)
    pgeCtrl: TPageControl;
    pgeInformation: TTabSheet;
    pgeCredits: TTabSheet;
    pnlBase: TPanel;
    pnlCredits: TPanel;
    lblProg: TLabel;
    lblCredits: TLabel;
    lblProgrammedBy: TLabel;
    lblCreatedBy: TLabel;
    lblVer: TLabel;
    lblTranslators: TLabel;
    Timer: TTimer;
    btnOk: TButton;
    lblDeKSoft1: TLabel;
    lblSubtitleAPIBy: TLabel;
    lblAML1: TLabel;
    lblDirectShowProg: TLabel;
    lblAML2: TLabel;
    lblDeKSoft2: TLabel;
    lblVersion: TLabel;
    lblSubtitleAPIVer: TLabel;
    lblSupportedFormats: TLabel;
    lblCopyright2: TLabel;
    lblWeb2: TLabel;
    lblEMail: TLabel;
    Bevel1: TBevel;
    pnlBelowTranslators: TPanel;
    lblBetaTesters: TLabel;
    lblBetaTester1: TLabel;
    lblBetaTester2: TLabel;
    lblBetaTester3: TLabel;
    lblBetaTester4: TLabel;
    lblForUpdVisit: TLabel;
    lblWeb: TLabel;
    imgIcon: TImage;
    lblCopyright: TLabel;
    lblAdditionalProgramming: TLabel;
    lblRoma1: TLabel;
    lblTranslatorsList: TLabel;
    lblBedazzle1: TLabel;
    lblDonation: TLabel;
    lblDefaultIconBy: TLabel;
    lblKornKid: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    procedure TimerTimer(Sender: TObject);
    procedure pgeCtrlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblWebMouseEnter(Sender: TObject);
    procedure lblWebClick(Sender: TObject);
    procedure lblWebMouseLeave(Sender: TObject);
    procedure lblEMailClick(Sender: TObject);
    procedure lblDonationClick(Sender: TObject);
  private
    procedure SetLanguage;
    procedure GetTranslators;
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmAbout.SetLanguage;
var
  LF: TIniFile;
  APIVersion: String;
begin
  APIVersion := IntToHex(SubtitleAPI.ModuleVersion, 3);
  Insert('.', APIVersion, Length(APIVersion)-1);

  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                     := Format(ReadString('About form', '01', 'About %s'), [ID_PROGRAM]);
      pgeInformation.Caption      := ReadString('About form', '02', 'Information');
      pgeCredits.Caption          := ReadString('About form', '03', 'Credits');
      lblVersion.Caption          := Format(ReadString('About form', '04', 'Version: %s'), [ID_VERSION]);
      lblSubtitleAPIVer.Caption   := Format(ReadString('About form', '05', '%s version: %s'), [ID_COMPANY + ' ' + 'Subtitle API', APIVersion]);
      lblSupportedFormats.Caption := Format(ReadString('About form', '06', '%d different supported formats'), [SubtitleAPI.FormatsCount]);
      lblDonation.Caption         := ReadString('About form', '07', 'If you like this software, we would really appreciate you could make a donation. Click here to do it.');
      lblCredits.Caption          := pgeCredits.Caption;
      lblProgrammedBy.Caption     := ReadString('About form', '08', 'Programmed by');
      lblAdditionalProgramming.Caption := ReadString('About form', '09', 'Additional programming');
      lblSubtitleAPIBy.Caption    := Format(ReadString('About form', '10', '%s by'), [ID_COMPANY + ' ' + 'Subtitle API']);
      lblDirectShowProg.Caption   := Format(ReadString('About form', '11', '%s programming'), ['DirectShow']);
      lblTranslators.Caption      := ReadString('About form', '12', 'Translators');
      lblBetaTesters.Caption      := ReadString('About form', '13', 'Beta testers');
      lblDefaultIconBy.Caption    := ReadString('About form', '14', 'Default icon by');
      lblForUpdVisit.Caption      := ReadString('About form', '15', 'For updates visit:');
      btnOk.Caption := BTN_OK;                     
      
      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnOK.ParentFont                    := True;
      lblVersion.ParentFont               := True;
      lblDonation.ParentFont              := True;
      lblVer.ParentFont                   := True;
      lblSupportedFormats.ParentFont      := True;
      lblCredits.ParentFont               := True;
      lblProgrammedBy.ParentFont          := True;
      lblAdditionalProgramming.ParentFont := True;
      lblSubtitleAPIBy.ParentFont         := True;
      lblDirectShowProg.ParentFont        := True;
      lblTranslators.ParentFont           := True;
      lblBetaTesters.ParentFont           := True;
      lblDefaultIconBy.ParentFont         := True;
      lblForUpdVisit.ParentFont           := True;
      Font                                := frmMain.Font;
      btnOk.Font.Style                    := frmMain.Font.Style + [fsBold];
      lblVersion.Font.Style               := frmMain.Font.Style + [fsBold];
      lblDonation.Font.Style              := frmMain.Font.Style + [fsBold];
      lblSupportedFormats.Font.Style      := frmMain.Font.Style + [fsBold];
      lblVer.Font.Style                   := frmMain.Font.Style + [fsBold];
      lblVer.Font.Size                    := frmMain.Font.Size + 2;
      lblCredits.Font.Style               := frmMain.Font.Style + [fsBold];
      lblProgrammedBy.Font.Style          := frmMain.Font.Style + [fsBold];
      lblAdditionalProgramming.Font.Style := frmMain.Font.Style + [fsBold];
      lblSubtitleAPIBy.Font.Style         := frmMain.Font.Style + [fsBold];
      lblDirectShowProg.Font.Style        := frmMain.Font.Style + [fsBold];
      lblTranslators.Font.Style           := frmMain.Font.Style + [fsBold];
      lblBetaTesters.Font.Style           := frmMain.Font.Style + [fsBold];
      lblDefaultIconBy.Font.Style         := frmMain.Font.Style + [fsBold];
      lblForUpdVisit.Font.Style           := frmMain.Font.Style + [fsBold];

    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.GetTranslators;
var
  Busca      : TSearchRec;
  i          : Integer;
  Trans      : String;
  Ini        : TIniFile;
  tmpList    : TStringList;
  MDC        : hDC;
  CurMetrics : TTextMetric;
  Curfont    : HFont;
begin
  tmpList := TStringList.Create;
  try
    i := FindFirst(ExtractFilePath(Application.ExeName)+'Langs\*.lng', faAnyFile,Busca);
    while i = 0 do
    begin
      Trans := Copy(Busca.Name, 1, Length(busca.Name)-4) + ', ';
      Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Langs\' + Busca.Name);
        Trans := Trans + Ini.ReadString('General', 'Translator', ID_COMPANY);
      Ini.Free;
      tmpList.Add(Trans);
      i := FindNext(Busca);
    end;
    FindClose(Busca);
    tmpList.Sort;
    lblTranslatorsList.Caption := '';
    for i := 0 to tmpList.Count-1 do
      if i <> tmpList.Count-1 then
        lblTranslatorsList.Caption := lblTranslatorsList.Caption + tmpList[i] + #13#10 else
        lblTranslatorsList.Caption := lblTranslatorsList.Caption + tmpList[i];

    // Get text height
    MDC := GetDC(0);
    CurFont := SelectObject(MDC, lblTranslatorsList.Font.Handle);
    GetTextMetrics(MDC, CurMetrics);
    SelectObject(MDC, CurFont);
    ReleaseDC(0, MDC);

    // Set top appropiately depending on the number of translators (lines count)
    lblTranslatorsList.Height := CurMetrics.tmHeight * tmpList.Count{-1};
    if lblTranslatorsList.Caption <> '' then
      pnlBelowTranslators.Top := lblTranslatorsList.Top + lblTranslatorsList.Height + 16 else
      pnlBelowTranslators.Top := lblTranslatorsList.Top; // If there are no translators...
    pnlCredits.Height := pnlBelowTranslators.Top + pnlBelowTranslators.Height;
  finally
    tmpList.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.TimerTimer(Sender: TObject);
begin
  pnlCredits.Top := pnlCredits.Top - 1;
  if pnlCredits.Top + PnlCredits.Height <= 0 then
    pnlCredits.Top := pnlBase.Height;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.pgeCtrlChange(Sender: TObject);
begin
  if pgeCtrl.ActivePageIndex = 0 then
    Timer.Enabled := False else
    Timer.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  // Set caption of label
  lblProg.Caption       := ID_PROGRAM;
  lblVer.Caption        := ID_VERSION;
  lblEMail.Caption      := Format('E-Mail: %s', [ID_EMAIL]);
  lblWeb.Caption        := ID_WEBPAGE;
  lblWeb2.Caption       := ID_WEBPAGE;
//  lblCopyright.Caption  := 'Copyright © 2001-2004 ' + ID_COMPANY;
  lblCopyright.Caption  := 'Copyright © 2001-2008 ' + ID_COMPANY;
  lblCopyright2.Caption := lblCopyright.Caption;
  SetLanguage;

  pgeCtrl.ActivePageIndex   := 0;
  pnlCredits.Top            := pnlBase.Height + 1;
  GetTranslators;
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblWebMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clBlue;
  (Sender as TLabel).Font.Style := (Sender as TLabel).Font.Style + [fsUnderline];
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblWebMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clWindowText;
  (Sender as TLabel).Font.Style := (Sender as TLabel).Font.Style - [fsUnderline];
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblWebClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(ID_WEBPAGE), nil, nil, SW_SHOW);
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblEMailClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('mailto:' + ID_EMAIL), nil, nil, SW_SHOW);
end;

// -----------------------------------------------------------------------------

procedure TfrmAbout.lblDonationClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(ID_DONATIONPAGE), nil, nil, SW_SHOW);
end;

// -----------------------------------------------------------------------------

end.
