unit formOutputSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, IniFiles, General, Functions,
  USubtitleAPI;

type
  TfrmOutputSettings = class(TForm)
    tvFormats: TTreeView;
    pgeFormats: TPageControl;
    pgeDVDSubtitle: TTabSheet;
    edtDVDSubtitleDiskId: TLabeledEdit;
    edtDVDSubtitleDVDTitle: TLabeledEdit;
    edtDVDSubtitleLanguage: TLabeledEdit;
    edtDVDSubtitleAuthor: TLabeledEdit;
    edtDVDSubtitleWeb: TLabeledEdit;
    edtDVDSubtitleInfo: TLabeledEdit;
    edtDVDSubtitleLicense: TLabeledEdit;
    pgeSAMI: TTabSheet;
    lblSAMISubtitle: TLabel;
    lblSAMIBackground: TLabel;
    lblSAMIAlign: TLabel;
    pnlSAMISubtitleColor: TPanel;
    pnlSAMIBackgroundColor: TPanel;
    rdoSAMILeft: TRadioButton;
    rdoSAMICenter: TRadioButton;
    rdoSAMIRight: TRadioButton;
    pnlSAMISample: TPanel;
    pgeSonicScenarist: TTabSheet;
    lblScenaristColor0: TLabel;
    lblScenaristColor1: TLabel;
    lblScenaristColor2: TLabel;
    lblScenaristColor3: TLabel;
    lblScenaristPaletteNumber: TLabel;
    lblScenaristContrast: TLabel;
    lblScenaristFramerate: TLabel;
    seScenaristContrast0: TSpinEdit;
    seScenaristContrast1: TSpinEdit;
    seScenaristContrast2: TSpinEdit;
    seScenaristContrast3: TSpinEdit;
    cmbScenaristFPS: TComboBox;
    seScenaristColor0: TSpinEdit;
    seScenaristColor1: TSpinEdit;
    seScenaristColor2: TSpinEdit;
    seScenaristColor3: TSpinEdit;
    chkScenaristDropFrame: TCheckBox;
    pgeSubStationAlpha: TTabSheet;
    edtSSATitle: TLabeledEdit;
    edtSSAScript: TLabeledEdit;
    pgeSubViewer: TTabSheet;
    lblSubViewer1Delay: TLabel;
    edtSubViewer1Title: TLabeledEdit;
    edtSubViewer1Author: TLabeledEdit;
    edtSubViewer1Source: TLabeledEdit;
    edtSubViewer1Program: TLabeledEdit;
    edtSubViewer1Path: TLabeledEdit;
    seSubViewer1Delay: TSpinEdit;
    pgeSubViewer2: TTabSheet;
    lblSubViewer2Delay: TLabel;
    lblSubViewer2CDTrack: TLabel;
    edtSubViewer2Title: TLabeledEdit;
    edtSubViewer2Author: TLabeledEdit;
    edtSubViewer2Source: TLabeledEdit;
    edtSubViewer2Program: TLabeledEdit;
    edtSubViewer2Path: TLabeledEdit;
    seSubViewer2Delay: TSpinEdit;
    seSubViewer2CDTrack: TSpinEdit;
    edtSubViewer2Comment: TLabeledEdit;
    btnSubViewer2SetFont: TButton;
    pnlSubViewer2Sample: TPanel;
    pgeTMPlayer: TTabSheet;
    gbTMPlayerFormat: TGroupBox;
    gbTMPlayerMasFormat: TGroupBox;
    gbTMPlayerMultilineFormat: TGroupBox;
    lblTMPlayerMultiline: TLabel;
    rdoTMPlayerFormat2: TRadioButton;
    rdoTMPlayerFormat1: TRadioButton;
    rdoTMPlayerPlusFormat2: TRadioButton;
    rdoTMPlayerPlusFormat1: TRadioButton;
    rdoTMPlayerMultiline: TRadioButton;
    btnOk: TButton;
    btnCancel: TButton;
    pnlHeading: TPanel;
    clrDlg: TColorDialog;
    fntDlg: TFontDialog;
    bvlSeparate1: TBevel;
    pgeCtrlSSA: TPageControl;
    pgeCosmetics: TTabSheet;
    pgeOthers: TTabSheet;
    pnlSSASample: TPanel;
    btnSSASetFont: TButton;
    pnlSSAShadow: TPanel;
    pnlSSATertiary: TPanel;
    pnlSSASecondary: TPanel;
    pnlSSAPrimary: TPanel;
    lblSSAColor: TLabel;
    lblSSAPrimary: TLabel;
    lblSSASecondary: TLabel;
    lblSSATertiary: TLabel;
    lblSSAShadow: TLabel;
    cmbSSAEncoding: TComboBox;
    lblSSAEncoding: TLabel;
    seSSAShadow: TSpinEdit;
    lblSSAShadowPos: TLabel;
    seSSAOutline: TSpinEdit;
    lblSSAOutline: TLabel;
    seSSARightMargin: TSpinEdit;
    lblSSARightMargin: TLabel;
    lblSSALeftMargin: TLabel;
    seSSALeftMargin: TSpinEdit;
    seSSAVerticalMargin: TSpinEdit;
    lblSSAVerticalMargin: TLabel;
    lblSSABorderStyle: TLabel;
    cmbSSABorderStyle: TComboBox;
    lblSSAAlignment: TLabel;
    cmbSSAAlignment: TComboBox;
    cmbSSASubTopMidTitle: TComboBox;
    btnSAMISetFont: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tvFormatsClick(Sender: TObject);
    procedure tvFormatsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOkClick(Sender: TObject);
    procedure rdoSAMILeftClick(Sender: TObject);
    procedure rdoSAMICenterClick(Sender: TObject);
    procedure rdoSAMIRightClick(Sender: TObject);
    procedure pnlSAMISubtitleColorClick(Sender: TObject);
    procedure pnlSAMIBackgroundColorClick(Sender: TObject);
    procedure cmbScenaristFPSChange(Sender: TObject);
    procedure pnlSSAPrimaryClick(Sender: TObject);
    procedure pnlSSASecondaryClick(Sender: TObject);
    procedure pnlSSATertiaryClick(Sender: TObject);
    procedure pnlSSAShadowClick(Sender: TObject);
    procedure btnSSASetFontClick(Sender: TObject);
    procedure btnSubViewer2SetFontClick(Sender: TObject);
    procedure lblTMPlayerMultilineClick(Sender: TObject);
    procedure btnSAMISetFontClick(Sender: TObject);
  private
    procedure SetLanguage;
    procedure SetPanelColor(Sender: TObject);
    procedure SetPanelFont(Panel: TPanel);
  public
    { Public declarations }
  end;

var
  frmOutputSettings: TfrmOutputSettings;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption := ReadString('Output settings','01','Output settings');

      // DVD Subtitle
      edtDVDSubtitleDiskId.EditLabel.Caption   := ReadString('Output settings','02','Disk ID:');
      edtDVDSubtitleDVDTitle.EditLabel.Caption := ReadString('Output settings','03','DVD Title:');
      edtDVDSubtitleLanguage.EditLabel.Caption := ReadString('Output settings','04','Language:');
      edtDVDSubtitleAuthor.EditLabel.Caption   := ReadString('Output settings','05','Author:');
      edtDVDSubtitleWeb.EditLabel.Caption      := ReadString('Output settings','06','Web:');
      edtDVDSubtitleInfo.EditLabel.Caption     := ReadString('Output settings','07','Info:');
      edtDVDSubtitleLicense.EditLabel.Caption  := ReadString('Output settings','08','License:');

      // SAMI
      lblSAMISubtitle.Caption   := ReadString('Output settings','09','Subtitle:');
      lblSAMIBackground.Caption := ReadString('Output settings','10','Background:');
      pnlSAMISample.Caption     := ReadString('Output settings','11','SAMPLE');
      btnSAMISetFont.Caption    := ReadString('Output settings','12','&Set font');

      lblSAMIAlign.Caption      := ReadString('Output settings','13','Align:');
      rdoSAMILeft.Caption       := ReadString('Output settings','14','Left');
      rdoSAMICenter.Caption     := ReadString('Output settings','15','Center');
      rdoSAMIRight.Caption      := ReadString('Output settings','16','Right');

      // Sonic Scenarist
      lblScenaristFramerate.Caption     := ReadString('Output settings','17','Framerate:');
      chkScenaristDropFrame.Caption     := ReadString('Output settings','18','Drop frame');
      lblScenaristColor0.Caption        := ReadString('Output settings','19','Color 0 (background):');
      lblScenaristColor1.Caption        := ReadString('Output settings','20','Color 1 (Font):');
      lblScenaristColor2.Caption        := ReadString('Output settings','21','Color 2 (outline):');
      lblScenaristColor3.Caption        := ReadString('Output settings','22','Color 3 (antialiasing):');
      lblScenaristPaletteNumber.Caption := ReadString('Output settings','23','Palette color n°:');
      lblScenaristContrast.Caption      := ReadString('Output settings','24','Contrast');

      // SubStation Alpha
      edtSSATitle.EditLabel.Caption  := ReadString('Output settings','25','Title:');
      edtSSAScript.EditLabel.Caption := ReadString('Output settings','26','Script:');
      pgeCosmetics.Caption           := ReadString('Output settings','27','Cosmetics');
      pgeOthers.Caption              := ReadString('Output settings','28','Others');
      // Cosmetics
      btnSSASetFont.Caption          := btnSAMISetFont.Caption;
      pnlSSASample.Caption           := pnlSAMISample.Caption;
      lblSSABorderStyle.Caption      := ReadString('Output settings','29','Border style:');
      cmbSSABorderStyle.Clear;
      cmbSSABorderStyle.Items.Add(ReadString('Output settings','30','Outline + drop shadow'));
      cmbSSABorderStyle.Items.Add(ReadString('Output settings','31','Opaque box'));
      lblSSAColor.Caption     := ReadString('Output settings','32','Color');
      lblSSAPrimary.Caption   := ReadString('Output settings','33','Primary:');
      lblSSASecondary.Caption := ReadString('Output settings','34','Secondary:');
      lblSSATertiary.Caption  := ReadString('Output settings','35','Tertiary:');
      lblSSAShadow.Caption    := ReadString('Output settings','36','Shadow:');
      // Others
      lblSSALeftMargin.Caption     := ReadString('Output settings','37','Left margin:');
      lblSSARightMargin.Caption    := ReadString('Output settings','38','Right margin:');
      lblSSAVerticalMargin.Caption := ReadString('Output settings','39','Vertical margin:');
      lblSSAOutline.Caption        := ReadString('Output settings','40','Outline:');
      lblSSAShadowPos.Caption      := lblSSAShadow.Caption;
      lblSSAAlignment.Caption      := ReadString('Output settings','41','Alignment:');
      cmbSSAAlignment.Clear;
      cmbSSAAlignment.Items.Add(rdoSAMILeft.Caption);
      cmbSSAAlignment.Items.Add(rdoSAMICenter.Caption);
      cmbSSAAlignment.Items.Add(rdoSAMIRight.Caption);
      lblSSAEncoding.Caption := ReadString('Output settings','42','Encoding:');

      // SubViewer
      edtSubViewer1Title.EditLabel.Caption     := edtSSATitle.EditLabel.Caption;
      edtSubViewer1Author.EditLabel.Caption    := edtDVDSubtitleAuthor.EditLabel.Caption;
      edtSubViewer1Source.EditLabel.Caption    := ReadString('Output settings','43','Source:');
      edtSubViewer1Program.EditLabel.Caption   := ReadString('Output settings','44','Program:');
      edtSubViewer1Path.EditLabel.Caption      := ReadString('Output settings','45','Path:');
      lblSubViewer1Delay.Caption               := ReadString('Output settings','46','Delay:');

      // SubViewer 2
      edtSubViewer2Title.EditLabel.Caption     := edtSSATitle.EditLabel.Caption;
      edtSubViewer2Author.EditLabel.Caption    := edtDVDSubtitleAuthor.EditLabel.Caption;
      edtSubViewer2Source.EditLabel.Caption    := edtSubViewer1Source.EditLabel.Caption;
      edtSubViewer2Program.EditLabel.Caption   := edtSubViewer1Program.EditLabel.Caption;
      edtSubViewer2Path.EditLabel.Caption      := edtSubViewer1Path.EditLabel.Caption;
      lblSubViewer2Delay.Caption               := lblSubViewer1Delay.Caption;
      lblSubViewer2CDTrack.Caption             := ReadString('Output settings','47','CD-Track:');
      edtSubViewer2Comment.EditLabel.Caption   := ReadString('Output settings','48','Comment:');
      btnSubViewer2SetFont.Caption             := btnSAMISetFont.Caption;
      pnlSubViewer2Sample.Caption              := pnlSAMISample.Caption;

      // TMPlayer
      gbTMPlayerFormat.Caption                 := ReadString('Output settings','49','TMPlayer Format');
      gbTMPlayerMasFormat.Caption              := ReadString('Output settings','50','TMPlayer+ Format');
      gbTMPlayerMultilineFormat.Caption        := ReadString('Output settings','51','TMPlayer Multiline Format');
             
      btnOk.Caption     := BTN_OK;
      btnCancel.Caption := BTN_CANCEL;

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

procedure TfrmOutputSettings.SetPanelColor(Sender: TObject);
begin
  clrDlg.Color := (Sender as TPanel).Color;
  if clrDlg.Execute then
    (Sender as TPanel).Color := clrDlg.Color;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.SetPanelFont(Panel: TPanel);
begin
  fntDlg.Font := Panel.Font;
  if fntDlg.Execute then
    Panel.Font := fntDlg.Font;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.FormCreate(Sender: TObject);
var
  i   : Integer;
  Ini : TIniFile;
begin
  for i := 0 to pgeFormats.PageCount-1 do
    tvFormats.Items.Add(nil, pgeFormats.Pages[i].Caption);
  pgeFormats.ActivePageIndex := 0;
  pnlHeading.Caption         := tvFormats.Items.Item[0].Text;
  SetLanguage;

  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Output.ini');
  try
    // ------------------------ //
    //       DVD Subtitle       //
    // ------------------------ //

    edtDVDSubtitleDiskId.Text   := Ini.ReadString('DVD Subtitle', 'Disk ID',   '');
    edtDVDSubtitleDVDTitle.Text := Ini.ReadString('DVD Subtitle', 'DVD Title', '');
    edtDVDSubtitleLanguage.Text := Ini.ReadString('DVD Subtitle', 'Language',  'EN');
    edtDVDSubtitleAuthor.Text   := Ini.ReadString('DVD Subtitle', 'Author',    '');
    edtDVDSubtitleWeb.Text      := Ini.ReadString('DVD Subtitle', 'Web',       '');
    edtDVDSubtitleInfo.Text     := Ini.ReadString('DVD Subtitle', 'Info',      '');
    edtDVDSubtitleLicense.Text  := Ini.ReadString('DVD Subtitle', 'License',   '');

    // ------------------------ //
    //           SAMI           //
    // ------------------------ //
    pnlSAMISample.Font.Name := Ini.ReadString( 'SAMI', 'Font', 'Tahoma');
    pnlSAMISample.Font.Size := Ini.ReadInteger('SAMI', 'Size', 24);

    if Ini.ReadBool('SAMI', 'Bold', True) then
      pnlSAMISample.Font.Style := pnlSAMISample.Font.Style + [fsBold];
    if Ini.ReadBool('SAMI', 'Italic', False) then
      pnlSAMISample.Font.Style := pnlSAMISample.Font.Style + [fsItalic];
    if Ini.ReadBool('SAMI', 'Underline', False) then
      pnlSAMISample.Font.Style := pnlSAMISample.Font.Style + [fsUnderline];

    pnlSAMISample.Font.Style := [];
    if Ini.ReadBool('SAMI', 'Bold', True) then
      pnlSAMISample.Font.Style := pnlSAMISample.Font.Style + [fsBold];
    if Ini.ReadBool('SAMI', 'Italic', False) then
      pnlSAMISample.Font.Style := pnlSAMISample.Font.Style + [fsItalic];
    if Ini.ReadBool('SAMI', 'Underline', False) then
      pnlSAMISample.Font.Style := pnlSAMISample.Font.Style + [fsUnderline];

    pnlSAMISubtitleColor.Color   := Ini.ReadInteger('SAMI', 'Subtitle color',   clWhite);
    pnlSAMISample.Font.Color     := Ini.ReadInteger('SAMI', 'Subtitle color',   clWhite);
    pnlSAMIBackgroundColor.Color := Ini.ReadInteger('SAMI', 'Background color', clBlack);
    pnlSAMISample.Color          := Ini.ReadInteger('SAMI', 'Background color', clBlack);
    rdoSAMILeft.Checked          := Ini.ReadString( 'SAMI', 'Align',            'Center') = 'Left';
    rdoSAMICenter.Checked        := Ini.ReadString( 'SAMI', 'Align',            'Center') = 'Center';
    rdoSAMIRight.Checked         := Ini.ReadString( 'SAMI', 'Align',            'Center') = 'Right';

    if rdoSAMILeft.Checked then
      pnlSAMISample.Alignment := taLeftJustify else
    if rdoSAMICenter.Checked then
      pnlSAMISample.Alignment := Classes.taCenter else
    if rdoSAMIRight.Checked then
      pnlSAMISample.Alignment := taRightJustify;


    // ------------------------ //
    //      Sonic Scenarist     //
    // ------------------------ //

    if Ini.ReadBool('Scenarist','PAL',True) then
    begin
      cmbScenaristFPS.ItemIndex := 0;
      chkScenaristDropFrame.Enabled := False;
      chkScenaristDropFrame.Checked := False;
    end else
    begin
      cmbScenaristFPS.ItemIndex := 1;
      chkScenaristDropFrame.Enabled := True;
    end;

    chkScenaristDropFrame.Checked := Ini.ReadBool(   'Scenarist', 'Drop frame', False);
    seScenaristColor0.Value       := Ini.ReadInteger('Scenarist', 'Color0',     3);
    seScenaristColor1.Value       := Ini.ReadInteger('Scenarist', 'Color1',     4);
    seScenaristColor2.Value       := Ini.ReadInteger('Scenarist', 'Color2',     3);
    seScenaristColor3.Value       := Ini.ReadInteger('Scenarist', 'Color3',     9);
    seScenaristContrast0.Value    := Ini.ReadInteger('Scenarist', 'Contrast0',  0);
    seScenaristContrast1.Value    := Ini.ReadInteger('Scenarist', 'Contrast1',  15);
    seScenaristContrast2.Value    := Ini.ReadInteger('Scenarist', 'Contrast2',  15);
    seScenaristContrast3.Value    := Ini.ReadInteger('Scenarist', 'Contrast3',  15);


    // ------------------------ //
    //         SubViewer        //
    // ------------------------ //

    edtSubViewer1Title.Text   := Ini.ReadString( 'SubViewer', 'Title',   '');
    edtSubViewer1Author.Text  := Ini.ReadString( 'SubViewer', 'Author',  '');
    edtSubViewer1Source.Text  := Ini.ReadString( 'SubViewer', 'Source',  '');
    edtSubViewer1Program.Text := Ini.ReadString( 'SubViewer', 'Program', '');
    edtSubViewer1Path.Text    := Ini.ReadString( 'SubViewer', 'Path',    '');
    seSubViewer1Delay.Value   := Ini.ReadInteger('SubViewer', 'Delay',   0);


    // ------------------------ //
    //        SubViewer 2       //
    // ------------------------ //

    edtSubViewer2Title.Text        := Ini.ReadString( 'SubViewer 2', 'Title',    '');
    edtSubViewer2Author.Text       := Ini.ReadString( 'SubViewer 2', 'Author',   '');
    edtSubViewer2Source.Text       := Ini.ReadString( 'SubViewer 2', 'Source',   '');
    edtSubViewer2Program.Text      := Ini.ReadString( 'SubViewer 2', 'Program',  '');
    edtSubViewer2Path.Text         := Ini.ReadString( 'SubViewer 2', 'Path',     '');
    seSubViewer2Delay.Value        := Ini.ReadInteger('SubViewer 2', 'Delay',     0);
    seSubViewer2CDTrack.Value      := Ini.ReadInteger('SubViewer 2', 'CD-Track',  0);
    edtSubViewer2Comment.Text      := Ini.ReadString( 'SubViewer 2', 'Comment',   '');
    pnlSubViewer2Sample.Font.Name  := Ini.ReadString( 'SubViewer 2', 'Font Name', 'Tahoma');
    pnlSubViewer2Sample.Font.Size  := Ini.ReadInteger('SubViewer 2', 'Font Size', 24);
    pnlSubViewer2Sample.Font.Color := Ini.ReadInteger('SubViewer 2', 'Color',     clWhite);
    if Ini.ReadBool('SubViewer 2', 'Bold', True) then
      pnlSubViewer2Sample.Font.Style := pnlSubViewer2Sample.Font.Style + [fsBold];
    if Ini.ReadBool('SubViewer 2', 'Italic', False) then
      pnlSubViewer2Sample.Font.Style := pnlSubViewer2Sample.Font.Style + [fsItalic];
    if Ini.ReadBool('SubViewer 2', 'Underline', False) then
      pnlSubViewer2Sample.Font.Style := pnlSubViewer2Sample.Font.Style + [fsUnderline];
    if Ini.ReadBool('SubViewer 2', 'Strikeout', False) then
      pnlSubViewer2Sample.Font.Style := pnlSubViewer2Sample.Font.Style + [fsStrikeOut];


    // ------------------------ //
    //     SubStation Alpha     //
    // ------------------------ //
    pgeCtrlSSA.ActivePageIndex := 0;
    AddCharsets(cmbSSAEncoding);
    edtSSATitle.Text        := Ini.ReadString( 'SSA', 'Title',     '<untitled>');
    edtSSAScript.Text       := Ini.ReadString( 'SSA', 'Script',    '<unknown>');
    pnlSSASample.Font.Name  := Ini.ReadString( 'SSA', 'Font name', 'Tahoma');
    pnlSSASample.Font.Size  := Ini.ReadInteger('SSA', 'Font size', 24);
    if Ini.ReadBool('SSA', 'Bold', True) then
      pnlSSASample.Font.Style := pnlSSASample.Font.Style + [fsBold];
    if Ini.ReadBool('SSA', 'Italic', False) then
      pnlSSASample.Font.Style := pnlSSASample.Font.Style + [fsItalic];
    if Ini.ReadInteger('SSA', 'BorderStyle', 1) = 1 then
      cmbSSABorderStyle.ItemIndex := 0 else
      cmbSSABorderStyle.ItemIndex := 1;
    pnlSSAPrimary.Color            := Ini.ReadInteger('SSA', 'Primary Color',   16777215);
    pnlSSASecondary.Color          := Ini.ReadInteger('SSA', 'Secondary Color', 16777215);
    pnlSSATertiary.Color           := Ini.ReadInteger('SSA', 'Tertiary Color',  16777215);
    pnlSSAShadow.Color             := Ini.ReadInteger('SSA', 'Shadow Color',    12632256);
    pnlSSASample.Font.Color        := pnlSSAPrimary.Color;
    pnlSSASample.Color             := clBlack;
    seSSALeftMargin.Value          := Ini.ReadInteger('SSA', 'Left margin',       30);
    seSSARightMargin.Value         := Ini.ReadInteger('SSA', 'Right margin',      30);
    seSSAVerticalMargin.Value      := Ini.ReadInteger('SSA', 'Vertical margin',   10);
    seSSAOutline.Value             := Ini.ReadInteger('SSA', 'Outline',           1);
    seSSAShadow.Value              := Ini.ReadInteger('SSA', 'Shadow',            1);
    cmbSSAAlignment.ItemIndex      := Ini.ReadInteger('SSA', 'Alignment',         2) - 1;
    cmbSSASubTopMidTitle.ItemIndex := Ini.ReadInteger('SSA', 'Type of subtitles', 0);
    case Ini.ReadInteger('SSA', 'Encoding', 0) of
      0:   cmbSSAEncoding.ItemIndex := 0;
      1:   cmbSSAEncoding.ItemIndex := 1;
      2:   cmbSSAEncoding.ItemIndex := 2;
      128: cmbSSAEncoding.ItemIndex := 3;
      129: cmbSSAEncoding.ItemIndex := 4;
      130: cmbSSAEncoding.ItemIndex := 5;
      134: cmbSSAEncoding.ItemIndex := 6;
      136: cmbSSAEncoding.ItemIndex := 7;
      161: cmbSSAEncoding.ItemIndex := 8;
      162: cmbSSAEncoding.ItemIndex := 9;
      163: cmbSSAEncoding.ItemIndex := 10;
      177: cmbSSAEncoding.ItemIndex := 11;
      178: cmbSSAEncoding.ItemIndex := 12;
      186: cmbSSAEncoding.ItemIndex := 13;
      204: cmbSSAEncoding.ItemIndex := 14;
      222: cmbSSAEncoding.ItemIndex := 15;
      238: cmbSSAEncoding.ItemIndex := 16;
    end;

    // ------------------------ //
    //          TMPlayer        //
    // ------------------------ //

    case Ini.ReadInteger('TMPlayer', 'Format', 0) of
      1: rdoTMPlayerFormat1.Checked := True;
      2: rdoTMPlayerFormat2.Checked := True;
      3: rdoTMPlayerPlusFormat1.Checked := True;
      4: rdoTMPlayerPlusFormat2.Checked := True else
      rdoTMPlayerMultiline.Checked := True;
    end;

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.tvFormatsClick(Sender: TObject);
begin
  pgeFormats.ActivePage := pgeFormats.Pages[tvFormats.Selected.Index];
  pnlHeading.Caption    := tvFormats.Selected.Text;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.tvFormatsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  pgeFormats.ActivePage := pgeFormats.Pages[tvFormats.Selected.Index];
  pnlHeading.Caption    := tvFormats.Selected.Text;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.btnOkClick(Sender: TObject);
var
  Ini       : TIniFile;
  i         : Integer;
  TextAlign : TTextAlign;
begin
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Output.ini');
  try
    // ------------------------ //
    //       DVD Subtitle       //
    // ------------------------ //
    Ini.WriteString('DVD Subtitle', 'Disk ID',   edtDVDSubtitleDiskId.Text);
    Ini.WriteString('DVD Subtitle', 'DVD Title', edtDVDSubtitleDVDTitle.Text);
    Ini.WriteString('DVD Subtitle', 'Language',  edtDVDSubtitleLanguage.Text);
    Ini.WriteString('DVD Subtitle', 'Author',    edtDVDSubtitleAuthor.Text);
    Ini.WriteString('DVD Subtitle', 'Web',       edtDVDSubtitleWeb.Text);
    Ini.WriteString('DVD Subtitle', 'Info',      edtDVDSubtitleInfo.Text);
    Ini.WriteString('DVD Subtitle', 'License',   edtDVDSubtitleLicense.Text);
    SubtitleAPI.SetOutputSettingsDVDSubtitle(True,
                                             edtDVDSubtitleDiskId.Text,
                                             edtDVDSubtitleDVDTitle.Text,
                                             edtDVDSubtitleLanguage.Text,
                                             edtDVDSubtitleAuthor.Text,
                                             edtDVDSubtitleWeb.Text,
                                             edtDVDSubtitleInfo.Text,
                                             edtDVDSubtitleLicense.Text);

    // ------------------------ //
    //           SAMI           //
    // ------------------------ //
    Ini.WriteString( 'SAMI', 'Font',             pnlSAMISample.Font.Name);
    Ini.WriteInteger('SAMI', 'Size',             pnlSAMISample.Font.Size);
    Ini.WriteInteger('SAMI', 'Subtitle color',   pnlSAMISubtitleColor.Color);
    Ini.WriteInteger('SAMI', 'Background color', pnlSAMIBackgroundColor.Color);
    Ini.WriteBool(   'SAMI', 'Bold',             fsBold in pnlSAMISample.Font.Style);
    Ini.WriteBool(   'SAMI', 'Italic',           fsItalic in pnlSAMISample.Font.Style);
    Ini.WriteBool(   'SAMI', 'Underline',        fsUnderline in pnlSAMISample.Font.Style);
    if rdoSAMILeft.Checked then
    begin
      Ini.WriteString('SAMI', 'Align', 'Left');
      TextAlign := taLeft;
    end else
    if rdoSAMIRight.Checked then
    begin
      Ini.WriteString('SAMI', 'Align', 'Right');
      TextAlign := taRight;
    end else
    begin
      Ini.WriteString('SAMI', 'Align', 'Center');
      TextAlign := taCenter;
    end;
    SubtitleAPI.SetOutputSettingsSAMI(True,
                                      pnlSAMISample.Font.Name,
                                      pnlSAMISample.Font.Size,
                                      fsBold in pnlSAMISample.Font.Style,
                                      fsItalic in pnlSAMISample.Font.Style,
                                      fsUnderline in pnlSAMISample.Font.Style,
                                      pnlSAMISubtitleColor.Color,
                                      pnlSAMIBackgroundColor.Color,
                                      TextAlign);

    // ------------------------ //
    //      Sonic Scenarist     //
    // ------------------------ //
    Ini.WriteBool(   'Scenarist', 'PAL',        cmbScenaristFPS.ItemIndex = 0);
    Ini.WriteBool(   'Scenarist', 'Drop frame', chkScenaristDropFrame.Checked);
    Ini.WriteInteger('Scenarist', 'Color0',     seScenaristColor0.Value);
    Ini.WriteInteger('Scenarist', 'Color1',     seScenaristColor1.Value);
    Ini.WriteInteger('Scenarist', 'Color2',     seScenaristColor2.Value);
    Ini.WriteInteger('Scenarist', 'Color3',     seScenaristColor3.Value);
    Ini.WriteInteger('Scenarist', 'Contrast0',  seScenaristContrast0.Value);
    Ini.WriteInteger('Scenarist', 'Contrast1',  seScenaristContrast1.Value);
    Ini.WriteInteger('Scenarist', 'Contrast2',  seScenaristContrast2.Value);
    Ini.WriteInteger('Scenarist', 'Contrast3',  seScenaristContrast3.Value);
    SubtitleAPI.SetOutputSettingsSonicScenarist(True,
                                                cmbScenaristFPS.ItemIndex = 0,
                                                chkScenaristDropFrame.Checked,
                                                seScenaristColor0.Value,
                                                seScenaristColor1.Value,
                                                seScenaristColor2.Value,
                                                seScenaristColor3.Value,
                                                seScenaristContrast0.Value,
                                                seScenaristContrast1.Value,
                                                seScenaristContrast2.Value,
                                                seScenaristContrast3.Value);

    // ------------------------ //
    //         SubViewer        //
    // ------------------------ //
    Ini.WriteString( 'SubViewer', 'Title',   edtSubViewer1Title.Text);
    Ini.WriteString( 'SubViewer', 'Author',  edtSubViewer1Author.Text);
    Ini.WriteString( 'SubViewer', 'Source',  edtSubViewer1Source.Text);
    Ini.WriteString( 'SubViewer', 'Program', edtSubViewer1Program.Text);
    Ini.WriteString( 'SubViewer', 'Path',    edtSubViewer1Path.Text);
    Ini.WriteInteger('SubViewer', 'Delay',   seSubViewer1Delay.Value);
    SubtitleAPI.SetOutputSettingsSubViewer1(True,
                                            edtSubViewer1Title.Text,
                                            edtSubViewer1Author.Text,
                                            edtSubViewer1Source.Text,
                                            edtSubViewer1Program.Text,
                                            edtSubViewer1Path.Text,
                                            seSubViewer1Delay.Value);

    // ------------------------ //
    //        SubViewer 2       //
    // ------------------------ //
    Ini.WriteString( 'SubViewer 2', 'Title',     edtSubViewer2Title.Text);
    Ini.WriteString( 'SubViewer 2', 'Author',    edtSubViewer2Author.Text);
    Ini.WriteString( 'SubViewer 2', 'Source',    edtSubViewer2Source.Text);
    Ini.WriteString( 'SubViewer 2', 'Program',   edtSubViewer2Program.Text);
    Ini.WriteString( 'SubViewer 2', 'Path',      edtSubViewer2Path.Text);
    Ini.WriteInteger('SubViewer 2', 'Delay',     seSubViewer2Delay.Value);
    Ini.WriteInteger('SubViewer 2', 'CD-Track',  seSubViewer2CDTrack.Value);
    Ini.WriteString( 'SubViewer 2', 'Comment',   edtSubViewer2Comment.Text);
    Ini.WriteString( 'SubViewer 2', 'Font Name', pnlSubViewer2Sample.Font.Name);
    Ini.WriteInteger('SubViewer 2', 'Font Size', pnlSubViewer2Sample.Font.Size);
    Ini.WriteInteger('SubViewer 2', 'Color',     pnlSubViewer2Sample.Font.Color);
    Ini.WriteBool(   'SubViewer 2', 'Bold',      fsBold in pnlSubViewer2Sample.Font.Style);
    Ini.WriteBool(   'SubViewer 2', 'Italic',    fsItalic in pnlSubViewer2Sample.Font.Style);
    Ini.WriteBool(   'SubViewer 2', 'Underline', fsUnderline in pnlSubViewer2Sample.Font.Style);
    Ini.WriteBool(   'SubViewer 2', 'Strikeout', fsStrikeOut in pnlSubViewer2Sample.Font.Style);
    SubtitleAPI.SetOutputSettingsSubViewer2(True,
                                            edtSubViewer2Title.Text,
                                            edtSubViewer2Author.Text,
                                            edtSubViewer2Source.Text,
                                            edtSubViewer2Program.Text,
                                            edtSubViewer2Path.Text,
                                            seSubViewer2Delay.Value,
                                            seSubViewer2CDTrack.Value,
                                            edtSubViewer2Comment.Text,
                                            pnlSubViewer2Sample.Font.Name,
                                            pnlSubViewer2Sample.Font.Size,
                                            pnlSubViewer2Sample.Font.Color,
                                            fsBold in pnlSubViewer2Sample.Font.Style,
                                            fsItalic in pnlSubViewer2Sample.Font.Style,
                                            fsUnderline in pnlSubViewer2Sample.Font.Style,
                                            fsStrikeOut in pnlSubViewer2Sample.Font.Style);

    // ------------------------ //
    //     SubStation Alpha     //
    // ------------------------ //
    Ini.WriteString( 'SSA', 'Title',           edtSSATitle.Text);
    Ini.WriteString( 'SSA', 'Script',          edtSSAScript.Text);
    Ini.WriteString( 'SSA', 'Font name',       pnlSSASample.Font.Name);
    Ini.WriteInteger('SSA', 'Font size',       pnlSSASample.Font.Size);
    Ini.WriteBool(   'SSA', 'Bold',            fsBold in pnlSSASample.Font.Style);
    Ini.WriteBool(   'SSA', 'Italic',          fsItalic in pnlSSASample.Font.Style);
    if cmbSSABorderStyle.ItemIndex = 0 then
      i := 1 else i := 3;
    Ini.WriteInteger('SSA', 'BorderStyle',       i);
    Ini.WriteInteger('SSA', 'Primary Color',     pnlSSAPrimary.Color);
    Ini.WriteInteger('SSA', 'Secondary Color',   pnlSSASecondary.Color);
    Ini.WriteInteger('SSA', 'Tertiary Color',    pnlSSATertiary.Color);
    Ini.WriteInteger('SSA', 'Shadow Color',      pnlSSAShadow.Color);
    Ini.WriteInteger('SSA', 'Left margin',       seSSALeftMargin.Value);
    Ini.WriteInteger('SSA', 'Right margin',      seSSARightMargin.Value);
    Ini.WriteInteger('SSA', 'Vertical margin',   seSSAVerticalMargin.Value);
    Ini.WriteInteger('SSA', 'Outline',           seSSAOutline.Value);
    Ini.WriteInteger('SSA', 'Shadow',            seSSAShadow.Value);
    Ini.WriteInteger('SSA', 'Alignment',         cmbSSAAlignment.ItemIndex + 1);
    Ini.WriteInteger('SSA', 'Type of subtitles', cmbSSASubTopMidTitle.ItemIndex);
    Ini.WriteInteger('SSA', 'Encoding',          StrCharsetToInt(cmbSSAEncoding.Items[cmbSSAEncoding.ItemIndex]));
    SubtitleAPI.SetOutputSettingsSubStationAlpha(True,
                                                 edtSSATitle.Text,
                                                 edtSSAScript.Text,
                                                 pnlSSASample.Font.Name,
                                                 pnlSSASample.Font.Size,
                                                 fsBold in pnlSSASample.Font.Style,
                                                 fsItalic in pnlSSASample.Font.Style,
                                                 i,
                                                 pnlSSAPrimary.Color,
                                                 pnlSSASecondary.Color,
                                                 pnlSSATertiary.Color,
                                                 pnlSSAShadow.Color,
                                                 seSSAOutline.Value,
                                                 seSSAShadow.Value,
                                                 (cmbSSAAlignment.ItemIndex + 1) or (cmbSSASubTopMidTitle.ItemIndex shl 2),
                                                 seSSALeftMargin.Value,
                                                 seSSARightMargin.Value,
                                                 seSSAVerticalMargin.Value,
                                                 StrCharsetToInt(cmbSSAEncoding.Items[cmbSSAEncoding.ItemIndex]));
                                                 
    // ------------------------ //
    //          TMPlayer        //
    // ------------------------ //
    if rdoTMPlayerFormat1.Checked then
    begin
      i := 1;
      SubtitleAPI.SetOutputSettingsTMPlayer(True, tfTimeStruct1);
    end else
    if rdoTMPlayerFormat2.Checked then
    begin
      i := 2;
      SubtitleAPI.SetOutputSettingsTMPlayer(True, tfTimeStruct2);
    end else
    if rdoTMPlayerPlusFormat1.Checked then
    begin
      i := 3;
      SubtitleAPI.SetOutputSettingsTMPlayer(True, tfPlusTimeStruct1);
    end else
    if rdoTMPlayerPlusFormat2.Checked then
    begin
      i := 4;
      SubtitleAPI.SetOutputSettingsTMPlayer(True, tfPlusTimeStruct2);
    end else
    begin
      i := 0;
      SubtitleAPI.SetOutputSettingsTMPlayer(True, tfMultiline);
    end;
    Ini.WriteInteger('TMPlayer','Format', i);

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.rdoSAMILeftClick(Sender: TObject);
begin
  pnlSAMISample.Alignment := taLeftJustify;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.rdoSAMICenterClick(Sender: TObject);
begin
  pnlSAMISample.Alignment := Classes.taCenter;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.rdoSAMIRightClick(Sender: TObject);
begin
  pnlSAMISample.Alignment := taRightJustify;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.pnlSAMISubtitleColorClick(Sender: TObject);
begin
  SetPanelColor(Sender);
  pnlSAMISample.Font.Color := pnlSAMISubtitleColor.Color;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.pnlSAMIBackgroundColorClick(Sender: TObject);
begin
  SetPanelColor(Sender);
  pnlSAMISample.Font.Color := pnlSAMISubtitleColor.Color;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.cmbScenaristFPSChange(Sender: TObject);
begin
  if cmbScenaristFPS.ItemIndex = 0 then
  begin
    chkScenaristDropFrame.Enabled := False;
    chkScenaristDropFrame.Checked := False;
  end else
    chkScenaristDropFrame.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.pnlSSAPrimaryClick(Sender: TObject);
begin
  SetPanelColor(Sender);
  pnlSSASample.Font.Color := (Sender as TPanel).Color;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.pnlSSASecondaryClick(Sender: TObject);
begin
  SetPanelColor(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.pnlSSATertiaryClick(Sender: TObject);
begin
  SetPanelColor(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.pnlSSAShadowClick(Sender: TObject);
begin
  SetPanelColor(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.btnSSASetFontClick(Sender: TObject);
begin
  fntDlg.Options := [];
  SetPanelFont(pnlSSASample);
  fntDlg.Options := [fdEffects];
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.btnSubViewer2SetFontClick(Sender: TObject);
begin
  fntDlg.Options := [fdEffects];
  SetPanelFont(pnlSubViewer2Sample);
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.lblTMPlayerMultilineClick(Sender: TObject);
begin
  rdoTMPlayerMultiline.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmOutputSettings.btnSAMISetFontClick(Sender: TObject);
begin
  fntDlg.Options := [fdEffects];
  SetPanelFont(pnlSAMISample);
  pnlSAMISubtitleColor.Color := pnlSAMISample.Font.Color;
end;

// -----------------------------------------------------------------------------

end.
