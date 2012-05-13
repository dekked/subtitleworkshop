unit formSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, CheckLst, Functions, Registry, IniFiles,
  General, USubtitleAPI, USubtitlesFunctions, FileTypes, MiSubtitulo,
  VideoPreview, NFormSizing, FastStrings, VirtualTrees, Mask;

type
  TfrmSettings = class(TForm)
    pnlHeading: TPanel;
    imgDrawing: TImage;
    lblTitle: TLabel;
    lblDescription: TLabel;
    bvlSeparator: TBevel;
    tvSettings: TTreeView;
    bvlSeparate1: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    dlgBrowse: TOpenDialog;
    dlgSetColor: TColorDialog;
    dlgSetFont: TFontDialog;
    pgeCtrl: TPageControl;
    pgeGeneral: TTabSheet;
    pgeFormats: TTabSheet;
    pgeFileTypes: TTabSheet;
    pgeSave: TTabSheet;
    pgeVideoPreview: TTabSheet;
    chkAlwaysOnTop: TCheckBox;
    chkInstance: TCheckBox;
    chkConfirmDelete: TCheckBox;
    chkInterpretInvalid: TCheckBox;
    chkAutosearchMovie: TCheckBox;
    chkForceWorkingTime: TCheckBox;
    chkKeepOrderOfLines: TCheckBox;
    chkSelectTextNL: TCheckBox;
    chkSelectTextPL: TCheckBox;
    bvlSeparate2: TBevel;
    chkWorkWithStyleTags: TCheckBox;
    edtRFLimit: TLabeledEdit;
    udRFLimit: TUpDown;
    lblDefaultFormat: TLabel;
    cmbDefaultFormat: TComboBox;
    lblFormatsToShow: TLabel;
    chkLstFormatsToShow: TCheckListBox;
    chkShowCustomFormats: TCheckBox;
    chkRegExtOnStart: TCheckBox;
    chkAssociateExtensions: TCheckBox;
    chklstExtensions: TCheckListBox;
    btnSelectAllExt: TButton;
    btnSelectZeroExt: TButton;
    chkAskToSave: TCheckBox;
    chkSaveAutomatically: TCheckBox;
    edtMinutes: TEdit;
    updMins: TUpDown;
    lblMinutes: TLabel;
    btnOutputSettings: TButton;
    pgeVideoPreviewSubs: TTabSheet;
    pgeExternalPreviewGeneral: TTabSheet;
    pgeExternalPreviewAdvanced: TTabSheet;
    lblParamDescription: TLabel;
    edtParameter: TLabeledEdit;
    cmbFormats: TComboBox;
    rdoCustomFormat: TRadioButton;
    rdoOriginalFormat: TRadioButton;
    lblSaveTempSubInFormat: TLabel;
    edtVidPlayer: TLabeledEdit;
    btnBrowse: TButton;
    btnDetect: TButton;
    rdoAskForDifferentVideo: TRadioButton;
    rdoTestWithVideo: TRadioButton;
    edtAVIFile: TEdit;
    btnBrowse2: TButton;
    chkDrawBorder: TCheckBox;
    chkDrawShadow: TCheckBox;
    chkTransparent: TCheckBox;
    btnSubFont: TButton;
    btnSubColor: TButton;
    btnBackground: TButton;
    edtBorderWidth: TLabeledEdit;
    edtShadowWidth: TLabeledEdit;
    udShadowWidth: TUpDown;
    udBorderWidth: TUpDown;
    pnlSubSample: TPanel;
    subSample: TMiSubtitulo;
    lblDoubleClickInSub: TLabel;
    cmbDoubleClickInSub: TComboBox;
    pgeProgramLook: TTabSheet;
    lblFontToUse: TLabel;
    cmbFonts: TComboBox;
    lblFontSize: TLabel;
    edtFontSize: TEdit;
    udFontSize: TUpDown;
    lblTextAndTransFieldsAlign: TLabel;
    cmbTextAlign: TComboBox;
    pgeListLook: TTabSheet;
    chkMarkUnTransSubs: TCheckBox;
    chkApplyStyle: TCheckBox;
    chkShowGridLines: TCheckBox;
    pnlUnTransColor: TPanel;
    pgeMenuLook: TTabSheet;
    chkUseOfficeXPStyleMenu: TCheckBox;
    chkUseGradientMenu: TCheckBox;
    chkShowHorzScrollBar: TCheckBox;
    pgeCharsets: TTabSheet;
    chkShowInMainForm: TCheckBox;
    lblOrgCharset: TLabel;
    cmbOrgCharset: TComboBox;
    cmbTransCharset: TComboBox;
    lblTransCharset: TLabel;
    edtSecsToJump1: TLabeledEdit;
    udSecsToJump1: TUpDown;
    cmbShiftDoubleClickInSub: TComboBox;
    lblShiftDoubleClickInSub: TLabel;
    edtSecsToJump2: TLabeledEdit;
    udSecsToJump2: TUpDown;
    btnSelectAllFormat: TButton;
    btnSelectZeroFormat: TButton;
    pgeAdvanced: TTabSheet;
    gbSmartLineAdjust: TGroupBox;
    edtTwoLinesIfLongerThan: TLabeledEdit;
    udTwoLinesIfLongerThan: TUpDown;
    lblCharacters1: TLabel;
    gbDivideLines: TGroupBox;
    lblCharacters2: TLabel;
    edtBreakLineAfter: TLabeledEdit;
    udBreakLineAfter: TUpDown;
    chkSLAAutomatically: TCheckBox;
    Bevel2: TBevel;
    lblSeconds: TLabel;
    lblRewindAndForward: TLabel;
    edtRewindAndForwardTime: TMaskEdit;
    lblDefaultAltPlayRate: TLabel;
    cmbDefaultAltPlayRate: TComboBox;
    edtMaxLineLength: TLabeledEdit;
    udMaxLineLength: TUpDown;
    lblCharacters3: TLabel;
    chkForceUsingRegions: TCheckBox;
    chkToggleBreakPoint: TCheckBox;
    chkSaveAsBackup: TCheckBox;
    edtShiftTime: TLabeledEdit;
    udShiftTime: TUpDown;
    lblMilliseconds: TLabel;
    chkNoInteractionWithTags: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure tvSettingsClick(Sender: TObject);
    procedure tvSettingsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOkClick(Sender: TObject);
    procedure btnSelectAllExtClick(Sender: TObject);
    procedure btnSelectZeroExtClick(Sender: TObject);
    procedure chkAssociateExtensionsClick(Sender: TObject);
    procedure chkSaveAutomaticallyClick(Sender: TObject);
    procedure cmbFontsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnOutputSettingsClick(Sender: TObject);
    procedure tvSettingsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure chkDrawBorderClick(Sender: TObject);
    procedure chkDrawShadowClick(Sender: TObject);
    procedure btnSubFontClick(Sender: TObject);
    procedure btnSubColorClick(Sender: TObject);
    procedure btnBackgroundClick(Sender: TObject);
    procedure edtShadowWidthChange(Sender: TObject);
    procedure edtBorderWidthChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDetectClick(Sender: TObject);
    procedure rdoAskForDifferentVideoClick(Sender: TObject);
    procedure rdoTestWithVideoClick(Sender: TObject);
    procedure btnBrowse2Click(Sender: TObject);
    procedure rdoOriginalFormatClick(Sender: TObject);
    procedure rdoCustomFormatClick(Sender: TObject);
    procedure chkRegExtOnStartMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chkRegExtOnStartKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chklstExtensionsClickCheck(Sender: TObject);
    procedure pnlUnTransColorClick(Sender: TObject);
    procedure chkUseOfficeXPStyleMenuClick(Sender: TObject);
    procedure btnSelectAllFormatClick(Sender: TObject);
    procedure btnSelectZeroFormatClick(Sender: TObject);
    procedure chkTransparentClick(Sender: TObject);
  private
    procedure SetLanguage;
    procedure UpdateSubSamplePos;
  public
    { Public declarations }
  end;

var
  frmSettings : TfrmSettings;
  AllVidFiles : String;
  ModifyExts  : Boolean = False;

implementation

uses formMain, formOutputSettings;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmSettings.SetLanguage;
var
  LF      : TIniFile;
  tmpItem : String;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);

  try
    With LF do
    begin
      AllVidFiles := ReadString('General','AllVidFiles','All supported video files');

      Caption                := ReadString('Settings Form','01','Settings');
      lblTitle.Caption       := ReadString('Settings Form','01','Settings');
      lblDescription.Caption := ReadString('Settings Form','02','Modify the program configuration');

      tvSettings.Items.Add(nil, ReadString('Settings Form','03', 'General'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 0;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-1], ReadString('Settings Form','04', 'Advanced'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 1;
      tvSettings.Items[tvSettings.Items.Count-1].MakeVisible;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-2], ReadString('Settings Form','05', 'Charsets'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 2;
      tvSettings.Items[tvSettings.Items.Count-1].MakeVisible;
      tvSettings.Items.Add(nil, ReadString('Settings Form','06', 'Formats'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 3;
      tvSettings.Items.Add(nil, ReadString('Settings Form','07', 'File types'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 4;
      tvSettings.Items.Add(nil, ReadString('Settings Form','08', 'Save'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 5;
      tvSettings.Items.Add(nil, ReadString('Settings Form','09', 'Video Preview'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 6;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-1], ReadString('Settings Form','10', 'Subtitles'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 7;
      tvSettings.Items[tvSettings.Items.Count-1].MakeVisible;
      tvSettings.Items.Add(nil, ReadString('Settings Form','11', 'External Preview'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := -1;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-1], ReadString('Settings Form','03', 'General'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 8;
      tvSettings.Items[tvSettings.Items.Count-1].MakeVisible;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-2], ReadString('Settings Form','04', 'Advanced'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 9;
      tvSettings.Items[tvSettings.Items.Count-2].MakeVisible;
      tvSettings.Items.Add(nil, ReadString('Settings Form','12', 'Look'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := -1;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-1], ReadString('Settings Form','13', 'Program'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 10;
      tvSettings.Items[tvSettings.Items.Count-1].MakeVisible;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-2], ReadString('Settings Form','14', 'List'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 11;
      tvSettings.Items[tvSettings.Items.Count-2].MakeVisible;
      tvSettings.Items.AddChild(tvSettings.Items[tvSettings.Items.Count-3], ReadString('Settings Form','15', 'Menu'));
      tvSettings.Items[tvSettings.Items.Count-1].ImageIndex := 12;
      tvSettings.Items[tvSettings.Items.Count-3].MakeVisible;
      tvSettings.Items[0].Selected := True;

      // General
      chkAlwaysOnTop.Caption           := ReadString('Settings Form', '16', 'Always on top');
      chkInstance.Caption              := ReadString('Settings Form', '17', 'Allow more than one instance running');
      chkConfirmDelete.Caption         := ReadString('Settings Form', '18', 'Confirm when deleting subtitles');
      chkInterpretInvalid.Caption      := ReadString('Settings Form', '19', 'Interpret invalid files as plain text');
      chkAutosearchMovie.Caption       := ReadString('Settings Form', '20', 'Autosearch for movie');
      chkForceWorkingTime.Caption      := ReadString('Settings Form', '21', 'Force working in time mode');
      chkKeepOrderOfLines.Caption      := ReadString('Settings Form', '22', 'Keep order of lines when reverse text');
      chkSelectTextNL.Caption          := ReadString('Settings Form', '23', 'Select text on jump to next line');
      chkSelectTextPL.Caption          := ReadString('Settings Form', '24', 'Select text on jump to previous line');
      chkNoInteractionWithTags.Caption := ReadString('Settings Form', '25', 'No interaction with tags');
      chkWorkWithStyleTags.Caption     := ReadString('Settings Form', '26', 'Work with style tags');
      edtRFLimit.EditLabel.Caption     := ReadString('Settings Form', '27', 'Recent files limit:');

      // Advanced
      gbSmartLineAdjust.Caption                 := ReadString('Settings Form', '28', 'Smart line adjust');
      edtTwoLinesIfLongerThan.EditLabel.Caption := ReadString('Settings Form', '29', 'Two lines if longer than:');
      lblCharacters1.Caption                    := ReadString('Settings Form', '30', 'characters');
      chkToggleBreakPoint.Caption               := ReadString('Settings Form', '31', 'Toggle breakpoint');
      gbDivideLines.Caption                     := ReadString('Settings Form', '32', 'Divide lines');
      edtBreakLineAfter.EditLabel.Caption       := ReadString('Settings Form', '33', 'Break line after:');
      lblCharacters2.Caption                    := lblCharacters1.Caption;
      chkSLAAutomatically.Caption               := ReadString('Settings Form', '34', 'Smart line adjust automatically');
      edtMaxLineLength.EditLabel.Caption        := ReadString('Settings Form', '35', 'Maximum line length:');
      lblCharacters3.Caption                    := lblCharacters1.Caption;
      edtShiftTime.EditLabel.Caption            := ReadString('Settings Form', '36', 'Shift time:');
      lblMilliseconds.Caption                   := ReadString('Settings Form', '37', 'milliseconds');

      // Charsets
      chkShowInMainForm.Caption := ReadString('Settings Form', '38', 'Show in main form');
      lblOrgCharset.Caption     := ReadString('Settings Form', '39', 'Original charset:');
      lblTransCharset.Caption   := ReadString('Settings Form', '40', 'Translation charset:');

      // Formats
      lblDefaultFormat.Caption     := ReadString('Settings Form', '41', 'Default format:');
      lblFormatsToShow.Caption     := ReadString('Settings Form', '42', 'Formats to show when "Save as":');
      chkShowCustomFormats.Caption := ReadString('Settings Form', '43', 'Show custom formats');
      btnSelectAllFormat.Caption   := ReadString('Settings Form', '44', 'Select &all');
      btnSelectZeroFormat.Caption  := ReadString('Settings Form', '45', 'Select &zero');

      // File types
      chkRegExtOnStart.Caption       := ReadString('Settings Form', '46', 'Register extensions on start');
      chkAssociateExtensions.Caption := ReadString('Settings Form', '47', 'Associate with most supported subtitle extensions');
      btnSelectAllExt.Caption        := btnSelectAllFormat.Caption;
      btnSelectZeroExt.Caption       := btnSelectZeroFormat.Caption;

      // Save
      chkAskToSave.Caption         := ReadString('Settings Form', '48', 'Ask to save on exit program/close subtitle');
      chkSaveAutomatically.Caption := ReadString('Settings Form', '49', 'Save work automatically every');
      lblMinutes.Caption           := ReadString('Settings Form', '50', 'minutes.');
      chkSaveAsBackup.Caption      := ReadString('Settings Form', '51', 'Save as backup');
      btnOutputSettings.Caption    := ReadString('Settings Form', '52', 'Output settings...');

      // Video preview
      lblDoubleClickInSub.Caption      := ReadString('Settings Form', '53', 'Double click in a subtitle:');
      edtSecsToJump1.EditLabel.Caption := ReadString('Settings Form', '54', 'Seconds to jump:');
      lblShiftDoubleClickInSub.Caption := ReadString('Settings Form', '55', 'Shift-double click in a subtitle:');
      edtSecsToJump2.EditLabel.Caption := edtSecsToJump1.EditLabel.Caption;
      cmbDoubleClickInSub.Clear;
      cmbShiftDoubleClickInSub.Clear;
      cmbDoubleClickInSub.Items.Add(ReadString('Settings Form', '56', 'Focus text box'));
      cmbDoubleClickInSub.Items.Add(ReadString('Settings Form', '57', 'Go subtitle''s time in video'));
      cmbDoubleClickInSub.Items.Add(ReadString('Settings Form', '58', 'Go N second(s) before subtitle in video'));
      cmbShiftDoubleClickInSub.Items := cmbDoubleClickInSub.Items;
      lblRewindAndForward.Caption   := ReadString('Settings Form', '59', 'Rewind and forward time:');
      lblSeconds.Caption            := ReadString('Settings Form', '60', 'seconds.');
      lblDefaultAltPlayRate.Caption := ReadString('Settings Form', '61', 'Default altered playback rate:');

      // Video Preview Subtitles
      chkDrawBorder.Caption        := ReadString('Settings Form', '62', 'Draw border');
      chkDrawShadow.Caption        := ReadString('Settings Form', '63', 'Draw shadow');
      chkTransparent.Caption       := ReadString('Settings Form', '64', 'Try transparent background');
      chkForceUsingRegions.Caption := ReadString('Settings Form', '65', 'Force using regions (may be slow)');
      btnSubFont.Caption     := ReadString('Settings Form', '66', 'Font...');
      btnSubColor.Caption    := ReadString('Settings Form', '67', 'Color...');
      btnBackground.Caption  := ReadString('Settings Form', '68', 'Background...');

      edtBorderWidth.EditLabel.Caption := ReadString('Settings Form', '69', 'Border width:');
      edtShadowWidth.EditLabel.Caption := ReadString('Settings Form', '70', 'Shadow width:');
      subSample.Text                   := ReadString('Settings Form', '71', 'SAMPLE');

      // External preview general
      edtVidPlayer.EditLabel.Caption  := ReadString('Settings Form', '72', 'Exe of the video player:');
      btnBrowse.Caption               := BTN_BROWSE;
      btnDetect.Caption               := ReadString('Settings Form', '73', 'Detect associated program');
      rdoAskForDifferentVideo.Caption := ReadString('Settings Form', '74', 'Ask for a different video each time');
      rdoTestWithVideo.Caption        := ReadString('Settings Form', '75', 'Always test with video:');
      btnBrowse2.Caption              := BTN_BROWSE;

      // External preview advanced
      lblSaveTempSubInFormat.Caption  := ReadString('Settings Form', '76', 'Save temporary subtitle in format:');
      rdoOriginalFormat.Caption       := ReadString('Settings Form', '77', 'Original format');
      rdoCustomFormat.Caption         := ReadString('Settings Form', '78', 'Custom format:');
      edtParameter.EditLabel.Caption  := ReadString('Settings Form', '79', 'Parameter to send to the video player:');
      lblParamDescription.Caption     := ReplaceString(ReadString('Settings Form', '80', 'VIDEO_FILE represents the video file in which you are going to test the subtitles. SUBT_FILE is the parameter in which the temporary subtitle file will be sent to the video player. You may add other parameters, for example full screen, etc.'), '|', #13#10);

      // Look / Program
      lblFontToUse.Caption               := ReadString('Settings Form', '81', 'Font to use in the program:');
      lblFontSize.Caption                := ReadString('Settings Form', '82', 'Font size:');
      lblTextAndTransFieldsAlign.Caption := ReadString('Settings Form', '83', '"Text" and "Translation" fields align:');
      cmbTextAlign.Items.Clear;
      tmpItem := ReadString('Settings Form','84','Left|Right|Center');
      cmbTextAlign.Items.Add(Copy(tmpItem, 1, Pos('|', tmpItem) - 1));
      cmbTextAlign.Items.Add(Copy(tmpItem, Pos('|', tmpItem) + 1, SmartPos('|', tmpItem, True, Pos('|', tmpItem) + 1) - (Pos('|', tmpItem) + 1)));
      cmbTextAlign.Items.Add(Copy(tmpItem, SmartPos('|', tmpItem, True, Pos('|', tmpItem) + 1) + 1, Length(tmpItem)));

      // Look / List
      chkShowGridLines.Caption     := ReadString('Settings Form', '85', 'Show grid lines');
      chkApplyStyle.Caption        := ReadString('Settings Form', '86', 'Apply style to subtitles');
      chkMarkUnTransSubs.Caption   := ReadString('Settings Form', '87', 'Mark untranslated subtitles with color:');
      chkShowHorzScrollBar.Caption := ReadString('Settings Form', '88', 'Show horizontal scrollbar');

      // Look / Menu
      chkUseOfficeXPStyleMenu.Caption := ReadString('Settings Form', '89', 'Use Office XP style Menu');
      chkUseGradientMenu.Caption      := ReadString('Settings Form', '90', 'Use gradient menu');

      btnOk.Caption     := BTN_OK;
      btnCancel.Caption := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnOk.ParentFont       := True;
      lblTitle.ParentFont    := True;
      Font                   := frmMain.Font;
      subSample.Font.Charset := Font.Charset;
      lblTitle.Font.Style    := frmMain.Font.Style + [fsBold];
      btnOk.Font.Style       := frmMain.Font.Style + [fsBold];

    end;
  finally
    LF.Free;
  end;

end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.UpdateSubSamplePos;
begin
  subSample.Left := (pnlSubSample.Width div 2) - (subSample.Width div 2);
  subSample.Top  := (pnlSubSample.Height div 2) - (subSample.Height div 2);
end;

// -----------------------------------------------------------------------------

function GetProgramAssociation(Ext : string): String;
var
  Reg : TRegistry;
  s   : String;
begin
  s := '';

  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;

  if Reg.OpenKey('.' + Ext + '\shell\open\command', False) <> False then
  begin
    S := Reg.ReadString('');
    Reg.CloseKey;
  end else
  begin
    if Reg.OpenKey('.' + Ext, False) <> False then
    begin
      s := Reg.ReadString('');
      Reg.CloseKey;

      if s <> '' then
      begin
        if Reg.OpenKey(s + '\shell\open\command',False) <> False then
          S := Reg.ReadString('');
        reg.CloseKey;
      end;
    end;
  end;

  if Pos('%', s) > 0 then
    Delete(s, Pos('%', s), Length(s));

  if ((Length(s) > 0) and (s[1] = '"')) then
    Delete(s, 1, 1);

  if ((Length(s) > 0) and (s[Length(s)] = '"')) then
    Delete(s, Length(s), 1);

  while ((Length(s) > 0) and ((S[Length(s)] = #32) or (s[Length(s)] = '"'))) do
    Delete(s, Length(s), 1);

  Result := S;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.tvSettingsCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSelectAllExtClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to chklstExtensions.Items.Count-1 do
    if not chklstExtensions.Checked[i] then
      chkLstExtensions.Checked[i] := True;
  ModifyExts := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSelectZeroExtClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to chklstExtensions.Items.Count-1 do
    if chklstExtensions.Checked[i] then
      chkLstExtensions.Checked[i] := False;
  ModifyExts := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkAssociateExtensionsClick(Sender: TObject);
var
  i: integer;
begin
  if chkAssociateExtensions.Checked then
  begin
    chklstExtensions.Enabled := True;
    btnSelectAllExt.Enabled := True;
    btnSelectZeroExt.Enabled := True;
    for i := 0 to chklstExtensions.Items.Count-1 do
      if not chklstExtensions.Checked[i] then
        chkLstExtensions.Checked[i] := True;
  end else
  begin
    chklstExtensions.ItemIndex := -1;
    chklstExtensions.Enabled := False;
    btnSelectAllExt.Enabled := False;
    btnSelectZeroExt.Enabled := False;
    for i := 0 to chklstExtensions.Items.Count-1 do
      if chklstExtensions.Checked[i] then
        chkLstExtensions.Checked[i] := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkSaveAutomaticallyClick(Sender: TObject);
begin
  edtMinutes.Enabled      := chkSaveAutomatically.Checked;
  updMins.Enabled         := chkSaveAutomatically.Checked;
  lblMinutes.Enabled      := chkSaveAutomatically.Checked;
  chkSaveAsBackup.Enabled := chkSaveAutomatically.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.cmbFontsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TComboBox do
  begin
     Canvas.Font.Name := Items[Index];
     Canvas.FillRect(Rect);
     Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnOutputSettingsClick(Sender: TObject);
begin
  frmOutputSettings := TfrmOutputSettings.Create(Application);
  frmOutputSettings.ShowModal;
  frmOutputSettings.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.tvSettingsClick(Sender: TObject);
begin
{  case tvSettings.Selected.ImageIndex of
    0: pgeCtrl.ActivePage := pgeGeneral;
    1: pgeCtrl.ActivePage := pgeCharsets;
    2: pgeCtrl.ActivePage := pgeFormats;
    3: pgeCtrl.ActivePage := pgeFileTypes;
    4: pgeCtrl.ActivePage := pgeSave;
    5: pgeCtrl.ActivePage := pgeVideoPreview;
    6: pgeCtrl.ActivePage := pgeVideoPreviewSubs;
    7: pgeCtrl.ActivePage := nil;
    8: pgeCtrl.ActivePage := pgeExternalPreviewGeneral;
    9: pgeCtrl.ActivePage := pgeExternalPreviewAdvanced;
    10: pgeCtrl.ActivePage := nil;
    11: pgeCtrl.ActivePage := pgeProgramLook;
    12: pgeCtrl.ActivePage := pgeListLook;
    13: pgeCtrl.ActivePage := pgeMenuLook;
  end; }
  pgeCtrl.ActivePageIndex := tvSettings.Selected.ImageIndex;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.tvSettingsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {case tvSettings.Selected.ImageIndex of
    0: pgeCtrl.ActivePage := pgeGeneral;
    1: pgeCtrl.ActivePage := pgeCharsets;
    2: pgeCtrl.ActivePage := pgeFormats;
    3: pgeCtrl.ActivePage := pgeFileTypes;
    4: pgeCtrl.ActivePage := pgeSave;
    5: pgeCtrl.ActivePage := pgeVideoPreview;
    6: pgeCtrl.ActivePage := pgeVideoPreviewSubs;
    7: pgeCtrl.ActivePage := nil;
    8: pgeCtrl.ActivePage := pgeExternalPreviewGeneral;
    9: pgeCtrl.ActivePage := pgeExternalPreviewAdvanced;
    10: pgeCtrl.ActivePage := nil;
    11: pgeCtrl.ActivePage := pgeProgramLook;
    12: pgeCtrl.ActivePage := pgeListLook;
    13: pgeCtrl.ActivePage := pgeMenuLook;
  end; }
  pgeCtrl.ActivePageIndex := tvSettings.Selected.ImageIndex;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.FormCreate(Sender: TObject);
var
  i     : Integer;
  Ini   : TIniFile;
  Reg   : TRegistry;
  Items : TStrings;
begin
  // W: 530   H: 421

  SetLanguage;
  pgeCtrl.ActivePage := pgeGeneral;

  Ini := TIniFile.Create(IniRoot);
  try
    // --------------------------------- //
    //              General              //
    // --------------------------------- //

    chkAlwaysOnTop.Checked           := Ini.ReadBool('Settings', 'Always on top', False);
    chkInstance.Checked              := Ini.ReadBool('Settings', 'Allow more than one instance', False);
    chkConfirmDelete.Checked         := Ini.ReadBool('Settings', 'Confirm when deleting a subtitle', False);
    chkInterpretInvalid.Checked      := Ini.ReadBool('Settings', 'Interpret invalid files as plain text', False);
    chkAutosearchMovie.Checked       := Ini.ReadBool('Settings', 'Autosearch for movie', True);
    chkForceWorkingTime.Checked      := Ini.ReadBool('Settings', 'Force working in time mode', False);
    chkKeepOrderOfLines.Checked      := Ini.ReadBool('Settings', 'Keep order of lines when reverse text', True);
    chkSelectTextNL.Checked          := Ini.ReadBool('Settings', 'Select text on jump to next line', True);
    chkSelectTextPL.Checked          := Ini.ReadBool('Settings', 'Select text on jump to previous line', True);
    chkNoInteractionWithTags.Checked := Ini.ReadBool('Settings', 'No interaction with tags', False);
    chkWorkWithStyleTags.Checked     := Ini.ReadBool('Settings', 'Work with style tags', True);
    udRFLimit.Position               := Ini.ReadInteger('Settings', 'MaxRF', 10);

    // --------------------------------- //
    //             Advanced              //
    // --------------------------------- //
    udTwoLinesIfLongerThan.Position := Ini.ReadInteger('Advanced', 'Two lines if longer than', 40);
    chkToggleBreakPoint.Checked     := Ini.ReadBool('Advanced', 'Toggle breakpoint', False);
    udBreakLineAfter.Position       := Ini.ReadInteger('Advanced', 'Break line after', 40);
    chkSLAAutomatically.Checked     := Ini.ReadBool('Advanced', 'Smart line adjust automatically', True);
    udMaxLineLength.Position        := Ini.ReadInteger('Advanced', 'Maximum line length', 45);
    udShiftTime.Position            := Ini.ReadInteger('Advanced', 'Shift time', 100);

    // --------------------------------- //
    //              Charsets             //
    // --------------------------------- //
    chkShowInMainForm.Checked := Ini.ReadBool('Settings', 'Show charsets in main form', True);
    AddCharsets(cmbOrgCharset);
    AddCharsets(cmbTransCharset);
    cmbOrgCharset.ItemIndex   := frmMain.cmbOrgCharset.ItemIndex;
    cmbTransCharset.ItemIndex := frmMain.cmbTransCharset.ItemIndex;

    // --------------------------------- //
    //               Formats             //
    // --------------------------------- //
    GetFormatsList(cmbDefaultFormat.Items);
    cmbDefaultFormat.ItemIndex := SubtitleAPI.GetFormatIndex(Ini.ReadString('Formats', 'Default format', 'SubRip')) - 1;
    GetFormatsList(chkLstFormatsToShow.Items);
    for i := 0 to chkLstFormatsToShow.Items.Count-1 do
      chkLstFormatsToShow.Checked[i] := Ini.ReadBool('Formats to show', SubtitleAPI.GetFormatName(i+1),True);
    chkShowCustomFormats.Checked := Ini.ReadBool('Formats', 'Show custom formats', False);

    // --------------------------------- //
    //             File types            //
    // --------------------------------- //
    SeparateExtensions(Items, GetExtensionsList);
    Items.Add('*' + ID_STPEXT); // Subtitle Translation Project (STP file)
    Items.Add('*' + ID_SRFEXT); // URUSoft Subtitle Report File
    chkLstExtensions.Items := Items;
    chkRegExtOnStart.Checked := Ini.ReadBool('File types', 'Register extensions on start', False);
    chkAssociateExtensions.Checked := Ini.ReadBool('File types', 'Associate extensions', True);
    SeparateExtensions(Items, Ini.ReadString('File types', 'Associated extensions', ''));
    for i := 0 to chkLstExtensions.Items.Count-1 do
      chkLstExtensions.Checked[i] := False;
    for i := 0 to Items.Count-1 do
      chkLstExtensions.Checked[chkLstExtensions.Items.IndexOf(Items[i])] := True;

    // --------------------------------- //
    //                Save               //
    // --------------------------------- //
    chkAskToSave.Checked         := Ini.ReadBool('Save', 'Ask to save', True);
    chkSaveAutomatically.Checked := Ini.ReadBool('Save', 'Save work automatically', False);
    updMins.Position             := Ini.ReadInteger('Save', 'Time interval', 60000) div 60000;
    chkSaveAsBackup.Checked      := Ini.ReadBool('Save', 'Save as backup', True);

    // --------------------------------- //
    //           Video preview           //
    // --------------------------------- //
    cmbDoubleClickInSub.ItemIndex      := Ini.ReadInteger('Video preview', 'Double click in a subtitle', 1);
    cmbShiftDoubleClickInSub.ItemIndex := Ini.ReadInteger('Video preview', 'Shift-double click in a subtitle', 2);
    udSecsToJump1.Position             := Ini.ReadInteger('Video preview', 'Seconds to jump 1', 1);
    udSecsToJump2.Position             := Ini.ReadInteger('Video preview', 'Seconds to jump 2', 1);
    edtRewindAndForwardTime.Text       := Ini.ReadString('Video preview', 'Rewind and forward', '0,500');
    cmbDefaultAltPlayRate.ItemIndex    := Ini.ReadInteger('Video preview', 'Default altered playback rate', 0);

    // --------------------------------- //
    //       Video preview/Subtitles     //
    // --------------------------------- //
    chkDrawBorder.Checked        := Ini.ReadBool('Video preview subtitles', 'Draw border', True);
    subSample.Border             := Ini.ReadBool('Video preview subtitles', 'Draw border', True);
    chkDrawShadow.Checked        := Ini.ReadBool('Video preview subtitles', 'Draw shadow', True);
    subSample.Shadow             := Ini.ReadBool('Video preview subtitles', 'Draw shadow', True);
    chkTransparent.Checked       := Ini.ReadBool('Video preview subtitles', 'Transparent', True);
    chkForceUsingRegions.Checked := Ini.ReadBool('Video preview subtitles', 'Force using regions', False);
    subSample.Font.Name          := Ini.ReadString('Video preview subtitles', 'Font name', 'Tahoma');
    subSample.Font.Size          := Ini.ReadInteger('Video preview subtitles', 'Font size', 14);
    subSample.TextColor          := Ini.ReadInteger('Video preview subtitles', 'Font color', clWhite);
    subSample.BackgroundColor    := Ini.ReadInteger('Video preview subtitles', 'Background color', clBtnFace);
    pnlSubSample.Color           := Ini.ReadInteger('Video preview subtitles', 'Background color', clBtnFace);
    udBorderWidth.Position       := Ini.ReadInteger('Video preview subtitles', 'Border width', 1);
    udShadowWidth.Position       := Ini.ReadInteger('Video preview subtitles', 'Shadow width', 1);
    subSample.Font.Style         := [];
    if Ini.ReadBool('Video preview subtitles', 'Bold', True) then
      subSample.Font.Style := subSample.Font.Style + [fsBold];
    if Ini.ReadBool('Video preview subtitles', 'Italic', False) then
      subSample.Font.Style := subSample.Font.Style + [fsItalic];
    if Ini.ReadBool('Video preview subtitles', 'Underline', False) then
      subSample.Font.Style := subSample.Font.Style + [fsUnderline];
    UpdateSubSamplePos;
    chkForceUsingRegions.Enabled := chkTransparent.Checked;
    
    // --------------------------------- //
    //      External preview/General     //
    // --------------------------------- //
    edtVidPlayer.Text               := Ini.ReadString('External Preview', 'Video player', '');
    rdoAskForDifferentVideo.Checked := Ini.ReadBool('External Preview', 'Ask for different video', True);
    rdoTestWithVideo.Checked        := not Ini.ReadBool('External Preview', 'Ask for different video', True);
    edtAVIFile.Text                 := Ini.ReadString('External Preview', 'Video to test', '');

    // --------------------------------- //
    //     External preview/Advanced     //
    // --------------------------------- //
    GetFormatsList(cmbFormats.Items);
    rdoOriginalFormat.Checked := Ini.ReadBool('External Preview','Save in original format',True);
    rdoCustomFormat.Checked   := not Ini.ReadBool('External Preview','Save in original format',True);
    cmbFormats.ItemIndex      := SubtitleAPI.GetFormatIndex(Ini.ReadString('External Preview', 'Custom format', 'SubRip')) - 1;
    edtParameter.Text         := Ini.ReadString('External Preview','Parameter','');

    // If ViPlay is installed we configure it automatically...
    if Ini.ReadString('External Preview', 'Video player', '') = '' then
    begin
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CURRENT_USER;
        if Reg.KeyExists('Software\URUSoft\ViPlay') then
        begin
          Reg.OpenKey('Software\URUSoft\ViPlay',False);
          if FileExists(Reg.ReadString('Install_Dir') + '\ViPlay.exe') then
          begin
            Ini.WriteString('External Preview','Video player',Reg.ReadString('Install_Dir') + '\ViPlay.exe');
            edtVidPlayer.Text := Reg.ReadString('Install_Dir') + '\ViPlay.exe';
            if edtParameter.Text = '' then
            begin
              edtParameter.Text := '/MOVIE:"VIDEO_FILE" /SUBTITLE:"SUBT_FILE" /FS';
              Ini.WriteString('External Preview', 'Parameter', '/MOVIE:"VIDEO_FILE" /SUBTITLE:"SUBT_FILE" /FS');
            end;
          end;
        end;
      Reg.Free;
    end;

    // --------------------------------- //
    //           Look / Program          //
    // --------------------------------- //
    cmbFonts.Items         := screen.Fonts;
    cmbFonts.ItemIndex     := cmbFonts.Items.IndexOf(Ini.ReadString('Program look', 'Font', 'Tahoma'));
    udFontSize.Position    := Ini.ReadInteger('Program look', 'Font size', 8);
    cmbTextAlign.ItemIndex := Ini.ReadInteger('Program look', '"Text" and "Translation" fields align', 2);
    if cmbTextAlign.ItemIndex <= -1 then
      cmbTextAlign.ItemIndex := 2;

    // --------------------------------- //
    //             Look / List           //
    // --------------------------------- //
    chkShowGridLines.Checked     := Ini.ReadBool('List look', 'Show grid lines', True);
    chkApplyStyle.Checked        := Ini.ReadBool('List look', 'Apply style to subtitles', True);
    chkMarkUnTransSubs.Checked   := Ini.ReadBool('List look', 'Mark untranslated subtitles', True);
    pnlUnTransColor.Color        := Ini.ReadInteger('List look', 'Untranslated subtitles color', clRed);
    chkShowHorzScrollBar.Checked := Ini.ReadBool('List look', 'Show horizontal scrollbar', True);

    // --------------------------------- //
    //             Look / Menu           //
    // --------------------------------- //
    chkUseOfficeXPStyleMenu.Checked := Ini.ReadBool('Menu look', 'Use Office XP Style menu', True);
    chkUseGradientMenu.Checked      := Ini.ReadBool('Menu look', 'Use gradient menu', True);
    chkUseGradientMenu.Enabled      := chkUseOfficeXPStyleMenu.Checked;

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnOkClick(Sender: TObject);
var
  i      : Integer;
  ExtStr : String;
  Ini    : TIniFile;
begin
  ExtStr := '';
  Ini := TIniFile.Create(IniRoot);
  try
    // --------------------------------- //
    //              General              //
    // --------------------------------- //

    Ini.WriteBool('Settings', 'Always on top',chkAlwaysOnTop.Checked);
    Ini.WriteBool('Settings', 'Allow more than one instance', chkInstance.Checked);
    Ini.WriteBool('Settings', 'Confirm when deleting a subtitle',chkConfirmDelete.Checked);
    Ini.WriteBool('Settings', 'Interpret invalid files as plain text',chkInterpretInvalid.Checked);
    Ini.WriteBool('Settings', 'Autosearch for movie',chkAutosearchMovie.Checked);
    Ini.WriteBool('Settings', 'Force working in time mode',chkForceWorkingTime.Checked);
    Ini.WriteBool('Settings', 'Keep order of lines when reverse text',chkKeepOrderOfLines.Checked);
    Ini.WriteBool('Settings', 'Select text on jump to next line',chkSelectTextNL.Checked);
    Ini.WriteBool('Settings', 'Select text on jump to previous line',chkSelectTextPL.Checked);
    Ini.WriteBool('Settings', 'No interaction with tags', chkNoInteractionWithTags.Checked);
    Ini.WriteBool('Settings', 'Work with style tags',chkWorkWithStyleTags.Checked);
    Ini.WriteInteger('Settings', 'MaxRF', udRFLimit.Position); // Recent files limit...
    frmMain.ConfirmDelete     := chkConfirmDelete.Checked;
    frmMain.InvalidFPlainText := chkInterpretInvalid.Checked;
    frmMain.AutosearchMovie   := chkAutoSearchMovie.Checked;
    frmMain.ForceWorkWithTime := chkForceWorkingTime.Checked;
    frmMain.KeepOrderOfLines  := chkKeepOrderOfLines.Checked;
    frmMain.SelTextNL         := chkSelectTextNL.Checked;
    frmMain.SelTextPL         := chkSelectTextPL.Checked;
    frmMain.RFMaxCount        := udRFLimit.Position;
    frmMain.UpdateRFMenus;
//    SubtitleAPI.NoInteractionWithTags := chkNoInteractionWithTags.Checked;
    SubtitleAPI.WorkWithTags          := chkWorkWithStyleTags.Checked;

    // --------------------------------- //
    //             Advanced              //
    // --------------------------------- //
    Ini.WriteInteger('Advanced', 'Two lines if longer than', udTwoLinesIfLongerThan.Position);
    Ini.WriteBool('Advanced', 'Toggle breakpoint', chkToggleBreakPoint.Checked);
    Ini.WriteInteger('Advanced', 'Break line after', udBreakLineAfter.Position);
    Ini.WriteBool('Advanced', 'Smart line adjust automatically', chkSLAAutomatically.Checked);
    Ini.WriteInteger('Advanced', 'Maximum line length', udMaxLineLength.Position);
    Ini.WriteInteger('Advanced', 'Shift time', udShiftTime.Position);
    frmMain.TwoLinesIfLongerThan := udTwoLinesIfLongerThan.Position;
    frmMain.ToggleBreakPoint     := chkToggleBreakPoint.Checked;
    frmMain.BreakLineAfter       := udBreakLineAfter.Position;
    frmMain.MaxLineLength        := udMaxLineLength.Position;
    frmMain.ShiftTime            := udShiftTime.Position;

    // --------------------------------- //
    //              Charsets             //
    // --------------------------------- //
    Ini.WriteBool('Settings', 'Show charsets in main form', chkShowInMainForm.Checked);
    Ini.WriteInteger('General', 'Original charset', cmbOrgCharset.ItemIndex);
    Ini.WriteInteger('General', 'Translation charset', cmbTransCharset.ItemIndex);
    frmMain.cmbOrgCharset.Visible        := chkShowInMainForm.Checked;
    frmMain.cmbTransCharset.Visible      := chkShowInMainForm.Checked;
    frmMain.cmbOrgCharset.ItemIndex      := cmbOrgCharset.ItemIndex;
    frmMain.cmbTransCharset.ItemIndex    := cmbTransCharset.ItemIndex;
    frmMain.OrgCharset                   := StrCharsetToInt(cmbOrgCharset.Items[cmbOrgCharset.ItemIndex]);
    frmMain.TransCharset                 := StrCharsetToInt(cmbTransCharset.Items[cmbTransCharset.ItemIndex]);
    frmMain.mmoSubtitleText.Font.Charset := frmMain.OrgCharset;
    frmMain.mmoTranslation.Font.Charset  := frmMain.TransCharset;
    if chkShowInMainForm.Checked then
      frmMain.cmbOCRScripts.Top := 288 else
      frmMain.cmbOCRScripts.Top := frmMain.cmbOrgCharset.Top;

    // --------------------------------- //
    //               Formats             //
    // --------------------------------- //
    Ini.WriteString('Formats','Default format', SubtitleAPI.GetFormatName(cmbDefaultFormat.ItemIndex + 1));
    for i := 0 to chkLstFormatsToShow.Items.Count-1 do
      Ini.WriteBool('Formats to show', SubtitleAPI.GetFormatName(i+1), chkLstFormatsToShow.Checked[i]);
    Ini.WriteBool('Formats','Show custom formats', chkShowCustomFormats.Checked);

    // --------------------------------- //
    //             File types            //
    // --------------------------------- //
    if ModifyExts then
    begin
      Ini.WriteBool('File types', 'Register extensions on start', chkRegExtOnStart.Checked);
      Ini.WriteBool('File types', 'Associate extensions', chkAssociateExtensions.Checked);
      for i := 0 to chkLstExtensions.Items.Count-1 do
        if chkLstExtensions.Checked[i] then
          ExtStr := ExtStr + chkLstExtensions.Items[i] + ';';
      Delete(ExtStr, Length(ExtStr), 1);
      Ini.WriteString('File types', 'Associated extensions', ExtStr);
      AssociateExtensions(ExtStr);
      ExtStr := '';
      for i := 0 to chkLstExtensions.Items.Count-1 do
        if chkLstExtensions.Checked[i] = False then
          ExtStr := ExtStr + chkLstExtensions.Items[i] + ';';
      Delete(ExtStr, Length(ExtStr), 1);
      AssociateExtensions(ExtStr, False);
    end;

    // --------------------------------- //
    //                Save               //
    // --------------------------------- //
    Ini.WriteBool('Save', 'Ask to save', chkAskToSave.Checked);
    Ini.WriteBool('Save', 'Save work automatically', chkSaveAutomatically.Checked);
    Ini.WriteInteger('Save', 'Time interval', updMins.Position * 60000);
    Ini.WriteBool('Save', 'Save as backup', chkSaveAsBackup.Checked);
    frmMain.AskToSave            := chkAskToSave.Checked;
    frmMain.tmrSaveWork.Enabled  := chkSaveAutomatically.Checked;
    frmMain.tmrSaveWork.Interval := updMins.Position * 60000;
    frmMain.SaveAsBackup         := chkSaveAsBackup.Checked;

    // --------------------------------- //
    //           Video preview           //
    // --------------------------------- //
    Ini.WriteInteger('Video preview', 'Double click in a subtitle', cmbDoubleClickInSub.ItemIndex);
    Ini.WriteInteger('Video preview', 'Shift-double click in a subtitle', cmbShiftDoubleClickInSub.ItemIndex);
    Ini.WriteInteger('Video preview', 'Seconds to jump 1', udSecsToJump1.Position);
    Ini.WriteInteger('Video preview', 'Seconds to jump 2', udSecsToJump2.Position);
    Ini.WriteString('Video preview', 'Rewind and forward', edtRewindAndForwardTime.Text);
    Ini.WriteInteger('Video preview', 'Default altered playback rate', cmbDefaultAltPlayRate.ItemIndex);
    frmMain.OnDoubleClick   := cmbDoubleClickInSub.ItemIndex;
    frmMain.OnShiftDblClick := cmbShiftDoubleClickInSub.ItemIndex;
    frmMain.SecsToJump1     := udSecsToJump1.Position;
    frmMain.SecsToJump2     := udSecsToJump2.Position;
    frmMain.RewFFTime       := StrSecToMS(edtRewindAndForwardTime.Text);
    frmMain.DefAltPlayRate  := cmbDefaultAltPlayRate.ItemIndex + 1;
    SetDefaultShortCut;

    // --------------------------------- //
    //       Video preview/Subtitles     //
    // --------------------------------- //
    Ini.WriteBool('Video preview subtitles','Draw border', chkDrawBorder.Checked);
    Ini.WriteBool('Video preview subtitles','Draw shadow', chkDrawShadow.Checked);
    Ini.WriteBool('Video preview subtitles','Transparent', chkTransparent.Checked);
    Ini.WriteBool('Video preview subtitles', 'Force using regions', chkForceUsingRegions.Checked);
    Ini.WriteString('Video preview subtitles', 'Font name', subSample.Font.Name);
    Ini.WriteInteger('Video preview subtitles', 'Font size', subSample.Font.Size);
    Ini.WriteInteger('Video preview subtitles', 'Font color', subSample.TextColor);
    Ini.WriteInteger('Video preview subtitles', 'Background color', subSample.BackgroundColor);
    Ini.WriteInteger('Video preview subtitles', 'Border width', udBorderWidth.Position);
    Ini.WriteInteger('Video preview subtitles', 'Shadow width', udShadowWidth.Position);
    Ini.WriteBool('Video preview subtitles', 'Bold', fsBold in subSample.Font.Style);
    Ini.WriteBool('Video preview subtitles', 'Italic', fsItalic in subSample.Font.Style);
    Ini.WriteBool('Video preview subtitles', 'Underline', fsUnderline in subSample.Font.Style);
    frmMain.subSubtitle.Border    := chkDrawBorder.Checked;
    frmMain.subSubtitle.Shadow    := chkDrawShadow.Checked;
    frmMain.TransparentSubs       := chkTransparent.Checked;
    frmMain.ForceUsingReg         := chkForceUsingRegions.Checked;
    frmMain.subSubtitle.ForceTransparency := chkForceUsingRegions.Checked;
    frmMain.subSubtitle.Font      := subSample.Font;
    frmMain.subSubtitle.TextColor := subSample.TextColor;
    if chkTransparent.Checked then
      frmMain.subSubtitle.BackgroundColor := GetColorKeyFUNC else
      frmMain.subSubtitle.BackgroundColor := subSample.BackgroundColor;
    frmMain.subSubtitle.BorderWidth := udBorderWidth.Position;
    frmMain.subSubtitle.ShadowWidth := udShadowWidth.Position;
    UpdateSubtitlesPos;

    // --------------------------------- //
    //      External preview/General     //
    // --------------------------------- //
    Ini.WriteString('External Preview', 'Video player', edtVidPlayer.Text);
    Ini.WriteBool('External Preview', 'Ask for different video', rdoAskForDifferentVideo.Checked);
    Ini.WriteString('External Preview', 'Video to test', edtAVIFile.Text);

    // --------------------------------- //
    //     External preview/Advanced     //
    // --------------------------------- //
    Ini.WriteBool('External Preview', 'Save in original format', rdoOriginalFormat.Checked);
    Ini.WriteString('External Preview', 'Custom format', SubtitleAPI.GetFormatName(cmbFormats.ItemIndex + 1));
    Ini.WriteString('External Preview', 'Parameter', edtParameter.Text);

    // --------------------------------- //
    //           Look / Program          //
    // --------------------------------- //
    Ini.WriteString('Program look', 'Font', cmbFonts.Items[cmbFonts.ItemIndex]);
    Ini.WriteInteger('Program look', 'Font size', udFontSize.Position);
    with frmMain do
    begin
      frmMain.Font.Name := cmbFonts.Items[cmbFonts.ItemIndex];
      frmMain.Font.Size := udFontSize.Position;
      MiMenu.Fuente.Name := Font.Name;
      MiMenu.Fuente.Size := Font.Size;
      lstSubtitles.ParentFont    := True;
      tmeShow.ParentFont         := True;
      tmeHide.ParentFont         := True;
      tmeDuration.ParentFont     := True;
      mmoSubtitleText.ParentFont := True;
      mmoTranslation.ParentFont  := True;
      tmeShow.Font.Style         := Font.Style + [fsBold];
      tmeHide.Font.Style         := Font.Style + [fsBold];
      tmeDuration.Font.Style     := Font.Style + [fsBold];
      mmoSubtitleText.Font.Style := Font.Style + [fsBold];
      mmoTranslation.Font.Style  := Font.Style + [fsBold];
      mmoSubtitleText.Font.Size  := Font.Size + 2;
      mmoTranslation.Font.Size   := Font.Size + 2;
    end;
    Ini.WriteInteger('Program look', '"Text" and "Translation" fields align', cmbTextAlign.ItemIndex);
    case cmbTextAlign.ItemIndex of
      0: begin frmMain.mmoSubtitleText.Alignment := taLeftJustify; frmMain.mmoTranslation.Alignment := taLeftJustify; end;
      1: begin frmMain.mmoSubtitleText.Alignment := taRightJustify; frmMain.mmoTranslation.Alignment := taRightJustify; end else
      begin frmMain.mmoSubtitleText.Alignment := Classes.taCenter; frmMain.mmoTranslation.Alignment := Classes.taCenter; end;
    end;
    frmMain.mmoSubtitleText.Font.Charset := frmMain.OrgCharset;
    frmMain.mmoTranslation.Font.Charset  := frmMain.TransCharset;

    // --------------------------------- //
    //             Look / List           //
    // --------------------------------- //
    Ini.WriteBool('List look', 'Show grid lines', chkShowGridLines.Checked);
    Ini.WriteBool('List look', 'Apply style to subtitles', chkApplyStyle.Checked);
    Ini.WriteBool('List look', 'Mark untranslated subtitles', chkMarkUnTransSubs.Checked);
    Ini.WriteInteger('List look', 'Untranslated subtitles color', pnlUnTransColor.Color);
    Ini.WriteBool('List look', 'Show horizontal scrollbar', chkShowHorzScrollBar.Checked);
    if chkShowGridLines.Checked then
      frmMain.lstSubtitles.TreeOptions.PaintOptions := [toShowHorzGridLines, toShowVertGridLines, toShowButtons, toShowDropmark, toShowTreeLines,toThemeAware,toUseBlendedImages] else
      frmMain.lstSubtitles.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowTreeLines,toThemeAware,toUseBlendedImages];
    frmMain.MarkUntransSubs  := chkMarkUntransSubs.Checked;
    frmMain.ApplyStyleInList := chkApplyStyle.Checked;
    frmMain.UnTransSubsColor := pnlUnTransColor.Color;
    if chkShowHorzScrollBar.Checked = False then
      frmMain.lstSubtitles.ScrollBarOptions.ScrollBars := ssVertical else
      frmMain.lstSubtitles.ScrollBarOptions.ScrollBars := ssBoth;
    frmMain.lstSubtitles.Refresh;

    // --------------------------------- //
    //             Look / Menu           //
    // --------------------------------- //
    Ini.WriteBool('Menu look', 'Use Office XP Style menu', chkUseOfficeXPStyleMenu.Checked);
    Ini.WriteBool('Menu look', 'Use gradient menu', chkUseGradientMenu.Checked);
    frmMain.MiMenu.Activo    := chkUseOfficeXPStyleMenu.Checked;
    frmMain.MiMenu.Degradado := chkUseGradientMenu.Checked;

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkDrawBorderClick(Sender: TObject);
begin
  subSample.Border := chkDrawBorder.Checked;
  UpdateSubSamplePos;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkDrawShadowClick(Sender: TObject);
begin
  subSample.Shadow := chkDrawShadow.Checked;
  UpdateSubSamplePos;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSubFontClick(Sender: TObject);
begin
  dlgSetFont.Font := subSample.Font;
  if (dlgSetFont.Execute) then
  begin
    subSample.Font := dlgSetFont.Font;
    UpdateSubSamplePos;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSubColorClick(Sender: TObject);
begin
  dlgSetColor.Color := subSample.TextColor;
  if (dlgSetColor.Execute) then
    subSample.TextColor := dlgSetColor.Color;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnBackgroundClick(Sender: TObject);
begin
  dlgSetColor.Color := subSample.BackgroundColor;
  if (dlgSetColor.Execute) then
  begin
    subSample.BackgroundColor := dlgSetColor.Color;
    pnlSubSample.Color        := dlgSetColor.Color;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.edtBorderWidthChange(Sender: TObject);
begin
  subSample.BorderWidth := udBorderWidth.Position;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.edtShadowWidthChange(Sender: TObject);
begin
  subSample.ShadowWidth := udShadowWidth.Position;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnBrowseClick(Sender: TObject);
begin
  dlgBrowse.Filter := 'Exe (*.exe)|*.exe';
  if (dlgBrowse.Execute) and (dlgBrowse.FileName <> '') then
    edtVidPlayer.Text := dlgBrowse.FileName;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnDetectClick(Sender: TObject);
var
  S: String;
begin
  S := AnsiLowerCase(GetProgramAssociation('avi'));
  edtVidPlayer.Text := Copy(S, 1, LastDelimiter('.', S) + 3);
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.rdoAskForDifferentVideoClick(Sender: TObject);
begin
  edtAVIFile.Enabled := False;
  btnBrowse2.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.rdoTestWithVideoClick(Sender: TObject);
begin
  edtAVIFile.Enabled := True;
  btnBrowse2.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnBrowse2Click(Sender: TObject);
begin
  dlgBrowse.Filter := AllSupportedFiles + '|*.asf;*.avi;*.mp4;*.divx;*.mpg;*.mpeg;*.m1v;*.qt;*.wmv|ASF (*.asf)|*.asf|AVI (*.avi)|*.avi|DivX (*.mp4; *.divx)|*.mp4; *.divx|MPEG (*.mpg; *.mpeg; *.m1v)|*.mpg; *.mpeg; *.m1v|QuickTime 2.0 (*.qt)|*.qt|WMV (*.wmv)|*.wmv';
  if (dlgBrowse.Execute) and (dlgBrowse.FileName <> '') then
    edtAviFile.Text := dlgBrowse.FileName;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.rdoOriginalFormatClick(Sender: TObject);
begin
  cmbFormats.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.rdoCustomFormatClick(Sender: TObject);
begin
  cmbFormats.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkRegExtOnStartMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ModifyExts := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkRegExtOnStartKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ModifyExts := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chklstExtensionsClickCheck(Sender: TObject);
begin
  ModifyExts := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.pnlUnTransColorClick(Sender: TObject);
begin
  if (dlgSetColor.Execute) then
    pnlUnTransColor.Color := dlgSetColor.Color;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkUseOfficeXPStyleMenuClick(Sender: TObject);
begin
  chkUseGradientMenu.Enabled := chkUseOfficeXPStyleMenu.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSelectAllFormatClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to chkLstFormatsToShow.Items.Count-1 do
    if not chkLstFormatsToShow.Checked[i] then
      chkLstFormatsToShow.Checked[i] := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.btnSelectZeroFormatClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to chkLstFormatsToShow.Items.Count-1 do
    if chkLstFormatsToShow.Checked[i] then
      chkLstFormatsToShow.Checked[i] := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmSettings.chkTransparentClick(Sender: TObject);
begin
  chkForceUsingRegions.Enabled := chkTransparent.Checked;
end;

// -----------------------------------------------------------------------------

end.
