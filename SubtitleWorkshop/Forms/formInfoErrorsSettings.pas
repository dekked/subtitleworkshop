unit formInfoErrorsSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CheckLst, ExtCtrls, IniFiles, General,
  InfoErrorsFunctions, ShellAPI;

type
  TfrmInfoErrorsSettings = class(TForm)
    pgeCtrl: TPageControl;
    pgeCheckFor: TTabSheet;
    pgeFix: TTabSheet;
    chkCheckEmptySubtitles: TCheckBox;
    chkCheckOverlapping: TCheckBox;
    chkCheckBadValues: TCheckBox;
    chkCheckHearingImpaired: TCheckBox;
    chkCheckTextBeforeColon: TCheckBox;
    chkCheckOnlyIfCapitalLetters: TCheckBox;
    chkCheckUnnecessaryDots: TCheckBox;
    chkCheckOverTwoLines: TCheckBox;
    chkCheckProhibitedChars: TCheckBox;
    chkCheckRepeatedChars: TCheckBox;
    chkCheckOCRErrors: TCheckBox;
    chkCheckRepeatedSubs: TCheckBox;
    chkCheckUnnecessarySpaces: TCheckBox;
    pgeUnnecessarySpaces: TTabSheet;
    lstSpacesToCheck: TCheckListBox;
    lblCheckFor: TLabel;
    lstSpacesToFix: TCheckListBox;
    lblFix: TLabel;
    chkFixEmptySubtitles: TCheckBox;
    chkFixOverlapping: TCheckBox;
    chkFixBadValues: TCheckBox;
    chkFixHearingImpaired: TCheckBox;
    chkFixTextBeforeColon: TCheckBox;
    chkFixOnlyIfCapitalLetters: TCheckBox;
    chkFixUnnecessaryDots: TCheckBox;
    chkFixOverTwoLines: TCheckBox;
    chkFixProhibitedChars: TCheckBox;
    chkFixRepeatedChars: TCheckBox;
    chkFixRepeatedSubs: TCheckBox;
    chkFixOCRErrors: TCheckBox;
    chkFixUnnecessarySpaces: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    pgeGeneral: TTabSheet;
    chkMarkErrorsInList: TCheckBox;
    btnSetColor: TButton;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    dlgSetColor: TColorDialog;
    chkMarkOnLoad: TCheckBox;
    chkFixOnLoad: TCheckBox;
    dlgLoad: TOpenDialog;
    btnEdit: TButton;
    pgeAdvanced: TTabSheet;
    edtRepeatableChars: TLabeledEdit;
    edtProhibitedChars: TLabeledEdit;
    edtToleranceRepeatedSubs: TLabeledEdit;
    udToleranceRepeatedSubs: TUpDown;
    lblMilliseconds: TLabel;
    edtSpaceAfterChars: TLabeledEdit;
    edtSpaceBeforeChars: TLabeledEdit;
    chkCheckSpaceAfterCustomChars: TCheckBox;
    chkCheckSpaceBeforeCustomChars: TCheckBox;
    chkFixSpaceAfterCustomChars: TCheckBox;
    chkFixSpaceBeforeCustomChars: TCheckBox;
    edtTooLongDuration: TLabeledEdit;
    udTooLongDur: TUpDown;
    edtTooShortDuration: TLabeledEdit;
    udTooShortDur: TUpDown;
    lblMilliseconds2: TLabel;
    lblMilliseconds3: TLabel;
    chkCheckTooLongDur: TCheckBox;
    chkCheckTooShortDur: TCheckBox;
    edtTooLongLine: TLabeledEdit;
    udTooLongLine: TUpDown;
    lblCharacters: TLabel;
    chkCheckTooLongLines: TCheckBox;
    chkFixOneUnitOverlap: TCheckBox;
    lblOCRDefFile: TLabel;
    cmbOCRFiles: TComboBox;
    chkShowConfInMainForm: TCheckBox;
    bvlSep1: TBevel;
    chkCheckLinesWithoutLetters: TCheckBox;
    bvlSep2: TBevel;
    bvlSep3: TBevel;
    chkFixLinesWithoutLetters: TCheckBox;
    bvlSep4: TBevel;
    bvlSep5: TBevel;
    chkCheckOpnDlgInSubsWithOneLine: TCheckBox;
    chkFixOpnDlgInSubsWithOneLine: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure btnSetColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkCheckTextBeforeColonClick(Sender: TObject);
    procedure chkFixTextBeforeColonClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmInfoErrorsSettings: TfrmInfoErrorsSettings;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                      := ReadString('Information and errors Settings', '01', 'Settings');
      pgeGeneral.Caption           := ReadString('Information and errors Settings', '02', 'General');
      pgeAdvanced.Caption          := ReadString('Information and errors Settings', '03', 'Advanced');
      pgeCheckFor.Caption          := ReadString('Information and errors Settings', '04', 'Check for');
      pgeFix.Caption               := ReadString('Information and errors Settings', '05', 'Fix');
      pgeUnnecessarySpaces.Caption := ReadString('Information and errors Settings', '06', 'Unnecessary spaces');

      // ---------------------- //
      //      General page      //
      // ---------------------- //
      chkShowConfInMainForm.Caption := ReadString('Information and errors Settings', '07', 'Show confirmations in main form on fix');
      chkMarkErrorsInList.Caption   := ReadString('Information and errors Settings', '08', 'Mark errors in main form''s list');
      btnSetColor.Caption           := ReadString('Information and errors Settings', '09', 'Set color...');
      chkBold.Caption               := ReadString('Information and errors Settings', '10', 'Bold');
      chkItalic.Caption             := ReadString('Information and errors Settings', '11', 'Italic');
      chkUnderline.Caption          := ReadString('Information and errors Settings', '12', 'Underline');
      chkMarkOnLoad.Caption         := ReadString('Information and errors Settings', '13', 'Mark errors on load subtitle');
      chkFixOnLoad.Caption          := ReadString('Information and errors Settings', '14', 'Fix errors on load subtitle');
      chkFixOneUnitOverlap.Caption  := ReadString('Information and errors Settings', '15', 'Fix one unit overlap at load');
      lblOCRDefFile.Caption         := ReadString('Information and errors Settings', '16', 'OCR Script:');
      btnEdit.Caption               := ReadString('Information and errors Settings', '17', '&Edit');

      // ---------------------- //
      //      Advanced page     //
      // ---------------------- //
      edtRepeatableChars.EditLabel.Caption       := ReadString('Information and errors Settings', '18', 'Repeatable characters:');
      edtProhibitedChars.EditLabel.Caption       := ReadString('Information and errors Settings', '19', 'Prohibited characters:');
      edtToleranceRepeatedSubs.EditLabel.Caption := ReadString('Information and errors Settings', '20', 'Tolerance for repeated subtitles:');
      lblMilliseconds.Caption                    := ReadString('Information and errors Settings', '21', 'milliseconds.');
      edtSpaceAfterChars.EditLabel.Caption       := ReadString('Information and errors Settings', '22', 'Space after characters:');
      edtSpaceBeforeChars.EditLabel.Caption      := ReadString('Information and errors Settings', '23', 'Space before characters:');
      edtTooLongDuration.EditLabel.Caption       := ReadString('Information and errors Settings', '24', 'Too long duration:');
      edtTooShortDuration.EditLabel.Caption      := ReadString('Information and errors Settings', '25', 'Too short duration:');
      edtTooLongLine.EditLabel.Caption           := ReadString('Information and errors Settings', '26', 'Too long line:');
      lblCharacters.Caption                      := ReadString('Information and errors Settings', '27', 'characters.');
      lblMilliseconds2.Caption                   := lblMilliseconds.Caption;
      lblMilliseconds3.Caption                   := lblMilliseconds.Caption;

      // ---------------------- //
      //        Check for       //
      // ---------------------- //
      chkCheckLinesWithoutLetters.Caption     := ReadString('Information and errors Settings', '28', 'Lines without letters');
      chkCheckEmptySubtitles.Caption          := ReadString('Information and errors Settings', '29', 'Empty subtitles');
      // ---
      chkCheckOverlapping.Caption             := ReadString('Information and errors Settings', '30', 'Overlapping subtitles');
      chkCheckBadValues.Caption               := ReadString('Information and errors Settings', '31', 'Bad values');
      chkCheckTooLongDur.Caption              := ReadString('Information and errors Settings', '32', 'Too long durations');
      chkCheckTooShortDur.Caption             := ReadString('Information and errors Settings', '33', 'Too short durations');
      chkCheckTooLongLines.Caption            := ReadString('Information and errors Settings', '34', 'Too long lines');
      chkCheckOverTwoLines.Caption            := ReadString('Information and errors Settings', '35', 'Subtitles over two lines');
      // ---
      chkCheckHearingImpaired.Caption         := ReadString('Information and errors Settings', '36', 'Hearing impaired subtitles');
      chkCheckTextBeforeColon.Caption         := ReadString('Information and errors Settings', '37', 'Text before colon (":")');
      chkCheckOnlyIfCapitalLetters.Caption    := ReadString('Information and errors Settings', '38', 'Only if text is in capital letters');
      chkCheckUnnecessaryDots.Caption         := ReadString('Information and errors Settings', '39', 'Unnecessary dots');
      chkCheckProhibitedChars.Caption         := ReadString('Information and errors Settings', '40', 'Prohibited characters');
      chkCheckRepeatedChars.Caption           := ReadString('Information and errors Settings', '41', 'Repeated characters');
      chkCheckRepeatedSubs.Caption            := ReadString('Information and errors Settings', '42', 'Repeated subtitles');
      chkCheckOCRErrors.Caption               := ReadString('Information and errors Settings', '43', 'OCR Errors');
      // ---
      chkCheckOpnDlgInSubsWithOneLine.Caption := ReadString('Information and errors Settings', '44', '"- " in subtitles with one line');
      chkCheckSpaceAfterCustomChars.Caption   := ReadString('Information and errors Settings', '45', 'Spaces after custom characters');
      chkCheckSpaceBeforeCustomChars.Caption  := ReadString('Information and errors Settings', '46', 'Spaces before custom characters');
      chkCheckUnnecessarySpaces.Caption       := ReadString('Information and errors Settings', '47', 'Unnecessary spaces');


      // ---------------------- //
      //           Fix          //
      // ---------------------- //
      chkFixLinesWithoutLetters.Caption     := chkCheckLinesWithoutLetters.Caption;
      chkFixEmptySubtitles.Caption          := chkCheckEmptySubtitles.Caption;
      // ---
      chkFixOverlapping.Caption             := chkCheckOverlapping.Caption;
      chkFixBadValues.Caption               := chkCheckBadValues.Caption;
      chkFixOverTwoLines.Caption            := chkCheckOverTwoLines.Caption;
      // ---
      chkFixHearingImpaired.Caption         := chkCheckHearingImpaired.Caption;
      chkFixTextBeforeColon.Caption         := chkCheckTextBeforeColon.Caption;
      chkFixOnlyIfCapitalLetters.Caption    := chkCheckOnlyIfCapitalLetters.Caption;
      chkFixUnnecessaryDots.Caption         := chkCheckUnnecessaryDots.Caption;
      chkFixProhibitedChars.Caption         := chkCheckProhibitedChars.Caption;
      chkFixRepeatedChars.Caption           := chkCheckRepeatedChars.Caption;
      chkFixRepeatedSubs.Caption            := chkCheckRepeatedSubs.Caption;
      chkFixOCRErrors.Caption               := chkCheckOCRErrors.Caption;
      // ---
      chkFixOpnDlgInSubsWithOneLine.Caption := chkCheckOpnDlgInSubsWithOneLine.Caption;
      chkFixSpaceAfterCustomChars.Caption   := chkCheckSpaceAfterCustomChars.Caption;
      chkFixSpaceBeforeCustomChars.Caption  := chkCheckSpaceBeforeCustomChars.Caption;
      chkFixUnnecessarySpaces.Caption       := chkCheckUnnecessarySpaces.Caption;

      // ---------------------- //
      //   Unnecessary spaces   //
      // ---------------------- //
      lblCheckFor.Caption := ReadString('Information and errors Settings', '48', 'Check for:');
      lblFix.Caption      := ReadString('Information and errors Settings', '49', 'Fix:');
      lstSpacesToCheck.Clear;
      lstSpacesToFix.Clear;
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '50', 'Enters and spaces at the beginning and end'));
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '51', 'Spaces between enters (left and right)'));
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '52', 'Double spaces and enters'));
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '53', 'Spaces in front of punctuation marks'));
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '54', 'Space after "¿" and "¡"'));
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '55', 'Space before "?" and "!"'));
      lstSpacesToCheck.Items.Add(ReadString('Information and errors Settings', '56', 'Spaces between numbers'));
      lstSpacesToFix.Items := lstSpacesToCheck.Items;

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
  end
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.FormCreate(Sender: TObject);
var
  Ini : TIniFile;
  i   : Integer;
begin
  pgeCtrl.ActivePageIndex := 0;
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    if frmMain.mnuOCRScripts.Count > 0 then
    begin
      cmbOCRFiles.Clear;
      // Add OCR Scripts
      for i := 2 to frmMain.mnuOCRScripts.Count-1 do
      begin
        cmbOCRFiles.Items.Add(frmMain.mnuOCRScripts.Items[i].Caption);
        if frmMain.mnuOCRScripts.Items[i].Checked then
          cmbOCRFiles.ItemIndex := i;
      end;
      cmbOCRFiles.ItemIndex := frmMain.cmbOCRScripts.ItemIndex;
    end else
    begin
      lblOCRDefFile.Visible := False;
      cmbOCRFiles.Visible   := False;
      btnEdit.Visible       := False;
    end;
    if (cmbOCRFiles.Items.Count >= 1) and (cmbOCRFiles.ItemIndex = -1) then
      cmbOCRFiles.ItemIndex := 0;

    // ------------ //
    // General page //                                           
    // ------------ //
    chkShowConfInMainForm.Checked := Ini.ReadBool('Information and Errors', 'Show confirmations in main form on fix', False);
    chkMarkErrorsInList.Checked   := Ini.ReadBool('Information and Errors', 'Mark errors in main form''s list', True);
    dlgSetColor.Color             := Ini.ReadInteger('Information and Errors', 'Mark with color', clRed);
    chkBold.Checked               := Ini.ReadBool('Information and Errors', 'Bold', True);
    chkItalic.Checked             := Ini.ReadBool('Information and Errors', 'Italic', False);
    chkUnderline.Checked          := Ini.ReadBool('Information and Errors', 'Underline', False);
    chkMarkOnLoad.Checked         := Ini.ReadBool('Information and Errors', 'Mark errors on load subtitle', False);
    chkFixOnLoad.Checked          := Ini.ReadBool('Information and Errors', 'Fix errors on load subtitle', False);
    chkFixOneUnitOverlap.Checked  := Ini.ReadBool('Information and Errors', 'Fix one unit overlap at load', False);

    // ------------- //
    // Advanced page //
    // ------------- //
    edtRepeatableChars.Text          := Ini.ReadString('Information and Errors', 'Repeatable chars', '-¡!¿?";\/_[]=');
    edtProhibitedChars.Text          := Ini.ReadString('Information and Errors', 'Prohibited chars', '@#*');
    udToleranceRepeatedSubs.Position := Ini.ReadInteger('Information and Errors', 'Tolerance for repeated subtitles', 100);
    edtSpaceAfterChars.Text          := Ini.ReadString('Information and Errors', 'Space after characters', '-');
    edtSpaceBeforeChars.Text         := Ini.ReadString('Information and Errors', 'Space before characters', '');
    udTooLongDur.Position            := Ini.ReadInteger('Information and Errors', 'Too long duration', 6000);
    udTooShortDur.Position           := Ini.ReadInteger('Information and Errors', 'Too short duration', 700);
    udTooLongLine.Position           := Ini.ReadInteger('Information and Errors', 'Too long line', 45);

    // ------------------- //
    // Errors to check for //
    // ------------------- //
    chkCheckLinesWithoutLetters.Checked     := Ini.ReadBool('Errors to check for', 'Lines without letters',              True);
    chkCheckEmptySubtitles.Checked          := Ini.ReadBool('Errors to check for', 'Empty subtitles',                    True);
    chkCheckOverlapping.Checked             := Ini.ReadBool('Errors to check for', 'Overlapping subtitles',              True);
    chkCheckBadValues.Checked               := Ini.ReadBool('Errors to check for', 'Bad values',                         True);
    chkCheckTooLongDur.Checked              := Ini.ReadBool('Errors to check for', 'Too long durations',                 True);
    chkCheckTooShortDur.Checked             := Ini.ReadBool('Errors to check for', 'Too short durations',                True);
    chkCheckTooLongLines.Checked            := Ini.ReadBool('Errors to check for', 'Too long lines',                     True);
    chkCheckOverTwoLines.Checked            := Ini.ReadBool('Errors to check for', 'Subtitles over two lines',           True);
    chkCheckHearingImpaired.Checked         := Ini.ReadBool('Errors to check for', 'Hearing impaired subtitles',         True);
    chkCheckTextBeforeColon.Checked         := Ini.ReadBool('Errors to check for', 'Text before colon (":")',            True);
    chkCheckOnlyIfCapitalLetters.Checked    := Ini.ReadBool('Errors to check for', 'Only if text is in capital letters', True);
    chkCheckUnnecessaryDots.Checked         := Ini.ReadBool('Errors to check for', 'Unnecessary dots',                   True);
    chkCheckProhibitedChars.Checked         := Ini.ReadBool('Errors to check for', 'Prohibited characters',              False);
    chkCheckRepeatedChars.Checked           := Ini.ReadBool('Errors to check for', 'Repeated characters',                True);
    chkCheckRepeatedSubs.Checked            := Ini.ReadBool('Errors to check for', 'Repeated subtitles',                 True);
    chkCheckOCRErrors.Checked               := Ini.ReadBool('Errors to check for', 'OCR Errors',                         True);
    chkCheckOpnDlgInSubsWithOneLine.Checked := Ini.ReadBool('Errors to check for', '"- " in subtitles with one line',    True);
    chkCheckSpaceAfterCustomChars.Checked   := Ini.ReadBool('Errors to check for', 'Space after custom characters',      True);
    chkCheckSpaceBeforeCustomChars.Checked  := Ini.ReadBool('Errors to check for', 'Space before custom characters',     False);
    chkCheckUnnecessarySpaces.Checked       := Ini.ReadBool('Errors to check for', 'Unnecessary spaces',                 True);

    // ------------- //
    // Errors to fix //
    // ------------- //
    chkFixLinesWithoutLetters.Checked     := Ini.ReadBool('Errors to fix', 'Lines without letters',              True);
    chkFixEmptySubtitles.Checked          := Ini.ReadBool('Errors to fix', 'Empty subtitles',                    True);
    chkFixOverlapping.Checked             := Ini.ReadBool('Errors to fix', 'Overlapping subtitles',              True);
    chkFixBadValues.Checked               := Ini.ReadBool('Errors to fix', 'Bad values',                         True);
    chkFixOverTwoLines.Checked            := Ini.ReadBool('Errors to fix', 'Subtitles over two lines',           True);
    chkFixHearingImpaired.Checked         := Ini.ReadBool('Errors to fix', 'Hearing impaired subtitles',         True);
    chkFixTextBeforeColon.Checked         := Ini.ReadBool('Errors to fix', 'Text before colon (":")',            True);
    chkFixOnlyIfCapitalLetters.Checked    := Ini.ReadBool('Errors to fix', 'Only if text is in capital letters', True);
    chkFixUnnecessaryDots.Checked         := Ini.ReadBool('Errors to fix', 'Unnecessary dots',                   True);
    chkFixProhibitedChars.Checked         := Ini.ReadBool('Errors to fix', 'Prohibited characters',              False);
    chkFixRepeatedChars.Checked           := Ini.ReadBool('Errors to fix', 'Repeated characters',                True);
    chkFixRepeatedSubs.Checked            := Ini.ReadBool('Errors to fix', 'Repeated subtitles',                 True);
    chkFixOCRErrors.Checked               := Ini.ReadBool('Errors to fix', 'OCR Errors',                         True);
    chkFixOpnDlgInSubsWithOneLine.Checked := Ini.ReadBool('Errors to fix', '"-" in subtitles with one line',     False);
    chkFixSpaceAfterCustomChars.Checked   := Ini.ReadBool('Errors to fix', 'Space after custom characters',      True);
    chkFixSpaceBeforeCustomChars.Checked  := Ini.ReadBool('Errors to fix', 'Space before custom characters',     False);
    chkFixUnnecessarySpaces.Checked       := Ini.ReadBool('Errors to fix', 'Unnecessary spaces',                 True);

    // --------------------------- //
    // Unnecessary spaces to check //
    // --------------------------- //
    lstSpacesToCheck.Checked[0] := Ini.ReadBool('Unnecessary spaces to check for', 'Enters and spaces at the beginning and end', True);
    lstSpacesToCheck.Checked[1] := Ini.ReadBool('Unnecessary spaces to check for', 'Spaces between enters (left and right)',     True);
    lstSpacesToCheck.Checked[2] := Ini.ReadBool('Unnecessary spaces to check for', 'Double spaces and enters',                   True);
    lstSpacesToCheck.Checked[3] := Ini.ReadBool('Unnecessary spaces to check for', 'Spaces in front of punctuation marks',       True);
    lstSpacesToCheck.Checked[4] := Ini.ReadBool('Unnecessary spaces to check for', 'Spaces after "¿" and "¡"',                   True);
    lstSpacesToCheck.Checked[5] := Ini.ReadBool('Unnecessary spaces to check for', 'Spaces before "?" and  "!"',                 True);
    lstSpacesToCheck.Checked[6] := Ini.ReadBool('Unnecessary spaces to check for', 'Spaces between numbers',                     True);

    // ------------------------- //
    // Unnecessary spaces to fix //
    // ------------------------- //
    lstSpacesToFix.Checked[0] := Ini.ReadBool('Unnecessary spaces to fix', 'Enters and spaces at the beginning and end', True);
    lstSpacesToFix.Checked[1] := Ini.ReadBool('Unnecessary spaces to fix', 'Spaces between enters (left and right)',     True);
    lstSpacesToFix.Checked[2] := Ini.ReadBool('Unnecessary spaces to fix', 'Double spaces and enters',                   True);
    lstSpacesToFix.Checked[3] := Ini.ReadBool('Unnecessary spaces to fix', 'Spaces in front of punctuation marks',       True);
    lstSpacesToFix.Checked[4] := Ini.ReadBool('Unnecessary spaces to fix', 'Spaces after "¿" and "¡"',                   True);
    lstSpacesToFix.Checked[5] := Ini.ReadBool('Unnecessary spaces to fix', 'Spaces before "?" and  "!"',                 True);
    lstSpacesToFix.Checked[6] := Ini.ReadBool('Unnecessary spaces to fix', 'Spaces between numbers',                     True);

    chkCheckTextBeforeColonClick(Sender);
    chkFixTextBeforeColonClick(Sender);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.btnOkClick(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    // ------------ //
    // General page //
    // ------------ //
    Ini.WriteBool('Information and Errors', 'Show confirmations in main form on fix', chkShowConfInMainForm.Checked);
    Ini.WriteBool('Information and Errors', 'Mark errors in main form''s list', chkMarkErrorsInList.Checked);
    Ini.WriteInteger('Information and Errors', 'Mark with color', dlgSetColor.Color);
    Ini.WriteBool('Information and Errors', 'Bold', chkBold.Checked);
    Ini.WriteBool('Information and Errors', 'Italic', chkItalic.Checked);
    Ini.WriteBool('Information and Errors', 'Underline', chkUnderline.Checked);
    Ini.WriteBool('Information and Errors', 'Mark errors on load subtitle', chkMarkOnLoad.Checked);
    Ini.WriteBool('Information and Errors', 'Fix errors on load subtitle', chkFixOnLoad.Checked);
    Ini.WriteBool('Information and Errors', 'Fix one unit overlap at load', chkFixOneUnitOverlap.Checked);
    ShowConfMainForm  := chkShowConfInMainForm.Checked;
    MarkErrorsInList  := chkMarkErrorsInList.Checked;
    MarkWithColor     := dlgSetColor.Color;
    MarkBold          := chkBold.Checked;
    MarkItalic        := chkItalic.Checked;
    MarkUnderline     := chkUnderline.Checked;
    MarkOnLoad        := chkMarkOnLoad.Checked;
    FixOnLoad         := chkFixOnLoad.Checked;
    FixOneUnitOverlap := chkFixOneUnitOverlap.Checked;
    if cmbOCRFiles.Items.Count > 0 then
    begin
      OCRDefFile        := ExtractFilePath(Application.ExeName) + 'OCRScripts\' + cmbOCRFiles.Items[cmbOCRFiles.ItemIndex] + ID_OCREXT;
      Ini.WriteString('Information and Errors', 'OCR Definitions file', OCRDefFile);
      frmMain.mnuOCRScripts.Items[cmbOCRFiles.ItemIndex].Checked := True;
      frmMain.cmbOCRScripts.ItemIndex := cmbOCRFiles.ItemIndex;
    end;

    // ------------- //
    // Advanced page //
    // ------------- //
    Ini.WriteString('Information and Errors', 'Repeatable chars', edtRepeatableChars.Text);
    Ini.WriteString('Information and Errors', 'Prohibited chars', edtProhibitedChars.Text);
    Ini.WriteInteger('Information and Errors', 'Tolerance for repeated subtitles', udToleranceRepeatedSubs.Position);
    Ini.WriteString('Information and Errors', 'Space after characters', edtSpaceAfterChars.Text);
    Ini.WriteString('Information and Errors', 'Space before characters', edtSpaceBeforeChars.Text);
    Ini.WriteInteger('Information and Errors', 'Too long duration', udTooLongDur.Position);
    Ini.WriteInteger('Information and Errors', 'Too short duration', udTooShortDur.Position);
    Ini.WriteInteger('Information and Errors', 'Too long line', udTooLongLine.Position);
    RepeatableChars      := edtRepeatableChars.Text;
    ProhibitedChars      := edtProhibitedChars.Text;
    ToleranceForRepeated := udToleranceRepeatedSubs.Position;
    SpaceAfterChars      := edtSpaceAfterChars.Text;
    SpaceBeforeChars     := edtSpaceBeforeChars.Text;
    TooLongDuration      := udTooLongDur.Position;
    TooShortDuration     := udTooShortDur.Position;
    TooLongLine          := udTooLongLine.Position;

    // ------------------- //
    // Errors to check for //
    // ------------------- //
    Ini.WriteBool('Errors to check for', 'Lines without letters',              chkCheckLinesWithoutLetters.Checked);
    Ini.WriteBool('Errors to check for', 'Empty subtitles',                    chkCheckEmptySubtitles.Checked);
    Ini.WriteBool('Errors to check for', 'Overlapping subtitles',              chkCheckOverlapping.Checked);
    Ini.WriteBool('Errors to check for', 'Bad values',                         chkCheckBadValues.Checked);
    Ini.WriteBool('Errors to check for', 'Too long durations',                 chkCheckTooLongDur.Checked);
    Ini.WriteBool('Errors to check for', 'Too short durations',                chkCheckTooShortDur.Checked);
    Ini.WriteBool('Errors to check for', 'Too long lines',                     chkCheckTooLongLines.Checked);
    Ini.WriteBool('Errors to check for', 'Subtitles over two lines',           chkCheckOverTwoLines.Checked);
    Ini.WriteBool('Errors to check for', 'Hearing impaired subtitles',         chkCheckHearingImpaired.Checked);
    Ini.WriteBool('Errors to check for', 'Text before colon (":")',            chkCheckTextBeforeColon.Checked);
    Ini.WriteBool('Errors to check for', 'Only if text is in capital letters', chkCheckOnlyIfCapitalLetters.Checked);
    Ini.WriteBool('Errors to check for', 'Unnecessary dots',                   chkCheckUnnecessaryDots.Checked);
    Ini.WriteBool('Errors to check for', 'Prohibited characters',              chkCheckProhibitedChars.Checked);
    Ini.WriteBool('Errors to check for', 'Repeated characters',                chkCheckRepeatedChars.Checked);
    Ini.WriteBool('Errors to check for', 'Repeated subtitles',                 chkCheckRepeatedSubs.Checked);
    Ini.WriteBool('Errors to check for', 'OCR Errors',                         chkCheckOCRErrors.Checked);
    Ini.WriteBool('Errors to check for', '"- " in subtitles with one line',    chkCheckOpnDlgInSubsWithOneLine.Checked);
    Ini.WriteBool('Errors to check for', 'Space after custom characters',      chkCheckSpaceAfterCustomChars.Checked);
    Ini.WriteBool('Errors to check for', 'Space before custom characters',     chkCheckSpaceBeforeCustomChars.Checked);
    Ini.WriteBool('Errors to check for', 'Unnecessary spaces',                 chkCheckUnnecessarySpaces.Checked);

    ErrorsToCheck.eLinesWithoutLetters  := chkCheckLinesWithoutLetters.Checked;
    ErrorsToCheck.eEmptySubtitle        := chkCheckEmptySubtitles.Checked;
    ErrorsToCheck.eOverlapping          := chkCheckOverlapping.Checked;
    ErrorsToCheck.eBadValues            := chkCheckBadValues.Checked;
    ErrorsToCheck.eTooLongDurations     := chkCheckTooLongDur.Checked;
    ErrorsToCheck.eTooShortDurations    := chkCheckTooShortDur.Checked;
    ErrorsToCheck.eTooLongLines         := chkCheckTooLongLines.Checked;
    ErrorsToCheck.eOverTwoLines         := chkCheckOverTwoLines.Checked;
    ErrorsToCheck.eHearingImpaired      := chkCheckHearingImpaired.Checked;
    ErrorsToCheck.eTextBeforeColon      := chkCheckTextBeforeColon.Checked;
    ErrorsToCheck.eOnlyIfCapitalLetters := chkCheckOnlyIfCapitalLetters.Checked;
    ErrorsToCheck.eUnnecessaryDots      := chkCheckUnnecessaryDots.Checked;
    ErrorsToCheck.eProhibitedCharacter  := chkCheckProhibitedChars.Checked;
    ErrorsToCheck.eRepeatedCharacter    := chkCheckRepeatedChars.Checked;
    ErrorsToCheck.eRepeatedSubtitle     := chkCheckRepeatedSubs.Checked;
    ErrorsToCheck.eOCRErrors            := chkCheckOCRErrors.Checked;
    ErrorsToCheck.eOpnDlgSubsOneLine    := chkCheckOpnDlgInSubsWithOneLine.Checked;
    ErrorsToCheck.eSpaceAfterCustChars  := chkCheckSpaceAfterCustomChars.Checked;
    ErrorsToCheck.eSpaceBeforeCustChars := chkCheckSpaceBeforeCustomChars.Checked;
    ErrorsToCheck.eUnnecessarySpaces    := chkCheckUnnecessarySpaces.Checked;


    // ------------- //
    // Errors to fix //
    // ------------- //
    Ini.WriteBool('Errors to fix', 'Lines without letters',              chkFixLinesWithoutLetters.Checked);
    Ini.WriteBool('Errors to fix', 'Empty subtitles',                    chkFixEmptySubtitles.Checked);
    Ini.WriteBool('Errors to fix', 'Overlapping subtitles',              chkFixOverlapping.Checked);
    Ini.WriteBool('Errors to fix', 'Bad values',                         chkFixBadValues.Checked);
    Ini.WriteBool('Errors to fix', 'Subtitles over two lines',           chkFixOverTwoLines.Checked);
    Ini.WriteBool('Errors to fix', 'Hearing impaired subtitles',         chkFixHearingImpaired.Checked);
    Ini.WriteBool('Errors to fix', 'Text before colon (":")',            chkFixTextBeforeColon.Checked);
    Ini.WriteBool('Errors to fix', 'Only if text is in capital letters', chkFixOnlyIfCapitalLetters.Checked);
    Ini.WriteBool('Errors to fix', 'Unnecessary dots',                   chkFixUnnecessaryDots.Checked);
    Ini.WriteBool('Errors to fix', 'Prohibited characters',              chkFixProhibitedChars.Checked);
    Ini.WriteBool('Errors to fix', 'Repeated characters',                chkFixRepeatedChars.Checked);
    Ini.WriteBool('Errors to fix', 'Repeated subtitles',                 chkFixRepeatedSubs.Checked);
    Ini.WriteBool('Errors to fix', 'OCR Errors',                         chkFixOCRErrors.Checked);
    Ini.WriteBool('Errors to fix', '"-" in subtitles with one line',     chkFixOpnDlgInSubsWithOneLine.Checked);
    Ini.WriteBool('Errors to fix', 'Space after custom characters',      chkFixSpaceAfterCustomChars.Checked);
    Ini.WriteBool('Errors to fix', 'Space before custom characters',     chkFixSpaceBeforeCustomChars.Checked);
    Ini.WriteBool('Errors to fix', 'Unnecessary spaces',                 chkFixUnnecessarySpaces.Checked);

    ErrorsToFix.eLinesWithoutLetters  := chkFixLinesWithoutLetters.Checked;
    ErrorsToFix.eEmptySubtitle        := chkFixEmptySubtitles.Checked;
    ErrorsToFix.eOverlapping          := chkFixOverlapping.Checked;
    ErrorsToFix.eBadValues            := chkFixBadValues.Checked;
    ErrorsToFix.eOverTwoLines         := chkFixOverTwoLines.Checked;
    ErrorsToFix.eHearingImpaired      := chkFixHearingImpaired.Checked;
    ErrorsToFix.eTextBeforeColon      := chkFixTextBeforeColon.Checked;
    ErrorsToFix.eOnlyIfCapitalLetters := chkFixOnlyIfCapitalLetters.Checked;
    ErrorsToFix.eUnnecessaryDots      := chkFixUnnecessaryDots.Checked;
    ErrorsToFix.eProhibitedCharacter  := chkFixProhibitedChars.Checked;
    ErrorsToFix.eRepeatedCharacter    := chkFixRepeatedChars.Checked;
    ErrorsToFix.eRepeatedSubtitle     := chkFixRepeatedSubs.Checked;
    ErrorsToFix.eOCRErrors            := chkFixOCRErrors.Checked;
    ErrorsToFix.eOpnDlgSubsOneLine    := chkFixOpnDlgInSubsWithOneLine.Checked;
    ErrorsToFix.eSpaceAfterCustChars  := chkFixSpaceAfterCustomChars.Checked;
    ErrorsToFix.eSpaceBeforeCustChars := chkFixSpaceBeforeCustomChars.Checked;
    ErrorsToFix.eUnnecessarySpaces    := chkFixUnnecessarySpaces.Checked;

    // --------------------------- //
    // Unnecessary spaces to check //
    // --------------------------- //
    Ini.WriteBool('Unnecessary spaces to check for', 'Enters and spaces at the beginning and end', lstSpacesToCheck.Checked[0]);
    Ini.WriteBool('Unnecessary spaces to check for', 'Spaces between enters (left and right)',     lstSpacesToCheck.Checked[1]);
    Ini.WriteBool('Unnecessary spaces to check for', 'Double spaces and enters',                   lstSpacesToCheck.Checked[2]);
    Ini.WriteBool('Unnecessary spaces to check for', 'Spaces in front of punctuation marks',       lstSpacesToCheck.Checked[3]);
    Ini.WriteBool('Unnecessary spaces to check for', 'Spaces after "¿" and "¡"',                   lstSpacesToCheck.Checked[4]);
    Ini.WriteBool('Unnecessary spaces to check for', 'Spaces before "?" and  "!"',                 lstSpacesToCheck.Checked[5]);
    Ini.WriteBool('Unnecessary spaces to check for', 'Spaces between numbers',                     lstSpacesToCheck.Checked[6]);
    ErrorsToCheck.eWhatUnnecessarySpaces := [];
    if lstSpacesToCheck.Checked[0] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [EntersAndSpacesBeginningEnd];
    if lstSpacesToCheck.Checked[1] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesBetweenEnters];
    if lstSpacesToCheck.Checked[2] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [DoubleSpacesAndEnters];
    if lstSpacesToCheck.Checked[3] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesFrontPunctuation];
    if lstSpacesToCheck.Checked[4] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesAfterQuestionAndExclamation];
    if lstSpacesToCheck.Checked[5] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesBeforeQuestionAndExclamation];
    if lstSpacesToCheck.Checked[6] then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesBetweenNumbers];

    // ------------------------- //
    // Unnecessary spaces to fix //
    // ------------------------- //
    Ini.WriteBool('Unnecessary spaces to fix', 'Enters and spaces at the beginning and end', lstSpacesToFix.Checked[0]);
    Ini.WriteBool('Unnecessary spaces to fix', 'Spaces between enters (left and right)',     lstSpacesToFix.Checked[1]);
    Ini.WriteBool('Unnecessary spaces to fix', 'Double spaces and enters',                   lstSpacesToFix.Checked[2]);
    Ini.WriteBool('Unnecessary spaces to fix', 'Spaces in front of punctuation marks',       lstSpacesToFix.Checked[3]);
    Ini.WriteBool('Unnecessary spaces to fix', 'Spaces after "¿" and "¡"',                   lstSpacesToFix.Checked[4]);
    Ini.WriteBool('Unnecessary spaces to fix', 'Spaces before "?" and  "!"',                 lstSpacesToFix.Checked[5]);
    Ini.WriteBool('Unnecessary spaces to fix', 'Spaces between numbers',                     lstSpacesToFix.Checked[6]);
    ErrorsToFix.eWhatUnnecessarySpaces := [];
    if lstSpacesToFix.Checked[0] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [EntersAndSpacesBeginningEnd];
    if lstSpacesToFix.Checked[1] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesBetweenEnters];
    if lstSpacesToFix.Checked[2] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [DoubleSpacesAndEnters];
    if lstSpacesToFix.Checked[3] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesFrontPunctuation];
    if lstSpacesToFix.Checked[4] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesAfterQuestionAndExclamation];
    if lstSpacesToFix.Checked[5] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesBeforeQuestionAndExclamation];
    if lstSpacesToFix.Checked[6] then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesBetweenNumbers];
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.btnSetColorClick(Sender: TObject);
begin
  dlgSetColor.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.chkCheckTextBeforeColonClick(
  Sender: TObject);
begin
  chkCheckOnlyIfCapitalLetters.Enabled := chkCheckTextBeforeColon.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.chkFixTextBeforeColonClick(
  Sender: TObject);
begin
  chkFixOnlyIfCapitalLetters.Enabled := chkFixTextBeforeColon.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrorsSettings.btnEditClick(Sender: TObject);
  function GetWindowsDirectory: String;
  var 
     pcWindowsDirectory : PChar;
     dwWDSize           : DWORD;
  begin 
    dwWDSize := MAX_PATH + 1;
    GetMem(pcWindowsDirectory, dwWDSize);
    try
      if Windows.GetWindowsDirectory(pcWindowsDirectory, dwWDSize) <> 0 then
        Result := pcWindowsDirectory;
    finally
      FreeMem(pcWindowsDirectory);
    end;
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
  end;
var
  WinDir  : String;
  OCRFile : String;
begin
  OCRFile := ExtractFilePath(Application.ExeName) + 'OCRScripts\' + cmbOCRFiles.Items[cmbOCRFiles.ItemIndex] + ID_OCREXT;
  if FileExists(OCRFile) then
  begin
    WinDir := GetWindowsDirectory;
    if FileExists(WinDir + 'notepad.exe') then
      ShellExecute(Handle, 'open', PChar(WinDir + 'notepad.exe'), PChar(OCRFile), '', SW_SHOWMAXIMIZED);
  end;
end;

// -----------------------------------------------------------------------------

end.
