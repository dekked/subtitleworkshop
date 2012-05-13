unit formSAMILangExtractor;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, HTMLPars, USubtitleAPI, FastStrings, IniFiles,
  FastStringFuncs, General, Functions, FileCtrl, ComCtrls;

type
  TfrmSAMILangExtractor = class(TForm)
    Panel1: TPanel;
    btnBrowse1: TButton;
    edtSAMIFile: TLabeledEdit;
    dlgLoadFile: TOpenDialog;
    edtOutputDir: TLabeledEdit;
    btnBrowse2: TButton;
    btnAutoDetect: TButton;
    lstLanguages: TListView;
    btnAddManually: TButton;
    btnExtract: TButton;
    lblTip: TLabel;
    btnCancel: TButton;
    pnlPleaseWait: TPanel;
    procedure btnBrowse1Click(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAutoDetectClick(Sender: TObject);
    procedure btnAddManuallyClick(Sender: TObject);
    procedure btnBrowse2Click(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmSAMILangExtractor : TfrmSAMILangExtractor;
  AddLang              : String;
  EnterClassName       : String;
  SelOutputDir         : String;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                         := ReadString('SAMI Language Extractor', '01', 'SAMI Language Extractor');
      edtSAMIFile.EditLabel.Caption   := ReadString('SAMI Language Extractor', '02', 'SAMI file:');
      btnBrowse1.Caption              := BTN_BROWSE;
      btnBrowse2.Caption              := BTN_BROWSE;
      edtOutputDir.EditLabel.Caption  := ReadString('SAMI Language Extractor', '03', 'Output directory:');
      lblTip.Caption                  := ReadString('SAMI Language Extractor', '04', 'The selected languages will be extracted:');
      lstLanguages.Columns[0].Caption := ReadString('SAMI Language Extractor', '05', 'Class');
      lstLanguages.Columns[1].Caption := ReadString('SAMI Language Extractor', '06', 'Language');
      btnAutoDetect.Caption           := ReadString('SAMI Language Extractor', '07', 'Auto detect all');
      btnAddManually.Caption          := ReadString('SAMI Language Extractor', '08', 'Add manually');
      btnExtract.Caption              := ReadString('SAMI Language Extractor', '09', 'Extract!');
      AddLang                         := ReadString('SAMI Language Extractor', '10', 'Add language');
      EnterClassName                  := ReadString('SAMI Language Extractor', '11', 'Enter the class name of the desired language:');
      SelOutputDir                    := edtOutputDir.EditLabel.Caption;
      pnlPleaseWait.Caption           := ReadString('SAMI Language Extractor', '12', 'Please wait...');
      btnCancel.Caption               := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnExtract.ParentFont    := True;
      pnlPleaseWait.ParentFont := True;
      Font                     := frmMain.Font;
      btnExtract.Font.Style    := frmMain.Font.Style + [fsBold];
      pnlPleaseWait.Font.Style := frmMain.Font.Style + [fsBold];
      pnlPleaseWait.Font.Size  := frmMain.Font.Size + 2;
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveSAMIFile(const OutputFile: String);
var
  SAMIFile: TStringList;
  i: Integer;
begin
  SAMIFile := TStringList.Create;
  try
    SAMIFile.Add('<SAMI>');
    SAMIFile.Add('<HEAD>');
    SAMIFile.Add('   <STYLE TYPE="Text/css">');
    SAMIFile.Add('   <!--');
    SAMIFile.Add('      P {margin-left: 29pt; margin-right: 29pt; font-size: 24pt; text-align: center; font-family: Tahoma; font-weight: bold; color: #FFFFFF; background-color: #000000;}');
    SAMIFile.Add('      .SUBTTL {Name: ''Subtitles''; Lang: en-US; SAMIType: CC;}');
    SAMIFile.Add('   -->');
    SAMIFile.Add('   </STYLE>');
    SAMIFile.Add('</HEAD>');
    SAMIFile.Add('<BODY>');
    for i := 0 to SubtitleAPI.SubtitleCount-1 do
    begin
      SAMIFile.Add('   <SYNC START=' + IntToStr(SubtitleAPI.GetInitialTime(i)) + '>');
      SAMIFile.Add('      <P CLASS=SUBTTL>' + SubtitleAPI.GetText(i));
      SAMIFile.Add('   <SYNC START=' + IntToStr(SubtitleAPI.GetFinalTime(i)) + '>');
      SAMIFile.Add('      <P CLASS=SUBTTL>&nbsp;');
    end;
    SAMIFile.Add('</BODY>');
    SAMIFile.Add('</SAMI>');
  finally
    SAMIFile.SaveToFile(OutputFile);
    SAMIFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.btnBrowse1Click(Sender: TObject);
begin
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
  begin
    if SubtitleAPI.GetFileFormat(dlgLoadFile.FileName) = SubtitleAPI.GetFormatIndex('SAMI Captioning') then
      edtSAMIFile.Text := dlgLoadFile.FileName else
      MsgBox(Format('"%s" is not a valid SAMI file.', [dlgLoadFile.FileName]), BTN_OK, '', '', MB_ICONERROR, frmSAMILangExtractor); 
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.btnExtractClick(Sender: TObject);
var
  Parser      : THTMLParser;
  Obj         : TObject;
  Tag         : THTMLTag;
  Param       : THTMLParam;
  i, c, z     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  MyVar       : Boolean;
  OutputDir   : String;
begin
  if FileExists(edtSAMIFile.Text) then
  begin
    OutputDir := edtOutputDir.Text;
    if OutputDir <> '' then
    begin
      if OutputDir[Length(OutputDir)] <> '\' then
        OutputDir := OutputDir + '\';
    end;
    if DirectoryExists(OutputDir) = False then
    begin
      if (OutputDir = '') or (ForceDirectories(OutputDir) = False) then
      begin
        MsgBox(ErrorMsg[15], BTN_OK, '', '', MB_ICONERROR, frmSAMILangExtractor);
        exit;
      end;
    end;
    if (lstLanguages.SelCount = 0) then
    begin
      MsgBox(ErrorMsg[16], BTN_OK, '', '', MB_ICONERROR, frmSAMILangExtractor);
      exit;
    end;
    pnlPleaseWait.Show;
    Application.ProcessMessages;
    Parser := THTMLParser.Create;
    try
      Parser.Lines.LoadFromFile(edtSAMIFile.Text);
      Parser.Execute;

      for z := 0 to lstLanguages.Items.Count-1 do
      begin
        Application.ProcessMessages;
        if lstLanguages.Items[z].Selected then
        begin
          SubtitleAPI.ClearSubtitles;

          InitialTime := -1;
          FinalTime   := -1;
          for i := 0 to Parser.Parsed.Count-1 do
          begin
            Obj := Parser.Parsed[i];
            if Obj.ClassType = THTMLTag then
            begin
              Tag := THTMLTag(Obj);

              if (Tag.Name = 'P') and (Tag.Params.Count = 1) then
              begin
                Param := Tag.Params[0];
                if (Param.Key = 'CLASS') and (Param.Value = lstLanguages.Items[z].Caption) then // If it is the correct class
                begin
                  c := i + 1;
                  Text := '';
                  MyVar := True;
                  while (c < Parser.Parsed.Count-1) and (MyVar = True) do
                  begin
                    Obj := Parser.Parsed[c];
                    if Obj.ClassType = THTMLText then
                      Text := Text + THTMLText(Obj).Line else
                    begin
                      //MyVar := False;
                      if Obj.ClassType = THTMLTag then
                      begin
                        MyVar := True;
                        Tag := THTMLTag(Obj);
                        if Tag.Name = 'BR' then
                          Text := Text + '<br>' else
                        {if Tag.Name = 'B' then
                          Text := Text + '<b>' else
                        if Tag.Name = '/B' then
                          Text := Text + '</b>' else
                        if Tag.Name = 'I' then
                          Text := Text + '<i>' else
                        if Tag.Name = '/I' then
                          Text := Text + '</i>' else
                        if Tag.Name = 'U' then
                          Text := Text + '<u>' else
                        if Tag.Name = '/U' then
                          Text := Text + '</u>' else }
                          MyVar := False;
                      end else
                        MyVar := False;
                    end;
                      //MyVar := False;
                    Inc(c);
                  end;
                  Text := Trim(StringReplace(Text, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]));

                  // Go backwards until we find start time...
                  c := i-1;
                  MyVar := True;
                  InitialTime := -1;
                  while (c > 0) and (MyVar = True) do
                  begin
                    Obj := Parser.Parsed[c];
                    if Obj.ClassType = THTMLTag then
                    begin
                      Tag := THTMLTag(Obj);
                      if (Tag.Name = 'SYNC') and (Tag.Params.Count = 1) then
                      begin
                        Param := Tag.Params[0];
                        if Param.Key = 'START' then
                        begin
                          if InitialTime = -1 then
                            InitialTime := StrToIntDef(Param.Value, -1);
                        end else
                          MyVar := False;
                      end;
                    end;
                    Dec(c);
                  end;
                end;
              end else
              if (Tag.Name = 'SYNC') and (Tag.Params.Count = 1) then
              begin
                Param := Tag.Params[0];
                if Param.Key = 'START' then
                begin
                  if InitialTime > -1 then
                    FinalTime := StrToIntDef(Param.Value, -1);
                end;
                if (InitialTime > -1) and (FinalTime > -1) and (Text <> '') then
                begin
                  SubtitleAPI.AddSubtitle(InitialTime, FinalTime, Text);
                  InitialTime := -1;
                  FinalTime := -1;
                end;
              end;
            end;
          end;

          if lstLanguages.Items[z].SubItems[0] <> '?' then
            SaveSAMIFile(OutputDir + ChangeFileExt(ExtractFileName(edtSAMIFile.Text), '.' + lstLanguages.Items[z].SubItems[0] + '.smi')) else
            SaveSAMIFile(OutputDir + ChangeFileExt(ExtractFileName(edtSAMIFile.Text), '.smi'));
        end;
      end;
    finally
      Parser.Free;
      pnlPleaseWait.Hide;
      frmSAMILangExtractor.Close;
    end;
  end else
  begin
    MsgBox(Format(ErrorMsg[17], [edtSAMIFile.Text]), BTN_OK, '', '', MB_ICONERROR, frmSAMILangExtractor);
    exit;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.FormCreate(Sender: TObject);
begin
  SetLanguage;
  SubtitleAPI.CreateNewSubtitle;
  pnlPleaseWait.Left := (ClientWidth div 2) - (pnlPleaseWait.Width div 2);
  pnlPleaseWait.Top  := (ClientHeight div 2) - (pnlPleaseWait.Height div 2);
  pnlPleaseWait.Hide;
end;

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.btnAutoDetectClick(Sender: TObject);
  function MakeOneLine(const Source: TStringList; AddSpaces: Boolean = False): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Source.Count-1 do
      if AddSpaces = False then
      Result := Result + Source[i] else
      Result := Result + ' ' + Source[i];
  end;
var
  SAMIFile : TStringList;
  a, b     : integer;
  BigStr   : String;
  RealClass: String;
  RealLang : String;
begin
  if FileExists(edtSAMIFile.Text) = False then
  begin
    MsgBox(Format(ErrorMsg[17], [edtSAMIFile.Text]), BTN_OK, '', '', MB_ICONERROR, frmSAMILangExtractor);
    exit;
  end;

  SAMIFile := TStringList.Create;
  try
    lstLanguages.Clear;
    SAMIFile.LoadFromFile(edtSAMIFile.Text);
    BigStr := MakeOneLine(SAMIFile);

    a := SmartPos('<STYLE', BigStr, False);
    b := SmartPos('</STYLE>', BigStr, False);
    if (a > 0) and (b > 0) then
    begin
      BigStr := Copy(BigStr, a, b);
      if (StringCount(BigStr, 'lang:', False) < 2) or (StringCount(BigStr, '.', False) < 2) then
        MsgBox(InfoMsg[05], BTN_OK, '', '', MB_ICONWARNING, frmSAMILangExtractor) else
      begin
        a := SmartPos('.', BigStr);
        while a > 0 do
        begin
          RealClass := Trim(Copy(BigStr, a + 1, SmartPos('{', BigStr, True, a + 1) - (a + 1)));
          BigStr := Copy(BigStr, SmartPos('{', BigStr, True, a) + 1, Length(BigStr));
          b := SmartPos('Name:', BigStr, False);
          RealLang := Copy(BigStr, b + 5, SmartPos(';', BigStr, True, b));
          if StringCount(RealLang, '"') = 2 then
          begin
            b := Pos('"', RealLang) + 1;
            RealLang := Copy(RealLang, b, SmartPos('"', RealLang, True, b) - b);
          end;

          lstLanguages.Items.Add;
          lstLanguages.Items[lstLanguages.Items.Count-1].Caption := RealClass;
          lstLanguages.Items[lstLanguages.Items.Count-1].SubItems.Add(RealLang);

          a := SmartPos('.', BigStr);
        end;

      end;
    end else
      MsgBox(InfoMsg[06], BTN_OK, '', '', MB_ICONWARNING, frmSAMILangExtractor);

  finally
    SAMIFile.Free;
  end;
  lstLanguages.SelectAll;
end;

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.btnAddManuallyClick(Sender: TObject);
var
  LangClassName: String;
begin
  if FileExists(edtSAMIFile.Text) = False then
  begin
    MsgBox(Format(ErrorMsg[17], [edtSAMIFile.Text]), BTN_OK, '', '', MB_ICONERROR, frmSAMILangExtractor);
    exit;
  end;
  if QueryInput(AddLang, EnterClassName, LangClassName, frmSAMILangExtractor) = 1 then
  begin
    lstLanguages.Items.Add;
    lstLanguages.Items[lstLanguages.Items.Count-1].Caption := LangClassName;
    lstLanguages.Items[lstLanguages.Items.Count-1].SubItems.Add('?');
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSAMILangExtractor.btnBrowse2Click(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory(SelOutputDir, '', Dir) then
    edtOutputDir.Text := Dir;
end;

// -----------------------------------------------------------------------------

end.
