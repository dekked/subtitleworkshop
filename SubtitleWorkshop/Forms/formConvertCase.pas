unit formConvertCase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, VirtualTrees, TreeViewHandle, General, Functions, Undo,
  IniFiles;

type
  TfrmConvertCase = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    pnlConvertCase: TPanel;
    rdoInverseType: TRadioButton;
    rdoTitleType: TRadioButton;
    rdoUpperCase: TRadioButton;
    rdoLowerCase: TRadioButton;
    rdoSentenceType: TRadioButton;
    pnlWhere: TPanel;
    rdoSelectedSubs: TRadioButton;
    rdoAllSubs: TRadioButton;
    chkDotsDetection: TCheckBox;
    chkOnlyFirstLetterOfFirstWord: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rdoSentenceTypeClick(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

  function CaseSentenceType(Str: String; UpperCaseFirst: Boolean; OnlyFirstLetterOfFirstWord: Boolean): String;
  function CaseTitleType(Str: String): String;
  function CaseInverseType(Str: String): String;
  
var
  frmConvertCase: TfrmConvertCase;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                               := ReadString('Convert case', '01', 'Convert case');
      rdoSentenceType.Caption               := ReadString('Convert case', '02', 'Sentence type.');
      chkOnlyFirstLetterOfFirstWord.Caption := ReadString('Convert case', '03', 'Only first letter of first word');
      chkDotsDetection.Caption              := ReadString('Convert case', '04', '"..." detection');
      rdoLowerCase.Caption                  := ReadString('Convert case', '05', 'lowercase');
      rdoUpperCase.Caption                  := ReadString('Convert case', '06', 'UPPERCASE');
      rdoTitleType.Caption                  := ReadString('Convert case', '07', 'Title Type');
      rdoInverseType.Caption                := ReadString('Convert case', '08', 'iNVERSE tYPE');
      rdoAllSubs.Caption                    := ReadString('Convert case', '09', 'All subtitles');
      rdoSelectedSubs.Caption               := ReadString('Convert case', '10', 'Selected subtitles only');

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

function CaseSentenceType(Str: String; UpperCaseFirst: Boolean; OnlyFirstLetterOfFirstWord: Boolean): String;
var
  i            : Cardinal; 
  AlreadyDone  : Boolean;
  FirstCharPos : Cardinal; // Position of first character which isn't a special one
begin
  AlreadyDone  := False;
  FirstCharPos := 0;
  for i := 0 to Length(Str) do
  begin
    if ((Str[i] in SpecialChars) = False) then
    begin
      if FirstCharPos = 0 then FirstCharPos := i;
      if (AlreadyDone = False) or ((FirstCharPos = i) and (Copy(Str, 1, 3) <> '...')) then
      begin
        if (FirstCharPos = i) then
        begin
          if UpperCaseFirst then
            Str[i] := AnsiUpperCase(Str[i])[1] else
          begin
            if OnlyFirstLetterOfFirstWord = False then
            if Pos('. ', Str) = 0 then
              Str[i] := AnsiLowerCase(Str[i])[1] else
              Str[i] := AnsiUpperCase(Str[i])[1];
          end;
        end else
          Str[i] := AnsiUpperCase(Str[i])[1];
        AlreadyDone := True;
      end else
      begin
        if OnlyFirstLetterOfFirstWord = False then
          Str[i] := AnsiLowerCase(Str[i])[1];
      end;
    end else
    if (Str[i] = '.') or (Str[i] = '?') or (Str[i] = '!') or (Str[i] = '-') then
    begin
      if (Copy(Str, i-2, 3) <> '...') then
        AlreadyDone := False else
        AlreadyDone := True;
    end;
  end;
  Result := Str;
end;

// -----------------------------------------------------------------------------

function CaseTitleType(Str: String): String;
var
  i: Integer;
begin
  Str := AnsiUpperCase(Copy(Str, 1, 1)) + AnsiLowerCase(Copy(Str, 2, Length(Str)));
  for i := 1 to Length(Str) do
  begin
    if (Str[i] in SpecialChars) then
      Str := Copy(Str, 1, i) + AnsiUpperCase(Copy(Str, i + 1, 1)) + AnsiLowerCase(Copy(Str, i + 2, Length(Str)));
  end;
  Result := Str;
end;

// -----------------------------------------------------------------------------

function CaseInverseType(Str: String): String;
  function IsUpperCase(Ch: Char): Boolean;
  begin
    if AnsiUpperCase(Ch) = Ch then
      Result := True else
      Result := False;
  end;
var
  i: Integer;
begin
  for i := 0 to Length(Str) do
  begin
    if IsUpperCase(Str[i]) then
      Str[i] := AnsiLowerCase(Str[i])[1] else
      Str[i] := AnsiUpperCase(Str[i])[1];
  end;
  Result := Str;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.btnOkClick(Sender: TObject);
var
  Node       : PVirtualNode;
  Text       : String;
  tmpText    : String;
  Bold       : Boolean;
  Italic     : Boolean;
  Underline  : Boolean;
  Color      : Integer;
  Ini        : TIniFile;
  ConvType   : Integer;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;
  
  if rdoAllSubs.Checked then
    Node := frmMain.lstSubtitles.GetFirst else
    Node := frmMain.lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    Text := GetSubText(Node);
    
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
    PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

    // We store if the subtitle has tags or not, because we don't have to
    // convert case of tags
    Bold      := Pos('<b>', Text) > 0;
    Italic    := Pos('<i>', Text) > 0;
    Underline := Pos('<u>', Text) > 0;
    Color     := GetSubColor(Text);

    Text := RemoveSWTags(Text, True, True, True, True);
    if Text <> '' then
    begin
      if rdoSentenceType.Checked then
      begin
        if chkDotsDetection.Checked then
        begin
          if Node = frmMain.lstSubtitles.GetFirst then
            Text := CaseSentenceType(Text, True, chkOnlyFirstLetterOfFirstWord.Checked) else
          begin
            tmpText := GetSubText(Node.PrevSibling);
            if Copy(tmpText, Length(tmpText)-2, 3) = '...' then
              Text := CaseSentenceType(Text, False, chkOnlyFirstLetterOfFirstWord.Checked) else
              Text := CaseSentenceType(Text, True, chkOnlyFirstLetterOfFirstWord.Checked);
          end;
        end else
          Text := CaseSentenceType(Text, True, chkOnlyFirstLetterOfFirstWord.Checked);
      end else
      if rdoLowerCase.Checked    then Text := AnsiLowerCase(Text) else
      if rdoUpperCase.Checked    then Text := AnsiUpperCase(Text) else
      if rdoTitleType.Checked    then Text := CaseTitleType(Text) else
      if rdoInverseType.Checked  then Text := CaseInverseType(Text);

      if SubtitleAPI.NoInteractionWithTags = False then
      begin
        // Restore tags
        if Underline = True then Text := '<u>' + Text;
        if Bold      = True then Text := '<b>' + Text;
        if Italic    = True then Text := '<i>' + Text;
        if Color > -1 then
          Text := SetColorTag(Text, Color);
      end;

      SetText(Node, Text);
    end;
    
    if rdoAllSubs.Checked then
        Node := Node.NextSibling else
        Node := frmMain.lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);

    if Text <> '' then UndoList.Add(UndoAction);
  end;

  if UndoList.Count > 0 then
    PUndoAction(UndoList.Last)^.BindToNext := False;      

  ConvType := 0;
  Ini := TIniFile.Create(IniRoot);
  try
    if rdoSentenceType.Checked then ConvType := 0 else
    if rdoLowerCase.Checked    then ConvType := 1 else
    if rdoUpperCase.Checked    then ConvType := 2 else
    if rdoTitleType.Checked    then ConvType := 3 else
    if rdoInverseType.Checked  then ConvType := 4;
    Ini.WriteBool('Convert case', 'Only first letter of first word', chkOnlyFirstLetterOfFirstWord.Checked);
    Ini.WriteBool('Convert case', '"..." detection', chkDotsDetection.Checked);
    Ini.WriteInteger('Convert case', 'Conversion type', ConvType);
    Ini.WriteBool('Convert case', 'All subtitles', rdoAllSubs.Checked);
  finally
    Ini.Free;
  end;

  frmMain.mnuUndo.Enabled := True;
  frmMain.RefreshTimes;
  frmMain.OrgModified := True;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    case Ini.ReadInteger('Convert case', 'Conversion type', 0) of
      1: rdoLowerCase.Checked := True;
      2: rdoUpperCase.Checked := True;
      3: rdoTitleType.Checked := True;
      4: rdoInverseType.Checked := True else
      rdoSentenceType.Checked := True;
    end;
    chkDotsDetection.Enabled              := rdoSentenceType.Checked;
    chkOnlyFirstLetterOfFirstWord.Checked := Ini.ReadBool('Convert case', 'Only first letter of first word', False);
    chkDotsDetection.Checked              := Ini.ReadBool('Convert case', '"..." detection', False);
    rdoAllSubs.Checked                    := Ini.ReadBool('Convert case', 'All subtitles', True);
    rdoSelectedSubs.Checked               := not Ini.ReadBool('Convert case', 'All subtitles', True);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.rdoSentenceTypeClick(Sender: TObject);
begin
  chkDotsDetection.Enabled := rdoSentenceType.Checked;
end;

// -----------------------------------------------------------------------------

end.
