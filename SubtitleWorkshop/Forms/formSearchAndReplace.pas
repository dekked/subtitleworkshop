unit formSearchAndReplace;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, VirtualTrees, Functions, General, IniFiles,
  TreeViewHandle, USubtitlesFunctions, Undo;

type
  TfrmSearchAndReplace = class(TForm)
    btnCancel: TButton;
    pgeCtrl: TPageControl;
    pgeSearch: TTabSheet;
    bvlSearch: TBevel;
    edtTextToFind: TLabeledEdit;
    btnSearch: TButton;
    pgeReplace: TTabSheet;
    bvlReplace: TBevel;
    edtTextToFind2: TLabeledEdit;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    edtReplaceBy: TLabeledEdit;
    btnFindNext: TButton;
    btnMoreLess: TButton;
    pnlExpand: TPanel;
    lblSearchForTextIn: TLabel;
    rdoFromSelItem: TRadioButton;
    rdoAllTheSubtitle: TRadioButton;
    chkCaseSensitive: TCheckBox;
    chkWholeWords: TCheckBox;
    lblCharset: TLabel;
    cmbCharset: TComboBox;
    chkPreserveCase: TCheckBox;
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMoreLessClick(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure cmbCharsetChange(Sender: TObject);
  private
    procedure SetLanguage;
  public
    { Public declarations }
  end;

var
  frmSearchAndReplace : TfrmSearchAndReplace;
  More                : String;
  Less                : String;
  EXPANDED_HEIGHT     : Integer;
  MINIMIZED_HEIGHT    : Integer;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    with LF do
    begin
      Caption                          := ReadString('Search & Replace','01','Search and Replace');
      pgeSearch.Caption                := ReadString('Search & Replace','02','Search');
      pgeReplace.Caption               := ReadString('Search & Replace','01','Search and Replace');
      edtTextToFind.EditLabel.Caption  := ReadString('Search & Replace','03','Text to find:');
      btnSearch.Caption                := ReadString('Search & Replace','04','&Search!');
      edtTextToFind2.EditLabel.Caption := ReadString('Search & Replace','03','Text to find:');
      edtReplaceBy.EditLabel.Caption   := ReadString('Search & Replace','05','Replace by:');
      btnFindNext.Caption              := ReadString('Search & Replace','06', 'Find &next');
      btnReplace.Caption               := ReadString('Search & Replace','07','&Replace');
      btnReplaceAll.Caption            := ReadString('Search & Replace','08','Replace &all');
      More                             := ReadString('Search & Replace','09','&More >');
      Less                             := ReadString('Search & Replace','10','&Less <');
      chkCaseSensitive.Caption         := ReadString('Search & Replace','11','Case sensitive');
      chkWholeWords.Caption            := ReadString('Search & Replace','12','Match whole words');
      chkPreserveCase.Caption          := ReadString('Search & Replace','13','Preserve case on replace');
      lblSearchForTextIn.Caption       := ReadString('Search & Replace','14','Search for text in:');
      rdoAllTheSubtitle.Caption        := ReadString('Search & Replace','15','All the subtitle');
      rdoFromSelItem.Caption           := ReadString('Search & Replace','16','From the selected item');
      lblCharset.Caption               := ReadString('Search & Replace','17','Charset:');
      btnCancel.Caption                := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnSearch.ParentFont     := True;
      btnReplaceAll.ParentFont := True;
      Font                     := frmMain.Font;
      btnSearch.Font.Style     := frmMain.Font.Style + [fsBold];
      btnReplaceAll.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.FormCreate(Sender: TObject);
var
  Ini     : TIniFile;
  Charset : Integer;
begin
  SetLanguage;
  AddCharsets(cmbCharset);
  Ini := TIniFile.Create(IniRoot);
    // We base on percentage to calculate the minimized height, so we can
    // avoid trouble with large fonts...

    // 388 __ 100%   MINIMIZED_HEIGHT is 65% EXPANDED_HEIGHT
    // 251 __ x

    if frmMain.mnuTranslatorMode.Checked = False then
    begin
      // 436 __ 100%
      // 389 __ x      Calculate proper expanded height of the form without the combobox
      EXPANDED_HEIGHT  := Round(Height * 0.89);
      MINIMIZED_HEIGHT := Round(EXPANDED_HEIGHT * 0.65);

      //  169 __ 100%  New height of the panel: 70% than before
      //  121 __ x
      pnlExpand.Height := Round(pnlExpand.Height * 0.72);
      Charset := StrCharsetToInt(frmMain.cmbOrgCharset.Items[frmMain.cmbOrgCharset.ItemIndex]);
      lblCharset.Hide;
      cmbCharset.Hide;
      cmbCharset.ItemIndex := frmMain.cmbOrgCharset.ItemIndex;
    end else
    begin
      EXPANDED_HEIGHT  := Height;
      // 436 __ 100%
      // 252 __ x     Calculate minimized height
      MINIMIZED_HEIGHT := Round(EXPANDED_HEIGHT * 0.58);
      cmbCharset.ItemIndex := frmMain.cmbTransCharset.ItemIndex;
      Charset := StrCharsetToInt(cmbCharset.Items[cmbCharset.ItemIndex]);
    end;

    edtTextToFind.Font.Charset  := Charset;
    edtTextToFind2.Font.Charset := Charset;
    edtReplaceBy.Font.Charset   := Charset;

    if Ini.ReadBool('Search & Replace','Expanded',False) then
    begin
      Height := EXPANDED_HEIGHT;
      btnMoreLess.Caption := Less;
    end else
    begin
      Height := MINIMIZED_HEIGHT;
      btnMoreLess.Caption := More;
    end;
    rdoAllTheSubtitle.Checked := Ini.ReadBool('Search & Replace','All the subtitle',False);
    rdoFromSelItem.Checked    := not Ini.ReadBool('Search & Replace','All the subtitle',False);
    chkCaseSensitive.Checked  := Ini.ReadBool('Search & Replace','Case sensitive',False);
    chkWholeWords.Checked     := Ini.ReadBool('Search & Replace','Whole Words',False);
    chkPreserveCase.Checked   := Ini.ReadBool('Search & Replace','Preserve case',False);
    edtTextToFind.Text        := frmMain.SearchWord;
    edtTextToFind2.Text       := frmMain.SearchWord;
  Ini.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
    Ini.WriteBool('Search & Replace','All the subtitle',rdoAllTheSubtitle.Checked);
    Ini.WriteBool('Search & Replace','Case sensitive',chkCaseSensitive.Checked);
    Ini.WriteBool('Search & Replace','Whole Words',chkWholeWords.Checked);
    Ini.WriteBool('Search & Replace','Preserve case',chkPreserveCase.Checked);
    Ini.WriteBool('Search & Replace','Expanded',Height = EXPANDED_HEIGHT);
  Ini.Free;
  frmMain.CaseSensitive   := chkCaseSensitive.Checked;
  frmMain.MatchWholeWords := chkWholeWords.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.FormActivate(Sender: TObject);
begin
  if pgeCtrl.ActivePage = pgeSearch then
  begin
    edtTextToFind.SetFocus;
    edtTextToFind.SelectAll;
  end else
  begin
    edtTextToFind2.SetFocus;
    edtTextToFind2.SelectAll;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.btnMoreLessClick(Sender: TObject);
begin
  if Height = EXPANDED_HEIGHT then
  begin
    Height := MINIMIZED_HEIGHT;
    btnMoreLess.Caption := More;
  end else
  begin
    Height := EXPANDED_HEIGHT;
    btnMoreLess.Caption := Less;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.btnSearchClick(Sender: TObject);
var
  TextToFind: String;
begin
  TextToFind := ReplaceString(edtTextToFind.Text, '|', #13#10);
  TextToFind := ReplaceString(TextToFind, #13#10#13#10, '|');
  if FindInNode(TextToFind, chkCaseSensitive.Checked, chkWholeWords.Checked, rdoFromSelItem.Checked) = nil then
    MsgBox(Format(InfoMsg[01], [TextToFind]), BTN_OK, '', '', MB_ICONINFORMATION, frmSearchAndReplace, StrCharsetToInt(cmbCharset.Items[cmbCharset.ItemIndex])) else
    begin
      frmMain.SearchWord := TextToFind;
      frmMain.RefreshTimes;
      Close;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.btnFindNextClick(Sender: TObject);
var
  TextToFind: String;
begin
  TextToFind := ReplaceString(edtTextToFind2.Text, '|', #13#10);
  TextToFind := ReplaceString(TextToFind, #13#10#13#10, '|');
  if FindInNode(TextToFind, chkCaseSensitive.Checked, chkWholeWords.Checked, rdoFromSelItem.Checked) = nil then
    MsgBox(Format(InfoMsg[01], [TextToFind]), BTN_OK, '', '', MB_ICONINFORMATION, frmSearchAndReplace, StrCharsetToInt(cmbCharset.Items[cmbCharset.ItemIndex])) else
    begin
      frmMain.SearchWord := TextToFind;
      frmMain.RefreshTimes;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.btnReplaceClick(Sender: TObject);
var
  NodeText   : String;
  NodeTrans  : String;
  TextToFind : String;
  ReplaceBy  : String;
  tmpStr     : String;
  Flag       : Boolean;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;
  Flag := False;

  TextToFind := ReplaceString(edtTextToFind2.Text, '|', #13#10);
  TextToFind := ReplaceString(TextToFind, #13#10#13#10, '|');
  ReplaceBy  := ReplaceString(edtReplaceBy.Text, '|', #13#10);
  ReplaceBy  := ReplaceString(ReplaceBy, #13#10#13#10, '|');

  New(UndoAction);
  UndoAction^.UndoActionType := uaFullTextChange;
  UndoAction^.BufferSize     := SizeOf(TFullTextChange);
  UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
  UndoAction^.Node           := frmMain.lstSubtitles.FocusedNode;
  UndoAction^.LineNumber     := frmMain.lstSubtitles.FocusedNode.Index;
  UndoAction^.BindToNext     := False;

  NodeText  := GetSubText(frmMain.lstSubtitles.FocusedNode);
  NodeTrans := GetSubTranslation(frmMain.lstSubtitles.FocusedNode);

  tmpStr := Replace(NodeText, TextToFind, ReplaceBy, chkCaseSensitive.Checked, chkWholeWords.Checked, chkPreserveCase.Checked);
  PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
  PFullTextChange(UndoAction^.Buffer)^.OldText := NodeText;
  if NodeText <> tmpStr then
  begin
    SetText(frmMain.lstSubtitles.FocusedNode, tmpStr);
    Flag := True;
  end;
  if frmMain.mnuTranslatorMode.Checked then
  begin
    tmpStr := Replace(NodeTrans, TextToFind, ReplaceBy, chkCaseSensitive.Checked, chkWholeWords.Checked, chkPreserveCase.Checked);
    if NodeTrans <> tmpStr then
    begin
      PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := NodeTrans;
      SetTranslation(frmMain.lstSubtitles.FocusedNode, tmpStr);
      Flag := True;
    end;
  end;
  if Flag = True then
  begin
    UndoList.Add(UndoAction);
    frmMain.mnuUndo.Enabled := True;
  end;

  if FindInNode(TextToFind, chkCaseSensitive.Checked, chkWholeWords.Checked, True) = nil then
    MsgBox(Format(InfoMsg[01], [TextToFind]), BTN_OK, '', '', MB_ICONINFORMATION, frmSearchAndReplace, StrCharsetToInt(cmbCharset.Items[cmbCharset.ItemIndex])) else

  if UndoList.Count > 0 then
    PUndoAction(UndoList.Last)^.BindToNext := False;         
  frmMain.SearchWord := TextToFind;
  frmMain.RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.btnReplaceAllClick(Sender: TObject);
var
  Data     : PSubtitleItem;
  Node     : PVirtualNode;
  LastNode : PVirtualNode;
  Text     : String;
  RepStr   : String;
  Replaces : Integer;
  Bold, Italic, Underline: Boolean;
  Color      : Integer;
  TextToFind : String;
  ReplaceBy  : String;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;
  
  Replaces := 0;
  LastNode := nil;
  TextToFind := ReplaceString(edtTextToFind2.Text, '|', #13#10);
  TextToFind := ReplaceString(TextToFind, #13#10#13#10, '|');
  ReplaceBy  := ReplaceString(edtReplaceBy.Text, '|', #13#10);
  ReplaceBy := ReplaceString(ReplaceBy, #13#10#13#10, '|');

  if rdoFromSelItem.Checked then
    Node := frmMain.lstSubtitles.GetFirstSelected else
    Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    UndoAction^.BindToNext     := True;

    Data := frmMain.lstSubtitles.GetNodeData(Node);
    Text := Data.Text;
    if Text <> '' then
    begin
      PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
      PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

      Bold      := Pos('<b>', Text) > 0;
      Italic    := Pos('<i>', Text) > 0;
      Underline := Pos('<u>', Text) > 0;
      Color     := GetSubColor(Text);
      Text      := RemoveSWTags(Text, True, True, True, True);

      RepStr := Replace(Text, TextToFind, ReplaceBy, chkCaseSensitive.Checked, chkWholeWords.Checked, chkPreserveCase.Checked);
      if RepStr <> Text then
      begin
        if frmMain.mnuTranslatorMode.Checked = False then
          UndoList.Add(UndoAction);
        if SubtitleAPI.NoInteractionWithTags = False then
        begin
          // Restore tags
          if Underline = True then RepStr := '<u>' + RepStr;
          if Bold      = True then RepStr := '<b>' + RepStr;
          if Italic    = True then RepStr := '<i>' + RepStr;
          if Color > -1 then
            RepStr := SetColorTag(RepStr, Color);
        end;

        Data.Text := RepStr;
        Inc(Replaces);
        LastNode := Node;
      end;
    end;

    if frmMain.mnuTranslatorMode.Checked then
    begin
      Text := Data.Translation;
      if Text <> '' then
      begin
        PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;
        PFullTextChange(UndoAction^.Buffer)^.OldTrans := Text;
        UndoList.Add(UndoAction);
        
        Bold      := Pos('<b>', Text) > 0;
        Italic    := Pos('<i>', Text) > 0;
        Underline := Pos('<u>', Text) > 0;
        Color     := GetSubColor(Text);
        Text      := RemoveSWTags(Text, True, True, True, True);

        RepStr := Replace(Text, TextToFind, ReplaceBy, chkCaseSensitive.Checked, chkWholeWords.Checked, chkPreserveCase.Checked);
        if RepStr <> Text then
        begin
          if SubtitleAPI.NoInteractionWithTags = False then
          begin
            // Restore tags
            if Underline = True then RepStr := '<u>' + RepStr;
            if Bold      = True then RepStr := '<b>' + RepStr;
            if Italic    = True then RepStr := '<i>' + RepStr;
            if Color > -1 then
              RepStr := SetColorTag(RepStr, Color);
          end;
          Data.Translation := RepStr;
          Inc(Replaces);
          LastNode := Node;
        end;
      end;
    end;

    Node := Node.NextSibling;
  end;

  if Assigned(LastNode) then
  begin
    frmMain.lstSubtitles.Selected[frmMain.lstSubtitles.FocusedNode] := False;
    frmMain.lstSubtitles.FocusedNode := LastNode;
    frmMain.lstSubtitles.Selected[LastNode] := True;
    frmMain.lstSubtitles.ScrollIntoView(LastNode, True);
  end;
  frmMain.RefreshTimes;

  if Replaces = 0 then
    MsgBox(Format(InfoMsg[01], [TextToFind]), BTN_OK, '', '', MB_ICONINFORMATION, frmSearchAndReplace, StrCharsetToInt(cmbCharset.Items[cmbCharset.ItemIndex])) else
  begin
    if UndoList.Count > 0 then
      PUndoAction(UndoList.Last)^.BindToNext := False;
    frmMain.mnuUndo.Enabled := True;
    MsgBox(Format(InfoMsg[08], [Replaces]), BTN_OK, '', '', MB_ICONINFORMATION, frmSearchAndReplace);
    frmMain.OrgModified   := True;
    frmMain.TransModified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSearchAndReplace.cmbCharsetChange(Sender: TObject);
var
  Charset: Integer;
begin
  Charset := StrCharsetToInt(cmbCharset.Items[cmbCharset.ItemIndex]);
  edtTextToFind.Font.Charset  := Charset;
  edtTextToFind2.Font.Charset := Charset;
  edtReplaceBy.Font.Charset   := Charset;
end;

// -----------------------------------------------------------------------------

end.
