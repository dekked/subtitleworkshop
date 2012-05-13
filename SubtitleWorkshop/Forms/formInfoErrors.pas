unit formInfoErrors;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, StdCtrls, ExtCtrls, TreeViewHandle, General, Functions, Undo,
  InfoErrorsFunctions, StrMan, USubtitlesFunctions, IniFiles, OCRScripts;

type
  TErrorClass = (etError, etWarning, etInfo, etFixed);
  TErrorDescription = (dNone,
                       // ----------- //
                       // Information //
                       // ----------- //
                       ifTotalErrors,
                       ifFixedErrors,
                       // ---------------------- //
                       // Descriptions of errors //
                       // ---------------------- //
                       deMarkedSubtitle,
                       // -------------
                       deLinesWithoutLetters,
                       deEmptySubtitle,
                       deOverlapping,
                       deBadValues,
                       deTooLongDuration,
                       deTooShortDuration,
                       deTooLongLine,
                       deOverTwoLines,
                       deHearingImpaired,
                       deTextBeforeColon,
                       deUnnecessaryDots,
                       deProhibitedCharacter,
                       deRepeatedCharacter,
                       deRepeatedSubtitle,
                       deOCRError,
                       deOpnDlgSubsOneLine,
                       deNoSpaceAfterChar,
                       deNoSpaceBeforeChar,
                       deUnnecessarySpaces,

                       // ---------------------------- //
                       // Descriptions of fixed errors //
                       // ---------------------------- //
                       dfLinesWithoutLetters,
                       dfEmptySubtitle,
                       dfOverlapping,
                       dfBadValues,
                       dfHearingImpairedAll, // All the subtitle is deleted
                       dfHearingImpairedPart, // Only a part is deleted
                       dfTextBeforeColon,
                       dfUnnecessaryDots,
                       dfOverTwoLines,
                       dfProhibitedCharacter,
                       dfRepeatedCharacter,
                       dfRepeatedSubtitle,
                       dfOCRError,
                       dfOpnDlgSubsOneLine,
                       dfNoSpaceAfterChar,
                       dfNoSpaceBeforeChar,
                       dfUnnecessarySpaces
                       );
  TInfoError = record
    Subtitle    : Integer;
    ErrorClass  : TErrorClass;
    Description : TErrorDescription;
    Tag         : Integer; // To store information values
  end;
  PInfoError = ^TInfoError;
  TfrmInfoErrors = class(TForm)
    bvlInfoErrors: TBevel;
    btnCheck: TButton;
    btnFixErrors: TButton;
    chkConfirm: TCheckBox;
    btnOk: TButton;
    btnSettings: TButton;
    lstErrors: TVirtualStringTree;
    procedure lstErrorsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure lstErrorsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure lstErrorsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure lstErrorsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnFixErrorsClick(Sender: TObject);
    procedure lstErrorsDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstErrorsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
  private
    procedure SetLanguage;
    procedure AddInfoError(Subtitle: Integer; ErrorClass: TErrorClass; ErrorDescription: TErrorDescription; Tag: Integer = -1);
  public
    { Public declarations }
  end;

var
  frmInfoErrors : TfrmInfoErrors;
  ErrorTypes    : array[1..4] of String;
  InfoMsgs      : array[1..2] of String;
  FixReports    : array[1..17] of String;

implementation

uses formInfoErrorsSettings, formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption                          := ReadString('Information and errors', '01', 'Information and errors');
      btnCheck.Caption                 := ReadString('Information and errors', '02', 'Check!');
      btnFixErrors.Caption             := ReadString('Information and errors', '03', 'Fix errors!');
      btnSettings.Caption              := ReadString('Information and errors', '04', '&Settings');
      chkConfirm.Caption               := ReadString('Information and errors', '05', 'Confirm each deletion');
      lstErrors.Header.Columns[0].Text := StringToWideStringEx(ReadString('Information and errors', '06', 'Subtitle'), CharSetToCodePage(frmMain.Font.Charset));
      lstErrors.Header.Columns[1].Text := StringToWideStringEx(ReadString('Information and errors', '07', 'Type'), CharSetToCodePage(frmMain.Font.Charset));
      lstErrors.Header.Columns[2].Text := StringToWideStringEx(ReadString('Information and errors', '08', 'Description'), CharSetToCodePage(frmMain.Font.Charset));

      // ------------- //
      //  Error types  //
      // ------------- //
      ErrorTypes[1] := ReadString('Information and errors', '09', 'Error');
      ErrorTypes[2] := ReadString('Information and errors', '10', 'Warning');
      ErrorTypes[3] := ReadString('Information and errors', '11', 'Info');
      ErrorTypes[4] := ReadString('Information and errors', '13', 'Fixed');

      // ---------------------- //
      //  Information messages  //
      // ---------------------- //
      InfoMsgs[1] := ReadString('Information and errors', '14', 'Total errors: %d');
      InfoMsgs[2] := ReadString('Information and errors', '15', 'Fixed errors: %d');

      // ------------- //
      //  Fix reports  //
      // ------------- //
      FixReports[01] := ReadString('Information and errors', '36', 'Deleted lines without letters');
      FixReports[02] := ReadString('Information and errors', '37', 'Empty subtitle was deleted');
      // ---
      FixReports[03] := ReadString('Information and errors', '38', 'Fixed overlapping');
      FixReports[04] := ReadString('Information and errors', '39', 'Fixed bad values');
      FixReports[05] := ReadString('Information and errors', '40', 'Subtitle over two lines was adjusted');
      // ---
      FixReports[06] := ReadString('Information and errors', '41', 'Removed hearing impaired subtitle');
      FixReports[07] := ReadString('Information and errors', '42', 'Hearing impaired part has been deleted');
      FixReports[08] := ReadString('Information and errors', '43', 'Removed text before colon (":")');
      FixReports[09] := ReadString('Information and errors', '44', 'Removed unnecessary dots');
      FixReports[10] := ReadString('Information and errors', '45', 'Had a prohibited character and was deleted');
      FixReports[11] := ReadString('Information and errors', '46', 'Fixed repeated character');
      FixReports[12] := ReadString('Information and errors', '47', 'Fixed repeated subtitle');
      FixReports[13] := ReadString('Information and errors', '48', 'Fixed OCR Errors');
      // ---
      FixReports[14] := ReadString('Information and errors', '49', 'Deleted "-" in subtitle with one line');
      FixReports[15] := ReadString('Information and errors', '50', 'Added space after custom character');
      FixReports[16] := ReadString('Information and errors', '51', 'Added space before custom character');
      FixReports[17] := ReadString('Information and errors', '52', 'Removed unnecessary spaces');

      lstErrors.Hint := ReadString('Information and errors', '53', 'Double-click to jump to line on main form');
      btnOk.Caption := BTN_OK;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnOk.ParentFont        := True;
      btnFixErrors.ParentFont := True;
      lstErrors.ParentFont    := True;
      Font                    := frmMain.Font;
      lstErrors.Header.Font   := Font;
      btnOk.Font.Style        := frmMain.Font.Style + [fsBold];
      btnFixErrors.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.lstErrorsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PInfoError;
begin
  Data             := Sender.GetNodeData(Node);
  Data.Subtitle    := -1;
  Data.ErrorClass  := etError;
  Data.Description := dNone;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.lstErrorsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PInfoError;
begin
  Data             := Sender.GetNodeData(Node);
  Data.Subtitle    := -1;
  Data.ErrorClass  := etError;
  Data.Description := dNone;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.lstErrorsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TInfoError);
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.lstErrorsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PInfoError;
begin
  Data := Sender.GetNodeData(Node);
  if (Data.Description = deMarkedSubtitle) or
     (Data.Description = ifTotalErrors) or
     (Data.Description = ifFixedErrors) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.lstErrorsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PInfoError;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Column of
      0:
        begin
          if Data.Subtitle > -1 then
            CellText := IntToStr(Data.Subtitle) else
            CellText := '-';
        end;
      1:
        begin
          case Data.ErrorClass of
            etError   : CellText := ErrorTypes[1];
            etWarning : CellText := ErrorTypes[2];
            etInfo    : CellText := ErrorTypes[3];
            etFixed   : CellText := ErrorTypes[4];
          end;
        end;
      2:
        begin
          case Data.Description of
            dNone : CellText := '';
            // ----------- //
            // Information //
            // ----------- //
            ifTotalErrors : CellText := Format(InfoMsgs[1], [Data.Tag]);
            ifFixedErrors : CellText := Format(InfoMsgs[2], [Data.Tag]);
            // ------------------------------------------------------- //
            // Error reports that appear when you press "Check" button //
            // ------------------------------------------------------- //
            deLinesWithoutLetters  : CellText := ErrorReports[01];
            deEmptySubtitle        : CellText := ErrorReports[02];
            deOverlapping          : CellText := ErrorReports[03];
            deBadValues            : CellText := ErrorReports[04];
            deTooLongDuration      : CellText := ErrorReports[05];
            deTooShortDuration     : CellText := ErrorReports[06];
            deTooLongLine          : CellText := ErrorReports[07];
            deOverTwoLines         : CellText := ErrorReports[08];
            deHearingImpaired      : CellText := ErrorReports[09];
            deTextBeforeColon      : CellText := ErrorReports[10];
            deUnnecessaryDots      : CellText := ErrorReports[11];
            deProhibitedCharacter  : CellText := ErrorReports[12];
            deRepeatedCharacter    : CellText := ErrorReports[13];
            deRepeatedSubtitle     : CellText := ErrorReports[14];
            deOCRError             : CellText := ErrorReports[15];
            deOpnDlgSubsOneLine    : CellText := ErrorReports[16];
            deNoSpaceAfterChar     : CellText := ErrorReports[17];
            deNoSpaceBeforeChar    : CellText := ErrorReports[18];
            deUnnecessarySpaces    : CellText := ErrorReports[19];
            deMarkedSubtitle       : CellText := ErrorReports[20];

            // -------------------------------------------------------- //
            // Report of fixes that appears when you press "Fix" button //
            // -------------------------------------------------------- //
            dfLinesWithoutLetters  : CellText := FixReports[01];
            dfEmptySubtitle        : CellText := FixReports[02];
            dfOverlapping          : CellText := FixReports[03];
            dfBadValues            : CellText := FixReports[04];
            dfOverTwoLines         : CellText := FixReports[05];
            dfHearingImpairedAll   : CellText := FixReports[06];
            dfHearingImpairedPart  : CellText := FixReports[07];
            dfTextBeforeColon      : CellText := FixReports[08];
            dfUnnecessaryDots      : CellText := FixReports[09];
            dfProhibitedCharacter  : CellText := FixReports[10];
            dfRepeatedCharacter    : CellText := FixReports[11];
            dfRepeatedSubtitle     : CellText := FixReports[12];
            dfOCRError             : CellText := FixReports[13];
            dfOpnDlgSubsOneLine    : CellText := FixReports[14];
            dfNoSpaceAfterChar     : CellText := FixReports[15];
            dfNoSpaceBeforeChar    : CellText := FixReports[16];
            dfUnnecessarySpaces    : CellText := FixReports[17];
          end;
        end;
    end;
    CellText := StringToWideStringEx(CellText, CharSetToCodePage(frmMain.Font.Charset));
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.AddInfoError(Subtitle: Integer; ErrorClass: TErrorClass; ErrorDescription: TErrorDescription; Tag: Integer = -1);
var
  Data: PInfoError;
begin
  lstErrors.RootNodeCount := lstErrors.RootNodeCount + 1;
  Data             := lstErrors.GetNodeData(lstErrors.GetLast);
  Data.Subtitle    := Subtitle;
  Data.ErrorClass  := ErrorClass;
  Data.Description := ErrorDescription;
  Data.Tag         := Tag;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.btnSettingsClick(Sender: TObject);
begin
  frmInfoErrorsSettings := TfrmInfoErrorsSettings.Create(Application);
  frmInfoErrorsSettings.ShowModal;
  frmInfoErrorsSettings.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.btnCheckClick(Sender: TObject);
var
  Node        : PVirtualNode;
  Data        : PSubtitleItem;
  ErrorsTypes : TErrorTypeSet;
  TotalErrors : Integer;
begin
  TotalErrors := 0;
  lstErrors.Clear;
  btnCheck.Enabled     := False;
  btnFixErrors.Enabled := False;
  if (ErrorsToCheck.eOCRErrors) and (FileExists(OCRDefFile)) then
    ParseOCRErrors(OCRDefFile);
  with frmMain do
  begin
    Node := lstSubtitles.GetFirst;

    while Assigned(Node) do
    begin
      Data := lstSubtitles.GetNodeData(Node);
      ErrorsTypes := GetError(Node, Node.PrevSibling);
      if (etEmptySubtitle        in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deEmptySubtitle); Inc(TotalErrors); end;
      if (etLinesWithoutLetters  in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deLinesWithoutLetters); Inc(TotalErrors); end;
      if (etOverlapping          in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deOverlapping); Inc(TotalErrors); end;
      if (etBadValues            in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deBadValues); Inc(TotalErrors); end;
      if (etTooLongDuration      in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etWarning, deTooLongDuration); Inc(TotalErrors); end;
      if (etTooShortDuration     in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etWarning, deTooShortDuration); Inc(TotalErrors); end;
      if (etTooLongLine          in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etWarning, deTooLongLine); Inc(TotalErrors); end;
      if (etOverTwoLines         in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etWarning, deOverTwoLines); Inc(TotalErrors); end;
      if (etHearingImpaired      in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deHearingImpaired); Inc(TotalErrors); end;
      if (etTextBeforeColon      in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etWarning, deTextBeforeColon); Inc(TotalErrors); end;
      if (etUnnecessaryDots      in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deUnnecessaryDots); Inc(TotalErrors); end;
      if (etProhibitedCharacter  in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deProhibitedCharacter); Inc(TotalErrors); end;
      if (etRepeatedCharacter    in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deRepeatedCharacter); Inc(TotalErrors); end;
      if (etRepeatedSubtitle     in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deRepeatedSubtitle); Inc(TotalErrors); end;
      if (etOCRErrors            in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deOCRError); Inc(TotalErrors); end;
      if (etOpnDlgSubsOneLine    in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deOpnDlgSubsOneLine); Inc(TotalErrors); end;
      if (etSpaceAfterCustChars  in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deNoSpaceAfterChar); Inc(TotalErrors); end;
      if (etSpaceBeforeCustChars in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deNoSpaceBeforeChar); Inc(TotalErrors); end;
      if (etUnnecessarySpaces    in ErrorsTypes) then begin AddInfoError(Node.Index + 1, etError, deUnnecessarySpaces); Inc(TotalErrors); end;

      if Data.Marked then
      begin
        AddInfoError(Node.Index + 1, etWarning, deMarkedSubtitle);
        Inc(TotalErrors);
      end;
      Node := Node.NextSibling;
    end;
    AddInfoError(-1, etInfo, ifTotalErrors, TotalErrors);
    lstErrors.ScrollIntoView(lstErrors.GetLast, True);
    lstSubtitles.Refresh;
  end;
  btnCheck.Enabled     := True;
  btnFixErrors.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.btnFixErrorsClick(Sender: TObject);
var
  Data, Data2 : PSubtitleItem;
  Node        : PVirtualNode;
  Sib         : PVirtualNode;
  FixedErrors : Integer;
  FixTypeSet  : TFixTypesSet;
  UndoAction  : PUndoAction;
begin
  FixedErrors := 0;
  lstErrors.Clear;
  btnCheck.Enabled     := False;
  btnFixErrors.Enabled := False;
  if (ErrorsToFix.eOCRErrors) and (FileExists(OCRDefFile)) then
    ParseOCRErrors(OCRDefFile);
  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
      
    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      Sib := Node.NextSibling;

      // Delete:
      //   ftEmptySubtitleDeleted
      //   ftRepeatedSubtitle
      //   ftSubDeletedProhibitedCharacter
      //   ftHearingImpairedDeleted

      Data := lstSubtitles.GetNodeData(Node);

      New(UndoAction);
      UndoAction^.UndoActionType := uaFullSubChange;
      UndoAction^.BufferSize     := SizeOf(TLineChange);
      UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := Node.Index;
      UndoAction^.BindToNext     := True;
      PLineChange(UndoAction^.Buffer).SubtitleItem := Data^;      

      FixTypeSet := FixError(Node, Node.PrevSibling, chkConfirm.Checked, Self);

      if (ftEmptySubtitleDeleted in FixTypeSet)          or
         (ftRepeatedSubtitle in FixTypeSet)              or
         (ftSubDeletedProhibitedCharacter in FixTypeSet) or
         (ftHearingImpairedDeleted in FixTypeSet)        then
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                    := uaDeleteLine;
        UndoAction^.BufferSize                        := SizeOf(TLineChange);
        UndoAction^.Buffer                            := AllocMem(UndoAction^.BufferSize);
        UndoAction^.BindToNext                        := True;
        UndoAction^.LineNumber                        := Node.Index;
        PLineChange(UndoAction^.Buffer)^.SubtitleItem := Data^;
        UndoList.Add(UndoAction);
      end else
      begin
        Data2 := lstSubtitles.GetNodeData(Node);
        if (PLineChange(UndoAction^.Buffer).SubtitleItem.InitialTime <> Data2^.InitialTime) or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.FinalTime   <> Data2^.FinalTime)   or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.Text        <> Data2^.Text)        or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.Translation <> Data2^.Translation) or
           (PLineChange(UndoAction^.Buffer).SubtitleItem.ErrorType   <> Data2^.ErrorType)   then        
        UndoList.Add(UndoAction);
      end;

      if ftLinesWithoutLettersDeleted in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfLinesWithoutLetters); Inc(FixedErrors); end;
      if ftEmptySubtitleDeleted in FixTypeSet then
      begin
        AddInfoError(Node.Index + 1, etFixed, dfEmptySubtitle);
        Inc(FixedErrors);
        lstSubtitles.DeleteNode(Node);
      end;
      if ftOverlapping           in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfOverlapping); Inc(FixedErrors); end;
      if ftBadValues             in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfBadValues); Inc(FixedErrors); end;
      if ftOverTwoLinesAdjusted  in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfOverTwoLines); Inc(FixedErrors); end;
      if ftHearingImpairedDeleted in FixTypeSet then
      begin
        AddInfoError(Node.Index + 1, etFixed, dfHearingImpairedAll);
        Inc(FixedErrors);
        lstSubtitles.DeleteNode(Node);
      end;
      if ftHearingImpairedPartDeleted    in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfHearingImpairedPart); Inc(FixedErrors); end;
      if ftTextBeforeColon               in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfTextBeforeColon); Inc(FixedErrors); end;
      if ftUnnecessaryDots               in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfUnnecessaryDots); Inc(FixedErrors); end;
      if ftSubDeletedProhibitedCharacter in FixTypeSet then
      begin
        AddInfoError(Node.Index + 1, etFixed, dfProhibitedCharacter);
        Inc(FixedErrors);
        lstSubtitles.DeleteNode(Node);
      end;
      if ftRepeatedCharacter in FixTypeSet then AddInfoError(Node.Index + 1, etFixed, dfRepeatedCharacter);
      if ftRepeatedSubtitle in FixTypeSet then
      begin
        AddInfoError(Node.Index + 1, etFixed, dfRepeatedSubtitle);
        Inc(FixedErrors);
        lstSubtitles.DeleteNode(Node);
      end;
      if ftOCRErrors               in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfOCRError); Inc(FixedErrors); end;
      if ftOpnDlgOneLineSubDeleted in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfOpnDlgSubsOneLine); Inc(FixedErrors); end;
      if ftSpaceAfterCustChars     in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfNoSpaceAfterChar); Inc(FixedErrors); end;
      if ftSpaceBeforeCustChars    in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfNoSpaceBeforeChar); Inc(FixedErrors); end;
      if ftUnnecessarySpaces       in FixTypeSet then begin AddInfoError(Node.Index + 1, etFixed, dfUnnecessarySpaces); Inc(FixedErrors); end;

      if CancelProcess then
      begin
        CancelProcess        := False;
        btnCheck.Enabled     := True;
        btnFixErrors.Enabled := True;
        OrgModified          := True;
        TransModified        := True;
        AddInfoError(-1, etInfo, ifFixedErrors, FixedErrors);
        lstErrors.ScrollIntoView(lstErrors.GetLast, True);
        if UndoList.Count > 0 then
        begin
          PUndoAction(UndoList.Last)^.BindToNext := False;
          mnuUndo.Enabled := True;
        end;
        lstSubtitles.Refresh;
        RefreshTimes;
        exit;
      end;

      Node := Sib;
    end;

    AddInfoError(-1, etInfo, ifFixedErrors, FixedErrors);
    lstErrors.ScrollIntoView(lstErrors.GetLast, True);
    
    if UndoList.Count > 0 then
    begin
      PUndoAction(UndoList.Last)^.BindToNext := False;
      mnuUndo.Enabled := True;
    end;    
    lstSubtitles.Refresh;
    RefreshTimes;
    OrgModified   := True;
    TransModified := True;
  end;

  btnCheck.Enabled     := True;
  btnFixErrors.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.lstErrorsDblClick(Sender: TObject);
var
  Data : PInfoError;
  Node : PVirtualNode;
begin
  Data := lstErrors.GetNodeData(lstErrors.FocusedNode);
  if Assigned(Data) then
  begin
    if Data.Subtitle > 0 then
    begin
      Node := GetNodeWithIndex(frmMain.lstSubtitles, Data.Subtitle - 1);
      frmMain.lstSubtitles.ScrollIntoView(Node, True);
      frmMain.lstSubtitles.Selected[frmMain.lstSubtitles.FocusedNode] := False;
      frmMain.lstSubtitles.FocusedNode := Node;
      frmMain.lstSubtitles.Selected[Node] := True;
      frmMain.RefreshTimes;
      Close;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.FormActivate(Sender: TObject);
begin
  Refresh;
  btnCheckClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
begin
  SetLanguage;
  Ini := TIniFile.Create(IniRoot);
  try
    chkConfirm.Checked := Ini.ReadBool('Information and Errors', 'Confirm each deletion', True);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmInfoErrors.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Information and Errors', 'Confirm each deletion', chkConfirm.Checked);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
