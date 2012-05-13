unit TreeViewHandle;

interface

uses Windows, SysUtils, Classes, VirtualTrees, USubtitleAPI,
     USubtitlesFunctions, ClipBrd, FastStrings;

// -----------------------------------------------------------------------------

type
  TErrorType = (etLinesWithoutLetters,
                etEmptySubtitle,
                // ----------------------
                etOverlapping,
                etBadValues,
                etTooLongDuration,
                etTooShortDuration,
                etTooLongLine,
                etOverTwoLines,
                // ----------------------
                etHearingImpaired,
                etTextBeforeColon,
                etUnnecessaryDots,
                etProhibitedCharacter,
                // ----------------------
                etRepeatedCharacter,
                etRepeatedSubtitle,
                etOCRErrors,
                // ----------------------
                etOpnDlgSubsOneLine,
                etSpaceAfterCustChars,
                etSpaceBeforeCustChars,
                etUnnecessarySpaces
                );
  TErrorTypeSet = set of TErrorType;
  TSubtitleItem = record
    InitialTime, FinalTime : Integer; // MILLISECONDS
    Text, Translation      : String;
    Marked                 : Boolean;
    ErrorType              : set of TErrorType;
  end;
  PSubtitleItem = ^TSubtitleItem;

// -----------------------------------------------------------------------------

function CharSetToCodePage(ciCharset: UINT): Cardinal;
function StringToWideStringEx(const S: string; CodePage: Word): WideString;
procedure AddArrayItems(Translation: Boolean = False);
procedure UpdateArray(Translated: Boolean = False);
procedure DeleteSelectedNodes;
procedure UnSelectAll(Sender: TVirtualStringTree);
function GetNodeWithIndex(Sender: TVirtualStringTree; NdIndex: Integer): PVirtualNode;
function InsertNode(After: Boolean = True; InitialTime: Integer = -1; FinalTime: Integer = -1; Text: String = ''; Translation: String = ''; Select: Boolean = True): PVirtualNode;
// ------------------
procedure SetStartTime(Node: PVirtualNode; StartTime: Integer);
procedure SetFinalTime(Node: PVirtualNode; FinalTime: Integer);
procedure SetText(Node: PVirtualNode; Text: String);
procedure SetTranslation(Node: PVirtualNode; Text: String);
procedure AddError(Node: PVirtualNode; Error: TErrorTypeSet);
// ------------------
function GetStartTime(Node: PVirtualNode): Integer;
function GetFinalTime(Node: PVirtualNode): Integer;
function GetSubText(Node: PVirtualNode): String;
function GetSubTranslation(Node: PVirtualNode): String;
// ------------------
procedure CopyNodesToClipBoard;
procedure PasteNodesFromClipBoard;
// ------------------
procedure SetDelay(Delay: Integer; OnlyToSelected: Boolean = False);
procedure ZeroFunction;
procedure SortSubtitles;
procedure ReverseText(KeepLinesOrder: Boolean);
procedure TextEffect(Effect: Byte; Param_1, Param_2: Integer);
procedure SwapOrgTrans(Sender: TObject; AddUndo: Boolean = True);

// -----------------------------------------------------------------------------

implementation

uses formMain, General, Functions, Undo;

// -----------------------------------------------------------------------------

{ Windows.pas doesn't declare TranslateCharsetInfo() correctly. }
function TranslateCharsetInfo(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall; external gdi32 name 'TranslateCharsetInfo';

function CharSetToCodePage(ciCharset: UINT): Cardinal;
var
  C: TCharsetInfo;
begin
  TranslateCharsetInfo(PDWORD(ciCharset), C, TCI_SRCCHARSET);
  Result := C.ciACP;
end;

// -----------------------------------------------------------------------------

function StringToWideStringEx(const S: string; CodePage: Word): WideString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PChar(S), InputLength, nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PChar(S), InputLength, PWideChar(Result), OutputLength);
end;

// -----------------------------------------------------------------------------

procedure AddArrayItems(Translation: Boolean = False);
var
  Data      : PSubtitleItem;
  Node      : PVirtualNode;
  NdIndex   : Integer;
  RootNodes : Integer;
begin
  frmMain.lstSubtitles.BeginUpdate;
  RootNodes := frmMain.lstSubtitles.RootNodeCount;
  if (RootNodes < SubtitleAPI.SubtitleCount) then
    frmMain.lstSubtitles.RootNodeCount := SubtitleAPI.SubtitleCount;
  Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Translation then
    begin
      NdIndex := Node.Index;
      if NdIndex < SubtitleAPI.SubtitleCount then
      begin
        Data.Translation := SubtitleAPI.GetText(Node.Index);
        if (Data.InitialTime = 0) and (Data.FinalTime = 0) then
        begin
          Data.InitialTime := SubtitleAPI.GetInitialTime(Node.Index);
          Data.FinalTime   := SubtitleAPI.GetFinalTime(Node.Index);
        end;
        Data.ErrorType := [];
      end;
    end else
    begin
      SubtitleAPI.GetSubtitle(Node.Index, Data.InitialTime, Data.FinalTime, Data.Text);
      Data.ErrorType := [];
    end;
    Node := frmMain.lstSubtitles.GetNextSibling(Node);
  end;
  if (Translation = False) and (frmMain.mnuTranslatorMode.Checked) then
    SetUntranslated;
  frmMain.lstSubtitles.EndUpdate;
end;

// -----------------------------------------------------------------------------

procedure UpdateArray(Translated: Boolean = False);
var
  Data: PSubtitleItem;
  Node: PVirtualNode;
begin
  SubtitleAPI.CreateNewSubtitle;
  Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Translated = False then
    begin
      if Data.Text <> '' then
        SubtitleAPI.AddSubtitle(Data.InitialTime, Data.FinalTime, Data.Text);
    end else
    begin
      if Data.Translation <> '' then
        SubtitleAPI.AddSubtitle(Data.InitialTime, Data.FinalTime, Data.Translation);
    end;
    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

procedure DeleteSelectedNodes;
var
  Node : PVirtualNode;
  i    : Integer;
begin
  with frmMain do
  begin
    if Assigned(lstSubtitles.FocusedNode) then
    begin
      if ConfirmDelete = True then
      begin
        if MsgBox(QuestionMsg[05], BTN_YES, BTN_NO, '', MB_ICONWARNING, frmMain) = 2 then
          exit;
      end;
      i := lstSubtitles.GetFirstSelected.Index;
      DeleteSelectedWithUndo;
      Node := GetNodeWithIndex(lstSubtitles, i);
      if not Assigned(Node) and (lstSubtitles.TotalCount > 0) then
        Node := GetNodeWithIndex(lstSubtitles, i-1);
      if Assigned(Node) then
      begin
        lstSubtitles.FocusedNode := Node;
        lstSubtitles.Selected[Node] := True;
      end;
    end;
    OrgModified   := True;
    TransModified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure UnSelectAll(Sender: TVirtualStringTree);
var
  Node: PVirtualNode;
begin
  Node := Sender.GetFirst;
  while Assigned(Node) do
  begin
    Sender.Selected[Node] := False;
    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

function GetNodeWithIndex(Sender: TVirtualStringTree; NdIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
  NodeIndex: Integer;
begin
  Result := nil;
  Node := Sender.GetFirst;
  while Assigned(Node) do
  begin
    NodeIndex := Node.Index;
    if NodeIndex = NdIndex then
    begin
      Result := Node;
      exit;
    end;
    Node := Node.NextSibling;
  end;
end;                                   

// -----------------------------------------------------------------------------

function InsertNode(After: Boolean = True; InitialTime: Integer = -1; FinalTime: Integer = -1; Text: String = ''; Translation: String = ''; Select: Boolean = True): PVirtualNode;
var
  Node: PVirtualNode;
  Time: Integer;
begin
  Result := nil;
  with frmMain do
  begin
    if Translation = '' then
      Translation := UnTranslatedSub;
    if lstSubtitles.Enabled then
    begin
      if Assigned(lstSubtitles.FocusedNode) then
      begin
        if After then
        begin
          lstSubtitles.InsertNode(lstSubtitles.FocusedNode, amInsertAfter);
          Node := lstSubtitles.FocusedNode.NextSibling;
        end else
        begin
          lstSubtitles.InsertNode(lstSubtitles.FocusedNode, amInsertBefore);
          Node := lstSubtitles.FocusedNode.PrevSibling;
        end;
        lstSubtitles.Selected[lstSubtitles.FocusedNode] := False;
        lstSubtitles.FocusedNode := Node;
        if Select then
          lstSubtitles.Selected[Node] := True;
        if After then
        begin
          if InitialTime = -1 then
            SetStartTime(Node, GetFinalTime(Node.PrevSibling) + 1) else
            SetStartTime(Node, InitialTime);
          if FinalTime = -1 then
            SetFinalTime(Node, GetStartTime(Node) + 1000) else
            SetFinalTime(Node, FinalTime);
        end else
        begin
          if InitialTime = -1 then
          begin
            Time := GetStartTime(Node.NextSibling) - 1001;
            if Time < 0 then Time := 0;
            SetStartTime(Node, Time);
          end else
            SetStartTime(Node, InitialTime);
          if FinalTime = -1 then
            SetFinalTime(Node, GetStartTime(Node) + 1000) else
            SetFinalTime(Node, FinalTime);
        end;
      end else
      begin
        lstSubtitles.InsertNode(nil, amInsertAfter);
        Node := lstSubtitles.GetFirst;
        lstSubtitles.FocusedNode := Node;
        if Select then
          lstSubtitles.Selected[Node] := True;
        if InitialTime = -1 then
          SetStartTime(Node, 0) else
          SetStartTime(Node, InitialTime);
        if FinalTime = -1 then
          SetFinalTime(Node, 1000) else
          SetFinalTime(Node, FinalTime);
      end;

      SetText(Node, Text);
      SetTranslation(Node, Translation);

      frmMain.OrgModified   := True;
      frmMain.TransModified := True;
      Result := Node;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetStartTime(Node: PVirtualNode; StartTime: Integer);
var
  Data: PSubtitleItem;
begin
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
    begin
      Data.InitialTime := StartTime;
      frmMain.OrgModified   := True;
      frmMain.TransModified := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetFinalTime(Node: PVirtualNode; FinalTime: Integer);
var
  Data: PSubtitleItem;
begin
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
    begin
      Data.FinalTime := FinalTime;
      frmMain.OrgModified   := True;
      frmMain.TransModified := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetText(Node: PVirtualNode; Text: String);
var
  Data: PSubtitleItem;
begin
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
    begin
      Data.Text := Text;
      frmMain.OrgModified := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetTranslation(Node: PVirtualNode; Text: String);
var
  Data: PSubtitleItem;
begin
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
    begin
      Data.Translation := Text;
      frmMain.TransModified := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure AddError(Node: PVirtualNode; Error: TErrorTypeSet);
var
  Data: PSubtitleItem;
begin
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
    begin
      if Error = [] then
        Data.ErrorType := [] else
        Data.ErrorType := Data.ErrorType + Error;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetStartTime(Node: PVirtualNode): Integer;
var
  Data: PSubtitleItem;
begin
  Result := 0;
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
      Result := Data.InitialTime;
  end;
end;

// -----------------------------------------------------------------------------

function GetFinalTime(Node: PVirtualNode): Integer;
var
  Data: PSubtitleItem;
begin
  Result := 0;
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
      Result := Data.FinalTime;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubText(Node: PVirtualNode): String;
var
  Data: PSubtitleItem;
begin
  Result := '';
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
      Result := Data.Text;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubTranslation(Node: PVirtualNode): String;
var
  Data: PSubtitleItem;
begin
  Result := '';
  if Assigned(Node) then
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if Assigned(Data) then
      Result := Data.Translation;
  end;
end;

// -----------------------------------------------------------------------------

procedure CopyNodesToClipBoard;
var
  Data : PSubtitleItem;
  Node : PVirtualNode;
  Text : String;
begin
  Text := '';
  try
    Node := frmMain.lstSubtitles.GetFirstSelected;
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    Text := IntToStr(Data.InitialTime) + '||' + IntToStr(Data.FinalTime) + '||' + ReplaceEnters(Data.Text, '\~') + '||' + ReplaceEnters(Data.Translation, '\~');
    Node := frmMain.lstSubtitles.GetNextSelected(Node);
    while Assigned(Node) do
    begin
      Data := frmMain.lstSubtitles.GetNodeData(Node);
      Text := Text + #13#10 + IntToStr(Data.InitialTime) + '||' + IntToStr(Data.FinalTime) + '||' + ReplaceEnters(Data.Text, '\~') + '||' + ReplaceEnters(Data.Translation, '\~');
      Node := frmMain.lstSubtitles.GetNextSelected(Node);
    end;
  finally
    ClipBoard.AsText := Text;
  end;
end;

// -----------------------------------------------------------------------------

procedure PasteNodesFromClipBoard;
  procedure SetParams(Node: PVirtualNode; tmpText: String);
  var
    PosIt: Integer;
  begin
    Posit := Pos('||', tmpText);
    SetStartTime(Node, StrToIntDef(Copy(tmpText, 1, Posit-1), 0));
    SetFinalTime(Node, StrToIntDef(Copy(tmpText, Posit+2, SmartPos('||', tmpText, True, PosIt + 2)-(PosIt+2)), 0));
    Posit := SmartPos('||', tmpText, True, PosIt + 2);
    SetText(Node, ReplaceString(Copy(tmpText, PosIt+2, SmartPos('||', tmpText, True, PosIt + 2)-(PosIt+2)), '\~', #13#10));
    Posit := SmartPos('||', tmpText, True, PosIt + 2);
    SetTranslation(Node, ReplaceString(Copy(tmpText, PosIt+2, Length(tmpText)), '\~', #13#10));
  end;
var
  Node       : PVirtualNode;
  Text       : String;
  PosIt      : Integer;
  tmpText    : String;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;
  Node := nil;
  
  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    ClipBoard.Open;
    Text := ClipBoard.AsText;
    ClipBoard.Close;

    PosIt := Pos(#13#10, Text);
    while PosIt > 0 do
    begin
      tmpText := Copy(Text, 1, PosIt - 1);
      if StringCount('||', tmpText) = 3 then
      begin
        Node := InsertNode(True, -1, -1, '', '', False);
        SetParams(Node, tmpText);

        New(UndoAction);
        UndoAction^.UndoActionType := uaInsertLine;
        UndoAction^.LineNumber     := Node.Index;
        UndoAction^.Node           := Node;
        UndoAction^.BindToNext     := True;
        UndoAction^.Buffer         := nil;
        UndoAction^.BufferSize     := 0;
        UndoList.Add(UndoAction);
      end;
      Text := Copy(Text, PosIt + 2, Length(Text));
      PosIt := Pos(#13#10, Text);
    end;
    if StringCount('||', Text) = 3 then
    begin
      Node := InsertNode(True, -1, -1, '', '', False);
      SetParams(Node, Text);

      New(UndoAction);
      UndoAction^.UndoActionType := uaInsertLine;
      UndoAction^.LineNumber     := Node.Index;
      UndoAction^.Node           := Node;
      UndoAction^.BindToNext     := False;
      UndoAction^.Buffer         := nil;
      UndoAction^.BufferSize     := 0;
      UndoList.Add(UndoAction);      
    end;
    if Assigned(Node) then
    begin
      UnSelectAll(frmMain.lstSubtitles);
      frmMain.lstSubtitles.FocusedNode := Node;
      frmMain.lstSubtitles.Selected[Node] := True;
    end;
  end;
  frmMain.mnuUndo.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure SetDelay(Delay: Integer; OnlyToSelected: Boolean = False);
var
  Node       : PVirtualNode;
  StartTime  : Integer;
  FinalTime  : Integer;
  UndoAction : PUndoAction;
begin
  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      if ((OnlyToSelected) and (lstSubtitles.Selected[Node] = True)) or (OnlyToSelected = False) then
      begin
        StartTime := GetStartTime(Node);
        FinalTime := GetFinalTime(Node);

        New(UndoAction);
        UndoAction^.UndoActionType := uaTimeChange;
        UndoAction^.BufferSize     := SizeOf(TTimeChange);
        UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
        UndoAction^.Node           := Node;
        UndoAction^.LineNumber     := Node.Index;
        UndoAction^.BindToNext     := True;
        PTimeChange(UndoAction^.Buffer)^.StartTime := StartTime;
        PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
        UndoList.Add(UndoAction);

        if StartTime + Delay > 0 then
          StartTime := StartTime + Delay else
          StartTime := 0;
        if FinalTime + Delay > 0 then
          FinalTime := FinalTime + Delay else
          FinalTime := 0;
        SetStartTime(Node, StartTime);
        SetFinalTime(Node, FinalTime);
      end;
      Node := Node.NextSibling;
    end;
    if UndoList.Count > 0 then
      PUndoAction(UndoList.Last)^.BindToNext := False;
    mnuUndo.Enabled := True;
    OrgModified   := True;
    TransModified := True;
    lstSubtitles.Refresh;
  end;
end;

// -----------------------------------------------------------------------------

procedure ZeroFunction;
var
  Node : PVirtualNode;
begin
  with frmMain do
  begin
    Node := lstSubtitles.GetFirst;
    if (Assigned(Node)) and (lstSubtitles.SelectedCount > 1) then
      SetDelay(-GetStartTime(Node), True);
  end;
end;

// -----------------------------------------------------------------------------

procedure SortSubtitles;
var
  i,a        : Integer;
  Data       : PSubtitleItem;
  Node       : PVirtualNode;
  tmp        : TSubtitleItem;
  List       : array of TSubtitleItem;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;

  SetLength(List, frmMain.lstSubtitles.RootNodeCount);

  Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    List[Node.Index] := Data^;
    Node := Node.NextSibling;
  end;

  for i := 0 to High(List) do
  begin
    for a := i to High(List) do
    begin
      if List[i].InitialTime > List[a].InitialTime then
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                 := uaTimeChange;
        UndoAction^.BufferSize                     := SizeOf(TTimeChange);
        UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
        UndoAction^.Node                           := GetNodeWithIndex(frmMain.lstSubtitles, i);
        UndoAction^.LineNumber                     := i;
        UndoAction^.BindToNext                     := True;
        PTimeChange(UndoAction^.Buffer)^.StartTime := List[i].InitialTime;
        PTimeChange(UndoAction^.Buffer)^.FinalTime := List[i].FinalTime;
        UndoList.Add(UndoAction);

        New(UndoAction);
        UndoAction^.UndoActionType := uaFullTextChange;
        UndoAction^.Node           := GetNodeWithIndex(frmMain.lstSubtitles, i);
        UndoAction^.LineNumber     := i;
        UndoAction^.BindToNext     := True;
        UndoAction^.BufferSize     := SizeOf(TFullTextChange);
        UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
        PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;
        PFullTextChange(UndoAction^.Buffer)^.OldText      := List[i].Text;
        PFullTextChange(UndoAction^.Buffer)^.OldTrans     := List[i].Translation;
        UndoList.Add(UndoAction);

        New(UndoAction);
        UndoAction^.UndoActionType                 := uaTimeChange;
        UndoAction^.BufferSize                     := SizeOf(TTimeChange);
        UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
        UndoAction^.Node                           := GetNodeWithIndex(frmMain.lstSubtitles, a);
        UndoAction^.LineNumber                     := a;
        UndoAction^.BindToNext                     := True;
        PTimeChange(UndoAction^.Buffer)^.StartTime := List[a].InitialTime;
        PTimeChange(UndoAction^.Buffer)^.FinalTime := List[a].FinalTime;
        UndoList.Add(UndoAction);

        New(UndoAction);
        UndoAction^.UndoActionType := uaFullTextChange;
        UndoAction^.Node           := GetNodeWithIndex(frmMain.lstSubtitles, a);
        UndoAction^.LineNumber     := a;
        UndoAction^.BindToNext     := True;
        UndoAction^.BufferSize     := SizeOf(TFullTextChange);
        UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
        PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;
        PFullTextChange(UndoAction^.Buffer)^.OldText      := List[a].Text;
        PFullTextChange(UndoAction^.Buffer)^.OldTrans     := List[a].Translation;
        UndoList.Add(UndoAction);

        tmp := List[i];
        List[i] := List[a];
        List[a] := tmp;        
      end;
    end;
  end;

  if UndoList.Count > 0 then
    PUndoAction(UndoList.Last)^.BindToNext := False;
      
  Node := frmMain.lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    Data.InitialTime := List[Node.Index].InitialTime;
    Data.FinalTime   := List[Node.Index].FinalTime;
    Data.Text        := List[Node.Index].Text;
    Data.Translation := List[Node.Index].Translation;
    Node := Node.NextSibling;
  end;

  frmMain.mnuUndo.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure ReverseText(KeepLinesOrder: Boolean);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;

  Node := frmMain.lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaRTLFix;
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    UndoAction^.BufferSize     := SizeOf(TRTLFix);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PRTLFix(UndoAction^.Buffer)^.ReverseText    := True;
    PRTLFix(UndoAction^.Buffer)^.KeepLinesOrder := KeepLinesOrder;
    PRTLFix(UndoAction^.Buffer)^.FixPunctuation := False;
    PRTLFix(UndoAction^.Buffer)^.Original       := True;

    SetText(Node, USubtitlesFunctions.ReverseText(GetSubText(Node), KeepLinesOrder));

    Node := frmMain.lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);
    UndoList.Add(UndoAction);    
  end;
  frmMain.mnuUndo.Enabled := True;                   
end;

// -----------------------------------------------------------------------------

procedure TextEffect(Effect: Byte; Param_1, Param_2: Integer);
  function TextEffect2(InitialTime: Integer; FinalTime: Integer; Text: String; Effect: Byte; Param1: Integer; Param2: Integer): PVirtualNode;
  var
    ITime      : Integer;
    FTime      : Integer;
    CurTime    : Integer;
    CharDur    : Integer;
    Pos1, Pos2 : Byte;
    SumLen     : Byte;
    s          : String;
    UndoAction : PUndoAction;
  begin
    ITime  := InitialTime;
    FTime  := FinalTime;
    Result := nil;

    case Effect of
      EffectFlash:
        begin
          repeat
            CurTime := ITime + Param1;
            if CurTime > FTime then CurTime := FTime;
            Result := InsertNode(True, ITime, CurTime, Text);

            New(UndoAction);
            UndoAction^.UndoActionType := uaInsertLine;
            UndoAction^.Node           := Result;
            UndoAction^.LineNumber     := Result.Index;
            UndoAction^.BindToNext     := True;
            UndoAction^.Buffer         := nil;
            UndoAction^.BufferSize     := 0;
            UndoList.Add(UndoAction);

            ITime := ITime + Param1 + Param2;
          until ITime >= FTime;
          exit;
        end;
      EffectType:
        begin
          // search for summary length of strings
          SumLen := 0;

          s    := Copy(Text, 1, Length(Text));
          Pos1 := Pos(#13, s);
          Pos2 := Pos(#10, s);

          while (Pos1 > 0) or (Pos2 > 0) do
          begin
            if Pos1 > 0 then
              SumLen := SumLen + Pos1-1 else
              SumLen := SumLen + Pos2-1;
            if Pos2 > 0 then
              s := Copy(s, Pos2+1, Length(s)) else
              s := Copy(s, Pos1+1, Length(s));
            Pos1 := Pos(#13, s);
            Pos2 := Pos(#10, s);
          end;

          SumLen := SumLen + Length(s);

          // now produce type
          CharDur := Round((Ftime - ITime)/SumLen);
          SumLen  := Length(Text);

          Pos1 := 0;
          while (Pos1 < SumLen) do
          begin
            Pos1 := Pos1 + 1;
            if (CompareStr(Copy(Text, Pos1, 1), #13) <> 0) and
               (CompareStr(Copy(Text, Pos1, 1), #10) <> 0) and
               (CompareStr(Copy(Text, Pos1, 1), ' ') <> 0) then
            begin
              s := Copy(Text, 1, Pos1);
              Result := InsertNode(True, ITime, ITime+CharDur, s);

              New(UndoAction);
              UndoAction^.UndoActionType := uaInsertLine;
              UndoAction^.Node           := Result;
              UndoAction^.LineNumber     := Result.Index;
              UndoAction^.BindToNext     := True;
              UndoAction^.Buffer         := nil;
              UndoAction^.BufferSize     := 0;
              UndoList.Add(UndoAction);

              ITime := ITime + CharDur;
            end;
          end;
          SetFinalTime(Result, FinalTime);
          exit;
        end;
    end;
  end;
var
  FocNode    : PVirtualNode;
  NewNode    : PVirtualNode;
  Data       : PSubtitleItem;
  UndoAction : PUndoAction;
begin
  if GetSubText(frmMain.lstSubtitles.FocusedNode) <> '' then
  begin
    ClearUndoList(RedoList);
    frmMain.mnuRedo.Enabled := False;

    FocNode := frmMain.lstSubtitles.FocusedNode;
    NewNode := TextEffect2(GetStartTime(frmMain.lstSubtitles.FocusedNode),
                           GetFinalTime(frmMain.lstSubtitles.FocusedNode),
                           GetSubText(frmMain.lstSubtitles.FocusedNode),
                           Effect,
                           Param_1,
                           Param_2);

    Data := frmMain.lstSubtitles.GetNodeData(FocNode);
    
    New(UndoAction);
    UndoAction^.UndoActionType := uaDeleteLine;
    UndoAction^.Node           := FocNode;
    UndoAction^.LineNumber     := FocNode.Index;
    UndoAction^.BindToNext     := False;
    UndoAction^.BufferSize     := SizeOf(TLineChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PLineChange(UndoAction^.Buffer)^.SubtitleItem := Data^;
    UndoList.Add(UndoAction);

    frmMain.lstSubtitles.DeleteNode(FocNode);
    if NewNode <> nil then
    begin
      frmMain.lstSubtitles.FocusedNode       := NewNode;
      frmMain.lstSubtitles.Selected[NewNode] := True;
    end;
    
    frmMain.mnuUndo.Enabled := True;
    frmMain.lstSubtitles.SetFocus;
    frmMain.RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure SwapOrgTrans(Sender: TObject; AddUndo: Boolean = True);
var
  tmpTrans   : String;
  Node       : PVirtualNode;
  OrgII      : Integer;
  UndoAction : PUndoAction;
begin
  with frmMain do
  begin
    if AddUndo then
    begin
      ClearUndoList(RedoList);
      mnuRedo.Enabled := False;

      New(UndoAction);
      UndoAction^.UndoActionType := uaSwapOrgTrans;
      UndoAction^.Node           := nil;
      UndoAction^.BufferSize     := 0;
      UndoAction^.Buffer         := nil;
      UndoAction^.BindToNext     := False;
      UndoList.Add(UndoAction);
    end;

    OrgII := cmbOrgCharset.ItemIndex;
    cmbOrgCharset.ItemIndex := cmbTransCharset.ItemIndex;
    cmbTransCharset.ItemIndex := OrgII;
    cmbOrgCharsetSelect(Sender);
    cmbTransCharsetSelect(Sender);
    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      tmpTrans := GetSubTranslation(Node);
      SetTranslation(Node, GetSubText(Node));
      SetText(Node, tmpTrans);
      Node := Node.NextSibling;
    end;
    tmpTrans      := TransFile;
    TransFile     := OrgFile;
    OrgFile       := tmpTrans;
    OrgModified   := True;
    TransModified := True;

    if AddUndo then mnuUndo.Enabled := True;
    SetFormCaption;
    RefreshTimes;
    lstSubtitles.Refresh;
  end;
end;

// -----------------------------------------------------------------------------

end.
