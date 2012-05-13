unit Undo;

// -----------------------------------------------------------------------------

interface

uses Windows, Messages, SysUtils, Forms, Classes, General, TreeViewHandle,  
     VirtualTrees, USubtitlesFunctions, Functions;

// -----------------------------------------------------------------------------

type
	TUndoActionType = (uaInsertLine,
                     uaDeleteLine,
                     uaTypeChar,
                     uaDeleteChar,
                     uaTimeChange,
                     uaFPSChange,
                     uaFullTextChange,
                     uaRTLFix,
                     uaSwapOrgTrans,
                     uaMarkSubtitle,
                     uaFullSubChange
                     );

	TUndoAction = record
		UndoActionType : TUndoActionType;
    Node           : PVirtualNode;
    LineNumber     : Integer;
		BindToNext     : Boolean;
		BufferSize     : Integer;
		Buffer         : Pointer; //Buffer specific to the type of the change
	end;
  PUndoAction = ^TUndoAction;

  TLineChange = record // For Delete Line and Full Subtitle Change
    SubtitleItem : TSubtitleItem;
  end;
  PLineChange = ^TLineChange;

  TCharChange = record // For Delete Char or Insert Char
    CharNum   : Integer;
    Character : Char;
    Original  : Boolean; //Original or translation
  end;
  PCharChange = ^TCharChange;

  TTimeChange = record
    StartTime : Integer;
    FinalTime : Integer;
  end;
  PTimeChange = ^TTimeChange;

  TFPSChange = record
    InputOrFPS : Boolean; //True for InputFPS, false for FPS
    OldValue   : Single;
  end;
  PFPSChange = ^TFPSChange;

  TFullTextChange = record
    OldText      : String;
    OldTrans     : String;
    OriginalOnly : Boolean; // True for original, false for both (translation and original)
  end;
  PFullTextChange = ^TFullTextChange;

  TRTLFix = record
    ReverseText    : Boolean;
    KeepLinesOrder : Boolean;
    FixPunctuation : Boolean;
    Original       : Boolean;
  end;
  PRTLFix = ^TRTLFix;
  
// -----------------------------------------------------------------------------

procedure ClearUndoList(UndoRedoList: TList);
procedure DetectChangesForUndo(const OldText, NewText: String; const Original: Boolean);
procedure DeleteSelectedWithUndo;
procedure UndoActionSet(OldList, NewList: TList);

// -----------------------------------------------------------------------------

var
  UndoList : TList;
  RedoList : TList;

// -----------------------------------------------------------------------------

implementation

uses formMain;

// -----------------------------------------------------------------------------

procedure ClearUndoList(UndoRedoList: TList);
var
  i: Integer;
begin
  for i := UndoRedoList.Count-1 downto 0 do
  begin
    if PUndoAction(UndoRedoList.Items[i]).UndoActionType = uaDeleteLine then
      System.Finalize(PLineChange(PUndoAction(UndoRedoList.Items[i]).Buffer)^);
    if PUndoAction(UndoRedoList.Items[i]).BufferSize > 0 then
      FreeMem(PUndoAction(UndoRedoList.Items[i])^.Buffer, PUndoAction(UndoRedoList.Items[i])^.BufferSize);
      System.Dispose(UndoRedoList.Items[i]);
  end;
  UndoRedoList.Clear;
end;

// -----------------------------------------------------------------------------

procedure DeleteSelectedWithUndo;
var
  Node       : PVirtualNode;
  Node2      : PVirtualNode;
  Data       : PSubtitleItem;
  UndoAction : PUndoAction;
begin
  with frmMain do
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    mnuUndo.Enabled := True;
    lstSubtitles.BeginUpdate;

    Node := lstSubtitles.GetFirstSelected;
    while Assigned(Node) do
    begin
      Node2 := lstSubtitles.GetNextSelected(Node);
      Data := lstSubtitles.GetNodeData(Node);
      New(UndoAction);
      UndoAction^.BufferSize                        := SizeOf(TLineChange);
      UndoAction^.Buffer                            := AllocMem(UndoAction^.BufferSize);
      UndoAction^.UndoActionType                    := uaDeleteLine;
      UndoAction^.BindToNext                        := True;
      UndoAction^.LineNumber                        := Node.Index;
      PLineChange(UndoAction^.Buffer)^.SubtitleItem := Data^;
      UndoList.Add(UndoAction);
      lstSubtitles.DeleteNode(Node);
      Node := Node2;
    end;
    if UndoList.Count > 0 then
      PUndoAction(UndoList.Last)^.BindToNext := False;    
    
    lstSubtitles.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure DetectChangesForUndo(const OldText, NewText: String; const Original: Boolean);
var
  L1,L2         : integer;
  iLow,ihigh    : integer;
  i             : integer;
  UndoAction    : PUndoAction;
begin
  with frmMain do
  begin
    if OldText <> NewText then
    begin
      L1 := Length(OldText);
      L2 := Length(NewText);
      ClearUndoList(RedoList);
      mnuRedo.Enabled := False;

      if (L1 > 0) and (L2 > 0) then
      begin
        iLow  := 0;
        iHigh := 0;
        while OldText[iLow] = NewText[iLow] do Inc(iLow);
        while OldText[L1-iHigh] = NewText[L2-iHigh] do Inc(iHigh);
        Dec(iLow);

        // The following takes care of the case if there is the same letter twice
        if (L1-iHigh) < iLow then iHigh := L1 - iLow else
        if (L2-iHigh) < iLow then iHigh := L2 - iLow;
      end else
      begin
        iLow  := 0;
        iHigh := 0;
      end;

      for i := L1-iHigh downto iLow+1 do
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                 := uaDeleteChar;
        UndoAction^.Node                           := lstSubtitles.FocusedNode;
        UndoAction^.LineNumber                     := UndoAction^.Node.Index;
        UndoAction^.BindToNext                     := True;
        UndoAction^.BufferSize                     := SizeOf(TCharChange);
        UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
        PCharChange(UndoAction^.Buffer)^.Original  := Original;
        PCharChange(UndoAction^.Buffer)^.CharNum   := i;
        PCharChange(UndoAction^.Buffer)^.Character := OldText[i];
        UndoList.Add(UndoAction);
      end;

      for i := iLow+1 to L2-iHigh do
      begin
        New(UndoAction);
        UndoAction^.UndoActionType                 := uaTypeChar;
        UndoAction^.Node                           := lstSubtitles.FocusedNode;
        UndoAction^.LineNumber                     := UndoAction^.Node.Index;
        UndoAction^.BindToNext                     := True;
        UndoAction^.BufferSize                     := SizeOf(TCharChange);
        UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
        PCharChange(UndoAction^.Buffer)^.Original  := Original;
        PCharChange(UndoAction^.Buffer)^.CharNum   := i;
        PCharChange(UndoAction^.Buffer)^.Character := NewText[i];
        UndoList.Add(UndoAction);
      end;

      PUndoAction(UndoList.Items[UndoList.Count-1])^.BindToNext := False;
      mnuUndo.Enabled := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure UndoActionSet(OldList, NewList: TList);

  procedure UndoSingleAction(const Bind: Boolean);
  var
    UndoAction1  : PUndoAction;
    UndoAction2  : PUndoAction;
    Node         : PVirtualNode;
    Data         : PSubtitleItem;
    tmpStr       : String;
    Time1, Time2 : Integer;
  begin
    with frmMain do
    begin
      UndoAction1 := OldList.Last;
      New(UndoAction2);
      UndoAction2^.BindToNext := Bind;

      case UndoAction1^.UndoActionType of
        uaInsertLine:
          begin
            UnSelectAll(lstSubtitles);
            UndoAction2^.UndoActionType := uaDeleteLine;
            UndoAction2^.BufferSize     := SizeOf(TLineChange);
            UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);
            UndoAction2^.LineNumber     := UndoAction1^.LineNumber;
            Data := lstSubtitles.GetNodeData(GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber));
            PLineChange(UndoAction2^.Buffer)^.SubtitleItem := Data^;

            Node := GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber);
            if Node <> lstSubtitles.GetFirst then
              lstSubtitles.FocusedNode := Node.PrevSibling else
              lstSubtitles.FocusedNode := Node.NextSibling;
            lstSubtitles.DeleteNode(Node);
            lstSubtitles.Selected[lstSubtitles.FocusedNode] := True;
          end;
        uaDeleteLine:
          begin
            UnSelectAll(lstSubtitles);
            UndoAction2^.UndoActionType := uaInsertLine;
            UndoAction2^.Buffer         := nil;
            UndoAction2^.BufferSize     := 0;

            if (lstSubtitles.RootNodeCount = 0) or (UndoAction1.LineNumber = 0) then
              UndoAction2^.Node := lstSubtitles.InsertNode(lstSubtitles.RootNode, amAddChildFirst) else
            begin
              Node := lstSubtitles.GetFirst;
              while Node.Index < Cardinal(UndoAction1^.LineNumber-1) do
                Node := Node.NextSibling;
              UndoAction2^.Node := lstSubtitles.InsertNode(Node, amInsertAfter);
            end;
            UndoAction2^.LineNumber := UndoAction2^.Node.Index;
            lstSubtitles.InvalidateNode(UndoAction2^.Node);

            Data := lstSubtitles.GetNodeData(UndoAction2^.Node);
            Data^.InitialTime := PSubtitleItem(UndoAction1^.Buffer)^.InitialTime;
            Data^.FinalTime   := PSubtitleItem(UndoAction1^.Buffer)^.FinalTime;
            Data^.Text        := PSubtitleItem(UndoAction1^.Buffer)^.Text;
            Data^.Translation := PSubtitleItem(UndoAction1^.Buffer)^.Translation;
            Data^.Marked      := PSubtitleItem(UndoAction1^.Buffer)^.Marked;
            Data^.ErrorType   := PSubtitleItem(UndoAction1^.Buffer)^.ErrorType;

            System.Finalize(PLineChange(UndoAction1^.Buffer)^);
            lstSubtitles.FocusedNode := UndoAction2^.Node;
            lstSubtitles.Selected[lstSubtitles.FocusedNode] := True;
          end;
        uaTypeChar:
          begin
            UndoAction2^.UndoActionType                 := uaDeleteChar;
            UndoAction2^.BufferSize                     := SizeOf(TCharChange);
            UndoAction2^.Buffer                         := AllocMem(UndoAction2^.BufferSize);
            UndoAction2^.LineNumber                     := UndoAction1^.LineNumber;
            UndoAction2^.Node                           := GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber);
            PCharChange(UndoAction2^.Buffer)^.CharNum   := PCharChange(UndoAction1^.Buffer)^.CharNum;
            PCharChange(UndoAction2^.Buffer)^.Character := PCharChange(UndoAction1^.Buffer)^.Character;
            PCharChange(UndoAction2^.Buffer)^.Original  := PCharChange(UndoAction1^.Buffer)^.Original;
            if PCharChange(UndoAction2^.Buffer)^.Original then
            begin
              tmpStr := GetSubText(UndoAction2^.Node);
              Delete(tmpStr, PCharChange(UndoAction2^.Buffer)^.CharNum, 1);
              SetText(UndoAction2^.Node, tmpStr);
            end else
            begin
              tmpStr := GetSubTranslation(UndoAction2^.Node);
              Delete(tmpStr,PCharChange(UndoAction2^.Buffer)^.CharNum, 1);
              SetTranslation(UndoAction2^.Node, tmpStr);
            end;

            lstSubtitles.InvalidateNode(UndoAction2^.Node);
            UnSelectAll(lstSubtitles);
            lstSubtitles.FocusedNode := UndoAction2^.Node;
            lstSubtitles.Selected[lstSubtitles.FocusedNode] := True;

            if GetFocus = mmoSubtitleText.Handle then
              mmoSubtitleText.SelStart := PCharChange(UndoAction2^.Buffer)^.CharNum-1 else
            if GetFocus = mmoTranslation.Handle then
              mmoTranslation.SelStart := PCharChange(UndoAction2^.Buffer)^.CharNum-1;

          end;
        uaDeleteChar:
          begin
            UndoAction2^.UndoActionType                := uaTypeChar;
            UndoAction2^.BufferSize                    := SizeOf(TCharChange);
            UndoAction2^.Buffer                        := AllocMem(UndoAction2^.BufferSize);
            UndoAction2^.LineNumber                    := UndoAction1^.LineNumber;
            UndoAction2^.Node                          := GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber);
            PCharChange(UndoAction2^.Buffer).CharNum   := PCharChange(UndoAction1^.Buffer).CharNum;
            PCharChange(UndoAction2^.Buffer).Character := PCharChange(UndoAction1^.Buffer).Character;
            PCharChange(UndoAction2^.Buffer).Original  := PCharChange(UndoAction1^.Buffer).Original;
            if PCharChange(UndoAction2^.Buffer).Original then
            begin
              tmpStr := GetSubText(UndoAction2^.Node);
              tmpStr := Copy(tmpStr, 1, PCharChange(UndoAction2^.Buffer).CharNum-1) +
                        PCharChange(UndoAction2^.Buffer).Character +
                        Copy(tmpStr, PCharChange(UndoAction2^.Buffer).CharNum, Length(tmpStr)-PCharChange(UndoAction2^.Buffer).CharNum+1);
              SetText(UndoAction2^.Node, tmpStr);
            end else
            begin
              tmpStr := GetSubTranslation(UndoAction2^.Node);
              tmpStr := Copy(tmpStr, 1, PCharChange(UndoAction2^.Buffer).CharNum-1)+
                        PCharChange(UndoAction2^.Buffer).Character +
                        Copy(tmpStr, PCharChange(UndoAction2^.Buffer).CharNum, Length(tmpStr)-PCharChange(UndoAction2^.Buffer).CharNum+1);
              SetTranslation(UndoAction2^.Node,tmpStr);
            end;

            lstSubtitles.InvalidateNode(UndoAction2^.Node);

            if GetFocus = mmoTranslation.Handle then
              mmoTranslation.SelStart := PCharChange(UndoAction2^.Buffer)^.CharNum else
              mmoSubtitleText.SelStart := PCharChange(UndoAction2^.Buffer)^.CharNum;
          end;
        uaTimeChange:
          begin
            UndoAction2^.UndoActionType := uaTimeChange;
            UndoAction2^.LineNumber     := UndoAction1^.LineNumber;
            UndoAction2^.Node           := GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber);
            UndoAction2^.BufferSize     := SizeOf(TTimeChange);
            UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);

            if PTimeChange(UndoAction1^.Buffer).StartTime > -1 then
            begin
              PTimeChange(UndoAction2^.Buffer).StartTime := GetStartTime(UndoAction2^.Node);
              SetStartTime(UndoAction2^.Node, PTimeChange(UndoAction1^.Buffer).StartTime);
            end else
              PTimeChange(UndoAction2^.Buffer).StartTime := -1;

            if PTimeChange(UndoAction1^.Buffer).FinalTime > -1 then
            begin
              PTimeChange(UndoAction2^.Buffer).FinalTime := GetFinalTime(UndoAction2^.Node);
              SetFinalTime(UndoAction2^.Node, PTimeChange(UndoAction1^.Buffer).FinalTime);
            end else
              PTimeChange(UndoAction2^.Buffer).FinalTime := -1;

            lstSubtitles.InvalidateNode(UndoAction2^.Node);
          end;
        uaFPSChange:
          begin
            // It is better to remember the floating point value here rather than
            // the index in the combobox, because the index can change while editing
            // (when the user adds a custom FPS)
            UndoAction2^.UndoActionType                := uaFPSChange;
            UndoAction2^.Node                          := nil;
            UndoAction2^.BufferSize                    := SizeOf(TFPSChange);
            UndoAction2^.Buffer                        := AllocMem(UndoAction2^.BufferSize);
            PFPSChange(UndoAction2^.Buffer).InputOrFPS := PFPSChange(UndoAction1^.Buffer).InputOrFPS;
            if PFPSChange(UndoAction2^.Buffer).InputOrFPS then
            begin
              PFPSChange(UndoAction2^.Buffer).OldValue := OldInputFPS;
              cmbInputFPS.ItemIndex := cmbInputFPS.Items.IndexOf(FormatFloat('#.###', PFPSChange(UndoAction1^.Buffer).OldValue));
              cmbInputFPS.SetFocus;

              Node := lstSubtitles.GetFirst;
              while Assigned(Node) do
              begin
                // ------------ //
                // Initial time //
                // ------------ //
                Time1 := TimeToFrames(GetStartTime(Node), GetInputFPS);
                SetStartTime(Node, FramesToTime(Time1, OldInputFPS));

                // ---------- //
                // Final time //
                // ---------- //
                Time2 := TimeToFrames(GetFinalTime(Node), GetInputFPS);
                SetFinalTime(Node, FramesToTime(Time2, OldInputFPS));

                Node := Node.NextSibling;
              end;

              OldInputFPS := PFPSChange(UndoAction1^.Buffer).OldValue;
            end else
            begin
              PFPSChange(UndoAction2^.Buffer).OldValue := OldFPS;
              cmbFPS.ItemIndex := cmbFPS.Items.IndexOf(FormatFloat('#.###', PFPSChange(UndoAction1^.Buffer).OldValue));
              cmbFPS.SetFocus;

              Node := lstSubtitles.GetFirst;
              while Assigned(Node) do
              begin
                // ------------ //
                // Initial time //
                // ------------ //
                Time1 := TimeToFrames(GetStartTime(Node), OldFPS);
                SetStartTime(Node, FramesToTime(Time1, GetFPS));

                // ---------- //
                // Final time //
                // ---------- //
                Time2 := TimeToFrames(GetFinalTime(Node), OldFPS);
                SetFinalTime(Node, FramesToTime(Time2, GetFPS));

                Node := Node.NextSibling;
              end;

              OldFPS := PFPSChange(UndoAction1^.Buffer).OldValue;
            end;
          end;
        uaFullTextChange:
          begin
            UndoAction2^.UndoActionType := uaFullTextChange;
            UndoAction2^.BufferSize     := SizeOf(TFullTextChange);
            UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);
            UndoAction2^.LineNumber     := UndoAction1^.LineNumber;
            UndoAction2^.Node           := GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber);

            PFullTextChange(UndoAction2^.Buffer)^.OldText      := GetSubText(UndoAction2^.Node);
            PFullTextChange(UndoAction2^.Buffer)^.OldTrans     := GetSubTranslation(UndoAction2^.Node);
            PFullTextChange(UndoAction2^.Buffer)^.OriginalOnly := PFullTextChange(UndoAction1^.Buffer)^.OriginalOnly;

            SetText(UndoAction2^.Node, PFullTextChange(UndoAction1^.Buffer)^.OldText);
            if PFullTextChange(UndoAction2^.Buffer)^.OriginalOnly = False then
              SetTranslation(UndoAction2^.Node, PFullTextChange(UndoAction1^.Buffer)^.OldTrans);
          end;
        uaRTLFix:
          begin
            UndoAction2^.UndoActionType := uaRTLFix;
            UndoAction2^.BufferSize     := SizeOf(TRTLFix);
            UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);
            UndoAction2^.LineNumber     := UndoAction1^.LineNumber;
            UndoAction2^.Node           := GetNodeWithIndex(lstSubtitles, UndoAction2^.LineNumber);
            PRTLFix(UndoAction2^.Buffer)^.ReverseText    := PRTLFix(UndoAction1^.Buffer).ReverseText;
            PRTLFix(UndoAction2^.Buffer)^.KeepLinesOrder := PRTLFix(UndoAction1^.Buffer).KeepLinesOrder;
            PRTLFix(UndoAction2^.Buffer)^.FixPunctuation := PRTLFix(UndoAction1^.Buffer).FixPunctuation;

            if PRTLFix(UndoAction1^.Buffer).ReverseText then
              SetText(UndoAction2^.Node, USubtitlesFunctions.ReverseText(GetSubText(UndoAction2^.Node), PRTLFix(UndoAction1^.Buffer).KeepLinesOrder));
            if PRTLFix(UndoAction1^.Buffer).FixPunctuation then
            begin
              if PRTLFix(UndoAction1^.Buffer).Original then
                SetText(UndoAction2^.Node, FixRTLPunctuation(GetSubText(UndoAction2^.Node))) else
                SetTranslation(UndoAction2^.Node, FixRTLPunctuation(GetSubTranslation(UndoAction2^.Node)));
            end;
          end;
        uaSwapOrgTrans:
          begin
            UndoAction2^.UndoActionType := uaSwapOrgTrans;
            UndoAction2^.BufferSize     := 0;
            UndoAction2^.Buffer         := nil;
            UndoAction2^.Node           := nil;
            SwapOrgTrans(frmMain as TObject, False);
          end;
        uaMarkSubtitle:
          begin
            UndoAction2^.UndoActionType := uaMarkSubtitle;
            UndoAction2^.BufferSize     := 0;
            UndoAction2^.Buffer         := nil;
            UndoAction2^.Node           := GetNodeWithIndex(lstSubtitles, UndoAction1^.LineNumber);
            UndoAction2^.LineNumber     := UndoAction1^.LineNumber;

            Data := lstSubtitles.GetNodeData(UndoAction2^.Node);
            Data.Marked := not Data.Marked;
          end;
        uaFullSubChange:
          begin
            UndoAction2^.UndoActionType := uaFullSubChange;
            UndoAction2^.BufferSize     := SizeOf(TLineChange);
            UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);
            UndoAction2^.Node           := GetNodeWithIndex(lstSubtitles, UndoAction1^.LineNumber);
            UndoAction2^.LineNumber     := UndoAction2^.Node.Index;
            Data := lstSubtitles.GetNodeData(UndoAction2^.Node);
            PLineChange(UndoAction2.Buffer)^.SubtitleItem.InitialTime := Data^.InitialTime;
            PLineChange(UndoAction2.Buffer)^.SubtitleItem.FinalTime   := Data^.FinalTime;
            PLineChange(UndoAction2.Buffer)^.SubtitleItem.Text        := Data^.Text;
            PLineChange(UndoAction2.Buffer)^.SubtitleItem.Translation := Data^.Translation;
            PLineChange(UndoAction2.Buffer)^.SubtitleItem.Marked      := Data^.Marked;
            PLineChange(UndoAction2.Buffer)^.SubtitleItem.ErrorType   := Data^.ErrorType;
            Data^.InitialTime := PLineChange(UndoAction1^.Buffer)^.SubtitleItem.InitialTime;
            Data^.FinalTime   := PLineChange(UndoAction1^.Buffer)^.SubtitleItem.FinalTime;
            Data^.Text        := PLineChange(UndoAction1^.Buffer)^.SubtitleItem.Text;
            Data^.Translation := PLineChange(UndoAction1^.Buffer)^.SubtitleItem.Translation;
            Data^.Marked      := PLineChange(UndoAction1^.Buffer)^.SubtitleItem.Marked;
            Data^.ErrorType   := PLineChange(UndoAction1^.Buffer)^.SubtitleItem.ErrorType;
          end;
      end;
      NewList.Add(UndoAction2);

      if UndoAction1^.BufferSize > 0 then
        System.FreeMem(UndoAction1^.Buffer, UndoAction1^.BufferSize);

      OldList.Remove(UndoAction1);
      Dispose(UndoAction1);
    end;
  end;

begin
  with frmMain do
  begin
    while (OldList.Count > 1) and (PUndoAction(OldList.Items[OldList.Count-2]).BindToNext) do
      UndoSingleAction(True);
    if OldList.Count > 0 then UndoSingleAction(False);
    mnuUndo.Enabled := UndoList.Count > 0;
    mnuRedo.Enabled := RedoList.Count > 0;
    lstSubtitles.Refresh;
    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

end.




