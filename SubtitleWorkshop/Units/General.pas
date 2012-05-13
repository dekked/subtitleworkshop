unit General;

interface

uses Forms, Windows, Classes, SysUtils, Controls, StdCtrls, VirtualTrees, 
     USubtitleAPI, TreeViewHandle, Mask, USubtitlesFunctions, Menus, IniFiles,
     VideoPreview, FastStrings, ShortCuts;

const
  ID_COMPANY      = 'URUSoft';
  ID_PROGRAM      = 'Subtitle Workshop';
//  ID_VERSION      = '2.51';
  ID_VERSION      = '2.51a';
  ID_EMAIL        = 'DeK@urusoft.net';
  ID_WEBPAGE      = 'http://www.urusoft.net';
  ID_DONATIONPAGE = 'https://www.paypal.com/xclick/business=descoins%40adinet.com.uy&item_name=URUSoft&no_note=1&tax=0&currency_code=USD';
  ID_ININAME      = 'SubtitleWorkshop.ini';
  ID_UPDATEINI    = 'http://www.urusoft.net/inis/swupdate.ini';
  ID_DLLNAME      = 'SubtitleAPI.dll';
  ID_DLLDIR       = 'SubtitleAPI';
  ID_CFPDIR       = 'CustomFormats';
  ID_STPEXT       = '.stp';
  ID_SRFEXT       = '.srf';
  ID_OCREXT       = '.ocr';
  ID_SHORTCUTS    = 'shortcuts.key';
  SpecialChars  : set of Char = ['¡', '!', '"', '#', '$', '%', '&', '''', '(', ')', '+', '-', '.', '/',
                                 ':', ';', '<', '=', '>', '¿', '?', '@', '[', '\', ']', '^', '_', '`',
                                 '{', '|', '}', '~', '€', '‚', 'ƒ', '„', '…', '†', '‡', 'ˆ', '‰', '‹',
                                 '‘', '’', '“', '”', '•', '-', '—', '˜', '™', '›', #13, #10, ' '];
  ID_TIMEOVERLAPPRECISION = 35;
  EffectFlash = 1;
  EffectType  = 2;

// -----------------------------------------------------------------------------

procedure LoadSubtitle(const FileName: String; FPS: Single; SubtitleFormat: Integer = 0; TranslatedFile: Boolean = False; EnableControls: Boolean = True);
procedure LoadPlainText(const FileName: String);
procedure LoadSRF(const FileName: String); // URUSoft Subtitle Report file
// ---------------
procedure ReadFromFile(FileName: String; SubFormat: Integer; Times, Texts: Boolean);
procedure LoadProject(const FileName: String);
function CloseSub: Boolean;
function AskToSaveFile: Boolean;
procedure SetFormCaption;
procedure SetUntranslated;
procedure SetTranslatorMode(const Flag: Boolean; SaveColsWidth: Boolean = True; ShowLeftPanel: Boolean = True);
procedure CommandLineProcess(Cli: String);
procedure GetCustomFormatInfo(const FileName: String; var Name, Extension, NewLineChar, TimeStructure: String; var Time, Frames: Boolean; var FPS: Single; Lines: TStrings);
procedure SaveCustomFormat(SavePathNameExt: String; FormatLines: TStrings; Time, Frames: Boolean; TimeStruct: String; FPS: Single; NewLineChar: String);
function SaveFile(FileName: WideString; FormatIndex: Integer; FPS: Single): Boolean;

// -----------------------------------------------------------------------------

var
  SubtitleAPI : TSubtitleAPI;
  // Root of the main ini file
  IniRoot : String;
  // ------------------ //
  //  Language strings  //
  // ------------------ //
  BTN_OK            : String;
  BTN_CANCEL        : String;
  BTN_BROWSE        : String;
  BTN_APPLY         : String;
  BTN_EXIT          : String;
  BTN_YES           : String;
  BTN_NO            : String;
  AllSupportedFiles : String;
  // -------------------- //
  //  Arrays of messages  //
  // -------------------- //
  ErrorMsg     : array[1..20] of String;
  QuestionMsg  : array[1..09] of String;
  InfoMsg      : array[1..11] of String;
  IEMsgBoxes   : array[1..05] of String;
  ErrorReports : array[1..20] of String;

// -----------------------------------------------------------------------------

implementation

uses formMain, formSaveAs, Functions, Undo, InfoErrorsFunctions;

// -----------------------------------------------------------------------------

procedure LoadSubtitle(const FileName: String; FPS: Single; SubtitleFormat: Integer = 0; TranslatedFile: Boolean = False; EnableControls: Boolean = True);
  const Exts: array[1..11] of String = ('asf', 'avi', 'mp4', 'divx', 'mkv', 'mpg', 'mpeg', 'm1v', 'ogm', 'qt', 'wmv');
  procedure FixOverlap;
  var
    Node: PVirtualNode;
  begin
    with frmMain do
    begin
      Node := lstSubtitles.GetFirst.NextSibling;
      while Assigned(Node) do
      begin
        if GetFinalTime(Node.PrevSibling) = GetStartTime(Node) then
          SetStartTime(Node, GetStartTime(Node) + ID_TIMEOVERLAPPRECISION);
        Node := Node.NextSibling;
      end;
    end;
  end;
var
  TempFileName  : String;
  MovieFileName : String;
  Ini           : TIniFile;
  i             : Integer;
begin
  if TranslatedFile = False then
  begin
    if CloseSub = False then exit;
    frmMain.lstSubtitles.Clear;
  end;

  if LowerCase(ExtractFileExt(FileName)) = ID_SRFEXT then
  begin
    Ini := TIniFile.Create(FileName);
    try
      if Ini.SectionExists('URUSoft Subtitle Report File') then LoadSRF(FileName);
    finally
      Ini.Free;
    end;
  end;

  with frmMain do
  begin
    Dec(SubtitleFormat, 1);
    if SubtitleFormat < 0 then SubtitleFormat := 0;
    if SubtitleFormat > SubtitleAPI.FormatsCount then SubtitleFormat := 0;

    if SubtitleAPI.LoadSubtitle(FileName, FPS, SubtitleFormat) = True then
    begin
      dlgLoadFile.InitialDir := ExtractFilePath(FileName);
      AddToRecent(FileName);
      //frmMain.lstSubtitles.BeginUpdate;
      AddArrayItems(TranslatedFile);
      if TranslatedFile = False then
      begin
        lstSubtitles.FocusedNode := lstSubtitles.GetFirst;
        lstSubtitles.Selected[lstSubtitles.GetFirst] := True;
        if (SubtitleAPI.IsFrameBased(SubtitleAPI.CurrentFormatIndex)) and (ForceWorkWithTime = False) then
        begin
          cmbMode.ItemIndex := 1;
          FormatType        := ftFrames;
        end else
        begin
          cmbMode.ItemIndex := 0;
          FormatType        := ftTime;
        end;
        OrgFormat   := SubtitleAPI.CurrentFormatIndex;
        OrgFile     := FileName;
        OrgModified := False;
      end else
      begin
        if TransModified = True then
        begin
          if AskToSaveFile then
          begin
            TransFormat   := SubtitleAPI.CurrentFormatIndex;
            TransFile     := FileName;
            TransModified := False;
          end;
        end else
        begin
          TransFormat   := SubtitleAPI.CurrentFormatIndex;
          TransFile     := FileName;
          TransModified := False;
        end;
      end;

      SubtitleAPI.ClearSubtitles; // So we don't waste memory

      ClearUndoList(UndoList);      
      ClearUndoList(RedoList);
      mnuUndo.Enabled := False;
      mnuRedo.Enabled := False;
      OldInputFPS := FPS;
      OldFPS := FPS;
      frmMain.cmbFPS.ItemIndex := frmMain.cmbInputFPS.ItemIndex;
      SetFormCaption;
      frmMain.RefreshTimes;
      if EnableControls then
        frmMain.EnableCtrls(True);

      // Fix one unit overlap at load
      if FixOneUnitOverlap then FixOverlap;

      if AutoSearchMovie then
      begin
        for i := Low(Exts) to High(Exts) do
        begin
          if StringCount('.', ExtractFileName(FileName)) = 1 then
            MovieFileName := ExtractFilePath(FileName) + Copy(ExtractFileName(FileName), 1, LastDelimiter('.', ExtractFileName(FileName))-1) + '.' + Exts[i] else
          begin
            TempFileName  := Copy(FileName, 1, LastDelimiter('.', FileName)-1);
            MovieFileName := TempFileName + '.' + Exts[i];
            
            if FileExists(MovieFileName) = False then
            begin
              while (StringCount('.', MovieFileName) > 1) do
              begin
                if FileExists(MovieFileName) then break;
                MovieFileName := ExtractFilePath(TempFileName) + Copy(ExtractFileName(TempFileName), 1, LastDelimiter('.', ExtractFileName(TempFileName))-1) + '.' + Exts[i];
                TempFileName := Copy(TempFileName, 1, LastDelimiter('.', TempFileName)-1);
              end;
            end;
          end;
          if FileExists(MovieFileName) = True then break;
        end;
        
        if FileExists(MovieFileName) then
        begin
          if LoadMovie(MovieFileName) then
            SetVideoPreviewMode(True);
          UpdateSubtitlesPos;
        end;
      end;

    end else
    begin
      if InvalidFPlainText = False then
      begin
        if SubtitleFormat = 0 then
          MsgBox(Format(ErrorMsg[03],[FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain) else
          MsgBox(Format(ErrorMsg[04],[FileName, SubtitleAPI.GetFormatName(SubtitleFormat)]), BTN_OK, '', '', MB_ICONERROR, frmMain);
        SetFormCaption;
      end else
      LoadPlainText(FileName);
    end;
  end;
  if (MarkOnLoad) or (FixOnLoad) then
  begin
    frmMain.Refresh;
    if (MarkOnLoad) and (FixOnLoad) then
    begin
      FixErrors;
      CheckMarkErrors;
    end;
    if (FixOnLoad) and (MarkOnLoad = False) then
      FixErrors;
    if (MarkOnLoad) and (FixOnLoad = False) then
      CheckMarkErrors;
  end;
//  frmMain.lstSubtitles.EndUpdate;
end;

// -----------------------------------------------------------------------------

procedure LoadPlainText(const FileName: String);
var
  PlainTxt    : TStringList;
  Node        : PVirtualNode;
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  InitialTime := 0;
  FinalTime   := 1000;
  PlainTxt    := TStringList.Create;
  try
    PlainTxt.LoadFromFile(FileName);
    with frmMain do
    begin
      for i := PlainTxt.Count-1 downto 0 do
      begin
        PlainTxt[i] := Trim(PlainTxt[i]);
        if PlainTxt[i] = '' then
          PlainTxt.Delete(i);
      end;
      lstSubtitles.RootNodeCount := PlainTxt.Count;
      dlgLoadFile.InitialDir := ExtractFilePath(FileName);
      
      Node := lstSubtitles.GetFirst;
      while Assigned(Node) do
      begin
        SetText(Node, PlainTxt[Node.Index]);
        SetText(Node, PlainTxt[Node.Index]);
        SetStartTime(Node, InitialTime);
        SetFinalTime(Node, FinalTime);
        AddError(Node, []);
        InitialTime := FinalTime + 1;
        FinalTime := FinalTime + 1001;
        Node := lstSubtitles.GetNextSibling(Node);
      end;
      OrgModified := False;
      TransModified := False;

      OldInputFPS := GetInputFPS;
      OldFPS      := GetFPS;
      SetFormCaption;
      RefreshTimes;
      EnableCtrls(True);
    end;
  finally
    PlainTxt.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadSRF(const FileName: String);
var
  Ini  : TIniFile;
  i    : Integer;
  Data : PSubtitleItem;
begin
  Ini := TIniFile.Create(FileName);
  try
    LoadSubtitle(Ini.ReadString('URUSoft Subtitle Report File', 'File', ''), GetFPS);
    for i := 0 to Ini.ReadInteger('URUSoft Subtitle Report File', 'Count', 0)-1 do
    begin
      Data        := frmMain.lstSubtitles.GetNodeData(GetNodeWithIndex(frmMain.lstSubtitles, Ini.ReadInteger('URUSoft Subtitle Report File', IntToStr(i), 0)));
      Data.Marked := True;
    end;
    frmMain.lstSubtitles.Refresh;
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ReadFromFile(FileName: String; SubFormat: Integer; Times, Texts: Boolean);
var
  Node       : PVirtualNode;
  NIndex     : Integer;
  UndoAction : PUndoAction;
begin
  with frmMain do
  begin
    if (Times = False) and (Texts = False) then exit;

    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    if SubtitleAPI.LoadSubtitle(FileName, GetInputFPS, SubFormat) then
    begin
      Node := lstSubtitles.GetFirst;
      while Assigned(Node) do
      begin
        NIndex := Node.Index;
        if NIndex < SubtitleAPI.SubtitleCount then
        begin
          if Times = True then
          begin
            New(UndoAction);
            UndoAction^.UndoActionType                 := uaTimeChange;
            UndoAction^.BufferSize                     := SizeOf(TTimeChange);
            UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
            UndoAction^.Node                           := Node;
            UndoAction^.LineNumber                     := Node.Index;
            UndoAction^.BindToNext                     := Node <> lstSubtitles.GetLast;
            PTimeChange(UndoAction^.Buffer)^.StartTime := GetStartTime(Node);
            PTimeChange(UndoAction^.Buffer)^.FinalTime := GetFinalTime(Node);
            UndoList.Add(UndoAction);

            SetStartTime(Node, SubtitleAPI.GetInitialTime(NIndex));
            SetFinalTime(Node, SubtitleAPI.GetFinalTime(NIndex));
            AddError(Node, []);
          end;
          if Texts = True then
          begin
            New(UndoAction);
            UndoAction^.UndoActionType                        := uaFullTextChange;
            UndoAction^.BufferSize                            := SizeOf(TFullTextChange);
            UndoAction^.Buffer                                := AllocMem(UndoAction^.BufferSize);
            UndoAction^.Node                                  := Node;
            UndoAction^.LineNumber                            := Node.Index;
            UndoAction^.BindToNext                            := Node <> lstSubtitles.GetLast;
            PFullTextChange(UndoAction^.Buffer)^.OldText      := GetSubText(Node);
            PFullTextChange(UndoAction^.Buffer)^.OldTrans     := '';
            PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
            UndoList.Add(UndoAction);
                      
            SetText(Node, SubtitleAPI.GetText(NIndex));
          end;
        end;
        Node := Node.NextSibling;
      end;
      OldInputFPS := GetInputFPS;
      OldFPS      := GetFPS;
    end else
    begin
      if (SubFormat = 0) then
        MsgBox(Format(ErrorMsg[03],[FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain) else
        MsgBox(Format(ErrorMsg[04],[FileName, SubtitleAPI.GetFormatName(SubFormat)]), BTN_OK, '', '', MB_ICONERROR, frmMain);
    end;
    
    SubtitleAPI.ClearSubtitles;
    mnuUndo.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadProject(const FileName: String);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    if FileExists(Ini.ReadString('Subtitle files', 'Original', '')) then
    begin
      CloseSub;
      LoadSubtitle(Ini.ReadString('Subtitle files', 'Original', ''), GetInputFPS);
      if FileExists(Ini.ReadString('Subtitle files', 'Translated', '')) then
      begin
        SetTranslatorMode(True);
        LoadSubtitle(Ini.ReadString('Subtitle files', 'Translated', ''), GetInputFPS, 0, True);
      end;
    end;
    if FileExists(Ini.ReadString('Movie file', 'Movie', '')) then
    begin
      SetVideoPreviewMode(True);
      LoadMovie(Ini.ReadString('Movie file', 'Movie', ''));
      if Ini.ReadInteger('Movie file', 'Position', 0) >= 0 then
        SetVideoPos(Ini.ReadInteger('Movie file', 'Position', 0));
    end;
    if Ini.ReadInteger('Other', 'Focused node', 0) >= 0 then
    begin
      UnSelectAll(frmMain.lstSubtitles);
      frmMain.lstSubtitles.FocusedNode := GetNodeWithIndex(frmMain.lstSubtitles, Ini.ReadInteger('Other', 'Focused node', 0));
      frmMain.lstSubtitles.ScrollIntoView(frmMain.lstSubtitles.FocusedNode,True);
      frmMain.lstSubtitles.Selected[frmMain.lstSubtitles.FocusedNode] := True;
      frmMain.RefreshTimes;
    end;
    frmMain.AddToRecent(FileName);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

function CloseSub: Boolean;
begin
  with frmMain do
  begin
    if AskToSaveFile = True then
    begin
      Result := True;
      SubtitleAPI.CloseSubtitle;
      lstSubtitles.Clear;
      tmeShow.Time     := 0;
      tmeHide.Time     := 0;
      tmeDuration.Time := 0;
      tmeShow.Clear;
      tmeHide.Clear;
      tmeDuration.Clear;
      lblText.Caption := LabelText + ':';
      lblTranslation.Caption := LabelTranslation + ':';
      mmoSubtitleText.Clear;
      mmoTranslation.Clear;
      OrgFile           := '';
      OrgFormat         := 0;
      OrgModified       := False;
      TransFile         := '';
      TransFormat       := 0;
      TransModified     := False;
      subSubtitle.Text  := '';
      OldInputFPS       := 0;
      OldFPS            := 0;
      frmMain.Caption   := ID_PROGRAM;
      Application.Title := frmMain.Caption;
      EnableCtrls(False);
      ClearUndoList(UndoList);
      ClearUndoList(RedoList);
      mnuUndo.Enabled := False;
      mnuRedo.Enabled := False;
      SetLength(frmMain.SyncPointsArray, 0);
    end else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

function AskToSaveFile: Boolean;
var
  MsgTxt: String;
begin
  Result := True;
  with frmMain do
  begin
    if mnuTranslatorMode.Checked = False then
      MsgTxt := Format(QuestionMsg[01], [OrgFile]) else
      MsgTxt := Format(QuestionMsg[03], [OrgFile]);

      // Original
      if (AskToSave) and (OrgModified) then
      begin
        case MsgBox(MsgTxt, BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONWARNING, frmMain) of
        1:
          begin
            UpdateArray;
            if OrgFile <> '' then
            begin
              if SaveFile(OrgFile, OrgFormat, GetFPS) then
                OrgModified := False else
              begin
                Result := False;
                exit;
              end;
            end else
            begin // If original file is new...
              frmSaveAs := TfrmSaveAs.Create(Application);
              frmSaveAs.SaveTranslation := False;
              frmSaveAs.ShowModal;
              frmSaveAs.Free;
            end;
            Result := True;
          end;
        2: Result := True;
        3:
          begin
            Result := False;
            exit;
          end;
        end;
      end;

      // Translated
      if (AskToSave) and (TransModified) and (mnuTranslatorMode.Checked) then
      begin
        case MsgBox(Format(QuestionMsg[04], [TransFile]), BTN_YES, BTN_NO, BTN_CANCEL, MB_ICONWARNING, frmMain) of
        1:
          begin
            UpdateArray(True);
            if TransFile <> '' then
            begin
              SubtitleAPI.SaveSubtitle(TransFile, TransFormat, GetFPS);
              TransModified := False;
            end else
            begin // If translated file is new...
              frmSaveAs := TfrmSaveAs.Create(Application);
              frmSaveAs.SaveTranslation := True;
              frmSaveAs.ShowModal;
              frmSaveAs.Free;
            end;
            Result := True;
          end;
        2: Result := True;
        3:
          begin
            Result := False;
            exit;
          end;
        end;
      end;

      SubtitleAPI.ClearSubtitles;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetFormCaption;
begin
  with frmMain do
  begin
    if mnuTranslatorMode.Checked then
    begin
      if (OrgFile <> '') and (TransFile <> '') then
        frmMain.Caption := ID_PROGRAM + ' - ' + ExtractFileName(OrgFile) + ' / ' + ExtractFileName(TransFile) else
      if (OrgFile <> '') and (TransFile = '') then
        frmMain.Caption := ID_PROGRAM + ' - ' + ExtractFileName(OrgFile) + ' / ?' else
      if (OrgFile = '') and (TransFile <> '') then
        frmMain.Caption := ID_PROGRAM + ' - ? / ' + ExtractFileName(TransFile) else
      if (OrgFile = '') and (TransFile = '') then
        frmMain.Caption := ID_PROGRAM;
    end else
    begin
      if OrgFile <> '' then
        frmMain.Caption := ID_PROGRAM + ' - ' + ExtractFileName(OrgFile) else
        frmMain.Caption := ID_PROGRAM;
    end;
    Application.Title := frmMain.Caption;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetUntranslated;
var
  Node    : PVirtualNode;
  Trans   : String;
  PrevMod : Boolean;
begin
  with frmMain do
  begin
    PrevMod := TransModified;
    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      Trans := Trim(GetSubTranslation(Node));
      if (Trans = '') then
        SetTranslation(Node, UntranslatedSub);
      Node := Node.NextSibling;
    end;
    TransModified := PrevMod;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetTranslatorMode(const Flag: Boolean; SaveColsWidth: Boolean = True; ShowLeftPanel: Boolean = True);
var
  Ini: TIniFile;
begin
  with frmMain do
  begin
    Ini := TIniFile.Create(IniRoot);
    try
      mnuTranslatorMode.Checked := Flag;
      if (Flag = True) then
      begin
        // Show new column
        lstSubtitles.Header.Columns[4].Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible];

        if SaveColsWidth then
        begin
          // Save columns width for normal mode
          Ini.WriteInteger('Columns width (normal mode)', '1', lstSubtitles.Header.Columns[0].Width);
          Ini.WriteInteger('Columns width (normal mode)', '2', lstSubtitles.Header.Columns[1].Width);
          Ini.WriteInteger('Columns width (normal mode)', '3', lstSubtitles.Header.Columns[2].Width);
          Ini.WriteInteger('Columns width (normal mode)', '4', lstSubtitles.Header.Columns[3].Width);
        end;

        // Read translator mode's column width
        lstSubtitles.Header.Columns[0].Width := Ini.ReadInteger('Columns width (translator mode)', '1', 40);
        lstSubtitles.Header.Columns[1].Width := Ini.ReadInteger('Columns width (translator mode)', '2', 80);
        lstSubtitles.Header.Columns[2].Width := Ini.ReadInteger('Columns width (translator mode)', '3', 80);
        lstSubtitles.Header.Columns[3].Width := Ini.ReadInteger('Columns width (translator mode)', '4', 340);
        lstSubtitles.Header.Columns[4].Width := Ini.ReadInteger('Columns width (translator mode)', '5', 340);

        SetUntranslated;
        lblTranslation.Caption := LabelTranslation + ':';
        lblTranslation.Enabled := InterfaceEnabled;
        mmoTranslation.Enabled := InterfaceEnabled;

        if mmoTranslation.Visible = False then
          mmoTranslation.Show;
        if lblTranslation.Visible = False then
          lblTranslation.Show;
        
        // Work with the menus...
        mnuLoadSubtitle.ShortCut := 0;
        mnuLoadSubtitle.Visible  := False;
        mnuSaveFile.ShortCut     := 0;
        mnuSaveFile.Visible      := False;
        mnuSaveFileAs.ShortCut   := 0;
        mnuSaveFileAs.Visible    := False;
        // ---
        mnuTranslatorSave.Visible := True;
        mnuLoad.Visible           := True;

        mnuLoadProject.ShortCut      := menuLoadProjSC;
        mnuLoadOriginal.ShortCut     := menuLoadOrgSC;
        mnuLoadTranslated.ShortCut   := menuLoadTransSC;
        mnuSaveProject.ShortCut      := menuSaveProjSC;
        mnuSaveOriginal.ShortCut     := menuSaveOrgSC;
        mnuSaveTranslated.ShortCut   := menuSaveTransSC;
        mnuSaveOriginalAs.ShortCut   := menuSaveOrgAsSC;
        mnuSaveTranslatedAs.ShortCut := menuSaveTransAsSC;

        if InterfaceEnabled then
          cmbTransCharset.Enabled := True;
        SetTranslationCtrlsPositions;
      end else
      if (Flag = False) then
      begin
        lstSubtitles.Header.Columns[4].Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark];

        if SaveColsWidth then
        begin
          // Save columns width for translator mode
          Ini.WriteInteger('Columns width (translator mode)', '1', lstSubtitles.Header.Columns[0].Width);
          Ini.WriteInteger('Columns width (translator mode)', '2', lstSubtitles.Header.Columns[1].Width);
          Ini.WriteInteger('Columns width (translator mode)', '3', lstSubtitles.Header.Columns[2].Width);
          Ini.WriteInteger('Columns width (translator mode)', '4', lstSubtitles.Header.Columns[3].Width);
          Ini.WriteInteger('Columns width (translator mode)', '5', lstSubtitles.Header.Columns[4].Width);
        end;

        // Read normal columns width
        lstSubtitles.Header.Columns[0].Width := Ini.ReadInteger('Columns width (normal mode)', '1', 40);
        lstSubtitles.Header.Columns[1].Width := Ini.ReadInteger('Columns width (normal mode)', '2', 80);
        lstSubtitles.Header.Columns[2].Width := Ini.ReadInteger('Columns width (normal mode)', '3', 80);
        lstSubtitles.Header.Columns[3].Width := Ini.ReadInteger('Columns width (normal mode)', '4', 345);

        mmoTranslation.Clear;
        mmoTranslation.Hide;
        lblTranslation.Hide;
        SetTranslationCtrlsPositions;

        // Menus...
        mnuLoadSubtitle.ShortCut := menuLoadSubSC;
        mnuLoadSubtitle.Visible  := True;
        mnuSaveFile.ShortCut     := menuSaveFileSC;
        mnuSaveFile.Visible      := True;
        mnuSaveFileAs.ShortCut   := menuSaveFileAsSC;
        mnuSaveFileAs.Visible    := True;
        // ---
        mnuTranslatorSave.Visible := False;
        mnuLoad.Visible           := False;

        mnuLoadProject.ShortCut      := 0;
        mnuLoadOriginal.ShortCut     := 0;
        mnuLoadTranslated.ShortCut   := 0;
        mnuSaveProject.ShortCut      := 0;
        mnuSaveOriginal.ShortCut     := 0;
        mnuSaveTranslated.ShortCut   := 0;
        mnuSaveOriginalAs.ShortCut   := 0;
        mnuSaveTranslatedAs.ShortCut := 0;
         
        cmbTransCharset.Enabled := False;
      end;
    finally
      Ini.Free;
    end;
  end;
  SetFormCaption;
end;

// -----------------------------------------------------------------------------

procedure CommandLineProcess(Cli: String);
  procedure ParamConvertFile(Param: String);
  var
    Input     : String;
    Output    : String;
    Format    : Integer;
    InputFPS  : Single;
    OutputFPS : Single;
  begin
    Param    := Copy(Param, 10, Length(Param)-1);
    Input    := Copy(Param, 1, Pos('/', Param)-1);               // Input file
    Param    := Copy(Param, Pos('/', Param) + 1, Length(Param));
    Output   := Copy(Param, 1, Pos('/',Param)-1);                // Output file
    Param     := Copy(Param, Pos('/', Param)+1, Length(Param));
    Format    := SubtitleAPI.GetFormatIndex(Copy(Param, 1, Pos('/', Param)-1)); // Output format
    Param     := Copy(Param, Pos('/', Param)+1, Length(Param));
    InputFPS  := StrToFloatDef(Copy(Param, 1, Pos('/', Param)-1), 25); // Input FPS
    Param     := Copy(Param, Pos('/', Param)+1, Length(Param));
    OutputFPS := StrToFloatDef(Copy(Param, 1, Length(Param)-1), 25); // Output FPS

    SubtitleAPI.LoadSubtitle(Input, InputFPS);
    SubtitleAPI.SaveSubtitle(Output, Format, OutputFPS);
    SubtitleAPI.CloseSubtitle;
    
    //ExitProcess(0);
    Application.Terminate;
  end;

  procedure ParamDelayFile(Param: String);
  var
    Input  : String;
    Output : String;
    FPS    : Single;
    Delay  : Integer;
  begin
    Param  := Copy(Param, 8, Length(Param)-1);
    Input  := Copy(Param, 1, Pos('/', Param)-1);
    Param  := Copy(Param, Pos('/', Param) + 1, Length(Param));
    Output := Copy(Param, 1, Pos('/',Param)-1);
    Param  := Copy(Param, Pos('/', Param)+1, Length(Param));
    FPS    := StrToFloatDef(Copy(Param, 1, Pos('/', Param)-1), 25);
    Param  := Copy(Param, Pos('/', Param)+1, Length(Param));
    Delay  := StrToIntDef(Copy(Param, 1, Length(Param)-1), 0);

    SubtitleAPI.LoadSubtitle(Input, FPS);
    try
      if Delay <> 0 then
        SubtitleAPI.SetAbsoluteDelay(Delay);
    finally
      SubtitleAPI.SaveSubtitle(Output, SubtitleAPI.CurrentFormatIndex, FPS);
      SubtitleAPI.CloseSubtitle;
    end;
    //ExitProcess(0);
    Application.Terminate;
  end;

  procedure SaveFormatsToFile;
  var
    i       : Integer;
    Formats : TStringList;
  begin
    Formats := TStringList.Create;
    Formats.Add('This are the formats you should use for command line parameters.');
    Formats.Add('Do NOT modify a single character because it will not work, note that it is not case sensitive.');
    Formats.Add('');
    try
      for i := 0 to SubtitleAPI.FormatsCount do
        Formats.Add(SubtitleAPI.GetFormatName(i));
    finally
      Formats.SaveToFile(ExtractFilePath(Application.ExeName) + 'SupportedFormats.txt');
      Formats.Free;
    end;
    Application.Terminate;
  end;

var
  Param: String;
begin
  if (AnsiUpperCase(Copy(Cli, 1, 6)) = '/OPEN(') or (FileExists(Cli)) then
  begin
    frmMain.Show;
    frmMain.Refresh;
    if FileExists(Cli) = False then
      Param := Copy(Cli, 7, Length(Cli) - 7) else
      Param := Cli;
    if (LowerCase(ExtractFileExt(Param)) = ID_STPEXT) then
      LoadProject(Param) else
    if (LowerCase(ExtractFileExt(Param)) = ID_SRFEXT) then
      LoadSRF(Param) else
    if (FileExists(Param)) then
      LoadSubtitle(Param, GetInputFPS);      
  end else
  if AnsiUpperCase(Copy(Cli, 1, 9)) = '/CONVERT(' then
    ParamConvertFile(Cli) else
  if AnsiUpperCase(Copy(Cli, 1, 7)) = '/DELAY(' then
    ParamDelayFile(Cli) else
  if AnsiUpperCase(Copy(Cli, 1, 20)) = '/GETSUPPORTEDFORMATS' then
    SaveFormatsToFile;
end;

// -----------------------------------------------------------------------------

procedure GetCustomFormatInfo(const FileName: String; var Name, Extension, NewLineChar, TimeStructure: String; var Time, Frames: Boolean; var FPS: Single; Lines: TStrings);
var
  Format : TStringList;
  i      : Integer;
  IsLine : Boolean;
  Ini    : TIniFile;
begin
  if FileExists(FileName) = True then
  begin
    if Assigned(Lines) = False then
      Lines := TStringList.Create;
    Lines.Clear;
    Format := TStringList.Create;
    Ini := TIniFile.Create(FileName);
    try
      Format.LoadFromFile(FileName);
      IsLine := False;
      for i := 0 to Format.Count-1 do
      begin
        if IsLine then Lines.Add(Format[i]);
        if AnsiLowerCase(Format[i]) = '[format text]' then IsLine := True;
      end;

      Name          := Ini.ReadString('Information', 'Name', 'Custom format');
      Extension     := Ini.ReadString('Information', 'Extension', '*.xxx');
      NewLineChar   := Ini.ReadString('Information', 'New line char', '|');
      TimeStructure := Ini.ReadString('Information', 'Time structure', 'hh:mm:ss,zzz');
      Time          := StrToBool(Ini.ReadString('Information', 'Time', 'True'));
      Frames        := not Time;
      FPS           := Ini.ReadFloat('Information', 'FPS', 25);

    finally
      Format.Free;
      Ini.Free;
    end;
  end;  
end;

// -----------------------------------------------------------------------------

procedure SaveCustomFormat(SavePathNameExt: String; FormatLines: TStrings; Time, Frames: Boolean; TimeStruct: String; FPS: Single; NewLineChar: String);
var
  i, a, Pad       : Integer;
  RepeatPartStart : Integer;
  EndRepeat       : Integer;
  tmpLines        : TStringList;
  RepeatPart      : TStringList;
  tmpSubtitle     : TStringList;
  tmpStr          : String;
begin
  if SubtitleAPI.SubtitleCount > 0 then
  begin
    tmpLines    := TStringList.Create;
    RepeatPart  := TStringList.Create;
    tmpSubtitle := TStringList.Create;

    RepeatPartStart := 0;
    EndRepeat       := 0;
    tmpLines.Text   := FormatLines.Text;

    for i := tmpLines.Count-1 downto 0 do
      if tmpLines[i] = '' then
        tmpLines[i] := '{BlankLine}';

    while (Pos('**', tmpLines.Text) > 0) and (Pos('!*', tmpLines.Text) > Pos('**', tmpLines.Text)) do
      tmpLines.Text := Copy(tmpLines.Text, 0, Pos('**', tmpLines.Text)-1) + Copy(tmpLines.Text, Pos('!*',tmpLines.Text) + 2, Length(tmpLines.Text));

    for i := tmpLines.Count-1 downto 0 do
      if tmpLines[i] = '' then
        tmpLines.Delete(i);

    for i := tmpLines.Count-1 downto 0 do
    begin
      if tmpLines[i] = '{BlankLine}' then
        tmpLines[i] := '' else
      if SmartPos('{repeatsub}', tmpLines[i], False) > 0 then
        RepeatPartStart := i;
      if SmartPos('{endrepeat}', tmpLines[i], False) > 0 then
        EndRepeat := i;
    end;

    for i := RepeatPartStart+1 to EndRepeat-1 do
      RepeatPart.Add(tmpLines[i]);

    for i := 0 to RepeatPartStart-1 do
      tmpSubtitle.Add(ReplaceString(tmpLines[i], '{asterisk}', '*'));

    for i := 0 to SubtitleAPI.SubtitleCount-1 do
    begin

      for a := 0 to RepeatPart.Count-1 do
      begin
        tmpSubtitle.Add(RepeatPart[a]);
        tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{asterisk}', '*');

        if (SmartPos('{SubCount', tmpSubtitle[tmpSubtitle.Count-1], False) > 0) and (Pos('}',tmpSubtitle[tmpSubtitle.Count-1]) > SmartPos('{SubCount', tmpSubtitle[tmpSubtitle.Count-1], False)) then
        begin
          if SmartPos('{SubCount,', tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
          begin
            tmpStr := Copy(tmpSubtitle[tmpSubtitle.Count-1], SmartPos('{SubCount,', tmpSubtitle[tmpSubtitle.Count-1], False) + 10, Length(tmpSubtitle[tmpSubtitle.Count-1])-1);
            if IsInteger(Copy(tmpStr, 1, Pos('}', tmpStr)-1)) then
              Pad := StrToInt(Copy(tmpStr, 1, Pos('}',tmpStr)-1)) else
              Pad := 0;
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{SubCount,' + IntToStr(Pad) + '}', PadLeft(IntToStr(i+1), '0', Pad));
          end else
          tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{SubCount}', IntToStr(i+1));
        end;

        if SmartPos('{swStart}', tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
        begin
          if Time = True then
          begin
            if AnsiUpperCase(TimeStruct) <> 'MS' then
              tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swStart}', TimeToString(SubtitleAPI.GetInitialTime(i), TimeStruct)) else
              tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swStart}', IntToStr(SubtitleAPI.GetInitialTime(i)));
          end else
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swStart}', IntToStr(TimeToFrames(SubtitleAPI.GetInitialTime(i), FPS)));
        end;

        if SmartPos('{swEnd}', tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
        begin
          if Time = True then
          begin
            if AnsiUpperCase(TimeStruct) <> 'MS' then
              tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swEnd}', TimeToString(SubtitleAPI.GetFinalTime(i), TimeStruct)) else
              tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swEnd}', IntToStr(SubtitleAPI.GetFinalTime(i)));
          end else
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swEnd}', IntToStr(TimeToFrames(SubtitleAPI.GetFinalTime(i), FPS)));
        end;

        if SmartPos('{swFrameStart', tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
        begin
          if SmartPos('{swFrameStart,',tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
          begin
            tmpStr := Copy(tmpSubtitle[tmpSubtitle.Count-1], SmartPos('{swFrameStart,', tmpSubtitle[tmpSubtitle.Count-1], False) + 14, Length(tmpSubtitle[tmpSubtitle.Count-1])-1);
            if IsInteger(Copy(tmpStr, 1, Pos('}',tmpStr)-1)) then
              Pad := StrToInt(Copy(tmpStr, 1, Pos('}',tmpStr)-1)) else
              Pad := 0;
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swFrameStart,' + IntToStr(Pad) + '}', PadLeft(IntToStr(TimeToFrames(SubtitleAPI.GetInitialTime(i), FPS)), '0', Pad));
          end else
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swFrameStart}',IntToStr(TimeToFrames(SubtitleAPI.GetInitialTime(i), FPS)));
        end;

        if SmartPos('{swFrameEnd', tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
        begin
          if SmartPos('{swFrameEnd,', tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
          begin
            tmpStr := Copy(tmpSubtitle[tmpSubtitle.Count-1], SmartPos('{swFrameEnd,', tmpSubtitle[tmpSubtitle.Count-1], False) + 12,Length(tmpSubtitle[tmpSubtitle.Count-1])-1);
            if IsInteger(Copy(tmpStr, 1, Pos('}',tmpStr)-1)) then
              Pad := StrToInt(Copy(tmpStr, 1, Pos('}',tmpStr)-1)) else
              Pad := 0;
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swFrameEnd,' + IntToStr(Pad) + '}', PadLeft(IntToStr(TimeToFrames(SubtitleAPI.GetFinalTime(i), FPS)), '0', Pad));
          end else
            tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swFrameEnd}', IntToStr(TimeToFrames(SubtitleAPI.GetFinalTime(i), FPS)));
        end;

        if SmartPos('{swText}',tmpSubtitle[tmpSubtitle.Count-1], False) > 0 then
        begin
          if AnsiLowerCase(NewLineChar) = '[enter]' then
            tmpStr := SubtitleAPI.GetText(i) else
            tmpStr := ReplaceEnters(SubtitleAPI.GetText(i), NewLineChar);
          tmpSubtitle[tmpSubtitle.Count-1] := ReplaceString(tmpSubtitle[tmpSubtitle.Count-1], '{swText}', tmpStr);
        end;
      end;

    end;

    for i := EndRepeat+1 to tmpLines.Count-1 do
      tmpSubtitle.Add(ReplaceString(tmpLines[i], '{asterisk}', '*'));

    tmpSubtitle.SaveToFile(SavePathNameExt);

    tmpLines.Free;
    RepeatPart.Free;
    tmpSubtitle.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SaveFile(FileName: WideString; FormatIndex: Integer; FPS: Single): Boolean;
begin
  Result := False;
  if FileExists(FileName) = False then
  begin
    SubtitleAPI.SaveSubtitle(FileName, FormatIndex, FPS);
    Result := True;
  end else
  begin
    if FileIsReadOnly(FileName) = False then
    begin
      SubtitleAPI.SaveSubtitle(FileName, FormatIndex, FPS);
      Result := True;
    end else
    begin
      case MsgBox(QuestionMsg[09], BTN_YES, BTN_NO, '', MB_ICONWARNING, frmMain) of
        1:
          begin
            if FileSetReadOnly(FileName, False) then
            begin
              SubtitleAPI.SaveSubtitle(FileName, FormatIndex, FPS);
              Result := True;
            end else
            begin
              MsgBox(Format(ErrorMsg[12], [FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain);
              Result := False;
              exit;
            end;
          end;
        2: exit;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
