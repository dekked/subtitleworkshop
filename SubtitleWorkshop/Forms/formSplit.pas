unit formSplit;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Mask, ComCtrls, VirtualTrees, General, Functions,
  TreeViewHandle, USubtitlesFunctions, FileCtrl, IniFiles, Buttons, FastStrings,
  VideoPreview, TimeMaskEdit;

type
  TSubtitlePart = record
    FirstNode : PVirtualNode;
    LastNode  : PVirtualNode;
    Length    : Integer;
    Lines     : Integer;
    FileName  : String;
  end;
  PSubtitlePart = ^TSubtitlePart;

  TfrmSplit = class(TForm)
    btnSplit: TButton;
    btnCancel: TButton;
    chkRecalculate: TCheckBox;
    dlgOpenAVI: TOpenDialog;
    pgeSplitMode: TPageControl;
    pgeSimple: TTabSheet;
    rdoSelectedItem: TRadioButton;
    rdoItemNumber: TRadioButton;
    rdoGivenTime: TRadioButton;
    rdoGivenFrame: TRadioButton;
    edtItemNumber: TEdit;
    edtGivenFrame: TEdit;
    rdoEndOfVideo: TRadioButton;
    edtEndOfVideo: TEdit;
    gbNaming1: TGroupBox;
    lblPart1Ext: TLabel;
    lblPart2Ext: TLabel;
    edtNameFile1: TLabeledEdit;
    edtNameFile2: TLabeledEdit;
    pgeAdvanced: TTabSheet;
    rdoEndOfVideos: TRadioButton;
    rdoEqualInLines: TRadioButton;
    rdoEqualInTime: TRadioButton;
    edtNumberOfParts: TLabeledEdit;
    udNumberOfParts: TUpDown;
    gbNaming2: TGroupBox;
    lblPlus: TLabel;
    lblAutoExt: TLabel;
    edtPrefixName: TEdit;
    chkAutoName: TCheckBox;
    cmbSuffixName: TComboBox;
    dlgSavePart: TSaveDialog;
    lstSplitParts: TVirtualStringTree;
    pnlOutput: TPanel;
    lblOutputFormat: TLabel;
    cmbOutputFormat: TComboBox;
    edtOutputDirectory: TLabeledEdit;
    btnBrowse2: TButton;
    btnBrowse1: TButton;
    tmeGivenTime: TTimeMaskEdit;
    procedure btnSplitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowse1Click(Sender: TObject);
    procedure chkAutoNameClick(Sender: TObject);
    procedure cmbOutputFormatChange(Sender: TObject);
    procedure cmbSuffixNameChange(Sender: TObject);
    procedure lstSplitPartsDblClick(Sender: TObject);
    procedure lstSplitPartsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure udNumberOfPartsChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure rdoSelectedItemClick(Sender: TObject);
    procedure lstSplitPartsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure lstSplitPartsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure lstSplitPartsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure rdoEqualInLinesClick(Sender: TObject);
    procedure lstSplitPartsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure btnBrowse2Click(Sender: TObject);
    procedure lstSplitPartsNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure lstSplitPartsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SetLanguage;
    procedure SetNumberOfParts(Number: Integer);
  public
    { Public declarations }
  end;

var
  frmSplit    : TfrmSplit;
  DoubleClickSelectMovie: String;
  VideoLength : Integer;
  SubLength   : Integer;
  TotalLines  : Integer;
  TotalTime   : Integer;
  PartLength  : Integer;

implementation

uses formMain;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmSplit.SetLanguage;
var
  LF: TIniFile;
begin
  LF := TIniFile.Create(frmMain.ActualLangFile);
  try
    With LF do
    begin
      Caption             := ReadString('Split','01','Split subtitle...');
      pgeSimple.Caption   := ReadString('Split','02','Simple');
      pgeAdvanced.Caption := ReadString('Split','03','Advanced');
      // Simple mode
      rdoSelectedItem.Caption        := ReadString('Split','04','Selected item');
      rdoItemNumber.Caption          := ReadString('Split','05','Item number:');
      rdoGivenTime.Caption           := ReadString('Split','06','Given time:');
      rdoGivenFrame.Caption          := ReadString('Split','07','Given frame:');
      rdoEndOfVideo.Caption          := ReadString('Split','08','End of video:');
      btnBrowse1.Caption             := BTN_BROWSE;
      gbNaming1.Caption              := ReadString('Split','09','Naming');
      edtNameFile1.EditLabel.Caption := Format(ReadString('Split','10','Part %d:'), [1]);
      edtNameFile2.EditLabel.Caption := Format(ReadString('Split','10','Part %d:'), [2]);
      // Advanced mode
      lstSplitParts.Header.Columns[0].Text := StringToWideStringEx(ReadString('Split','11','FileName'), CharSetToCodePage(frmMain.Font.Charset));
      lstSplitParts.Header.Columns[1].Text := StringToWideStringEx(ReadString('Split','12','Length'), CharSetToCodePage(frmMain.Font.Charset));
      lstSplitParts.Header.Columns[2].Text := StringToWideStringEx(ReadString('Split','13','Lines'), CharSetToCodePage(frmMain.Font.Charset));

      rdoEqualInTime.Caption               := ReadString('Split','14','Parts equal in time');
      rdoEqualInLines.Caption              := ReadString('Split','15','Parts equal in lines');
      rdoEndOfVideos.Caption               := ReadString('Split','16','At the ends of videos');
      edtNumberOfParts.EditLabel.Caption   := ReadString('Split','17','Number of parts:');
      gbNaming2.Caption                    := gbNaming1.Caption;
      chkAutoName.Caption                  := ReadString('Split','18','Auto-name the parts:');
      // Others
      edtOutputDirectory.EditLabel.Caption := ReadString('Split','19','Output directory:');
      btnBrowse2.Caption                   := BTN_BROWSE;
      lblOutputFormat.Caption              := ReadString('Split','20','Output format:');
      chkRecalculate.Caption               := ReadString('Split','21','Recalculate time values');
      DoubleClickSelectMovie               := ReadString('Split','22','Double click to select movie');
      btnSplit.Caption                     := ReadString('Split','23','&Split!');
      btnCancel.Caption                    := BTN_CANCEL;

      // ------------------ //
      //      Set font      //
      // ------------------ //
      btnSplit.ParentFont := True;
      Font                := frmMain.Font;
      btnSplit.Font.Style := frmMain.Font.Style + [fsBold];
    end;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.SetNumberOfParts(Number: Integer);
var
  i         : Integer;
  Found     : Boolean;
  TempDelay : Integer;
  SubNode   : PVirtualNode;
  PartNode  : PVirtualNode;
  SubData   : PSubtitleItem;
  PartData  : PSubtitlePart;
begin
  // Range checking
  if (Number < 2) then Number := 2;
  if (Number > udNumberOfParts.Max) then
    Number := udNumberOfParts.Max;
  if Number < 2 then exit;
  // Delete parts that are exceeding the new number
  lstSplitParts.RootNodeCount := Number;

  if rdoEqualInLines.Checked then
  begin
    PartLength := TotalLines div Number;
    SubNode    := frmMain.lstSubtitles.GetFirst;
    SubData    := frmMain.lstSubtitles.GetNodeData(SubNode);
    PartNode   := lstSplitParts.GetFirst;
    while Assigned(PartNode.NextSibling) do
    begin
      PartData           := lstSplitParts.GetNodeData(PartNode);
      PartData.Length    := SubData.InitialTime;
      PartData.FirstNode := SubNode;
      for i := 1 to PartLength-1 do
        SubNode := SubNode.NextSibling;
      if Integer(PartNode.Index) < (TotalLines mod Number) then //distribute leftovers
        SubNode := SubNode.NextSibling;
      SubData           := frmMain.lstSubtitles.GetNodeData(SubNode);
      PartData.LastNode := SubNode;
      PartData.Lines    := PartData.LastNode.Index-PartData.FirstNode.Index+1;
      PartData.Length   := SubData.FinalTime-PartData.Length;
      SubNode           := SubNode.NextSibling;
      SubData           := frmMain.lstSubtitles.GetNodeData(SubNode);
      PartNode          := PartNode.NextSibling;
    end;
    PartData           := lstSplitParts.GetNodeData(PartNode);
    PartData.Length    := SubData.InitialTime;
    PartData.FirstNode := SubNode;
    SubNode            := frmMain.lstSubtitles.GetLast;
    SubData            := frmMain.lstSubtitles.GetNodeData(SubNode);
    PartData.LastNode  := SubNode;
    PartData.Lines     := PartData.LastNode.Index-PartData.FirstNode.Index+1;
    PartData.Length    := SubData.FinalTime-PartData.Length;
  end else
  if rdoEqualInTime.Checked then
  begin
    PartLength := TotalTime div Number;
    SubNode    := frmMain.lstSubtitles.GetFirst;
    PartNode   := lstSplitParts.GetFirst;
    PartData   := lstSplitParts.GetNodeData(PartNode);
    while Assigned(PartNode.NextSibling) do
    begin
      if PartData.Length = (PartLength + 1) then
        TempDelay := (PartLength + 1) * (Integer(PartNode.Index) + 1) else
        TempDelay := PartLength * (Integer(PartNode.Index) + 1) + TotalLines mod Number;
      PartData := lstSplitParts.GetNodeData(PartNode);
      PartData.Length := PartLength;
      if Integer(PartNode.Index) < (TotalLines mod Number) then //distribute leftovers
        Inc(PartData.Length);
      SubData := frmMain.lstSubtitles.GetNodeData(SubNode);
      while SubData.InitialTime > TempDelay do //our part didn't get any lines
      begin
        PartData.Lines     := 0;
        PartData.FirstNode := nil;
        PartData.LastNode  := nil;
        PartNode           := PartNode.NextSibling;
        PartData           := lstSplitParts.GetNodeData(PartNode);
        if PartData.Length = (PartLength + 1) then
          TempDelay := (PartLength + 1) * (Integer(PartNode.Index) + 1) else
          TempDelay := (PartLength) * (Integer(PartNode.Index) + 1) + TotalLines mod Number;
      end;
      PartData.FirstNode := SubNode;
      Found := False;
      while (Found = False) do
      begin
        SubData := frmMain.lstSubtitles.GetNodeData(SubNode);
        Found   := SubData.FinalTime > TempDelay;
        SubNode := SubNode.NextSibling;
      end;
      if (Assigned(SubNode)) and (Assigned(SubNode.PrevSibling)) then
        PartData.LastNode := SubNode.PrevSibling;
      PartData.Lines    := PartData.LastNode.Index-PartData.FirstNode.Index + 1;
      
      PartNode := PartNode.NextSibling;
    end;
    PartData           := lstSplitParts.GetNodeData(PartNode);
    PartData.Length    := PartLength;
    PartData.FirstNode := SubNode;
    PartData.LastNode  := frmMain.lstSubtitles.GetLast;
    PartData.Lines     := PartData.LastNode.Index-PartData.FirstNode.Index + 1;
  end else
  begin
    PartNode := lstSplitParts.GetFirst;
    while Assigned(PartNode.NextSibling) do
    begin
      PartData        := lstSplitParts.GetNodeData(PartNode);
      PartData.Length := 0;
      PartData.Lines  := 0;
      PartNode        := PartNode.NextSibling;
    end;
    PartData        := lstSplitParts.GetNodeData(PartNode);
    PartData.Length := TotalTime;
    PartData.Lines  := TotalLines;
  end;

  lstSplitParts.ScrollIntoView(lstSplitParts.GetLast, True);
  lstSplitParts.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.btnSplitClick(Sender: TObject);
var
  SplitIn     : PVirtualNode;
  GivenTime   : Integer;
  SubFormat   : Integer;
  TimeToDelay : Integer;
  OutPath     : String;
  Extension   : String;
  PartNode    : PVirtualNode;
  PartData    : PSubtitlePart;
begin
  // ------------------------------------------------------------------------ //
  //                         Simple split - Two parts                         //
  // ------------------------------------------------------------------------ //
  if pgeSplitMode.ActivePage = pgeSimple then
  begin
    SplitIn := nil;

    with frmMain do
    begin
      if rdoSelectedItem.Checked then
        SplitIn := lstSubtitles.FocusedNode else
      if rdoItemNumber.Checked then
      begin
        SplitIn := lstSubtitles.GetFirst;
        while (Assigned(SplitIn)) and ((Integer(SplitIn.Index)+1) <> StrToInt(edtItemNumber.Text))do
          SplitIn := SplitIn.NextSibling;
        if SplitIn = nil then
        begin
          MsgBox(ErrorMsg[09], BTN_OK, '', '', MB_ICONERROR, frmSplit);
          exit;
        end;
      end else
      if (rdoGivenTime.Checked) or (rdoGivenFrame.Checked) or (rdoEndOfVideo.Checked) then
      begin
        GivenTime := 0;
        if (rdoGivenTime.Checked) then
          GivenTime := tmeGivenTime.Time else
        if (rdoGivenFrame.Checked) then
          GivenTime := FramesToTime(StrToInt(edtGivenFrame.Text), GetFPS) else
        if (rdoEndOfVideo.Checked) then
        begin
          GivenTime := VideoLength;
          if FileExists(edtEndOfVideo.Text) = False then
          begin
            MsgBox(Format(ErrorMsg[05], [edtEndOfVideo.Text]), BTN_OK, '', '', MB_ICONERROR, frmSplit);
            exit;
          end;
        end;
        SplitIn := lstSubtitles.GetFirst;
        while Assigned(SplitIn) do
        begin
          if GivenTime < GetStartTime(SplitIn) then
          begin
            SplitIn := SplitIn.PrevSibling;
            break;
          end else
            SplitIn := SplitIn.NextSibling;
        end;
      end;

      if Assigned(SplitIn) then
      begin
        SubFormat := cmbOutputFormat.ItemIndex + 1;
        Extension := GetFormatExt(SubFormat);

        if edtOutputDirectory.Text <> '' then
        begin
          OutPath := edtOutputDirectory.Text;
          if OutPath[Length(OutPath)] <> '\' then OutPath := OutPath + '\';
          if DirectoryExists(OutPath) = False then
            ForceDirectories(OutPath);
        end else
          OutPath := ExtractFilePath(frmMain.OrgFile);

        try
          UpdateArray;
          // Save first file
          SubtitleAPI.SaveSubtitle(OutPath + edtNameFile1.Text + Extension, SubFormat, GetFPS, 0, SplitIn.Index);
          if chkRecalculate.Checked then
          begin
            if rdoEndOfVideo.Checked then
              TimeToDelay := VideoLength else
              TimeToDelay := SubtitleAPI.GetFinalTime(SplitIn.Index);
            // Set delay
            SubtitleAPI.SetAbsoluteDelay(- TimeToDelay, SplitIn.Index + 1, SubtitleAPI.SubtitleCount);
          end;

          // Save second file
          SubtitleAPI.SaveSubtitle(OutPath + edtNameFile2.Text + Extension, SubFormat, GetFPS, SplitIn.Index + 1, SubtitleAPI.SubtitleCount - 1);
        finally
          SubtitleAPI.ClearSubtitles;
          frmSplit.Close;
          AddToRecent(OutPath + edtNameFile1.Text + Extension);
          AddToRecent(OutPath + edtNameFile2.Text + Extension);
        end;
      end;
    end;
  end else
  // ------------------------------------------------------------------------ //
  //               Advanced split - Indefinite number of parts                //
  // ------------------------------------------------------------------------ //
  begin
    //Checking that all parts are initialized
    //For example that for all the parts valid movies were selected
    PartNode := lstSplitParts.GetFirst;
    while Assigned(PartNode) do
    begin
      PartData := lstSplitParts.GetNodeData(PartNode);
      if (PartData.Length = 0) or (PartData.Lines <= 0) then
        exit else
        PartNode := PartNode.NextSibling;
    end;

    if edtOutputDirectory.Text <> '' then
    begin
      OutPath := edtOutputDirectory.Text;
      if OutPath[Length(OutPath)] <> '\' then OutPath := OutPath + '\';
      if DirectoryExists(OutPath) = False then ForceDirectories(OutPath);
    end else
      OutPath := ExtractFilePath(frmMain.OrgFile);

    PartNode := lstSplitParts.GetFirst;

    try
      UpdateArray;

      while Assigned(PartNode) do
      begin
        PartData := lstSplitParts.GetNodeData(PartNode);
        SubtitleAPI.SaveSubtitle(OutPath + PartData.FileName,   // Output file
                                 cmbOutputFormat.ItemIndex + 1, // Output format
                                 GetFPS,                        // Output FPS
                                 PartData.FirstNode.Index,      // From subtitle
                                 PartData.LastNode.Index        // To subtitle
                                 );      
        frmMain.AddToRecent(OutPath + PartData.FileName);

        if chkRecalculate.Checked then
        begin
          // Set delay
          if rdoEqualInTime.Checked then
            TimeToDelay := PartLength else
            TimeToDelay := PartData.Length;
          SubtitleAPI.SetAbsoluteDelay(-TimeToDelay, PartData.LastNode.Index + 1, SubtitleAPI.SubtitleCount);
        end;
        PartNode := PartNode.NextSibling;
      end;

    finally
      SubtitleAPI.ClearSubtitles;
      frmSplit.Close;
    end;

  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.FormCreate(Sender: TObject);
var
  Ini      : TIniFile;
  FName    : String;
  Data     : PSubtitleItem;
  Node     : PVirtualNode;
  EndTime  : Integer;
  MaxSpace : Integer;
  i        : Integer;
begin
  SetLanguage;
  GetFormatsList(cmbOutputFormat.Items);
  TotalLines          := frmMain.lstSubtitles.RootNodeCount;
  Data                := frmMain.lstSubtitles.GetNodeData(frmMain.lstSubtitles.GetLast);
  TotalTime           := Data.FinalTime;
  rdoEqualInLines.Tag := frmMain.lstSubtitles.RootNodeCount;
  rdoEndOfVideos.Tag  := frmMain.lstSubtitles.RootNodeCount;
  Node                := frmMain.lstSubtitles.GetFirst;
  MaxSpace            := 0;
  EndTime             := 0;
  while Assigned(Node) do
  begin
    Data := frmMain.lstSubtitles.GetNodeData(Node);
    if (Data.InitialTime - EndTime > MaxSpace) then
      MaxSpace := Data.InitialTime-EndTime;
    EndTime := Data.FinalTime;
    Node    := Node.NextSibling;
  end;
  rdoEqualInTime.Tag := Trunc(TotalTime/MaxSpace);

  //Change here to the recent selected
  rdoEqualInLinesClick(rdoEqualInLines);
  chkAutoNameClick(chkAutoName);

  if Player.Initialized then
  begin
    VideoLength           := VideoDuration div 10000;
    rdoEndOfVideo.Checked := True;
    edtEndOfVideo.Text    := frmMain.MovieFile;
  end;
  
  Ini := TIniFile.Create(IniRoot);
  try
    if Ini.ReadBool('Split', 'SimpleMode', True) then
      pgeSplitMode.ActivePage := pgeSimple else
      pgeSplitMode.ActivePage := pgeAdvanced;

    FName := Copy(ExtractFileName(frmMain.OrgFile), 0, LastDelimiter('.', ExtractFileName(frmMain.OrgFile))-1);
    edtNameFile1.Text := FName + '_1';
    edtNameFile2.Text := FName + '_2';

    cmbOutputFormat.ItemIndex := SubtitleAPI.GetFormatIndex(Ini.ReadString('Formats','Default format', 'SubRip')) - 1;
    cmbOutputFormatChange(Self); // Modify extension where required

    cmbSuffixName.Clear;
    for i := 0 to Ini.ReadInteger('Split suffixes', 'Count', 0) do
    begin
      FName := Ini.ReadString('Split suffixes', IntToStr(i), '');
      if (FName <> '') and (cmbSuffixName.Items.IndexOf(FName) = -1) then
        cmbSuffixName.Items.Add(FName);
    end;
    if cmbSuffixName.Items.Count = 0 then
    begin
      cmbSuffixName.Items.Add('.ENG');
      cmbSuffixName.Items.Add('.ESP');
      cmbSuffixName.Items.Add('.FR');
      cmbSuffixName.Items.Add('.GER');
      cmbSuffixName.Items.Add('.HEB');
      cmbSuffixName.Items.Add('.RUS');
      cmbSuffixName.ItemIndex := 0;      
    end else
    begin
      i := Ini.ReadInteger('Split suffixes', 'ItemIndex', 0);
      if i < 0 then i := 0;
      if i >= cmbSuffixName.Items.Count then i := cmbSuffixName.Items.Count-1;
      cmbSuffixName.ItemIndex := i;
    end;

    case Ini.ReadInteger('Split', 'Parts equal in', 1) of
      0: rdoEqualInTime.Checked := True;
      1: rdoEqualInLines.Checked := True else
        rdoEndOfVideos.Checked := True;
    end;

    udNumberOfParts.Position := Ini.ReadInteger('Split', 'Number of parts', 2);
    edtPrefixName.Text       := Ini.ReadString('Split', 'Auto-Naming', '%ORGFILENAME.CD%NUM');
    chkRecalculate.Checked   := Ini.ReadBool('Split', 'Recalculate times', True);
    edtOutputDirectory.Text  := ExtractFilePath(frmMain.OrgFile);
    dlgOpenAVI.InitialDir    := Ini.ReadString('Split', 'Video directory', '');

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.FormDestroy(Sender: TObject);
var
  Ini : TIniFile;
  i   : Integer;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    Ini.WriteBool('Split', 'SimpleMode', pgeSplitMode.ActivePage = pgeSimple);
    Ini.WriteInteger('Split', 'Number of parts', udNumberOfParts.Position);
    if rdoEqualInTime.Checked then i := 0 else
    if rdoEqualInLines.Checked then i := 1 else
      i := 2;
    Ini.WriteInteger('Split', 'Parts equal in', i);
    Ini.WriteBool('Split', 'Auto name parts', chkAutoName.Checked);

    if cmbSuffixName.Text <> '' then
    begin
      if cmbSuffixName.Items.IndexOf(cmbSuffixName.Text) = -1 then
        cmbSuffixName.Items.Add(cmbSuffixName.Text);
    end;

    Ini.WriteInteger('Split suffixes', 'Count', cmbSuffixName.Items.Count-1);
    Ini.WriteInteger('Split suffixes', 'ItemIndex', cmbSuffixName.ItemIndex);
    for i := 0 to cmbSuffixName.Items.Count-1 do
      Ini.WriteString('Split suffixes', IntToStr(i), cmbSuffixName.Items[i]);

    Ini.ReadString('Split', 'Auto-Naming', edtPrefixName.Text);
    Ini.WriteBool('Split', 'Recalculate times', chkRecalculate.Checked);
  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.btnBrowse1Click(Sender: TObject);
var
  FPS : Single;
begin
  if (dlgOpenAVI.Execute) and ((dlgOpenAVI.FileName <> '') and (GetVideoInfo(dlgOpenAVI.FileName, FPS, VideoLength) = False)) then
  begin
    VideoLength := 0;
    MsgBox(Format(ErrorMsg[05], [dlgOpenAVI.FileName]), BTN_OK, '', '', MB_ICONERROR, frmSplit);
  end else
  begin
    rdoEndOfVideo.Checked := True;
    edtEndOfVideo.Text    := dlgOpenAVI.FileName;
    dlgOpenAVI.InitialDir := ExtractFilePath(dlgOpenAVI.FileName);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.chkAutoNameClick(Sender: TObject);
begin
  edtPrefixName.Enabled  := chkAutoName.Checked;
  lblPlus.Enabled        := chkAutoName.Checked;
  cmbSuffixName.Enabled  := chkAutoName.Checked;
  lblAutoExt.Enabled     := chkAutoName.Checked;
  if chkAutoName.Checked then
  begin
    lstSplitParts.TreeOptions.MiscOptions := lstSplitParts.TreeOptions.MiscOptions - [toEditable];
    cmbSuffixNameChange(Self);
  end else
    lstSplitParts.TreeOptions.MiscOptions := lstSplitParts.TreeOptions.MiscOptions + [toEditable];
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.cmbOutputFormatChange(Sender: TObject);
var
  Ext  : String;
  Desc : String;
  p    : Integer;
begin
  SubtitleAPI.GetFormatInfo(cmbOutputFormat.ItemIndex+1, Desc, Ext);
  dlgSavePart.Filter := Desc + '(' + Ext + ')|' + Ext + '|All files(*.*)|*.*';
  P := Pos(';',Ext);
  if P = 0 then
    Ext := Copy(Ext, 2, Length(Ext)-1) else
    Ext := Copy(Ext, 2, P-2);
  lblPart1Ext.Caption  := Ext;
  lblPart2Ext.Caption  := Ext;
  lblAutoExt.Caption   := Ext;
  if chkAutoName.Checked then
    cmbSuffixNameChange(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.cmbSuffixNameChange(Sender: TObject);
var
  Node : PVirtualNode;
  Data : PSubtitlePart;
  Name : String;
begin
  Node := lstSplitParts.GetFirst;
  While Assigned(Node) do
  begin
    Data          := lstSplitParts.GetNodeData(Node);
    Name          := FastReplace(edtPrefixName.Text + cmbSuffixName.Text + lblAutoExt.Caption, '%NUM', IntToStr(Node.Index+1));
    Name          := FastReplace(Name, '%ORGFILENAME', Copy(ExtractFileName(frmMain.OrgFile), 1, LastDelimiter('.', ExtractFileName(frmMain.OrgFile))-1));
    Data.FileName := Name;
    Node          := Node.NextSibling;
  end;
  lstSplitParts.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsDblClick(Sender: TObject);
var
  FPS         : Single;
  Data, Data2 : PSubtitlePart;
  SubData     : PSubtitleItem;
  ND, ND2     : PVirtualNode;
  TotalTime   : Integer;
begin
  Data := lstSplitParts.GetNodeData(lstSplitParts.FocusedNode);

  // We don't allow to choose an AVI for the last part because it will be calculated automatically
  if (rdoEndOfVideos.Checked) and (Assigned(lstSplitParts.FocusedNode.NextSibling)) and (Data.Length = 0) then
  begin
    if lstSplitParts.FocusedNode <> lstSplitParts.GetFirst then
    begin
      Data2 := lstSplitParts.GetNodeData(lstSplitParts.FocusedNode.PrevSibling);
      if Data2.Length = 0 then
      begin
        MsgBox(ErrorMsg[14], BTN_OK, '', '', MB_ICONERROR, frmSplit);
        exit;
      end;
    end else
    begin
      Data2        := lstSplitParts.GetNodeData(lstSplitParts.GetFirst);
      Data2.Length := 0;
    end;
    if (dlgOpenAVI.Execute) and (dlgOpenAVI.FileName <> '') then
    begin
      if GetVideoInfo(dlgOpenAVI.FileName, FPS, Data.Length) = False then
      begin
        MsgBox(Format(ErrorMsg[05], [dlgOpenAVI.FileName]), BTN_OK, '', '', MB_ICONERROR, frmSplit);
        exit;
      end;

      Data.Lines   := 0;
      Data2        := lstSplitParts.GetNodeData(lstSplitParts.GetLast);
      // Data.Length is length of video file, Data2.Length is length of the previous part (0 if we are setting movie for the first part)
      Data2.Length := Data2.Length-Data.Length;
      ND           := lstSplitParts.GetFirst;
      ND2          := frmMain.lstSubtitles.GetFirst;
      TotalTime    := 0;

      while (Assigned(ND.NextSibling)) and (Data.Length > 0) do
      begin
        Data      := lstSplitParts.GetNodeData(ND);
        TotalTime := TotalTime + Data.Length;
        if Data.Lines = 0 then
        begin
          Data.FirstNode := ND2;
          SubData        := frmMain.lstSubtitles.GetNodeData(ND2);
          while (Assigned(ND2)) and (SubData.InitialTime < TotalTime) do
          begin
            ND2     := ND2.NextSibling;
            SubData := frmMain.lstSubtitles.GetNodeData(ND2);
          end;

          if (Assigned(ND2)) and (Assigned(ND2.PrevSibling)) then
          begin
            Data.LastNode := ND2.PrevSibling;
            Data.Lines    := Data.LastNode.Index - Data.FirstNode.Index + 1;
            Data2.Lines   := Data2.Lines - Data.Lines;
          end else
            Data2.Lines := 0;
        end else
          ND2 := Data.LastNode.NextSibling;
          
        ND := ND.NextSibling;
      end;

      //This is for the last part
      Data           := lstSplitParts.GetNodeData(ND);
      Data.FirstNode := ND2;
      Data.LastNode  := frmMain.lstSubtitles.GetLast;
      if Assigned(ND2) then
        Data.Lines := Data.LastNode.Index - Data.FirstNode.Index + 1 else
        Data.Lines := 0;

      if (Data2.Length < 1) or (Data2.Lines < 1) then
      begin
        MsgBox(InfoMsg[04], BTN_OK, '', '', MB_ICONERROR, frmSplit);
        rdoEqualInLinesClick(rdoEndOfVideos);
      end;
      
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F2     : if chkAutoName.Checked = False then lstSplitParts.EditNode(lstSplitParts.FocusedNode, 0);
    VK_RETURN : lstSplitPartsDblClick(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.udNumberOfPartsChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  AllowChange := False;
  if (NewValue > 1) and (NewValue <= Integer(frmMain.lstSubtitles.RootNodeCount)) then
  begin
    AllowChange := True;
    SetNumberOfParts(NewValue);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.rdoSelectedItemClick(Sender: TObject);
begin
  edtItemNumber.Enabled := rdoItemNumber.Checked;
  tmeGivenTime.Enabled  := rdoGivenTime.Checked;
  edtGivenFrame.Enabled := rdoGivenFrame.Checked;
  edtEndOfVideo.Enabled := rdoEndOfVideo.Checked;
  btnBrowse1.Enabled    := rdoEndOfVideo.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSubtitlePart);
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PSubtitlePart;
  Name: String;
begin
  Data := Sender.GetNodeData(Node);
  Name := FastReplace(edtPrefixName.Text + cmbSuffixName.Text + lblAutoExt.Caption, '%NUM', IntToStr(Node.Index+1));
  Name := FastReplace(Name, '%ORGFILENAME', Copy(ExtractFileName(frmMain.OrgFile), 1, LastDelimiter('.', ExtractFileName(frmMain.OrgFile))-1));
  Data.FileName := Name;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
 Data: PSubtitlePart;
begin
  Data := Sender.GetNodeData(Node);
  Case Column of
    0: CellText := Data.FileName;
    1: if Data.Length > 0 then
         CellText := TimeToString(Data.Length) else
         CellText := DoubleClickSelectMovie;
    2: if Data.Length > 0 then
       begin
         if Data.Lines > -1 then
           CellText := IntToStr(Data.Lines) else
           CellText := '?';
       end else
         CellText := '';
  end;
  CellText := StringToWideStringEx(CellText, CharSetToCodePage(frmMain.Font.Charset));
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.rdoEqualInLinesClick(Sender: TObject);
begin
  udNumberOfParts.Max := TComponent(Sender).Tag;
  if StrToIntDef(edtNumberOfParts.Text, udNumberOfParts.Max) > udNumberOfParts.Max then
  begin
    udNumberOfParts.Position := udNumberOfParts.Max;
    edtNumberOfParts.Text    := IntToStr(udNumberOfParts.Position);
  end;
  SetNumberOfParts(udNumberOfParts.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PSubtitlePart;
begin
  Data := Sender.GetNodeData(Node);
  if (Data.Length = 0) and (Column = 1) then
    TargetCanvas.Font.Color := clRed;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.btnBrowse2Click(Sender: TObject);
var
  Dir : String;
begin
  if SelectDirectory(edtOutputDirectory.EditLabel.Caption, '', Dir) then
    edtOutputDirectory.Text := Dir;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  Data: PSubtitlePart;
begin
  Data          := lstSplitParts.GetNodeData(Node);
  Data.FileName := NewText;
end;

// -----------------------------------------------------------------------------

procedure TfrmSplit.lstSplitPartsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PSubtitlePart;
begin
  Data := lstSplitParts.GetNodeData(Node);
  Data.FirstNode := nil;
  Data.LastNode  := nil;
  Data.Length    := 0;
  Data.Lines     := 0;
  Data.FileName  := '';
end;

// -----------------------------------------------------------------------------

end.

