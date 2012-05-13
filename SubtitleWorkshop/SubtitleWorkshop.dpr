program SubtitleWorkshop;



uses
  Forms,
  IniFiles,
  SysUtils,
  Windows,
  Messages,
  formAbout in 'Forms\formAbout.pas' {frmAbout},
  formAdjustSubsEnterNewSyncPoint in 'Forms\formAdjustSubsEnterNewSyncPoint.pas' {frmEnterNewSyncPoint},
  formAdjustSubtitles in 'Forms\formAdjustSubtitles.pas' {frmAdjustSubtitles},
  formAutomaticDurations in 'Forms\formAutomaticDurations.pas' {frmAutomaticDurations},
  formBatchConvert in 'Forms\formBatchConvert.pas' {frmBatchConvert},
  formConvertCase in 'Forms\formConvertCase.pas' {frmConvertCase},
  formCustomFormats in 'Forms\formCustomFormats.pas' {frmCustomFormats},
  formDivideLines in 'Forms\formDivideLines.pas' {frmDivideLines},
  formDurationLimits in 'Forms\formDurationLimits.pas' {frmDurationLimits},
  formInfoErrors in 'Forms\formInfoErrors.pas' {frmInfoErrors},
  formInfoErrorsSettings in 'Forms\formInfoErrorsSettings.pas' {frmInfoErrorsSettings},
  formJoin in 'Forms\formJoin.pas' {frmJoin},
  formMain in 'Forms\formMain.pas' {frmMain},
  formOutputSettings in 'Forms\formOutputSettings.pas' {frmOutputSettings},
  formSAMILangExtractor in 'Forms\formSAMILangExtractor.pas' {frmSAMILangExtractor},
  formSaveAs in 'Forms\formSaveAs.pas' {frmSaveAs},
  formSearchAndReplace in 'Forms\formSearchAndReplace.pas' {frmSearchAndReplace},
  formSetDelay in 'Forms\formSetDelay.pas' {frmSetDelay},
  formSettings in 'Forms\formSettings.pas' {frmSettings},
  formSplit in 'Forms\formSplit.pas' {frmSplit},
  formTimeExpanderReducer in 'Forms\formTimeExpanderReducer.pas' {frmTimeExpanderReducer},
  formVariousInfo in 'Forms\formVariousInfo.pas' {frmVariousInfo},
  FastStringFuncs in 'Units\FastStringFuncs.pas',
  FastStrings in 'Units\FastStrings.pas',
  FileTypes in 'Units\FileTypes.pas',
  Functions in 'Units\Functions.pas',
  General in 'Units\General.pas',
  HTMLPars in 'Units\HTMLPars.pas',
  InfoErrorsFunctions in 'Units\InfoErrorsFunctions.pas',
  OCRScripts in 'Units\OCRScripts.pas',
  RegExpr in 'Units\RegExpr.pas',
  Shortcuts in 'Units\Shortcuts.pas',
  StrMan in 'Units\StrMan.pas',
  TreeViewHandle in 'Units\TreeViewHandle.pas',
  Undo in 'Units\Undo.pas',
  USubtitleAdjust in 'Units\USubtitleAdjust.pas',
  USubtitleApi in 'Units\USubtitleApi.pas',
  USubtitlesFunctions in 'Units\USubtitlesFunctions.pas',
  VideoPreview in 'Units\VideoPreview.pas',
  VTInPlaceEdition in 'Units\VTInPlaceEdition.pas',
  VirtualTrees in '..\..\..\..\Delphi\Componentes\VirtualTreeView\VirtualTrees.pas',
  DirectShow9 in 'DirectX Units\DirectShow9.pas',
  DirectDraw in 'DirectX Units\DirectDraw.pas',
  DirectSound in 'DirectX Units\DirectSound.pas',
  Direct3D9 in 'DirectX Units\Direct3D9.pas',
  DXTypes in 'DirectX Units\DXTypes.pas';

{$R *.res}

// -----------------------------------------------------------------------------

var
  hWnd : THandle;
  cds  : CopyDataStruct;
  Ini  : TIniFile;
begin
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\SubtitleWorkshop.ini');
  if Ini.ReadBool('Settings','Allow more than one instance', False) = False then
  begin
    hWnd := FindWindow('SubtitleWorkshop', nil);
    if (hWnd <> 0) then
    begin
      cds.dwData := 0;
      cds.cbData := Length(ParamStr(1));
      cds.lpData := PChar(ParamStr(1));
      if ParamCount > 0 then
        SendMessage(hwnd,WM_COPYDATA, 0, Integer(@cds));
      Ini.Free;
      ExitProcess(0);
    end;
  end;
  Ini.Free;

  Application.Initialize;
  Application.Title := 'Subtitle Workshop';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
// -----------------------------------------------------------------------------

end.
