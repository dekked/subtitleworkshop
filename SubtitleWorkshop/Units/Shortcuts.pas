unit Shortcuts;

interface

uses Forms, Windows, SysUtils, Classes, Consts, Menus, VideoPreview;

// -----------------------------------------------------------------------------

procedure ReadSetShortcuts;
procedure ParseSetShortcut(Line: String);

var
  menuLoadSubSC     : Integer;
  // ---
  menuLoadProjSC    : Integer;
  menuLoadOrgSC     : Integer;
  menuLoadTransSC   : Integer;
  // ---
  menuSaveFileSC    : Integer;
  menuSaveFileAsSC  : Integer;
  // ---
  menuSaveProjSC    : Integer;
  menuSaveOrgSC     : Integer;
  menuSaveTransSC   : Integer;
  menuSaveOrgAsSC   : Integer;
  menuSaveTransAsSC : Integer;

// -----------------------------------------------------------------------------

implementation

uses formMain, General;

// -----------------------------------------------------------------------------

// Constants
const
  mCtrl  = 'CTRL';
  mAlt   = 'ALT';
  mShift = 'SHIFT';
  mNone  = 'NONE';

  //------------------------------------//
  //    Actions that are not in a menu  //
  //------------------------------------//
  aJumpToNextLine    = 'AJUMPTONEXTLINE';
  aJumpToPrevLine    = 'AJUMPTOPREVLINE';
  aJumpToNextSub     = 'AJUMPTONEXTSUB';
  aJumpToPrevSub     = 'AJUMPTOPREVSUB';
  aDefaultSlowMotion = 'ADEFAULTSLOWMOTION';
  aSubDblClick       = 'ASUBDBLCLICK';
  aZeroFunction      = 'AZEROFUNCTION';
  aPascalScript1     = 'APASCALSCRIPT1';
  aPascalScript2     = 'APASCALSCRIPT2';
  aPascalScript3     = 'APASCALSCRIPT3';
  aPascalScript4     = 'APASCALSCRIPT4';
  aPascalScript5     = 'APASCALSCRIPT5';

  // ------------------ //
  //      File menu     //
  // ------------------ //
  aNewSub      = 'ANEWSUB';
  aLoad        = 'ALOAD';
  aLoadOrg     = 'ALOADORG';
  aLoadTrans   = 'ALOADTRANS';
  aLoadProj    = 'ALOADPROJ';
  aSave        = 'ASAVE';
  aSaveAs      = 'ASAVEAS';
  aSaveOrg     = 'ASAVEORG';
  aSaveOrgAs   = 'ASAVEORGAS';
  aSaveTrans   = 'ASAVETRANS';
  aSaveTransAs = 'ASAVETRANSAS';
  aSaveProj    = 'ASAVEPROJ';
  aCloseSub    = 'ACLOSESUB';
  aExit        = 'AEXIT';
  // ------------------ //
  //      Edit menu     //
  // ------------------ //
  aUndo                = 'AUNDO';
  aRedo                = 'AREDO';
  aInsertSub           = 'AINSERTSUB';
  aInsertBefore        = 'AINSERTBEFORE';
  aCut                 = 'ACUT';
  aCopy                = 'ACOPY';
  aPaste               = 'APASTE';
  aSelectAll           = 'ASELECTALL';
  // Timings
  aDurationLimits      = 'ADURATIONLIMITS';
  aSetDelay            = 'ASETDELAY';
  aAdjustSubtitles     = 'AADJUSTSUBTITLES';
  aAdjustToSyncSubs    = 'AADJUSTTOSYNCSUBS';
  aTimeExpanderReducer = 'ATIMEEXPANDERREDUCER';
  aExtendLength        = 'AEXTENDLENGTH';
  aAutomaticDurations  = 'AAUTOMATICDURATIONS';
  aReadTimingsFromFile = 'AREADTIMINGSFROMFILE';
  aShiftMoreMS         = 'ASHIFTMOREMS';
  aShiftLessMS         = 'ASHIFTLESSMS';
  // Texts
  aSmartLineAdjust     = 'ASMARTLINEADJUST';
  aConvertCase         = 'ACONVERTCASE';
  aUnbreakSubs         = 'AUNBREAKSUBS';
  aDivideLines         = 'ADIVIDELINES';
  aFastDivideLines     = 'AFASTDIVIDELINES';
  aSetMaxLineLength    = 'ASETMAXLINELENGTH';
  aReadTextsFromFile   = 'AREADTEXTSFROMFILE';
  // Subtitles
  aCombineSubs = 'ACOMBINESUBS';
  // Effects
  aTypeEffect  = 'ATYPEEFFECT';
  aFastFlash   = 'AFASTFLASH';
  aMediumFlash = 'AMEDIUMFLASH';
  aSlowFlash   = 'ASLOWFLASH';
  // Right-to-Left
  aReverseText    = 'AREVERSETEXT';
  aFixPunctuation = 'AFIXPUNCTUATION';
  // -------------
  aSort          = 'ASORT';
  aDelUnnLinks   = 'ADELUNNLINKS';
  aMarkSelSubs   = 'AMARKSELSUBS';
  aUnmarkSelSubs = 'AUNMARKSELSUBS';
  // Translation
  aTranslatorMode = 'ATRANSLATORMODE';
  aSwap           = 'ASWAP';
  // ------------------ //
  //     Search menu    //
  // ------------------ //
  aSearch           = 'ASEARCH';
  aFindNext         = 'AFINDNEXT';
  aSearchAndReplace = 'ASEARCHANDREPLACE';
  aGoToLineNum      = 'AGOTOLINENUM';
  // ------------------ //
  //      Tools menu    //
  // ------------------ //
  aSpellCheck    = 'ASPELLCHECK';
  aBatchConvert  = 'ABATCHCONVERT';
  aSplitSubtitle = 'ASPLITSUBTITLE';
  aJoinSubtitles = 'AJOINSUBTITLES';
  // Information and errors...
  aInfoErrors         = 'AINFOERRORS';
  aVariousInfo        = 'AVARIOUSINFO';
  aInfoErrorsSettings = 'AINFOERRORSSETTINGS';
  aRecheckErrors      = 'ARECHECKERRORS';
  aFixAllErrors       = 'AFIXALLERRORS';
  aFixErrorsSelSubs   = 'AFIXERRORSSELSUBS';
  aJumpToNextError    = 'AJUMPTONEXTERROR';
  // -------------------------
  aAddFPSFromAVI     = 'AADDFPSFROMAVI';
  aExternalPreview   = 'AEXTERNALPREVIEW';
  aSAMILangExtractor = 'ASAMILANGEXTRACTOR';
  // ------------------ //
  //      Movie menu    //
  // ------------------ //
  aLoadMovie           = 'ALOADMOVIE';
  aCloseMovie          = 'ACLOSEMOVIE';
  aMovieInfo           = 'AMOVIEINFO';
  aSetVideoPreviewMode = 'ASETVIDEOPREVIEWMODE';
  // Playback
  aPlayPause           = 'APLAYPAUSE';
  aStop                = 'ASTOP';
  aRewind              = 'AREWIND';
  aForward             = 'AFORWARD';
  aBack5Sec            = 'ABACK5SEC';
  aFwd5Sec             = 'AFWD5SEC';
  // Subtitles
  aMoveSubtitle        = 'AMOVESUBTITLE';
  aSetStartTime        = 'ASETSTARTTIME';
  aSetFinalTime        = 'ASETFINALTIME';
  aStartSubtitle       = 'ASTARTSUBTITLE';
  aEndSubtitle         = 'AENDSUBTITLE';
  // Synchronization
  aFirstSyncPoint = 'AFIRSTSYNCPOINT';
  aLastSyncPoint  = 'ALASTSYNCPOINT';
  aAddSyncPoint   = 'AADDSYNCPOINT';
  // Display
  aDisplayOriginal     = 'ADISPLAYORIGINAL';
  aDisplayTranslation  = 'ADISPLAYTRANSLATION';
  // ---------------- //
  //   Settings menu  //
  // ---------------- //
  aSettings         = 'ASETTINGS';
  aOutputSettings   = 'AOUTPUTSETTINGS';
  aShowLeftPanel    = 'ASHOWLEFTPANEL';
  aShowTimeControls = 'ASHOWTIMECONTROLS';
  // ---------------- //
  //     Help menu    //
  // ---------------- //
  aHelp           = 'AHELP';
  aAbout          = 'AABOUT';
  aCheckForNewVer = 'ACHECKFORNEWVER';

// -----------------------------------------------------------------------------

procedure ParseSetShortcut(Line: String);
var
  Action   : String;
  Modifier : String;
  Key      : String;
  KeyChar  : Char;
  KeyWord  : Word;
  Menu     : TMenuItem;
  State    : TShiftState;
begin
  Menu     := nil;
  KeyChar  := #0;
  KeyWord  := 0;

  Line     := Copy(UpperCase(Line), 8, Length(Line));
  Action   := Copy(Line, 1, Pos(',', Line)-1);
  Line     := Copy(Line, Pos(',', Line)+1, Length(Line));
  Modifier := Copy(Line, 1, Pos(',', Line)-1);
  Key      := Copy(Line, Pos(',', Line)+1, Length(Line)-1-Pos(',', Line));
  if Length(Key) = 1 then
  begin
    KeyChar := Key[1];
    KeyWord := Word(KeyChar);
  end else
  begin
    if Key = 'F1'  then KeyWord := VK_F1 else
    if Key = 'F2'  then KeyWord := VK_F2 else
    if Key = 'F3'  then KeyWord := VK_F3 else
    if Key = 'F4'  then KeyWord := VK_F4 else
    if Key = 'F5'  then KeyWord := VK_F5 else
    if Key = 'F6'  then KeyWord := VK_F6 else
    if Key = 'F7'  then KeyWord := VK_F7 else
    if Key = 'F8'  then KeyWord := VK_F8 else
    if Key = 'F9'  then KeyWord := VK_F9 else
    if Key = 'F10' then KeyWord := VK_F10 else
    if Key = 'F11' then KeyWord := VK_F11 else
    if Key = 'F12' then KeyWord := VK_F12 else
    if Key = UpperCase(SmkcBkSp)  then KeyWord := VK_BACK else
    if Key = UpperCase(SmkcTab)   then KeyWord := VK_TAB else
    if Key = UpperCase(SmkcEsc)   then KeyWord := VK_ESCAPE else
    if Key = UpperCase(SmkcEnter) then KeyWord := VK_RETURN else
    if Key = UpperCase(SmkcSpace) then KeyWord := VK_SPACE else
    if Key = UpperCase(SmkcPgUp)  then KeyWord := VK_PRIOR else
    if Key = UpperCase(SmkcPgDn)  then KeyWord := VK_NEXT else
    if Key = UpperCase(SmkcEnd)   then KeyWord := VK_END else
    if Key = UpperCase(SmkcLeft)  then KeyWord := VK_LEFT else
    if Key = UpperCase(SmkcUp)    then KeyWord := VK_UP else
    if Key = UpperCase(SmkcRight) then KeyWord := VK_RIGHT else
    if Key = UpperCase(SmkcDown)  then KeyWord := VK_DOWN else
    if Key = UpperCase(SmkcIns)   then KeyWord := VK_INSERT else
    if Key = UpperCase(SmkcDel)   then KeyWord := VK_DELETE;
  end;

  State := [];
  if Modifier <> mNone then
  begin
    if (Pos(mCtrl, Modifier) > 0)  then State := State + [ssCtrl];
    if (Pos(mShift, Modifier) > 0) then State := State + [ssShift];
    if (Pos(mAlt, Modifier) > 0)   then State := State + [ssAlt];
  end;


  //------------------------------------//
  //    Actions that are not in a menu  //
  //------------------------------------//
  if Action = aJumpToNextLine    then Menu := frmMain.mnuJumpToNextLine else
  if Action = aJumpToPrevLine    then Menu := frmMain.mnuJumpToPrevLine else
  if Action = aJumpToNextSub     then Menu := frmMain.mnuJumpToNextSub else
  if Action = aJumpToPrevSub     then Menu := frmMain.mnuJumpToPrevSub else
  if Action = aDefaultSlowMotion then
  begin
    DefAltPlayRateShortcut := ShortCut(KeyWord, State);
    SetDefaultShortCut;
  end else
  if Action = aSubDblClick       then Menu := frmMain.mnuSubDblClick else
  if Action = aZeroFunction      then Menu := frmMain.mnuZeroFunction else

  // ------------------ //
  //      File menu     //
  // ------------------ //
  if Action = aNewSub    then Menu := frmMain.mnuNewSubtitle else
  // Load actions
  if Action = aLoad      then Menu := frmMain.mnuLoadSubtitle else
  if Action = aLoadOrg   then Menu := frmMain.mnuLoadOriginal else
  if Action = aLoadTrans then Menu := frmMain.mnuLoadTranslated else
  if Action = aLoadProj  then Menu := frmMain.mnuLoadProject else
  // Save actions
  if Action = aSave        then Menu := frmMain.mnuSaveFile else
  if Action = aSaveAs      then Menu := frmMain.mnuSaveFileAs else
  if Action = aSaveOrg     then Menu := frmMain.mnuSaveOriginal else
  if Action = aSaveOrgAs   then Menu := frmMain.mnuSaveOriginalAs else
  if Action = aSaveTrans   then Menu := frmMain.mnuSaveTranslated else
  if Action = aSaveTransAs then Menu := frmMain.mnuSaveTranslatedAs else
  if Action = aSaveProj    then Menu := frmMain.mnuSaveProject else
  //
  if Action = aCloseSub then Menu := frmMain.mnuClose else
  if Action = aExit     then Menu := frmMain.mnuExit else

  // ------------------ //
  //      Edit menu     //
  // ------------------ //
  if Action = aUndo         then Menu := frmMain.mnuUndo else
  if Action = aRedo         then Menu := frmMain.mnuRedo else
  if Action = aInsertSub    then Menu := frmMain.mnuInsertSubtitle else
  if Action = aInsertBefore then Menu := frmMain.mnuInsertBefore else
  if Action = aCut          then Menu := frmMain.mnuCut else
  if Action = aCopy         then Menu := frmMain.mnuCopy else
  if Action = aPaste        then Menu := frmMain.mnuPaste else
  if Action = aSelectAll    then Menu := frmMain.mnuSelectAll else
  // Edit/Timings
  if Action = aDurationLimits      then Menu := frmMain.mnuSetDurationLimits else
  if Action = aSetDelay            then Menu := frmMain.mnuSetDelay else
  if Action = aAdjustSubtitles     then Menu := frmMain.mnuAdjustSubtitles else
  if Action = aAdjustToSyncSubs    then Menu := frmMain.mnuAdjustToSyncSubs else
  if Action = aTimeExpanderReducer then Menu := frmMain.mnuTimeExpanderReducer else
  if Action = aExtendLength        then Menu := frmMain.mnuExtendLength else
  if Action = aAutomaticDurations  then Menu := frmMain.mnuAutomaticDurations else
  if Action = aReadTimingsFromFile then Menu := frmMain.mnuReadTimesFromFile else
  if Action = aShiftMoreMS         then Menu := frmMain.mnuShiftPlusMS else
  if Action = aShiftLessMS         then Menu := frmMain.mnuShiftLessMS else

  // Edit/Texts
  if Action = aSmartLineAdjust   then Menu := frmMain.mnuSmartLineAdjust else
  if Action = aConvertCase       then Menu := frmMain.mnuConvertCase else
  if Action = aUnbreakSubs       then Menu := frmMain.mnuUnbreakSubtitles else
  if Action = aDivideLines       then Menu := frmMain.mnuDivideLines else
  if Action = aFastDivideLines   then Menu := frmMain.mnuFastDivideLines else
  if Action = aSetMaxLineLength  then Menu := frmMain.mnuSetMaxLineLength else
  if Action = aReadTextsFromFile then Menu := frmMain.mnuReadTextsFromFile else
  // Edit/Subtitles
  if Action = aCombineSubs then Menu := frmMain.mnuCombineSubtitles else
  // Effects
  if Action = aTypeEffect then Menu := frmMain.mnuTypeEffect else
  if Action = aFastFlash then Menu := frmMain.mnuFastFlash else
  if Action = aMediumFlash then Menu := frmMain.mnuMediumFlash else
  if Action = aSlowFlash then Menu := frmMain.mnuSlowFlash else
  // Right to left
  if Action = aReverseText    then Menu := frmMain.mnuReverseText else
  if Action = aFixPunctuation then Menu := frmMain.mnuFixPunctuation else
  // -------------
  if Action = aSort           then Menu := frmMain.mnuSort else
  if Action = aDelUnnLinks    then Menu := frmMain.mnuDeleteUnnecessaryLinks else
  if Action = aMarkSelSubs    then Menu := frmMain.mnuMarkSelectedSubs else
  if Action = aUnmarkSelSubs  then Menu := frmMain.mnuUnMarkSelectedSubs else
  if Action = aTranslatorMode then Menu := frmMain.mnuTranslatorMode else
  if Action = aSwap           then Menu := frmMain.mnuSwap else

  // ------------------ //
  //     Search menu    //
  // ------------------ //
  if Action = aSearch           then Menu := frmMain.mnuSubSearch else
  if Action = aFindNext         then Menu := frmMain.mnuFindNext else
  if Action = aSearchAndReplace then Menu := frmMain.mnuSearchAndReplace else
  if Action = aGoToLineNum      then Menu := frmMain.mnuGoToLineNumber else

  // ------------------ //
  //      Tools menu    //
  // ------------------ //
  if Action = aSpellCheck    then Menu := frmMain.mnuSpellCheck else
  if Action = aBatchConvert  then Menu := frmMain.mnuBatchConvert else
  if Action = aSplitSubtitle then Menu := frmMain.mnuSplitSubtitle else
  if Action = aJoinSubtitles then Menu := frmMain.mnuJoinSubtitle else
  // Information and errors
  if Action = aInfoErrors         then Menu := frmMain.mnuInformationAndErrors else
  if Action = aVariousInfo        then Menu := frmMain.mnuVariousInformation else
  if Action = aInfoErrorsSettings then Menu := frmMain.mnuInfoErrorsSettings else
  if Action = aRecheckErrors      then Menu := frmMain.mnuRecheckErrors else
  if Action = aFixAllErrors       then Menu := frmMain.mnuFixAllErrors else
  if Action = aFixErrorsSelSubs   then Menu := frmMain.mnuFixErrorsInSelSubs else
  if Action = aJumpToNextError    then Menu := frmMain.mnuJumpToNextError else
  // -------------------------
  if Action = aAddFPSFromAVI     then Menu := frmMain.mnuAddFPSfromAVI else
  if Action = aExternalPreview   then Menu := frmMain.mnuExternalPreview else
  if Action = aSAMILangExtractor then Menu := frmMain.mnuSAMILangExtractor else
  if Action = aPascalScript1 then
  begin
    if frmMain.mnuPascalScripts.Count > 0 then
      Menu := frmMain.mnuPascalScripts[0];
  end else
  if Action = aPascalScript2 then
  begin
    if frmMain.mnuPascalScripts.Count > 1 then
      Menu := frmMain.mnuPascalScripts[1];
  end else
  if Action = aPascalScript3 then
  begin
    if frmMain.mnuPascalScripts.Count > 2 then
      Menu := frmMain.mnuPascalScripts[2];
  end else
  if Action = aPascalScript4 then
  begin
    if frmMain.mnuPascalScripts.Count > 3 then
      Menu := frmMain.mnuPascalScripts[3];
  end else
  if Action = aPascalScript5 then
  begin
    if frmMain.mnuPascalScripts.Count > 4 then
      Menu := frmMain.mnuPascalScripts[4];
  end else

  // ------------------ //
  //      Movie menu    //
  // ------------------ //
  if Action = aLoadMovie           then Menu := frmMain.mnuOpenMovie else
  if Action = aCloseMovie          then Menu := frmMain.mnuCloseMovie else
  if Action = aMovieInfo           then Menu := frmMain.mnuMovieInfo else
  if Action = aSetVideoPreviewMode then Menu := frmMain.mnuVideoPreviewMode;
  // Playback
  if Action = aPlayPause then Menu := frmMain.mnuPlayPause else
  if Action = aStop      then Menu := frmMain.mnuStop else
  if Action = aRewind    then Menu := frmMain.mnuRewind else
  if Action = aForward   then Menu := frmMain.mnuForward else
  if Action = aBack5Sec  then Menu := frmMain.mnuBack5Sec else
  if Action = aFwd5Sec   then Menu := frmMain.mnuFwd5Sec else
  // Subtitles
  if Action = aMoveSubtitle  then Menu := frmMain.mnuMoveSubtitle else
  if Action = aSetStartTime  then Menu := frmMain.mnuSetStartTime else
  if Action = aSetFinalTime  then Menu := frmMain.mnuSetFinalTime else
  if Action = aStartSubtitle then Menu := frmMain.mnuStartSubtitle else
  if Action = aEndSubtitle   then Menu := frmMain.mnuEndSubtitle else
  // Synchronization
  if Action = aFirstSyncPoint then Menu := frmMain.mnuFirstSyncPoint else
  if Action = aLastSyncPoint  then Menu := frmMain.mnuLastSyncPoint else
  if Action = aAddSyncPoint   then Menu := frmMain.mnuAddSyncPoint else
  // Display
  if Action = aDisplayOriginal    then Menu := frmMain.mnuDisplayOriginal else
  if Action = aDisplayTranslation then Menu := frmMain.mnuDisplayTranslation else

  // ------------------ //
  //    Settings menu   //
  // ------------------ //
  if Action = aSettings         then Menu := frmMain.mnuSubSettings else
  if Action = aOutputSettings   then Menu := frmMain.mnuOutputSettings else
  if Action = aShowLeftPanel    then Menu := frmMain.mnuShowLeftPanel else
  if Action = aShowTimeControls then Menu := frmMain.mnuShowTimeControls else

  // ---------------- //
  //     Help menu    //
  // ---------------- //
  if Action = aHelp           then Menu := frmMain.mnuHelpOfProgram else
  if Action = aAbout          then Menu := frmMain.mnuAboutSubtitleWorkshop else
  if Action = aCheckForNewVer then Menu := frmMain.mnuCheckForNewVersion;

  if Assigned(Menu) then
  begin
    if (KeyChar = 'NONE') and (Modifier = 'NONE') then
      Menu.ShortCut := 0 else
      Menu.ShortCut := ShortCut(KeyWord, State);

    if Menu = frmMain.mnuLoadSubtitle     then menuLoadSubSC := Menu.ShortCut else
    if Menu = frmMain.mnuLoadProject      then menuLoadProjSC := Menu.ShortCut else
    if Menu = frmMain.mnuLoadOriginal     then menuLoadOrgSC := Menu.ShortCut else
    if Menu = frmMain.mnuLoadTranslated   then menuLoadTransSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveFile         then menuSaveFileSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveFileAs       then menuSaveFileAsSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveProject      then menuSaveProjSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveOriginal     then menuSaveOrgSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveTranslated   then menuSaveTransSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveOriginalAs   then menuSaveOrgAsSC := Menu.ShortCut else
    if Menu = frmMain.mnuSaveTranslatedAs then menuSaveTransAsSC := Menu.ShortCut;
  end;
end;

// -----------------------------------------------------------------------------

procedure ReadSetShortcuts;
var
  ShortCutFile : String;
  List         : TStringList;
  i            : Integer;
begin
  ShortCutFile := ExtractFilePath(Application.ExeName) + ID_SHORTCUTS;
  if FileExists(ShortCutFile) then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(ShortCutFile);
      for i := 0 to List.Count-1 do
      begin
        if UpperCase(Copy(List[i], 1, 7)) = 'SETKEY(' then
          ParseSetShortcut(List[i]);
      end;
    finally
      List.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
