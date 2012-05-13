unit formMain;

{$WARN UNIT_PLATFORM OFF}

interface                                                                                    

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, VirtualTrees, ComCtrls, Mask, TreeViewHandle,
  USubtitlesFunctions, Functions, MiMenu, General, USubtitleAPI, IniFiles,
  ToolWin, MiSubtitulo, ImgList, VideoPreview, DirectShow9, ShellAPI, MiHint,
  FileTypes, NFormSizing, FastStrings, InfoErrorsFunctions, ClipBrd, URLMon,
  SWSeekBar, SWButton, USSpeller, Undo, SWTimeCounter, StrMan, ShortCuts,
  TimeMaskEdit, VTInPlaceEdition, FileCtrl, IFPS3CompExec, ifpsComp, ifps3,
  ifpii_controls, ifpii_std, ifpii_classes, ifpii_graphics, ifpii_forms,
  ifpii_stdctrls, ifpii_extctrls, ifpii_menus, ifpiir_controls, ifpiir_std,
  ifpiir_classes, ifpiir_graphics, ifpiir_forms, ifpiir_stdctrls,
  ifpiir_extctrls, ifpiir_menus, ifpidateutils, ifpidateutilsr;

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuNewSubtitle: TMenuItem;
    mnuLoad: TMenuItem;
    mnuLoadOriginal: TMenuItem;
    mnuLoadTranslated: TMenuItem;
    mnuLoadSubtitle: TMenuItem;
    mnuRecent: TMenuItem;
    N10: TMenuItem;
    mnuClearList: TMenuItem;
    N5: TMenuItem;
    mnuTranslatorSave: TMenuItem;
    mnuSaveOriginal: TMenuItem;
    mnuSaveTranslated: TMenuItem;
    N12: TMenuItem;
    mnuSaveOriginalAs: TMenuItem;
    mnuSaveTranslatedAs: TMenuItem;
    mnuSaveFile: TMenuItem;
    mnuSaveFileAs: TMenuItem;
    N1: TMenuItem;
    mnuClose: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuInsertSubtitle: TMenuItem;
    mnuRemoveSelected: TMenuItem;
    N18: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    N3: TMenuItem;
    mnuSelectAll: TMenuItem;
    N13: TMenuItem;
    mnuTimings: TMenuItem;
    mnuSetDurationLimits: TMenuItem;
    mnuSetDelay: TMenuItem;
    mnuAdjustSubtitles: TMenuItem;
    N16: TMenuItem;
    mnuReverseText: TMenuItem;
    mnuSort: TMenuItem;
    mnuTranslatorMode: TMenuItem;
    mnuSearch: TMenuItem;
    mnuSubSearch: TMenuItem;
    mnuFindNext: TMenuItem;
    mnuSearchAndReplace: TMenuItem;
    N6: TMenuItem;
    mnuGoToLineNumber: TMenuItem;
    mnuTools: TMenuItem;
    mnuBatchConvert: TMenuItem;
    N2: TMenuItem;
    mnuSplitSubtitle: TMenuItem;
    mnuJoinSubtitle: TMenuItem;
    N11: TMenuItem;
    mnuInformationAndErrors: TMenuItem;
    mnuAddFPSfromAVI: TMenuItem;
    N7: TMenuItem;
    mnuExternalPreview: TMenuItem;
    mnuZeroFunction: TMenuItem;
    mnuMovie: TMenuItem;
    mnuOpenMovie: TMenuItem;
    mnuCloseMovie: TMenuItem;
    N14: TMenuItem;
    mnuVideoPreviewMode: TMenuItem;
    N9: TMenuItem;
    mnuPlayPause: TMenuItem;
    mnuStop: TMenuItem;
    N15: TMenuItem;
    mnuBack5Sec: TMenuItem;
    mnuFwd5Sec: TMenuItem;
    mnuSettings: TMenuItem;
    mnuSubSettings: TMenuItem;
    mnuOutputSettings: TMenuItem;
    N4: TMenuItem;
    mnuLanguage: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpOfProgram: TMenuItem;
    mnuAboutSubtitleWorkshop: TMenuItem;
    N8: TMenuItem;
    mnuCheckForNewVersion: TMenuItem;
    pnlControl: TPanel;
    lblMode: TLabel;
    lblFPS: TLabel;
    lblInputFPS: TLabel;
    lblWorkWith: TLabel;
    cmbMode: TComboBox;
    cmbFPS: TComboBox;
    cmbInputFPS: TComboBox;
    rdoDuration: TRadioButton;
    rdoFinalTime: TRadioButton;
    rdoBoth: TRadioButton;
    lblText: TLabel;
    lblDuration: TLabel;
    mmoSubtitleText: TMemo;
    lblHide: TLabel;
    lblShow: TLabel;
    MiMenu: TMiMenu;
    dlgLoadFile: TOpenDialog;
    tmrVideo: TTimer;
    mmoTranslation: TMemo;
    lblTranslation: TLabel;
    N17: TMenuItem;
    mnuSubtitleToDisplay: TMenuItem;
    mnuDisplayOriginal: TMenuItem;
    mnuDisplayTranslation: TMenuItem;
    mnuPlayback: TMenuItem;
    mnuVidSubtitles: TMenuItem;
    mnuSetStartTime: TMenuItem;
    mnuSetFinalTime: TMenuItem;
    N19: TMenuItem;
    mnuStartSubtitle: TMenuItem;
    mnuEndSubtitle: TMenuItem;
    mnuSubtitles: TMenuItem;
    mnuCombineSubtitles: TMenuItem;
    mnuSmartLineAdjust: TMenuItem;
    cmbOrgCharset: TComboBox;
    cmbTransCharset: TComboBox;
    mnuAddSyncPoint: TMenuItem;
    mnuLoadProject: TMenuItem;
    N20: TMenuItem;
    mnuSaveProject: TMenuItem;
    N21: TMenuItem;
    mnuTranslation: TMenuItem;
    N22: TMenuItem;
    mnuSwap: TMenuItem;
    N23: TMenuItem;
    mnuExtendLength: TMenuItem;
    mnuStylePopup: TPopupMenu;
    mnuBold: TMenuItem;
    mnuItalic: TMenuItem;
    mnuUnderline: TMenuItem;
    mnuTexts: TMenuItem;
    mnuConvertCase: TMenuItem;
    N24: TMenuItem;
    mnuUnbreakSubtitles: TMenuItem;
    N25: TMenuItem;
    mnuSetColor: TMenuItem;
    dlgColor: TColorDialog;
    mnuRemoveColorTags: TMenuItem;
    MiHint: TMiHint;
    tmrSaveWork: TTimer;
    N26: TMenuItem;
    mnuReadTimesFromFile: TMenuItem;
    N27: TMenuItem;
    mnuReadTextsFromFile: TMenuItem;
    mnuAdjustToSyncSubs: TMenuItem;
    mnuRightToLeft: TMenuItem;
    mnuFixPunctuation: TMenuItem;
    NFormSizing: TNFormSizing;
    mnuAdjust: TMenuItem;
    mnuTimeExpanderReducer: TMenuItem;
    mnuPlaybackRate: TMenuItem;
    mnu50P: TMenuItem;
    mnu60P: TMenuItem;
    mnu70P: TMenuItem;
    mnu80P: TMenuItem;
    mnu90P: TMenuItem;
    mnu100P: TMenuItem;
    imgLstMenu: TImageList;
    mnuInfoErrors: TMenuItem;
    N28: TMenuItem;
    mnuRecheckErrors: TMenuItem;
    mnuFixAllErrors: TMenuItem;
    N29: TMenuItem;
    mnuJumpToNextError: TMenuItem;
    mnuInfoErrorsSettings: TMenuItem;
    N30: TMenuItem;
    mnuDeleteUnnecessaryLinks: TMenuItem;
    mnuDivideLines: TMenuItem;
    mnuRewind: TMenuItem;
    N31: TMenuItem;
    mnuForward: TMenuItem;
    mnuSpellCheck: TMenuItem;
    N32: TMenuItem;
    USSpellCheck: TUSSpell;
    mnuSetMaxLineLength: TMenuItem;
    mnuInsertBefore: TMenuItem;
    N33: TMenuItem;
    mnuMarkSelectedSubs: TMenuItem;
    mnuUnMarkSelectedSubs: TMenuItem;
    mnuFixErrorsInSelSubs: TMenuItem;
    mnuAutomaticDurations: TMenuItem;
    mnuSAMILangExtractor: TMenuItem;
    N35: TMenuItem;
    N36: TMenuItem;
    mnuMoveSubtitle: TMenuItem;
    mnuVariousInformation: TMenuItem;
    mnuSubDblClick: TMenuItem;
    mnuJumpToNextLine: TMenuItem;
    mnuJumpToPrevLine: TMenuItem;
    mnuShowLeftPanel: TMenuItem;
    pnlParent2: TPanel;
    pnlParent1: TPanel;
    spSplitter: TSplitter;
    pnlVideo: TPanel;
    pnlVideoControls: TPanel;
    sbSeekBar: TSWSeekBar;
    btnPause: TSWButton;
    btnPlay: TSWButton;
    btnStop: TSWButton;
    btnRew: TSWButton;
    btnForward: TSWButton;
    btnAlterPlaybackRate: TSWButton;
    btnSetStartTime: TSWButton;
    btnSetFinalTime: TSWButton;
    btnStartSubtitle: TSWButton;
    btnEndSubtitle: TSWButton;
    btnAddSyncPoint: TSWButton;
    btnScrollList: TSWButton;
    btnPrevSub: TSWButton;
    btnNextSub: TSWButton;
    tcTimeCounter: TSWTimeCounter;
    btnMoveSubtitle: TSWButton;
    pnlVideoDisplay: TPanel;
    subSubtitle: TMiSubtitulo;
    lstSubtitles: TVirtualStringTree;
    mnuEffects: TMenuItem;
    mnuTypeEffect: TMenuItem;
    mnuFlash: TMenuItem;
    mnuFastFlash: TMenuItem;
    mnuMediumFlash: TMenuItem;
    mnuSlowFlash: TMenuItem;
    N37: TMenuItem;
    mnuShowTimeControls: TMenuItem;
    mnuOCRScripts: TMenuItem;
    mnuShiftPlusMS: TMenuItem;
    mnuShiftLessMS: TMenuItem;
    mnuMovieInfo: TMenuItem;
    mnuFastDivideLines: TMenuItem;
    tmeShow: TTimeMaskEdit;
    tmeHide: TTimeMaskEdit;
    tmeDuration: TTimeMaskEdit;
    mnuSaveMediaStartupFile: TMenuItem;
    mnuSaveASX: TMenuItem;
    mnuSaveSMIL: TMenuItem;
    N34: TMenuItem;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    mnuPascalScripts: TMenuItem;
    psCompExec: TIFPS3CompExec;
    mnu40P: TMenuItem;
    mnu30P: TMenuItem;
    mnu20P: TMenuItem;
    mnu10P: TMenuItem;
    btnSyncPoint1: TSWButton;
    btnSyncPoint2: TSWButton;
    mnuSynchronization: TMenuItem;
    N38: TMenuItem;
    mnuFirstSyncPoint: TMenuItem;
    mnuLastSyncPoint: TMenuItem;
    cmbOCRScripts: TComboBox;
    mnuShowInMainForm: TMenuItem;
    N39: TMenuItem;
    mnuUseInPlaceEdition: TMenuItem;
    mnuShowSubtitles: TMenuItem;
    mnu150P: TMenuItem;
    mnu200P: TMenuItem;
    mnu400P: TMenuItem;
    mnu300P: TMenuItem;
    mnuJumpToNextSub: TMenuItem;
    mnuJumpToPrevSub: TMenuItem;
    procedure lstSubtitlesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure lstSubtitlesFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure lstSubtitlesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure lstSubtitlesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormCreate(Sender: TObject);
    procedure spSplitterMoved(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuVideoPreviewModeClick(Sender: TObject);
    procedure mnuCloseClick(Sender: TObject);
    procedure mnuLoadSubtitleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFileClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuClearListClick(Sender: TObject);
    procedure lstSubtitlesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure lstSubtitlesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnuOpenMovieClick(Sender: TObject);
    procedure mnuCloseMovieClick(Sender: TObject);
    procedure tmrVideoTimer(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure mnuPlayPauseClick(Sender: TObject);
    procedure mnuStopClick(Sender: TObject);
    procedure mnuMovieClick(Sender: TObject);
    procedure mnuSubSettingsClick(Sender: TObject);
    procedure rdoDurationClick(Sender: TObject);
    procedure rdoFinalTimeClick(Sender: TObject);
    procedure rdoBothClick(Sender: TObject);
    procedure mnuSaveFileClick(Sender: TObject);
    procedure mnuGoToLineNumberClick(Sender: TObject);
    procedure mnuTranslatorModeClick(Sender: TObject);
    procedure mnuLoadOriginalClick(Sender: TObject);
    procedure mnuLoadTranslatedClick(Sender: TObject);
    procedure mnuSaveTranslatedClick(Sender: TObject);
    procedure mnuSaveFileAsClick(Sender: TObject);
    procedure mnuSaveTranslatedAsClick(Sender: TObject);
    procedure mmoTranslationChange(Sender: TObject);
    procedure mmoSubtitleTextChange(Sender: TObject);
    procedure lstSubtitlesDblClick(Sender: TObject);
    procedure mnuDisplayOriginalClick(Sender: TObject);
    procedure mnuDisplayTranslationClick(Sender: TObject);
    procedure lstSubtitlesPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure btnSetStartTimeClick(Sender: TObject);
    procedure btnSetFinalTimeClick(Sender: TObject);
    procedure btnStartSubtitleClick(Sender: TObject);
    procedure btnEndSubtitleClick(Sender: TObject);
    procedure mnuInsertSubtitleClick(Sender: TObject);
    procedure mnuRemoveSelectedClick(Sender: TObject);
    procedure lstSubtitlesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstSubtitlesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuZeroFunctionClick(Sender: TObject);
    procedure mnuSortClick(Sender: TObject);
    procedure mnuReverseTextClick(Sender: TObject);
    procedure mnuSetStartTimeClick(Sender: TObject);
    procedure mnuSetFinalTimeClick(Sender: TObject);
    procedure mnuStartSubtitleClick(Sender: TObject);
    procedure mnuEndSubtitleClick(Sender: TObject);
    procedure mnuCombineSubtitlesClick(Sender: TObject);
    procedure mnuSmartLineAdjustClick(Sender: TObject);
    procedure mnuNewSubtitleClick(Sender: TObject);
    procedure btnAddSyncPointClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuBack5SecClick(Sender: TObject);
    procedure mnuFwd5SecClick(Sender: TObject);
    procedure mnuSetDurationLimitsClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuSetDelayClick(Sender: TObject);
    procedure mnuAdjustSubtitlesClick(Sender: TObject);
    procedure mnuSubSearchClick(Sender: TObject);
    procedure mnuSearchClick(Sender: TObject);
    procedure mnuFindNextClick(Sender: TObject);
    procedure mnuSearchAndReplaceClick(Sender: TObject);
    procedure mnuLoadProjectClick(Sender: TObject);
    procedure mnuSaveProjectClick(Sender: TObject);
    procedure mnuBatchConvertClick(Sender: TObject);
    procedure mnuSwapClick(Sender: TObject);
    procedure mnuExtendLengthClick(Sender: TObject);
    procedure mnuSplitSubtitleClick(Sender: TObject);
    procedure mnuBoldClick(Sender: TObject);
    procedure mnuItalicClick(Sender: TObject);
    procedure mnuUnderlineClick(Sender: TObject);
    procedure mnuStylePopupPopup(Sender: TObject);
    procedure mnuConvertCaseClick(Sender: TObject);
    procedure mnuUnbreakSubtitlesClick(Sender: TObject);
    procedure mnuSetColorClick(Sender: TObject);
    procedure mnuRemoveColorTagsClick(Sender: TObject);
    procedure mnuToolsClick(Sender: TObject);
    procedure mnuJoinSubtitleClick(Sender: TObject);
    procedure mnuAddFPSfromAVIClick(Sender: TObject);
    procedure mnuInformationAndErrorsClick(Sender: TObject);
    procedure tmrSaveWorkTimer(Sender: TObject);
    procedure cmbInputFPSKeyPress(Sender: TObject; var Key: Char);
    procedure cmbFPSKeyPress(Sender: TObject; var Key: Char);
    procedure mnuReadTimesFromFileClick(Sender: TObject);
    procedure mnuReadTextsFromFileClick(Sender: TObject);
    procedure mnuAdjustToSyncSubsClick(Sender: TObject);
//  procedure lstSubtitlesGetHint(Sender: TBaseVirtualTree;      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;      var CellText: WideString);  // removed by BDZL
    procedure lstSubtitlesGetHint(Sender: TBaseVirtualTree;      Node: PVirtualNode; Column: TColumnIndex;          var LineBreakStyle: TVTTooltipLineBreakStyle;      var HintText: WideString);
    procedure mnuFixPunctuationClick(Sender: TObject);
    procedure mnuTimeExpanderReducerClick(Sender: TObject);
    procedure mnuAboutSubtitleWorkshopClick(Sender: TObject);
    procedure mnuExternalPreviewClick(Sender: TObject);
    procedure mnuOutputSettingsClick(Sender: TObject);
    procedure pnlVideoDisplayClick(Sender: TObject);
    procedure subSubtitleClick(Sender: TObject);
    procedure mnu10PClick(Sender: TObject);
    procedure mnu20PClick(Sender: TObject);
    procedure mnu30PClick(Sender: TObject);
    procedure mnu40PClick(Sender: TObject);
    procedure mnu50PClick(Sender: TObject);
    procedure mnu60PClick(Sender: TObject);
    procedure mnu70PClick(Sender: TObject);
    procedure mnu80PClick(Sender: TObject);
    procedure mnu90PClick(Sender: TObject);
    procedure mnu100PClick(Sender: TObject);
    procedure mnuRecheckErrorsClick(Sender: TObject);
    procedure mnuJumpToNextErrorClick(Sender: TObject);
    procedure mnuFixAllErrorsClick(Sender: TObject);
    procedure mnuInfoErrorsSettingsClick(Sender: TObject);
    procedure cmbOrgCharsetSelect(Sender: TObject);
    procedure cmbTransCharsetSelect(Sender: TObject);
    procedure cmbModeSelect(Sender: TObject);
    procedure cmbInputFPSSelect(Sender: TObject);
    procedure cmbFPSSelect(Sender: TObject);
    procedure mnuDeleteUnnecessaryLinksClick(Sender: TObject);
    procedure mnuCheckForNewVersionClick(Sender: TObject);
    procedure mnuDivideLinesClick(Sender: TObject);
    procedure sbSeekBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbSeekBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbSeekBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnAlterPlaybackRateClick(Sender: TObject);
    procedure mnuRewindClick(Sender: TObject);
    procedure mnuForwardClick(Sender: TObject);
    procedure btnScrollListClick(Sender: TObject);
    procedure mnuSpellCheckClick(Sender: TObject);
    procedure btnRewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnForwardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuHelpOfProgramClick(Sender: TObject);
    procedure tmrSeekRewFFTimer(Sender: TObject);
    procedure btnRewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnForwardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuSetMaxLineLengthClick(Sender: TObject);
    procedure mnuInsertBeforeClick(Sender: TObject);
    procedure mnuMarkSelectedSubsClick(Sender: TObject);
    procedure mnuUnMarkSelectedSubsClick(Sender: TObject);
    procedure mnuFixErrorsInSelSubsClick(Sender: TObject);
    procedure mnuAutomaticDurationsClick(Sender: TObject);
    procedure mnuSAMILangExtractorClick(Sender: TObject);
    procedure btnPrevSubClick(Sender: TObject);
    procedure btnNextSubClick(Sender: TObject);
    procedure btnMoveSubtitleClick(Sender: TObject);
    procedure mnuMoveSubtitleClick(Sender: TObject);
    procedure mnuVariousInformationClick(Sender: TObject);
    procedure mnuSubDblClickClick(Sender: TObject);
    procedure mnuJumpToNextLineClick(Sender: TObject);
    procedure mnuJumpToPrevLineClick(Sender: TObject);
    procedure mnuShowLeftPanelClick(Sender: TObject);
    procedure mnuTypeEffectClick(Sender: TObject);
    procedure mnuFastFlashClick(Sender: TObject);
    procedure mnuMediumFlashClick(Sender: TObject);
    procedure mnuSlowFlashClick(Sender: TObject);
    procedure mnuShowTimeControlsClick(Sender: TObject);
    procedure mnuShiftPlusMSClick(Sender: TObject);
    procedure mnuShiftLessMSClick(Sender: TObject);
    procedure mnuMovieInfoClick(Sender: TObject);
    procedure mnuFastDivideLinesClick(Sender: TObject);
    procedure mnuAddSyncPointClick(Sender: TObject);
    procedure tmeShowTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
    procedure tmeHideTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
    procedure tmeDurationTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
    procedure lstSubtitlesCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure lstSubtitlesEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure mnuSaveASXClick(Sender: TObject);
    procedure mnuSaveSMILClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure psCompExecCompile(Sender: TIFPS3CompExec);
    procedure psCompExecExecute(Sender: TIFPS3CompExec);
    procedure psCompExecCompImport(Sender: TObject;
      x: TIFPSPascalCompiler);
    procedure psCompExecExecImport(Sender: TObject; se: TIFPSExec;
      x: TIFPSRuntimeClassImporter);
    procedure psCompExecAfterExecute(Sender: TIFPS3CompExec);
    procedure btnSyncPoint1Click(Sender: TObject);
    procedure btnSyncPoint2Click(Sender: TObject);
    procedure mnuFirstSyncPointClick(Sender: TObject);
    procedure mnuLastSyncPointClick(Sender: TObject);
    procedure cmbOCRScriptsSelect(Sender: TObject);
    procedure mnuShowInMainFormClick(Sender: TObject);
    procedure mnuUseInPlaceEditionClick(Sender: TObject);
    procedure mnuShowSubtitlesClick(Sender: TObject);
    procedure mnu150PClick(Sender: TObject);
    procedure mnu200PClick(Sender: TObject);
    procedure mnu300PClick(Sender: TObject);
    procedure mnu400PClick(Sender: TObject);
    procedure mnuJumpToNextSubClick(Sender: TObject);
    procedure mnuJumpToPrevSubClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    procedure ReadString(var Msg: TWMCOPYDATA); message WM_COPYDATA;
    procedure GetLangs;
    procedure GetOCRScripts;
    procedure GetPascalScripts;
    procedure mnuLanguageClick(Sender: TObject);
    procedure mnuOCRScriptClick(Sender: TObject);
    procedure mnuPascalScriptClick(Sender: TObject);
    procedure SetLanguage(LangFile: String);
    procedure AppExeption(Sender: TObject; E: Exception);
    procedure DroppedFile(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    FormatType     : (ftTime, ftFrames);
    ActualLangFile : String;
    // -----------------//
    //   Loaded files   //
    // -----------------//
    OrgFile       : String;
    OrgFormat     : Integer;
    OrgModified   : Boolean;
    TransFile     : String;
    TransFormat   : Integer;
    TransModified : Boolean;
    OldInputFPS   : Single;
    OldFPS        : Single;
    MovieFile     : String;
    // -------------------------//
    // Global boolean variables //
    // -------------------------//
    ConfirmDelete     : Boolean; // Confirm when deleting subtitles
    InvalidFPlainText : Boolean; // Interpret invalid files as plain text
    AutoSearchMovie   : Boolean; // Autosearch for movie
    ForceWorkWithTime : Boolean; // Force working in time mode
    KeepOrderOfLines  : Boolean; // Keep order of lines when reverse text
    SelTextNL         : Boolean; // Select text on jump to next line
    SelTextPL         : Boolean; // Select text on jump to previous line
    AskToSave         : Boolean; // Ask to save on exit program/close subtitle
    SaveAsBackup      : Boolean; // Save as backup
    MarkUntransSubs   : Boolean; // Mark untranslated subtitles
    ApplyStyleInList  : Boolean; // Apply style to subtitles in the list
    ScrollList        : Boolean; // Scroll list to current subtitle
    // ------------------------
    RFMaxCount : Integer; // Max. number of recent files
    // --------------//
    // Video Preview //
    // --------------//
//    pnlVideoHeight  : Integer; // Height of the video panel
    BackgroundColor : Integer; // Background color of subtitles
    TransparentSubs : Boolean; // Transparent subtitles
    ForceUsingReg   : Boolean; // Force transparency using regions
    MovieFPS        : Double;  // FPS of the movie
    OnDoubleClick   : Byte;    // Action when double click in a subtitle and we are in video preview mode
    OnShiftDblClick : Byte;    // Action when shift-double click in a subtitle and we are in video preview mode
    SecsToJump1     : Byte;    // Seconds to jump on double click
    SecsToJump2     : Byte;    // Seconds to jump on shift-double click
    RewFFTime       : Integer; // Time to rewind/forward
    DefAltPlayRate  : Integer; // Default altered playback rate
    // For the Rew and FF buttons
    tmrSeekRewFF : TTimer;
    Seeking      : Boolean;
    // ----------------//
    // Synchronization //
    // ----------------//
    FirstDialogInVideo : Integer;
    LastDialogInVideo  : Integer;
    SyncPointsArray    : array of TSyncPoint;
    // -------------------//
    //  Search & Replace  //
    // -------------------//
    SearchWord      : String; // String to use with "Find next" menu
    CaseSensitive   : Boolean;
    MatchWholeWords : Boolean;
    // ---------//
    // Advanced //
    // ---------//
    TwoLinesIfLongerThan : Integer;
    ToggleBreakPoint     : Boolean;
    BreakLineAfter       : Integer;
    MaxLineLength        : Integer;
    ShiftTime            : Integer;
    // ---------//
    //  Others  //
    // ---------//
    UnTransSubsColor : Integer;
    OrgCharset       : Integer;
    TransCharset     : Integer;
    AdjustFormOpened : Boolean;
    // ----------
    procedure UpdateRFMenus;
    procedure RefreshTimes;
    procedure AddToRecent(const FileName: String);
    procedure RFMenuClick(Sender: TObject);
    procedure EnableCtrls(const Flag: boolean);
    procedure SetTranslationCtrlsPositions;
  end;

var
  frmMain          : TfrmMain;
  InterfaceEnabled : Boolean;
  RFMenuItems      : array[0..20] of TMenuItem;
  CurrRewOrFF      : Boolean;
  tmrRewFF         : TTimer;
  // Go to line number...
  GoToLineNum  : String;
  EnterLineNum : String;
  // Language strings
  LabelText         : String;
  LabelTranslation  : String;
  TransLeftLines    : String;
  TextOrTransLength : String;
  ManualFile        : String;
  SelectOutputDir   : String;
  ID_PLAINTEXT      : String;
  ID_STPROJECT      : String;
  ID_SRF            : String;
  ShiftMS           : String;
  // Recent files
  RecentFiles : TStringList;
  // Translator mode
  UntranslatedSub : String;
  // ---------------
  SyncPoint: TClassicSyncPoints;

implementation

uses formSettings, formSaveAs, formDurationLimits, formAdjustSubtitles,
  formSetDelay, formSearchAndReplace, formBatchConvert, formSplit,
  formConvertCase, formJoin, formTimeExpanderReducer, formAbout,
  formOutputSettings, formInfoErrors, formInfoErrorsSettings,
  formDivideLines, formAutomaticDurations, formSAMILangExtractor,
  formVariousInfo;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WinClassName := 'SubtitleWorkshop';
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ReadString(var Msg: TWMCOPYDATA);
begin
  CommandLineProcess(PChar(String(Msg.CopyDataStruct.lpData)));
  if IsIconic(Application.Handle) = True then
    Application.Restore else
    Application.BringToFront;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                      VirtualTreeView Handle Functions                      //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.lstSubtitlesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PSubtitleItem;
begin
  if Assigned(Node) then
  begin
    Data             := Sender.GetNodeData(Node);
    Data.InitialTime := 0;
    Data.FinalTime   := 0;
    Data.Text        := '';
    Data.Translation := '';
    Data.ErrorType   := [];
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PSubtitleItem;
begin
  Data             := Sender.GetNodeData(Node);
  Data.InitialTime := 0;
  Data.FinalTime   := 0;
  Data.Text        := '';
  Data.Translation := '';
  Data.Marked      := False;
  Data.ErrorType   := [];
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSubtitleItem);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PSubtitleItem;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Column of
      0: CellText := IntToStr(Node.Index+1);
      1:
        begin
          if frmMain.FormatType = ftTime then
            CellText := TimeToString(Data.InitialTime) else
            CellText := IntToStr(TimeToFrames(Data.InitialTime, GetInputFPS));
        end;
      2:
        begin
          if frmMain.FormatType = ftTime then
            CellText := TimeToString(Data.FinalTime) else
            CellText := IntToStr(TimeToFrames(Data.FinalTime, GetInputFPS));
        end;
      3: CellText := StringToWideStringEx(ReplaceEnters(Data.Text, '|'), CharsetToCodePage(OrgCharset));
      4: CellText := StringToWideStringEx(ReplaceEnters(Data.Translation, '|'), CharsetToCodePage(TransCharset));
    end;
  end;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                             Handle exceptions                              //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.AppExeption(Sender: TObject; E: Exception);
begin
  if MsgBox(Format(ErrorMsg[02],[E.ClassName, Sender.ClassName, (Sender as TComponent).Name, E.Message, ID_EMAIL]), BTN_OK, BTN_CANCEL, '', MB_ICONERROR, frmMain) = 2 then
    Application.Terminate;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                      Language file related procedures                      //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.SetLanguage(LangFile: String);
var
  LF  : TIniFile;
  tmp :  String;
begin
  if ExtractFileExt(LangFile) <> '.lng' then
    LangFile := LangFile + '.lng';
  LangFile := ExtractFilePath(Application.ExeName) + 'Langs\' + LangFile;

  ActualLangFile := LangFile;
  LF := TIniFile.Create(LangFile);
  try
    With LF do
    begin
      frmMain.Font.Charset      := ReadInteger('General', 'Font charset', 0);
      tcTimeCounter.FontCharset := frmMain.Font.Charset;
      tcTimeCounter.Text4       := ReadString('General', 'FPS', 'FPS');
      ID_PLAINTEXT              := ReadString('General', 'PlainText', 'Plain Text') + ' (*.txt)|*.txt';
      ID_STPROJECT              := ReadString('General', 'STP', 'Subtitle Translation Project');
      ID_SRF                    := ReadString('General', 'SRF', 'Subtitle Report File') + ' (*.srf)|*.srf';

      // Botones standard
      BTN_OK     := ReadString('Standard buttons', '01', '&Ok');
      BTN_CANCEL := ReadString('Standard buttons', '02', '&Cancel');
      BTN_BROWSE := ReadString('Standard buttons', '03', '&Browse');
      BTN_APPLY  := ReadString('Standard buttons', '04', '&Apply');
      BTN_EXIT   := ReadString('Standard buttons', '05', '&Exit');
      BTN_YES    := ReadString('Standard buttons', '06', '&Yes');
      BTN_NO     := ReadString('Standard buttons', '07', '&No');

      // Go to line...
      GoToLineNum  := ReadString('Go to line', '01', 'Go to line number');
      EnterLineNum := ReadString('Go to line', '02', 'Enter line number:');

      // Standard String
      AllSupportedFiles := ReadString('General', 'AllFiles', 'All supported files');

      lblMode.Caption := ReadString('Main Form', '01', 'Mode:');
      cmbMode.Clear;
      cmbMode.Items.Add(ReadString('Main Form', '02', 'Time (recommended)'));
      cmbMode.Items.Add(ReadString('Main Form', '03', 'Frames'));
      cmbMode.ItemIndex   := 0;
      lblInputFPS.Caption := ReadString('Main Form', '04', 'Input FPS:');
      lblFPS.Caption      := ReadString('Main Form', '05', 'FPS:');
      lblWorkWith.Caption := ReadString('Main Form', '06', 'Work with:');
      tmp                 := ReadString('Main Form', '07', 'Duration|Final time|Both');
      rdoDuration.Caption   := Copy(tmp, 1, Pos('|', tmp) - 1);
      rdoFinalTime.Caption  := Copy(tmp, Pos('|', tmp) + 1, SmartPos('|', tmp, True, Pos('|', tmp) + 1) - (Pos('|', tmp) + 1));
      rdoBoth.Caption       := Copy(tmp, SmartPos('|', tmp, True, Pos('|', tmp) + 1) + 1, Length(tmp));

      lstSubtitles.Header.Columns[0].Text := StringToWideStringEx(ReadString('Main Form', '08', 'Num'), CharSetToCodePage(frmMain.Font.Charset));
      lstSubtitles.Header.Columns[1].Text := StringToWideStringEx(ReadString('Main Form', '09', 'Show'), CharSetToCodePage(frmMain.Font.Charset));
      lblShow.Caption                     := ReadString('Main Form', '09', 'Show') + ':';
      lstSubtitles.Header.Columns[2].Text := StringToWideStringEx(ReadString('Main Form', '10', 'Hide'), CharSetToCodePage(frmMain.Font.Charset));
      lblHide.Caption                     := ReadString('Main Form', '10', 'Hide') + ':';
      LabelText                           := ReadString('Main Form', '11', 'Text');
      LabelTranslation                    := ReadString('Main Form', '12', 'Translation');
      TransLeftLines                      := ReadString('Main Form', '13', '%d untranslated lines');
      lblDuration.Caption                 := ReadString('Main Form', '14', 'Duration:');
      TextOrTransLength                   := ReadString('Main Form', '15', '%s (%s characters):');
      UntranslatedSub                     := ReadString('Main Form', '16', '- Untranslated subtitle -');
      lstSubtitles.Header.Columns[3].Text := StringToWideStringEx(LabelText, CharSetToCodePage(frmMain.Font.Charset));
      lstSubtitles.Header.Columns[4].Text := StringToWideStringEx(LabelTranslation, CharSetToCodePage(frmMain.Font.Charset));

      if Assigned(lstSubtitles.FocusedNode) then
        mmoSubtitleTextChange(frmMain as TObject) else
        lblText.Caption := LabelText + ':';

      if mnuTranslatorMode.Checked then
      begin
        if Assigned(lstSubtitles.FocusedNode) then
          mmoTranslationChange(frmMain as TObject) else
          lblTranslation.Caption := LabelTranslation + ':';
      end;

      // --------------- //
      //    Main Menu    //
      // --------------- //

      // Header
      mnuFile.Caption     := ReadString('Main menu header', '01', 'File');
      mnuEdit.Caption     := ReadString('Main menu header', '02', 'Edit');
      mnuSearch.Caption   := ReadString('Main menu header', '03', 'Search');
      mnuTools.Caption    := ReadString('Main menu header', '04', 'Tools');
      mnuMovie.Caption    := ReadString('Main menu header', '05', 'Movie');
      mnuSettings.Caption := ReadString('Main menu header', '06', 'Settings');
      mnuHelp.Caption     := ReadString('Main menu header', '07', 'Help');

      // --------- //
      // File menu //
      // --------- //
      mnuNewSubtitle.Caption  := ReadString('Main menu/File', '01', 'New subtitle...');
      mnuLoadSubtitle.Caption := ReadString('Main menu/File', '02', 'Load subtitle...');
      mnuRecent.Caption       := ReadString('Main menu/File', '03', 'Recent files');
      mnuClearList.Caption    := ReadString('Main menu/File', '04', 'Clear list');
      // Translator mode
      mnuLoad.Caption             := ReadString('Main menu/File', '05', 'Load');
      mnuLoadProject.Caption      := ReadString('Main menu/File', '06', 'Project...');
      mnuLoadOriginal.Caption     := ReadString('Main menu/File', '07', 'Original');
      mnuLoadTranslated.Caption   := ReadString('Main menu/File', '08', 'Translated');
      mnuTranslatorSave.Caption   := ReadString('Main menu/File', '09', 'Save');
      mnuSaveProject.Caption      := mnuLoadProject.Caption;
      mnuSaveOriginal.Caption     := mnuLoadOriginal.Caption;
      mnuSaveTranslated.Caption   := mnuLoadTranslated.Caption;
      mnuSaveOriginalAs.Caption   := ReadString('Main menu/File', '10', 'Original as...');
      mnuSaveTranslatedAs.Caption := ReadString('Main menu/File', '11', 'Translated as...');
      // ------
      mnuSaveFile.Caption   := mnuTranslatorSave.Caption;
      mnuSaveFileAs.Caption := ReadString('Main menu/File', '12', 'Save as...');
      mnuClose.Caption      := ReadString('Main menu/File', '13', 'Close');
      mnuExit.Caption       := ReadString('Main menu/File', '14', 'Exit');

      // --------- //
      // Edit menu //
      // --------- //
      mnuUndo.Caption           := ReadString('Main menu/Edit', '01', 'Undo');
      mnuRedo.Caption           := ReadString('Main menu/Edit', '02', 'Redo');
      mnuInsertSubtitle.Caption := ReadString('Main menu/Edit', '03', 'Insert subtitle');
      mnuInsertBefore.Caption   := ReadString('Main menu/Edit', '04', 'Insert before');
      mnuRemoveSelected.Caption := ReadString('Main menu/Edit', '05', 'Remove selected');
      mnuCut.Caption            := ReadString('Main menu/Edit', '06', 'Cut');
      mnuCopy.Caption           := ReadString('Main menu/Edit', '07', 'Copy');
      mnuPaste.Caption          := ReadString('Main menu/Edit', '08', 'Paste');
      mnuSelectAll.Caption      := ReadString('Main menu/Edit', '09', 'Select all');

      // ------------ //
      // Edit/Timings //
      // ------------ //
      mnuTimings.Caption             := ReadString('Main menu/Edit/Timings', '01', 'Timings');
      mnuSetDurationLimits.Caption   := ReadString('Main menu/Edit/Timings', '02', 'Set duration limits...');
      mnuSetDelay.Caption            := ReadString('Main menu/Edit/Timings', '03', 'Set delay...');
      mnuAdjust.Caption              := ReadString('Main menu/Edit/Timings', '04', 'Adjust');
      mnuAdjustSubtitles.Caption     := ReadString('Main menu/Edit/Timings', '05', 'Adjust subtitles...');
      mnuAdjustToSyncSubs.Caption    := ReadString('Main menu/Edit/Timings', '06', 'Adjust to synchronized subtitles');
      mnuTimeExpanderReducer.Caption := ReadString('Main menu/Edit/Timings', '07', 'Time expander/reducer');
      mnuExtendLength.Caption        := ReadString('Main menu/Edit/Timings', '08', 'Extend length');
      mnuAutomaticDurations.Caption  := ReadString('Main menu/Edit/Timings', '09', 'Automatic durations');
      mnuReadTimesFromFile.Caption   := ReadString('Main menu/Edit/Timings', '10', 'Read timings from file');
      ShiftMS                        := ReadString('Main menu/Edit/Timings', '11', 'Shift %s milliseconds');

      // ---------- //
      // Edit/Texts //
      // ---------- //
      mnuTexts.Caption             := ReadString('Main menu/Edit/Texts', '01', 'Texts');
      mnuSmartLineAdjust.Caption   := ReadString('Main menu/Edit/Texts', '02', 'Smart line adjust');
      mnuConvertCase.Caption       := ReadString('Main menu/Edit/Texts', '03', 'Convert case...');
      mnuUnbreakSubtitles.Caption  := ReadString('Main menu/Edit/Texts', '04', 'Unbreak subtitles');
      mnuDivideLines.Caption       := ReadString('Main menu/Edit/Texts', '05', 'Divide lines...');
      mnuFastDivideLines.Caption   := ReadString('Main menu/Edit/Texts', '06', 'Fast divide lines');
      mnuSetMaxLineLength.Caption  := ReadString('Main menu/Edit/Texts', '07', 'Set maximum line length');
      mnuReadTextsFromFile.Caption := ReadString('Main menu/Edit/Texts', '08', 'Read texts from file');

      // -------------- //
      // Edit/Subtitles //
      // -------------- //
      mnuSubtitles.Caption              := ReadString('Main menu/Edit/Subtitles', '01', 'Subtitles');
      mnuCombineSubtitles.Caption       := ReadString('Main menu/Edit/Subtitles', '02', 'Combine subtitles');
      // ---
      mnuEffects.Caption     := ReadString('Main menu/Edit/Subtitles', '03', 'Effects');
      mnuTypeEffect.Caption  := ReadString('Main menu/Edit/Subtitles', '04', 'Type effect');
      mnuFlash.Caption       := ReadString('Main menu/Edit/Subtitles', '05', 'Flash');
      mnuFastFlash.Caption   := ReadString('Main menu/Edit/Subtitles', '06', 'Fast flash');
      mnuMediumFlash.Caption := ReadString('Main menu/Edit/Subtitles', '07', 'Medium flash');
      mnuSlowFlash.Caption   := ReadString('Main menu/Edit/Subtitles', '08', 'Slow flash');
      // ---
      mnuRightToLeft.Caption            := ReadString('Main menu/Edit/Subtitles', '09', 'Right-to-Left');
      mnuReverseText.Caption            := ReadString('Main menu/Edit/Subtitles', '10', 'Reverse text');
      mnuFixPunctuation.Caption         := ReadString('Main menu/Edit/Subtitles', '11', 'Fix punctuation');
      mnuSort.Caption                   := ReadString('Main menu/Edit/Subtitles', '12', 'Sort');
      mnuDeleteUnnecessaryLinks.Caption := ReadString('Main menu/Edit/Subtitles', '13', 'Delete unnecessary links');
      mnuMarkSelectedSubs.Caption       := ReadString('Main menu/Edit/Subtitles', '14', 'Mark selected subtitles');
      mnuUnmarkSelectedSubs.Caption     := ReadString('Main menu/Edit/Subtitles', '15', 'Unmark selected subtitles');

      // ---------------- //
      // Edit/Translation //
      // ---------------- //
      mnuTranslation.Caption    := ReadString('Main menu/Edit/Translation', '01', 'Translation');
      mnuTranslatorMode.Caption := ReadString('Main menu/Edit/Translation', '02', 'Translator mode');
      mnuSwap.Caption           := ReadString('Main menu/Edit/Translation', '03', 'Swap');

      // ----------- //
      // Search menu //
      // ----------- //
      mnuSubSearch.Caption        := ReadString('Main menu/Search', '01', 'Search...');
      mnuFindNext.Caption         := ReadString('Main menu/Search', '02', 'Find next');
      mnuSearchAndReplace.Caption := ReadString('Main menu/Search', '03', 'Search && Replace...');
      mnuGoToLineNumber.Caption   := ReadString('Main menu/Search', '04', 'Go to line number...');

      // ---------- //
      // Tools menu //
      // ---------- //
      mnuSpellCheck.Caption           := ReadString('Main menu/Tools', '01', 'Spell check');
      mnuBatchConvert.Caption         := ReadString('Main menu/Tools', '02', 'Batch convert...');
      mnuSplitSubtitle.Caption        := ReadString('Main menu/Tools', '03', 'Split subtitle...');
      mnuJoinSubtitle.Caption         := ReadString('Main menu/Tools', '04', 'Join subtitles...');
      mnuInfoErrors.Caption           := ReadString('Main menu/Tools', '05', 'Information and errors');
      mnuInformationAndErrors.Caption := ReadString('Main menu/Tools', '06', 'Information and errors...');
      mnuVariousInformation.Caption   := ReadString('Main menu/Tools', '07', 'Various information...');
      mnuOCRScripts.Caption           := ReadString('Main menu/Tools', '08', 'OCR Scripts');
      mnuShowInMainForm.Caption       := ReadString('Main menu/Tools', '09', 'Show in main form');
      mnuInfoErrorsSettings.Caption   := ReadString('Main menu/Tools', '10', 'Settings...');
      mnuRecheckErrors.Caption        := ReadString('Main menu/Tools', '11', 'Recheck errors');
      mnuFixAllErrors.Caption         := ReadString('Main menu/Tools', '12', 'Fix all errors');
      mnuFixErrorsInSelSubs.Caption   := ReadString('Main menu/Tools', '13', 'Fix errors (selected subtitles)');
      mnuJumpToNextError.Caption      := ReadString('Main menu/Tools', '14', 'Jump to next error');
      mnuAddFPSfromAVI.Caption        := ReadString('Main menu/Tools', '15', 'Add FPS from AVI');
      mnuExternalPreview.Caption      := ReadString('Main menu/Tools', '16', 'External preview');
      mnuSAMILangExtractor.Caption    := ReadString('Main menu/Tools', '17', 'SAMI language extractor');
      mnuPascalScripts.Caption        := ReadString('Main menu/Tools', '18', 'Pascal scripts');

      // ---------- //
      // Movie menu //
      // ---------- //
      mnuOpenMovie.Caption            := ReadString('Main menu/Movie', '01', 'Open...');
      mnuCloseMovie.Caption           := ReadString('Main menu/Movie', '02', 'Close');
      mnuMovieInfo.Caption            := ReadString('Main menu/Movie', '03', 'Information...');
      mnuVideoPreviewMode.Caption     := ReadString('Main menu/Movie', '04', 'Video preview mode');
      mnuShowSubtitles.Caption        := ReadString('Main menu/Movie', '05', 'Show subtitles');
      mnuSaveMediaStartupFile.Caption := ReadString('Main menu/Movie', '06', 'Save media startup file');

      // -------------- //
      // Movie/Playback //
      // -------------- //
      mnuPlayback.Caption     := ReadString('Main menu/Movie/Playback', '01', 'Playback');
      mnuPlayPause.Caption    := ReadString('Main menu/Movie/Playback', '02', 'Play/Pause');
      mnuStop.Caption         := ReadString('Main menu/Movie/Playback', '03', 'Stop');
      mnuRewind.Caption       := ReadString('Main menu/Movie/Playback', '04', 'Rewind');
      mnuForward.Caption      := ReadString('Main menu/Movie/Playback', '05', 'Forward');
      mnuBack5Sec.Caption     := ReadString('Main menu/Movie/Playback', '06', 'Back 5 seconds');
      mnuFwd5Sec.Caption      := ReadString('Main menu/Movie/Playback', '07', 'Forward 5 seconds');
      mnuPlaybackRate.Caption := ReadString('Main menu/Movie/Playback', '08', 'Playback rate');

      // --------------- //
      // Movie/Subtitles //
      // --------------- //
      mnuVidSubtitles.Caption  := ReadString('Main menu/Movie/Subtitles', '01', 'Subtitles');
      mnuMoveSubtitle.Caption  := ReadString('Main menu/Movie/Subtitles', '02', 'Move subtitle');
      mnuSetStartTime.Caption  := ReadString('Main menu/Movie/Subtitles', '03', 'Set start time');
      mnuSetFinalTime.Caption  := ReadString('Main menu/Movie/Subtitles', '04', 'Set final time');
      mnuStartSubtitle.Caption := ReadString('Main menu/Movie/Subtitles', '05', 'Start subtitle');
      mnuEndSubtitle.Caption   := ReadString('Main menu/Movie/Subtitles', '06', 'End subtitle');

      // --------------------- //
      // Movie/Synchronization //
      // --------------------- //
      mnuSynchronization.Caption := ReadString('Main menu/Movie/Synchronization', '01', 'Synchronization');
      mnuFirstSyncPoint.Caption  := ReadString('Main menu/Movie/Synchronization', '02', 'First sync point');
      mnuLastSyncPoint.Caption   := ReadString('Main menu/Movie/Synchronization', '03', 'Last sync point');
      mnuAddSyncPoint.Caption    := ReadString('Main menu/Movie/Synchronization', '04', 'Add synchronization point');

      // ------------- //
      // Movie/Display //
      // ------------- //
      mnuSubtitleToDisplay.Caption  := ReadString('Main menu/Movie/Display', '01', 'Display');
      mnuDisplayOriginal.Caption    := ReadString('Main menu/Movie/Display', '02', 'Original');
      mnuDisplayTranslation.Caption := ReadString('Main menu/Movie/Display', '03', 'Translation');

      // ------------- //
      // Settings menu //
      // ------------- //
      mnuSubSettings.Caption       := mnuInfoErrorsSettings.Caption;
      mnuOutputSettings.Caption    := ReadString('Main menu/Settings', '01', 'Output settings...');
      mnuLanguage.Caption          := ReadString('Main menu/Settings', '02', 'Language');
      mnuShowLeftPanel.Caption     := ReadString('Main menu/Settings', '03', 'Show left panel');
      mnuShowTimeControls.Caption  := ReadString('Main menu/Settings', '04', 'Show time controls');
      mnuUseInPlaceEdition.Caption := ReadString('Main menu/Settings', '05', 'Use in-place edition');

      // --------- //
      // Help menu //
      // --------- //
      mnuHelpOfProgram.Caption         := mnuHelp.Caption;
      mnuAboutSubtitleWorkshop.Caption := Format(ReadString('Main menu/Help', '01', 'About %s...'), [ID_PROGRAM]);
      mnuCheckForNewVersion.Caption    := ReadString('Main menu/Help', '02', 'Check for new version');

      // ---------- //
      // Popup menu //
      // ---------- //
      mnuItalic.Caption          := ReadString('Popup Menu', '01', 'Italic');
      mnuBold.Caption            := ReadString('Popup Menu', '02', 'Bold');
      mnuUnderline.Caption       := ReadString('Popup Menu', '03', 'Underline');
      mnuSetColor.Caption        := ReadString('Popup Menu', '04', 'Set color');
      mnuRemoveColorTags.Caption := ReadString('Popup Menu', '05', 'Remove color tags');

      // ------------------------ //
      //    Video Preview Hints   //
      // ------------------------ //
      btnPlay.Hint               := Format(ReadString('Video preview hints', '01', 'Play/Pause (%s)'), [ShortCutToText(mnuPlayPause.ShortCut)]);
      btnPause.Hint              := btnPlay.Hint;
      btnStop.Hint               := Format(ReadString('Video preview hints', '02', 'Stop (%s)'), [ShortCutToText(mnuStop.ShortCut)]);
      btnScrollList.Hint         := ReadString('Video preview hints', '03', 'Toggle scroll list');
      btnPrevSub.Hint            := ReadString('Video preview hints', '04', 'Jump to previous subtitle');
      btnNextSub.Hint            := ReadString('Video preview hints', '05', 'Jump to next subtitle');
      btnRew.Hint                := Format(ReadString('Video preview hints', '06', 'Rewind (%s)'), [ShortCutToText(mnuRewind.ShortCut)]);
      btnForward.Hint            := Format(ReadString('Video preview hints', '07', 'Forward (%s)'), [ShortCutToText(mnuForward.ShortCut)]);
      btnAlterPlaybackRate.Hint  := ReadString('Video preview hints', '08', 'Alter playback rate');
      btnMoveSubtitle.Hint       := Format(ReadString('Video preview hints', '09', 'Move subtitle (%s)'), [ShortCutToText(mnuMoveSubtitle.ShortCut)]);
      btnSetStartTime.Hint       := Format(ReadString('Video preview hints', '10', 'Set start time (%s)'), [ShortCutToText(mnuSetStartTime.ShortCut)]);
      btnSetFinalTime.Hint       := Format(ReadString('Video preview hints', '11', 'Set final time (%s)'), [ShortCutToText(mnuSetFinalTime.ShortCut)]);
      btnStartSubtitle.Hint      := Format(ReadString('Video preview hints', '12', 'Start subtitle (%s)'), [ShortCutToText(mnuStartSubtitle.ShortCut)]);
      btnEndSubtitle.Hint        := Format(ReadString('Video preview hints', '13', 'End subtitle (%s)'), [ShortCutToText(mnuEndSubtitle.ShortCut)]);
      btnSyncPoint1.Hint         := Format(ReadString('Video preview hints', '14', 'Mark as first sync point (%s)'), [ShortCutToText(mnuFirstSyncPoint.ShortCut)]);
      btnSyncPoint2.Hint         := Format(ReadString('Video preview hints', '15', 'Mark as last sync point (%s)'), [ShortCutToText(mnuLastSyncPoint.ShortCut)]);
      btnAddSyncPoint.Hint       := Format(ReadString('Video preview hints', '16', 'Add subtitle/video synchronization point (%s)'), [ShortCutToText(mnuAddSyncPoint.ShortCut)]);

      // Help file
      HelpFile := ReadString('General', 'Help', 'Manual.html');

      SelectOutputDir := ReadString('Split','19','Output directory:');

      // ------------------------ //
      //      Error messages      //
      // ------------------------ //
      ErrorMsg[01] := ReadString('Error messages', '01', 'Could not initialize subtitle API. Please download %s from %s and copy it to the "%s" directory.');
      ErrorMsg[02] := ReadString('Error messages', '02', 'The following error (%s) was derived from object %s (%s):||"%s"||Please write to %s informing what you were doing when this error occurred.||Press Ok to continue, and Cancel to exit the program.');
      ErrorMsg[03] := ReadString('Error messages', '03', 'The file "%s" is a bad subtitle or an unsupported format.');
      ErrorMsg[04] := ReadString('Error messages', '04', 'The file "%s" is not in %s format or is not a valid subtitle file.');
      ErrorMsg[05] := ReadString('Error messages', '05', '"%s" is not a valid video file.');
      ErrorMsg[06] := ReadString('Error messages', '06', 'Not a valid line number.');
      ErrorMsg[07] := ReadString('Error messages', '07', 'Select at least one extension to search for.');
      ErrorMsg[08] := ReadString('Error messages', '08', 'The search path does not exist.');
      ErrorMsg[09] := ReadString('Error messages', '09', 'Item number is not a valid item.');
      ErrorMsg[10] := ReadString('Error messages', '10', 'Please add two subtitles or more.');
      ErrorMsg[11] := ReadString('Error messages', '11', 'Failed to connect to server!.');
      ErrorMsg[12] := ReadString('Error messages', '12', 'Couldn''t remove the read-only attribute from "%s".||Possibly on a write protected drive.');
      ErrorMsg[13] := ReadString('Error messages', '13', 'Error trying to connect to Microsoft Word!.');
      ErrorMsg[14] := ReadString('Error messages', '14', 'Select the video(s) for previous part(s) first!.');
      ErrorMsg[15] := ReadString('Error messages', '15', 'Select a valid output directory.');
      ErrorMsg[16] := ReadString('Error messages', '16', 'Select at least one language to extract.');
      ErrorMsg[17] := ReadString('Error messages', '17', '"%s" is not a valid SAMI file.');
      ErrorMsg[18] := ReadString('Error messages', '18', 'Select a subtitle part in the list.');
      ErrorMsg[19] := ReadString('Error messages', '19', 'If you select a movie fragment for one part of the subtitle you need to select a movie fragment for all parts of the subtitle (except the last part of course).');
      ErrorMsg[20] := ReadString('Error messages', '20', 'You need two or more points.');

      // ------------------------ //
      //    Question messages     //
      // ------------------------ //
      QuestionMsg[01] := ReadString('Question messages', '01', 'File "%s" has changed.||Do you want to save the changes?.');
      QuestionMsg[02] := ReadString('Question messages', '02', 'File "%s" already exists.||Do you want to replace it?.');
      QuestionMsg[03] := ReadString('Question messages', '03', 'Original file ("%s") has changed.||Do you want to save the changes?.');
      QuestionMsg[04] := ReadString('Question messages', '04', 'Translated file ("%s") has changed.||Do you want to save the changes?.');
      QuestionMsg[05] := ReadString('Question messages', '05', 'The selected subtitles are about to be deleted.||Do you want to proceed?.');
      QuestionMsg[06] := ReadString('Question messages', '06', 'Point 1 Subtitle: %s|Point 1 Movie: %s||Point 2 Subtitle: %s|Point 2 Movie: %s||Synchronize subtitle?');
      QuestionMsg[07] := ReadString('Question messages', '07', 'The video player''s exe hasn''t been specified or doesn''t exist.||Do you want to configure external player?.');
      QuestionMsg[08] := ReadString('Question messages', '08', 'A new version was found!.||Do you want to see the change list?.');
      QuestionMsg[09] := ReadString('Question messages', '09', 'The file you are trying to save is read-only.||Try to save anyway?.');

      // ------------------------ //
      //   Information messages   //
      // ------------------------ //
      InfoMsg[01] := ReadString('Information messages', '01', 'Could''t find more instances of "%s".');
      InfoMsg[02] := ReadString('Information messages', '02', 'No new version available.');
      InfoMsg[03] := ReadString('Information messages', '03', 'Spell check finished.||%d change(s) made.');
      InfoMsg[04] := ReadString('Information messages', '04', 'Too many parts. Resetting...');
      InfoMsg[05] := ReadString('Information messages', '05', 'Only one language was found.||If the file contains more languages, add the ones you wish manually opening the file in a text editor and searching for the correct class name.');
      InfoMsg[06] := ReadString('Information messages', '06', 'Couldn''t locate "STYLE" start and/or close tag!.||Probably because of that reason, this file doesn''t contain more than one language.|If it does, try to add manually searching for the class name of the desired language(s).');
      InfoMsg[07] := ReadString('Information messages', '07', 'Fittest font size for playback is: %d');
      InfoMsg[08] := ReadString('Information messages', '08', 'Total replacements: %d');
      InfoMsg[09] := ReadString('Information messages', '09', 'File name: %s|Size: %s|FPS: %s|Duration: %s|Total frames: %d|Resolution: %dx%d|FourCC: %s');
      InfoMsg[10] := ReadString('Information messages', '10', 'Some of the streams of the movie could not be rendered correctly. Probably the audio or video codec is missing.');
      InfoMsg[11] := ReadString('Information messages', '11', 'It is not necessary to set the movie fragment corresponding to the last subtitle.');

      // --------------------------------- //
      //  Information and errors messages  //
      // --------------------------------- //
      IEMsgBoxes[01] := ReadString('Information and errors messages', '01', 'Subtitle number %d is an empty subtitle and is about to be deleted.||Do you want to proceed?.');
      IEMsgBoxes[02] := ReadString('Information and errors messages', '02', 'Subtitle number %d:||"%s" has a prohibited character and is about to be deleted.||Do you want to proceed?.');
      IEMsgBoxes[03] := ReadString('Information and errors messages', '03', 'Subtitle number %d:||"%s" is about to be deleted.||Do you want to proceed?.');
      IEMsgBoxes[04] := ReadString('Information and errors messages', '04', 'Part of subtitle number %d:||"%s" is about to be deleted, so the subtitle will be:||"%s"||Do you want to proceed?.');
      IEMsgBoxes[05] := ReadString('Information and errors messages', '05', 'Subtitle number %d:||"%s" is a one-line subtitle and starts with "-".||Do you want to delete the "-" at the beginning?.');

      // --------------- //
      //  Error reports  //
      // --------------- //
      ErrorReports[01] := ReadString('Information and errors', '16', 'Contains lines without letters');
      ErrorReports[02] := ReadString('Information and errors', '17', 'Empty subtitle');
      // ---
      ErrorReports[03] := ReadString('Information and errors', '18', 'Overlapping with previous subtitle');
      ErrorReports[04] := ReadString('Information and errors', '19', 'Bad values');
      ErrorReports[05] := ReadString('Information and errors', '20', 'Too long duration');
      ErrorReports[06] := ReadString('Information and errors', '21', 'Too short duration');
      ErrorReports[07] := ReadString('Information and errors', '22', 'Too long line(s)');
      ErrorReports[08] := ReadString('Information and errors', '23', 'Over two lines');
      // ---
      ErrorReports[09] := ReadString('Information and errors', '24', 'Hearing impaired');
      ErrorReports[10] := ReadString('Information and errors', '25', 'Has text before colon (":")');
      ErrorReports[11] := ReadString('Information and errors', '26', 'Unnecessary dots');
      ErrorReports[12] := ReadString('Information and errors', '27', 'Contains a prohibited character');
      ErrorReports[13] := ReadString('Information and errors', '28', 'Repeated character');
      ErrorReports[14] := ReadString('Information and errors', '29', 'Repeated subtitle');
      ErrorReports[15] := ReadString('Information and errors', '30', 'OCR error');
      // ---
      ErrorReports[16] := ReadString('Information and errors', '31', 'One line subtitle starts with "-"');
      ErrorReports[17] := ReadString('Information and errors', '32', 'No space after custom character');
      ErrorReports[18] := ReadString('Information and errors', '33', 'No space before custom character');
      ErrorReports[19] := ReadString('Information and errors', '34', 'Unnecessary spaces');
      // ---
      ErrorReports[20] := ReadString('Information and errors', '35', 'Marked subtitle');

    end;
  finally
    LF.Free;
  end;

  LF := TIniFile.Create(ExtractFilePath(Application.ExeName) + ID_ININAME);
  try
    Font.Name := LF.ReadString('Program look', 'Font', 'Tahoma');
    Font.Size := LF.ReadInteger('Program look', 'Font size', 8);
    MiMenu.Fuente.Name         := Font.Name;
    MiMenu.Fuente.Size         := Font.Size;
    MiMenu.Fuente.Charset      := frmMain.Font.Charset;
    lstSubtitles.ParentFont    := True;
    lstSubtitles.Header.Font   := Font;
    tmeShow.ParentFont         := True;
    tmeHide.ParentFont         := True;
    tmeDuration.ParentFont     := True;
    mmoSubtitleText.ParentFont := True;
    mmoTranslation.ParentFont  := True;
    tmeShow.Font.Style         := Font.Style + [fsBold];
    tmeHide.Font.Style         := Font.Style + [fsBold];
    tmeDuration.Font.Style     := Font.Style + [fsBold];
    mmoSubtitleText.Font.Style := Font.Style + [fsBold];
    mmoTranslation.Font.Style  := Font.Style + [fsBold];
    mmoSubtitleText.Font.Size  := Font.Size + 2;
    mmoTranslation.Font.Size   := Font.Size + 2;
  finally
    LF.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuLanguageClick(Sender: TObject);
begin
  SetLanguage((Sender as TMenuItem).Caption);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuOCRScriptClick(Sender: TObject);
begin
  OCRDefFile := ExtractFilePath(Application.ExeName) + 'OCRScripts\' + (Sender as TMenuItem).Caption + ID_OCREXT;
  cmbOCRScripts.ItemIndex := cmbOCRScripts.Items.IndexOf(ExtractFileName(Copy(OCRDefFile, 1, Length(OCRDefFile)-4)));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.GetLangs;
var
  Busca : TSearchRec;
  i     : Integer;
  A     : TMenuItem;
  Langs : TStringList;
begin
  Langs := TStringList.Create;
  try
    i := FindFirst(ExtractFilePath(Application.ExeName) + 'Langs\*.lng', faAnyFile, Busca);
    while i = 0 do
    begin
      Langs.Add(Copy(Busca.Name, 1, Length(Busca.Name)-4));
      i := FindNext(Busca);
    end;
    FindClose(Busca);

    Langs.Sort;
    for i := 0 to Langs.Count-1 do
    begin
      A := TMenuItem.Create(Self);
      A.Caption := Langs[i];
      A.OnClick := mnuLanguageClick;
      mnuLanguage.Add(a);
    end;
  finally
    Langs.Free;
  end;

  if mnuLanguage.Count = 0 then
  begin
    mnuLanguage.Visible := False;
    N4.Visible          := False;
  end else
  begin
    mnuLanguage.Visible := True;
    N4.Visible          := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.GetOCRScripts;
var
  Busca   : TSearchRec;
  i       : Integer;
  A       : TMenuItem;     
  Scripts : TStringList;
begin
  Scripts := TStringList.Create;
  try
    i := FindFirst(ExtractFilePath(Application.ExeName) + 'OCRScripts\*' + ID_OCREXT, faAnyFile, Busca);
    while i = 0 do
    begin
      Scripts.Add(Copy(Busca.Name, 1, Length(Busca.Name)-4));
      i := FindNext(Busca);
    end;
    FindClose(Busca);

    Scripts.Sort;
    for i := 0 to Scripts.Count-1 do
    begin
      A := TMenuItem.Create(Self);
      A.Caption    := Scripts[i];
      A.OnClick    := mnuOCRScriptClick;
      A.RadioItem  := True;
      A.GroupIndex := 1;
      A.AutoCheck  := True;
      mnuOCRScripts.Add(A);
    end;
    cmbOCRScripts.Items.Assign(Scripts);
  finally
    Scripts.Free;
  end;

  if mnuOCRScripts.Count = 0 then
    mnuOCRScripts.Visible := False else
    mnuOCRScripts.Visible := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.GetPascalScripts;
var
  Busca   : TSearchRec;
  i       : Integer;
  A       : TMenuItem;     
  Scripts : TStringList;
begin
  Scripts := TStringList.Create;
  try
    i := FindFirst(ExtractFilePath(Application.ExeName) + 'PascalScripts\*.pas', faAnyFile, Busca);
    while i = 0 do
    begin
      Scripts.Add(Copy(Busca.Name, 1, Length(Busca.Name)-4));
      i := FindNext(Busca);
    end;
    FindClose(Busca);

    Scripts.Sort;
    for i := 0 to Scripts.Count-1 do
    begin
      A := TMenuItem.Create(Self);
      A.Caption    := Scripts[i];
      A.OnClick    := mnuPascalScriptClick;
      mnuPascalScripts.Add(A);
    end;
  finally
    Scripts.Free;
  end;

  if mnuPascalScripts.Count = 0 then
    mnuPascalScripts.Visible := False else
    mnuPascalScripts.Visible := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.EnableCtrls(const Flag: boolean);
begin
  lblMode.Enabled         := Flag;
  cmbMode.Enabled         := Flag;
  lblFPS.Enabled          := Flag;
  cmbFPS.Enabled          := Flag;
  lblWorkWith.Enabled     := Flag;
  rdoDuration.Enabled     := Flag;
  rdoFinalTime.Enabled    := Flag;
  rdoBoth.Enabled         := Flag;
  cmbOrgCharset.Enabled   := Flag;
  cmbTransCharset.Enabled := (Flag) and (mnuTranslatorMode.Checked);

  lstSubtitles.Enabled    := Flag;
  lblShow.Enabled         := Flag;
  tmeShow.Enabled         := Flag;
  lblHide.Enabled         := Flag;
  tmeHide.Enabled         := Flag;
  lblDuration.Enabled     := Flag;
  tmeDuration.Enabled     := Flag;
  lblText.Enabled         := Flag;
  mmoSubtitleText.Enabled := Flag;

  if mnuTranslatorMode.Checked then
  begin
    mmoTranslation.Enabled := Flag;
    lblTranslation.Enabled := Flag;
  end;

  InterfaceEnabled := Flag;

  if Flag = True then
  begin
    if rdoDuration.Checked then
    begin
      lblHide.Enabled     := False;
      tmeHide.Enabled     := False;
      lblDuration.Enabled := True;
      tmeDuration.Enabled := True;
    end else
    if rdoFinalTime.Checked then
    begin
      lblHide.Enabled     := True;
      tmeHide.Enabled     := True;
      lblDuration.Enabled := False;
      tmeDuration.Enabled := False;
    end else
    if rdoBoth.Checked then
    begin
      lblHide.Enabled     := True;
      tmeHide.Enabled     := True;
      lblDuration.Enabled := True;
      tmeDuration.Enabled := True;
    end;
    if frmMain.Visible = False then
      frmMain.Show;
    lstSubtitles.SetFocus;
  end;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                                Drag & Drop                                 //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.DroppedFile(var Msg: TWMDropFiles);
var
  FileName : array[0..MAX_PATH] of Char;
  Ext      : String;
begin
  try
    if DragQueryFile(Msg.Drop, 0, FileName, MAX_PATH) > 0 then
    begin
      Ext := LowerCase(ExtractFileExt(FileName));
      if (Ext = ID_STPEXT) then // Subtitle translation project
        LoadProject(FileName) else
      if (Ext = ID_SRFEXT) then // URUSoft Subtitle Report file
        LoadSRF(FileName) else
      // Video file
      if (Ext = '.asf') or (Ext = '.avi') or (Ext = '.mp4') or (Ext = '.mkv') or
         (Ext = '.divx') or (Ext = '.mp3') or (Ext = '.mpg') or (Ext = '.mpeg') or
         (Ext = '.m1v') or (Ext = '.ogg') or (Ext = '.ogm') or (Ext = '.qt') or
         (Ext = '.vob') or (Ext = '.wav') or (Ext = '.wmv') then
      begin
        if LoadMovie(FileName) = False then
          MsgBox(Format(ErrorMsg[05], [FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain) else
        begin
          if mnuVideoPreviewMode.Checked = False then
            SetVideoPreviewMode(True);
        end;
      end else
      begin // Subtitle file
        if (OrgFile = '') or (mnuTranslatorMode.Checked = False) then
          LoadSubtitle(FileName, GetInputFPS) else
          LoadSubtitle(FileName, GetFPS, 0, True);
      end;
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                       Refresh times & texts function                       //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.RefreshTimes;
var
  Data : PSubtitleItem;
  s    : Integer;
begin
  if (lstSubtitles.SelectedCount = 1) and (Assigned(lstSubtitles.FocusedNode)) and (lstSubtitles.Selected[lstSubtitles.FocusedNode]) then
  begin
    Data := lstSubtitles.GetNodeData(lstSubtitles.FocusedNode);
    if Assigned(Data) then
    begin
      s := mmoSubtitleText.SelStart;
      mmoSubtitleText.Text := Data.Text;
      mmoSubtitleText.SelStart := s;
      if mnuTranslatorMode.Checked then
      begin
        s := mmoTranslation.SelStart;
        mmoTranslation.Text := Data.Translation;
        mmoTranslation.SelStart := s;
      end;
      tmeShow.Time     := Data.InitialTime;
      tmeHide.Time     := Data.FinalTime;
      tmeDuration.Time := Data.FinalTime - Data.InitialTime;

      if FormatType = ftTime then
      begin
        tmeShow.TimeMode     := tmTime;
        tmeHide.TimeMode     := tmTime;
        tmeDuration.TimeMode := tmTime;
      end else
      begin
        tmeShow.TimeMode     := tmFrames;
        tmeHide.TimeMode     := tmFrames;
        tmeDuration.TimeMode := tmFrames;
      end;
    end;
  end else
  begin
    tmeShow.Clear;
    tmeHide.Clear;
    tmeDuration.Clear;
    mmoSubtitleText.Clear;
    lblText.Caption := LabelText + ':';
    if mnuTranslatorMode.Checked then
    begin
      lblTranslation.Caption := LabelTranslation + ':';
      mmoTranslation.Clear;
    end;
  end;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//                            Recent files related                            //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.RFMenuClick(Sender: TObject);
var
  i   : Integer;
  Ext : String;
begin
  for i := 0 to High(RFMenuItems) do
    if RFMenuItems[i] = Sender then
    begin
      Ext := LowerCase(ExtractFileExt(RecentFiles[i]));
      if (Ext = ID_STPEXT) then // Subtitle Translation Project
        LoadProject(RecentFiles[i]) else
      begin
        if (mnuTranslatorMode.Checked) and (OrgFile <> '') then
          LoadSubtitle(RecentFiles[i], GetInputFPS, 0, True) else
          LoadSubtitle(RecentFiles[i], GetInputFPS);
      end;
      break;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateRFMenus;
var
  i: Integer;
begin
  for i := Low(RFMenuItems) to High(RFMenuItems) do
  begin
    with RFMenuItems[i] do
    begin
      if i < RecentFiles.Count then
      begin
        if i < RFMaxCount then
        begin
          Visible := True;
          Caption := '&' + IntToStr(i+1) + ' ' + RecentFiles[i];
        end else
        begin
          Visible := False;
          Caption := '';
        end;
      end
      else
        Visible := False;
    end;
  end;
  if MiMenu.Activo then
    MiMenu.Activo := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.AddToRecent(const FileName: String);
var
  i: Integer;
begin
  for i := RecentFiles.Count-1 downto 0 do
    if AnsiCompareFileName(FileName, RecentFiles[i]) = 0 then
      RecentFiles.Delete(i);
  RecentFiles.Insert(0, FileName);
  UpdateRFMenus;
end;

// ---------------------------------------------------------------------------//
//                                                                            //
//           Form Create - Load all settings & initialize variables           //
//                                                                            //
// ---------------------------------------------------------------------------//

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Ini       : TIniFile;
  i         : Integer;
  NewItem   : TMenuItem;
  TextAlign : TTextAlign;
begin
  Application.OnException := AppExeption; // Procedure to handle exceptions

  // ---------------------------//
  //   Initialize SubtitleAPI   //
  // ---------------------------//

  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\' + ID_ININAME);
  try
    GetLangs;
    GetPascalScripts;
    GetOCRScripts;
    SetLanguage(Ini.ReadString('Language', 'Language', 'English'));
    ReadSetShortcuts;

    // ---------------------------------//
    //    Initialize SubtitleAPI DLL    //
    // ---------------------------------//
    SubtitleAPI := TSubtitleAPI.Create(ExtractFilePath(Application.ExeName) + ID_DLLDIR + '\' +ID_DLLNAME);

    if SubtitleAPI.Initialized = False then
    begin
      MsgBox(Format(ErrorMsg[01], [ID_DLLNAME, ID_WEBPAGE, ExtractFilePath(Application.ExeName) + ID_DLLDIR]), BTN_EXIT, '', '', MB_ICONERROR, Self);
      SubtitleAPI.Free;
      Ini.Free;
      ExitProcess(0);
    end;

    // ---------------------------//
    //    Initialize variables    //
    // ---------------------------//
    Caption               := ID_PROGRAM;
    DecimalSeparator      := ',';
    OrgFile               := '';
    TransFile             := '';
    MovieFile             := '';
    SearchWord            := '';
    OldInputFPS           := 0;
    OldFPS                := 0;
    IniRoot               := ExtractFilePath(Application.ExeName) + '\' + ID_ININAME;
    FormatType            := ftTime;
    RecentFiles           := TStringList.Create;
    FirstDialogInVideo    := -1;
    LastDialogInVideo     := -1;
    sbSeekBar.Position    := 0;
    dlgLoadFile.Filter    := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
    AdjustFormOpened      := False;
    SetLength(frmMain.SyncPointsArray, 0); 
    EnableCtrls(False);
    AddFPS(cmbInputFPS);
    AddFPS(cmbFPS);
    // Edit boxes FPS
    OldFPS          := GetFPS;
    tmeShow.FPS     := OldFPS;
    tmeHide.FPS     := OldFPS;
    tmeDuration.FPS := OldFPS;

    AddCharsets(cmbOrgCharset);
    AddCharsets(cmbTransCharset);

    tmeShow.Clear;
    tmeHide.Clear;
    tmeDuration.Clear;

    // Undo and redo lists
    UndoList        := TList.Create;
    RedoList        := TList.Create;
    mnuUndo.Enabled := False;
    mnuRedo.Enabled := False;

    mnuShowInMainForm.Checked := Ini.ReadBool('General', 'Show OCR Scripts in main form', True);

    // --------------------------------- //
    //            Recent Files           //
    // --------------------------------- //

    // We read the maximum, default value is 10
    RFMaxCount := Ini.ReadInteger('Settings','MaxRF',10);
    // Create all the menus
    for i := High(RFMenuItems) downto 0 do
    begin
      NewItem         := TMenuItem.Create(Self);
      NewItem.OnClick := RFMenuClick;
      mnuRecent.Insert(0,NewItem);
      RFMenuItems[i]  := NewItem;
    end;
    // Add recent files from ini to the TStringList
    AddRecentFiles;
    UpdateRFMenus;

    // --------------------------------- //
    //      Window position & size       //
    // --------------------------------- //
    Position := poDesigned;
    Left     := Ini.ReadInteger('Main Window', 'Left', (Screen.Width div 2) - (frmMain.Width div 2));
    Top      := Ini.ReadInteger('Main Window','Top', (Screen.Height div 2) - (frmMain.Height div 2));
    Width    := Ini.ReadInteger('Main Window','Width', 724);
    Height   := Ini.ReadInteger('Main Window','Height', 405);
    if Ini.ReadBool('Main Window', 'Maximized', True) then
      WindowState := wsMaximized else
      WindowState := wsNormal;

    // --------------------------------- //
    //              General              //
    // --------------------------------- //

    // Read last folder
    dlgLoadFile.InitialDir := Ini.ReadString('General', 'Last folder', '');

    if Ini.ReadBool('Settings','Always on top',False) then
      SetWindowPos(frmMain.Handle, HWND_TOPMOST, frmMain.Left, frmMain.Top, frmMain.Width, frmMain.Height, SWP_NOMOVE + SWP_NOSIZE);
      SetWindowPos(frmMain.Handle, HWND_NOTOPMOST, frmMain.Left, frmMain.Top, frmMain.Width, frmMain.Height, SWP_NOMOVE + SWP_NOSIZE);

    case Ini.ReadInteger('General', 'Work with', 3) of
      1: rdoDuration.Checked := True;
      2: rdoFinalTime.Checked := True else
      rdoBoth.Checked := True;
    end;

    // --------------------------------- //
    //              Charsets             //
    // --------------------------------- //
    cmbOrgCharset.ItemIndex       := Ini.ReadInteger('General', 'Original charset', 0);
    cmbTransCharset.ItemIndex     := Ini.ReadInteger('General', 'Translated charset', 0);
    OrgCharset                    := StrCharsetToInt(cmbOrgCharset.Items[cmbOrgCharset.ItemIndex]);
    TransCharset                  := StrCharsetToInt(cmbTransCharset.Items[cmbTransCharset.ItemIndex]);
    mmoSubtitleText.Font.Charset  := OrgCharset;
    mmoTranslation.Font.Charset   := TransCharset;
    if Ini.ReadBool('Settings', 'Show charsets in main form', True) then
    begin
      frmMain.cmbOCRScripts.Top := 288;
      cmbOrgCharset.Show;
      cmbTransCharset.Show;
    end else
    begin
      frmMain.cmbOCRScripts.Top := frmMain.cmbOrgCharset.Top;
      cmbOrgCharset.Hide;
      cmbTransCharset.Hide;
    end;

    // --------------------------------- //
    //        Particular settings        //
    // --------------------------------- //
    ConfirmDelete        := Ini.ReadBool('Settings', 'Confirm when deleting a subtitle', False);
    InvalidFPlainText    := Ini.ReadBool('Settings', 'Interpret invalid files as plain text', False);
    AutoSearchMovie      := Ini.ReadBool('Settings', 'Autosearch for movie', True);
    ForceWorkWithTime    := Ini.ReadBool('Settings', 'Force working in time mode', False);
    KeepOrderOfLines     := Ini.ReadBool('Settings', 'Keep order of lines when reverse text', True);
    SelTextNL            := Ini.ReadBool('Settings', 'Select text on jump to next line', True);
    SelTextPL            := Ini.ReadBool('Settings', 'Select text on jump to previous line', True);
    RFMaxCount           := Ini.ReadInteger('Settings', 'MaxRF', 10);
    // Tags settings
    SubtitleAPI.NoInteractionWithTags := Ini.ReadBool('Settings', 'No interaction with tags', False);
    SubtitleAPI.WorkWithTags          := Ini.ReadBool('Settings', 'Work with style tags', True);

    // ------------ //
    //   Advanced   //
    // ------------ //
    TwoLinesIfLongerThan := Ini.ReadInteger('Advanced', 'Two lines if longer than', 40);
    ToggleBreakPoint     := Ini.ReadBool('Advanced', 'Toggle breakpoint', False);
    BreakLineAfter       := Ini.ReadInteger('Advanced', 'Break line after', 40);
    MaxLineLength        := Ini.ReadInteger('Advanced', 'Maximum line length', 45);
    ShiftTime            := Ini.ReadInteger('Advanced', 'Shift time', 100);

    // --------------------------------- //
    //            Save related           //
    // --------------------------------- //

    AskToSave            := Ini.ReadBool('Save', 'Ask to save', True);
    tmrSaveWork.Enabled  := Ini.ReadBool('Save', 'Save work automatically', False);
    tmrSaveWork.Interval := Ini.ReadInteger('Save', 'Time interval', 60000);
    SaveAsBackup         := Ini.ReadBool('Save', 'Save as backup', True); 

    // --------------------------------- //
    //           Video Preview           //
    // --------------------------------- //
    //pnlVideoHeight  := Ini.ReadInteger('Video preview', 'Video panel height', (pnlParent.Height div 2) - (spSplitter.Height div 2));
    OnDoubleClick   := Ini.ReadInteger('Video preview', 'Double click in a subtitle', 1);
    OnShiftDblClick := Ini.ReadInteger('Video preview', 'Shift-double click in a subtitle', 2);
    SecsToJump1     := Ini.ReadInteger('Video preview', 'Seconds to jump 1', 1);
    SecsToJump2     := Ini.ReadInteger('Video preview', 'Seconds to jump 2', 1);
    case Ini.ReadInteger('Video preview', 'Playback rate', 0) of
      0: mnu100P.Checked := True;
      1: mnu10P.Checked  := True;
      2: mnu20P.Checked  := True;
      3: mnu30P.Checked  := True;
      4: mnu40P.Checked  := True;
      5: mnu50P.Checked  := True;
      6: mnu60P.Checked := True;
      7: mnu70P.Checked := True;
      8: mnu80P.Checked := True;
      9: mnu90P.Checked := True;
      10: mnu200P.Checked := True;
      11: mnu300P.Checked := True;
      12: mnu400P.Checked := True;
    end;
    RewFFTime := StrSecToMS(Ini.ReadString('Video preview', 'Rewind and forward', '0,500'));
    frmMain.DefAltPlayRate := Ini.ReadInteger('Video preview', 'Default altered playback rate', 0) + 1;
    SetDefaultShortCut;
    ScrollList := Ini.ReadBool('Video preview', 'Scroll list', False);
    mnuDisplayOriginal.Checked := Ini.ReadBool('Video preview', 'Displaying original', True);
    mnuDisplayTranslation.Checked := not mnuDisplayOriginal.Checked;
    mnuShowSubtitles.Checked := Ini.ReadBool('Video preview', 'Show subtitles', True);

    // --------------------------------- //
    //      Video Preview subtitles      //
    // --------------------------------- //
    subSubtitle.Border      := Ini.ReadBool('Video preview subtitles','Draw border', True);
    subSubtitle.Shadow      := Ini.ReadBool('Video preview subtitles','Draw shadow', True);
    TransparentSubs         := Ini.ReadBool('Video preview subtitles','Transparent', True);
    ForceUsingReg           := Ini.ReadBool('Video preview subtitles','Force using regions', False);
    subSubtitle.Font.Name   := Ini.ReadString('Video preview subtitles', 'Font name', 'Tahoma');
    subSubtitle.Font.Size   := Ini.ReadInteger('Video preview subtitles', 'Font size', 14);
    subSubtitle.TextColor   := Ini.ReadInteger('Video preview subtitles', 'Font color', clWhite);
    BackgroundColor         := Ini.ReadInteger('Video preview subtitles', 'Background color', clBtnFace);
    subSubtitle.BorderWidth := Ini.ReadInteger('Video preview subtitles', 'Border width', 1);
    subSubtitle.ShadowWidth := Ini.ReadInteger('Video preview subtitles', 'Shadow width', 1);
    subSubtitle.Font.Style  := [];
    if Ini.ReadBool('Video preview subtitles', 'Bold', True) then
      subSubtitle.Font.Style := subSubtitle.Font.Style + [fsBold];
    if Ini.ReadBool('Video preview subtitles', 'Italic', False) then
      subSubtitle.Font.Style := subSubtitle.Font.Style + [fsItalic];
    if Ini.ReadBool('Video preview subtitles', 'Underline', False) then
      subSubtitle.Font.Style := subSubtitle.Font.Style + [fsUnderline];

    // -------------------------------- //
    //      Information and Errors      //
    // -------------------------------- //
    ShowConfMainForm  := Ini.ReadBool('Information and Errors', 'Show confirmations in main form on fix', False);
    MarkErrorsInList  := Ini.ReadBool('Information and Errors', 'Mark errors in main form''s list', True);
    MarkWithColor     := Ini.ReadInteger('Information and Errors', 'Mark with color', clRed);
    MarkBold          := Ini.ReadBool('Information and Errors', 'Bold', True);
    MarkItalic        := Ini.ReadBool('Information and Errors', 'Italic', False);
    MarkUnderline     := Ini.ReadBool('Information and Errors', 'Underline', False);
    MarkOnLoad        := Ini.ReadBool('Information and Errors', 'Mark errors on load subtitle', False);
    FixOneUnitOverlap := Ini.ReadBool('Information and Errors', 'Fix one unit overlap at load', False);
    FixOnLoad         := Ini.ReadBool('Information and Errors', 'Fix errors on load subtitle', False);
    OCRDefFile        := Ini.ReadString('Information and Errors', 'OCR Definitions file', ExtractFilePath(Application.ExeName) + 'OCRScripts\Default' + ID_OCREXT);

    for i := 0 to mnuOCRScripts.Count-1 do
      if mnuOCRScripts.Items[i].Caption = ExtractFileName(Copy(OCRDefFile, 1, Length(OCRDefFile)-4)) then
        mnuOCRScripts.Items[i].Checked := True;
    cmbOCRScripts.ItemIndex := cmbOCRScripts.Items.IndexOf(ExtractFileName(Copy(OCRDefFile, 1, Length(OCRDefFile)-4)));    

    // ----------------------------- //
    //            Advanced           //
    // ----------------------------- //
    RepeatableChars      := Ini.ReadString('Information and Errors', 'Repeatable chars', '-¡!¿?";\/_[]=');
    ProhibitedChars      := Ini.ReadString('Information and Errors', 'Prohibited chars', '@#*');
    ToleranceForRepeated := Ini.ReadInteger('Information and Errors', 'Tolerance for repeated subtitles', 100);
    SpaceAfterChars      := Ini.ReadString('Information and Errors', 'Space after characters', '-');
    SpaceBeforeChars     := Ini.ReadString('Information and Errors', 'Space before characters', '');
    TooLongDuration      := Ini.ReadInteger('Information and Errors', 'Too long duration', 6000);
    TooShortDuration     := Ini.ReadInteger('Information and Errors', 'Too short duration', 700);
    TooLongLine          := Ini.ReadInteger('Information and Errors', 'Too long line', 45);

    // ----------------------------- //
    //      Errors to check for      //
    // ----------------------------- //
    ErrorsToCheck.eLinesWithoutLetters  := Ini.ReadBool('Errors to check for', 'Lines without letters',              True);
    ErrorsToCheck.eEmptySubtitle        := Ini.ReadBool('Errors to check for', 'Empty subtitles',                    True);
    ErrorsToCheck.eOverlapping          := Ini.ReadBool('Errors to check for', 'Overlapping subtitles',              True);
    ErrorsToCheck.eBadValues            := Ini.ReadBool('Errors to check for', 'Bad values',                         True);
    ErrorsToCheck.eTooLongDurations     := Ini.ReadBool('Errors to check for', 'Too long durations',                 True);
    ErrorsToCheck.eTooShortDurations    := Ini.ReadBool('Errors to check for', 'Too short durations',                True);
    ErrorsToCheck.eTooLongLines         := Ini.ReadBool('Errors to check for', 'Too long lines',                     True);
    ErrorsToCheck.eOverTwoLines         := Ini.ReadBool('Errors to check for', 'Subtitles over two lines',           True);
    ErrorsToCheck.eHearingImpaired      := Ini.ReadBool('Errors to check for', 'Hearing impaired subtitles',         True);
    ErrorsToCheck.eTextBeforeColon      := Ini.ReadBool('Errors to check for', 'Text before colon (":")',            True);
    ErrorsToCheck.eOnlyIfCapitalLetters := Ini.ReadBool('Errors to check for', 'Only if text is in capital letters', True);
    ErrorsToCheck.eUnnecessaryDots      := Ini.ReadBool('Errors to check for', 'Unnecessary dots',                   True);
    ErrorsToCheck.eProhibitedCharacter  := Ini.ReadBool('Errors to check for', 'Prohibited characters',              False);
    ErrorsToCheck.eRepeatedCharacter    := Ini.ReadBool('Errors to check for', 'Repeated characters',                True);
    ErrorsToCheck.eRepeatedSubtitle     := Ini.ReadBool('Errors to check for', 'Repeated subtitles',                 True);
    ErrorsToCheck.eOCRErrors            := Ini.ReadBool('Errors to check for', 'OCR Errors',                         True);
    ErrorsToCheck.eOpnDlgSubsOneLine    := Ini.ReadBool('Errors to check for', '"- " in subtitles with one line',    True);
    ErrorsToCheck.eSpaceAfterCustChars  := Ini.ReadBool('Errors to check for', 'Space after custom characters',      True);
    ErrorsToCheck.eSpaceBeforeCustChars := Ini.ReadBool('Errors to check for', 'Space before custom characters',     False);
    ErrorsToCheck.eUnnecessarySpaces    := Ini.ReadBool('Errors to check for', 'Unnecessary spaces',                 True);
    ErrorsToCheck.eWhatUnnecessarySpaces := [];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Enters and spaces at the beginning and end', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [EntersAndSpacesBeginningEnd];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Spaces between enters (left and right)', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesBetweenEnters];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Double spaces and enters', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [DoubleSpacesAndEnters];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Spaces in front of punctuation marks', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesFrontPunctuation];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Spaces after "¿" and "¡"', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesAfterQuestionAndExclamation];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Spaces before "?" and  "!"', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesBeforeQuestionAndExclamation];
    if Ini.ReadBool('Unnecessary spaces to check for', 'Spaces between numbers', True) then
      ErrorsToCheck.eWhatUnnecessarySpaces := ErrorsToCheck.eWhatUnnecessarySpaces + [SpacesBetweenNumbers];


    // ----------------------- //
    //      Errors to fix      //
    // ----------------------- //
    ErrorsToFix.eLinesWithoutLetters  := Ini.ReadBool('Errors to fix', 'Lines without letters',              True);
    ErrorsToFix.eEmptySubtitle        := Ini.ReadBool('Errors to fix', 'Empty subtitles',                    True);
    ErrorsToFix.eOverlapping          := Ini.ReadBool('Errors to fix', 'Overlapping subtitles',              True);
    ErrorsToFix.eBadValues            := Ini.ReadBool('Errors to fix', 'Bad values',                         True);
    ErrorsToFix.eOverTwoLines         := Ini.ReadBool('Errors to fix', 'Subtitles over two lines',           True);
    ErrorsToFix.eHearingImpaired      := Ini.ReadBool('Errors to fix', 'Hearing impaired subtitles',         True);
    ErrorsToFix.eTextBeforeColon      := Ini.ReadBool('Errors to fix', 'Text before colon (":")',            True);
    ErrorsToFix.eOnlyIfCapitalLetters := Ini.ReadBool('Errors to fix', 'Only if text is in capital letters', True);
    ErrorsToFix.eUnnecessaryDots      := Ini.ReadBool('Errors to fix', 'Unnecessary dots',                   True);
    ErrorsToFix.eProhibitedCharacter  := Ini.ReadBool('Errors to fix', 'Prohibited characters',              False);
    ErrorsToFix.eRepeatedCharacter    := Ini.ReadBool('Errors to fix', 'Repeated characters',                True);
    ErrorsToFix.eRepeatedSubtitle     := Ini.ReadBool('Errors to fix', 'Repeated subtitles',                 True);
    ErrorsToFix.eOCRErrors            := Ini.ReadBool('Errors to fix', 'OCR Errors',                         True);
    ErrorsToFix.eOpnDlgSubsOneLine    := Ini.ReadBool('Errors to fix', '"-" in subtitles with one line',     False);
    ErrorsToFix.eSpaceAfterCustChars  := Ini.ReadBool('Errors to fix', 'Space after custom characters',      True);
    ErrorsToFix.eSpaceBeforeCustChars := Ini.ReadBool('Errors to fix', 'Space before custom characters',     False);
    ErrorsToFix.eUnnecessarySpaces    := Ini.ReadBool('Errors to fix', 'Unnecessary spaces',                 True);
    ErrorsToFix.eWhatUnnecessarySpaces := [];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Enters and spaces at the beginning and end', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [EntersAndSpacesBeginningEnd];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Spaces between enters (left and right)', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesBetweenEnters];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Double spaces and enters', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [DoubleSpacesAndEnters];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Spaces in front of punctuation marks', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesFrontPunctuation];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Spaces after "¿" and "¡"', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesAfterQuestionAndExclamation];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Spaces before "?" and  "!"', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesBeforeQuestionAndExclamation];
    if Ini.ReadBool('Unnecessary spaces to fix', 'Spaces between numbers', True) then
      ErrorsToFix.eWhatUnnecessarySpaces := ErrorsToFix.eWhatUnnecessarySpaces + [SpacesBetweenNumbers];

    // --------------------------------- //
    //             File Types            //
    // --------------------------------- //

    if Ini.ReadBool('File types', 'Register extensions on start', False) then
      AssociateExtensions(Ini.ReadString('File types', 'Associated extensions', ''), True);

    // --------------------------------- //
    //           Program Look            //
    // --------------------------------- //
    case Ini.ReadInteger('Program look', '"Text" and "Translation" fields align', 2) of
      0: begin mmoSubtitleText.Alignment := taLeftJustify; mmoTranslation.Alignment := taLeftJustify; end;
      1: begin mmoSubtitleText.Alignment := taRightJustify; mmoTranslation.Alignment := taRightJustify; end else
      begin mmoSubtitleText.Alignment := Classes.taCenter; mmoTranslation.Alignment := Classes.taCenter; end;
    end;

    // --------------------------------- //
    //             List Look             //
    // --------------------------------- //
    if Ini.ReadBool('List look', 'Show grid lines', True) then
      frmMain.lstSubtitles.TreeOptions.PaintOptions := [toShowHorzGridLines, toShowVertGridLines, toShowButtons, toShowDropmark, toShowTreeLines,toThemeAware,toUseBlendedImages] else
      frmMain.lstSubtitles.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowTreeLines,toThemeAware,toUseBlendedImages];
    ApplyStyleInList := Ini.ReadBool('List look', 'Apply style to subtitles', True);
    MarkUnTransSubs  := Ini.ReadBool('List look', 'Mark untranslated subtitles', True);
    UnTransSubsColor := Ini.ReadInteger('List look', 'Untranslated subtitles color', clRed);
    if Ini.ReadBool('List look', 'Show horizontal scrollbar', True) = False then
      lstSubtitles.ScrollBarOptions.ScrollBars := ssVertical else
      lstSubtitles.ScrollBarOptions.ScrollBars := ssBoth;

    // ------------------------- //
    //          Menu Look        //
    // ------------------------- //
    MiMenu.Activo    := Ini.ReadBool('Menu look', 'Use Office XP Style menu', True);
    MiMenu.Degradado := Ini.ReadBool('Menu look', 'Use gradient menu', True);

    // --------------------------------- //
    //             Interfaces            //
    // --------------------------------- //

    mnuShowLeftPanel.Checked := Ini.ReadBool('Interface', 'Show left panel', True);
    if Ini.ReadBool('Interface', 'Show time controls', True) then
      mnuShowTimeControls.Checked := True else
    begin
      mnuShowTimeControls.Checked := False;
      mnuShowTimeControlsClick(Sender);
    end;
    if Ini.ReadBool('Interface', 'Use in-place edition', False) then
    begin
      mnuUseInPlaceEdition.Checked := True;
      lstSubtitles.TreeOptions.MiscOptions := lstSubtitles.TreeOptions.MiscOptions + [toEditable];
    end else
    begin
      mnuUseInPlaceEdition.Checked := False;
      lstSubtitles.TreeOptions.MiscOptions := lstSubtitles.TreeOptions.MiscOptions - [toEditable];
    end;

    SetVideoPreviewMode(Ini.ReadBool('Interface', 'Video Preview Mode', False));
    SetTranslatorMode(Ini.ReadBool('Interface', 'Translator mode', False), False);
    EnableVPCtrls(False);

    // --------------------------------------//
    //    Command line parameters reading    //
    // --------------------------------------//

    for i := 1 to ParamCount do
      CommandLineProcess(ParamStr(i));

  finally
    Ini.Free;
  end;

  // ------------------------------------------------------------------------ //
  //                            Set Output settings                           //
  // ------------------------------------------------------------------------ //
  
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Output.ini');
  try
    // ------------------------ //
    //       DVD Subtitle       //
    // ------------------------ //
    SubtitleAPI.SetOutputSettingsDVDSubtitle(True,
                                             Ini.ReadString('DVD Subtitle', 'Disk ID',   ''),
                                             Ini.ReadString('DVD Subtitle', 'DVD Title', ''),
                                             Ini.ReadString('DVD Subtitle', 'Language',  'EN'),
                                             Ini.ReadString('DVD Subtitle', 'Author',    ''),
                                             Ini.ReadString('DVD Subtitle', 'Web',       ''),
                                             Ini.ReadString('DVD Subtitle', 'Info',      ''),
                                             Ini.ReadString('DVD Subtitle', 'License',   ''));

    // ------------------------ //
    //           SAMI           //
    // ------------------------ //
    if Ini.ReadString('SAMI', 'Align', 'Center') = 'Left' then
      TextAlign := taLeft else
    if Ini.ReadString('SAMI', 'Align', 'Center') = 'Right' then
      TextAlign := taRight else
      TextAlign := taCenter;
    SubtitleAPI.SetOutputSettingsSAMI(True,
                                      Ini.ReadString( 'SAMI', 'Font',           'Tahoma'),
                                      Ini.ReadInteger('SAMI', 'Size',           24),
                                      Ini.ReadBool(   'SAMI', 'Bold',           True),
                                      Ini.ReadBool(   'SAMI', 'Italic',         False),
                                      Ini.ReadBool(   'SAMI', 'Underline',      False),
                                      Ini.ReadInteger('SAMI', 'Subtitle color', clWhite),
                                      Ini.ReadInteger('SAMI', 'Background color', clBlack),
                                      TextAlign);

    // ------------------------ //
    //      Sonic Scenarist     //
    // ------------------------ //
    SubtitleAPI.SetOutputSettingsSonicScenarist(True,
                                                Ini.ReadBool(   'Scenarist', 'PAL',        True),
                                                Ini.ReadBool(   'Scenarist', 'Drop frame', False),
                                                Ini.ReadInteger('Scenarist', 'Color0',     3),
                                                Ini.ReadInteger('Scenarist', 'Color1',     4),
                                                Ini.ReadInteger('Scenarist', 'Color2',     3),
                                                Ini.ReadInteger('Scenarist', 'Color3',     9),
                                                Ini.ReadInteger('Scenarist', 'Contrast0',  0),
                                                Ini.ReadInteger('Scenarist', 'Contrast1',  15),
                                                Ini.ReadInteger('Scenarist', 'Contrast2',  15),
                                                Ini.ReadInteger('Scenarist', 'Contrast3',  15));

    // ------------------------ //
    //         SubViewer        //
    // ------------------------ //
    SubtitleAPI.SetOutputSettingsSubViewer1(True,
                                            Ini.ReadString( 'SubViewer', 'Title',   ''),
                                            Ini.ReadString( 'SubViewer', 'Author',  ''),
                                            Ini.ReadString( 'SubViewer', 'Source',  ''),
                                            Ini.ReadString( 'SubViewer', 'Program', ''),
                                            Ini.ReadString( 'SubViewer', 'Path',    ''),
                                            Ini.ReadInteger('SubViewer', 'Delay',   0));

    // ------------------------ //
    //        SubViewer 2       //
    // ------------------------ //
    SubtitleAPI.SetOutputSettingsSubViewer2(True,
                                            Ini.ReadString( 'SubViewer 2', 'Title',     ''),
                                            Ini.ReadString( 'SubViewer 2', 'Author',    ''),
                                            Ini.ReadString( 'SubViewer 2', 'Source',    ''),
                                            Ini.ReadString( 'SubViewer 2', 'Program',   ''),
                                            Ini.ReadString( 'SubViewer 2', 'Path',      ''),
                                            Ini.ReadInteger('SubViewer 2', 'Delay',     0),
                                            Ini.ReadInteger('SubViewer 2', 'CD-Track',  0),
                                            Ini.ReadString( 'SubViewer 2', 'Comment',   ''),
                                            Ini.ReadString( 'SubViewer 2', 'Font Name', 'Tahoma'),
                                            Ini.ReadInteger('SubViewer 2', 'Font Size', 24),
                                            Ini.ReadInteger('SubViewer 2', 'Color',     clWhite),
                                            Ini.ReadBool(   'SubViewer 2', 'Bold',      True),
                                            Ini.ReadBool(   'SubViewer 2', 'Italic',    False),
                                            Ini.ReadBool(   'SubViewer 2', 'Underline', False),
                                            Ini.ReadBool(   'SubViewer 2', 'Strikeout', False));

    // ------------------------ //
    //     SubStation Alpha     //
    // ------------------------ //
    SubtitleAPI.SetOutputSettingsSubStationAlpha(True,
                                                 Ini.ReadString( 'SSA', 'Title', '<untitled>'),
                                                 Ini.ReadString( 'SSA', 'Script', '<unknown>'),
                                                 Ini.ReadString( 'SSA', 'Font name', 'Tahoma'),
                                                 Ini.ReadInteger('SSA', 'Font size', 24),
                                                 Ini.ReadBool('SSA', 'Bold', True),
                                                 Ini.ReadBool('SSA', 'Italic', False),
                                                 Ini.ReadInteger('SSA', 'BorderStyle', 1),
                                                 Ini.ReadInteger('SSA', 'Primary Color', 16777215),
                                                 Ini.ReadInteger('SSA', 'Secondary Color', 16777215),
                                                 Ini.ReadInteger('SSA', 'Tertiary Color', 16777215),
                                                 Ini.ReadInteger('SSA', 'Shadow Color', 12632256),
                                                 Ini.ReadInteger('SSA', 'Outline', 1),
                                                 Ini.ReadInteger('SSA', 'Shadow', 1),
                                                 (Ini.ReadInteger('SSA', 'Alignment', 2) or (Ini.ReadInteger('SSA', 'Type of subtitles', 1) shl 2)),
                                                 Ini.ReadInteger('SSA', 'Left margin', 30),
                                                 Ini.ReadInteger('SSA', 'Right margin', 30),
                                                 Ini.ReadInteger('SSA', 'Vertical margin', 415),
                                                 Ini.ReadInteger('SSA', 'Encoding', 0)
                                                 );
    // ------------------------ //
    //          TMPlayer        //
    // ------------------------ //

    case Ini.ReadInteger('TMPlayer', 'Format', 0) of
      1: SubtitleAPI.SetOutputSettingsTMPlayer(True, tfTimeStruct1);
      2: SubtitleAPI.SetOutputSettingsTMPlayer(True, tfTimeStruct2);
      3: SubtitleAPI.SetOutputSettingsTMPlayer(True, tfPlusTimeStruct1);
      4: SubtitleAPI.SetOutputSettingsTMPlayer(True, tfPlusTimeStruct2) else
      SubtitleAPI.SetOutputSettingsTMPlayer(True, tfMultiline);
    end;
  finally
    Ini.Free;
  end;

  DragAcceptFiles(frmMain.Handle, True); // For drag & drop
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.spSplitterMoved(Sender: TObject);
begin
  lstSubtitles.Top    := spSplitter.Top + spSplitter.Height;
  lstSubtitles.Height := pnlParent1.Height - spSplitter.top - spSplitter.Height;
  UpdateSubtitlesPos;
  UpdateVideoPos;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormResize(Sender: TObject);
begin
  lstSubtitles.Width := pnlParent1.Width;
  if (mnuVideoPreviewMode.Checked) then
  begin
    SetVideoPreviewMode(True);
    UpdateSubtitlesPos;
    UpdateVideoPos;
    if Player.Initialized then
      SetTimeCounter(GetCurrentPos) else
      tcTimeCounter.Hide;
  end;
  SetTranslationCtrlsPositions;
  cmbOCRScripts.Visible := (mnuShowInMainForm.Checked) and (pnlControl.Height > (cmbOCRScripts.Height + cmbOCRScripts.Top + 3));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuVideoPreviewModeClick(Sender: TObject);
begin
  SetVideoPreviewMode(not mnuVideoPreviewMode.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuCloseClick(Sender: TObject);
begin
  CloseSub;
end;
                                
// -----------------------------------------------------------------------------

procedure TfrmMain.mnuLoadSubtitleClick(Sender: TObject);
begin
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
  begin
    if (dlgLoadFile.FilterIndex) <= (SubtitleAPI.FormatsCount+1) then
      LoadSubtitle(dlgLoadFile.FileName, GetInputFPS, dlgLoadFile.FilterIndex) else
    begin
      if (dlgLoadFile.FilterIndex) = (SubtitleAPI.FormatsCount+2) then
        LoadSRF(dlgLoadFile.FileName) else
        LoadPlainText(dlgLoadFile.FileName);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Ini : TIniFile;
  a   : Integer;
begin
  SubtitleAPI.Free;
  SaveFPS(cmbInputFPS);

  Ini := TIniFile.Create(IniRoot);
  try
    // Save current language...
    Ini.WriteString('Language', 'Language', Copy(ExtractFileName(ActualLangFile), 0, Length(ExtractFileName(ActualLangFile))-4));
    // Save recent files...
    SaveRecentFiles;
    // Free recent files TStringList...
    RecentFiles.Free;
    // Current OCR Script...
    Ini.WriteString('Information and Errors', 'OCR Definitions file', OCRDefFile);
    Ini.WriteBool('General', 'Show OCR Scripts in main form', mnuShowInMainForm.Checked);

    // Work with...
    if rdoDuration.Checked then
      Ini.WriteInteger('General','Work with', 1) else
    if rdoFinalTime.Checked then
      Ini.WriteInteger('General','Work with', 2) else
    if rdoBoth.Checked then
      Ini.WriteInteger('General','Work with', 3);

    // Save last folder
    Ini.WriteString('General', 'Last folder', dlgLoadFile.InitialDir);

    // Charsets...
    Ini.WriteInteger('General', 'Original charset', cmbOrgCharset.ItemIndex);
    Ini.WriteInteger('General', 'Translated charset', cmbTransCharset.ItemIndex);

    if mnuTranslatorMode.Checked = False then
    begin
      // Columns width for normal mode
      Ini.WriteInteger('Columns width (normal mode)', '1', lstSubtitles.Header.Columns[0].Width);
      Ini.WriteInteger('Columns width (normal mode)', '2', lstSubtitles.Header.Columns[1].Width);
      Ini.WriteInteger('Columns width (normal mode)', '3', lstSubtitles.Header.Columns[2].Width);
      Ini.WriteInteger('Columns width (normal mode)', '4', lstSubtitles.Header.Columns[3].Width);
    end else
    begin
      // Columns width for translator mode
      Ini.WriteInteger('Columns width (translator mode)', '1', lstSubtitles.Header.Columns[0].Width);
      Ini.WriteInteger('Columns width (translator mode)', '2', lstSubtitles.Header.Columns[1].Width);
      Ini.WriteInteger('Columns width (translator mode)', '3', lstSubtitles.Header.Columns[2].Width);
      Ini.WriteInteger('Columns width (translator mode)', '4', lstSubtitles.Header.Columns[3].Width);
      Ini.WriteInteger('Columns width (translator mode)', '5', lstSubtitles.Header.Columns[4].Width);
    end;  

    // --------------------------------- //
    //          WINDOW POSITION          //
    // --------------------------------- //
    if WindowState = wsMaximized then
    begin
      Ini.WriteBool('Main Window', 'Maximized', True);
    end else
    begin
      Ini.WriteInteger('Main Window', 'Left', Left);
      Ini.WriteInteger('Main Window','Top', Top);
      Ini.WriteInteger('Main Window','Width', Width);
      Ini.WriteInteger('Main Window','Height', Height);
      Ini.WriteBool('Main Window', 'Maximized', False);
    end;

    // ----------------- //
    //   Video preview   //
    // ----------------- //
    Ini.WriteInteger('Video preview', 'Video panel height', pnlVideo.Height);
    if mnu10P.Checked then a := 1 else
    if mnu20P.Checked then a := 2 else
    if mnu30P.Checked then a := 3 else
    if mnu40P.Checked then a := 4 else
    if mnu50P.Checked then a := 5 else
    if mnu60P.Checked then a := 6 else
    if mnu70P.Checked then a := 7 else
    if mnu80P.Checked then a := 8 else
    if mnu90P.Checked then a := 9 else
    if mnu200P.Checked then a := 10 else
    if mnu300P.Checked then a := 11 else
    if mnu400P.Checked then a := 12 else
      a := 0;
    Ini.WriteInteger('Video preview', 'Playback rate', a);
    Ini.WriteBool('Video preview', 'Scroll list', ScrollList);
    Ini.WriteBool('Video preview', 'Displaying original', mnuDisplayOriginal.Checked);
    Ini.WriteBool('Video preview', 'Show subtitles', mnuShowSubtitles.Checked);

    // ---------------- //
    //     Interfaces   //
    // ---------------- //
    Ini.WriteBool('Interface', 'Translator mode', mnuTranslatorMode.Checked);
    Ini.WriteBool('Interface', 'Video Preview Mode', mnuVideoPreviewMode.Checked);
    Ini.WriteBool('Interface', 'Show left panel', mnuShowLeftPanel.Checked);
    Ini.WriteBool('Interface', 'Show time controls', mnuShowTimeControls.Checked);
    Ini.WriteBool('Interface', 'Use in-place edition', mnuUseInPlaceEdition.Checked);

    ClearUndoList(UndoList);
    ClearUndoList(RedoList);

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFileClick(Sender: TObject);
begin
  mnuRecent.Visible           := (RecentFiles.Count > 0) and (RFMaxCount > 0);
  mnuLoadSubtitle.Enabled     := mnuTranslatorMode.Checked = False;
  mnuSaveFile.Enabled         := (lstSubtitles.RootNodeCount > 0);
  mnuSaveFileAs.Enabled       := (lstSubtitles.RootNodeCount > 0);
  mnuTranslatorSave.Enabled   := (lstSubtitles.RootNodeCount > 0);
  mnuSaveProject.Enabled      := (lstSubtitles.RootNodeCount > 0);
  mnuSaveOriginal.Enabled     := (lstSubtitles.RootNodeCount > 0);
  mnuSaveTranslated.Enabled   := (lstSubtitles.RootNodeCount > 0);
  mnuSaveOriginalAs.Enabled   := (lstSubtitles.RootNodeCount > 0) and (mnuTranslatorMode.Checked);
  mnuSaveTranslatedAs.Enabled := (lstSubtitles.RootNodeCount > 0) and (mnuTranslatorMode.Checked);
  mnuLoadTranslated.Enabled   := (OrgFile <> '');
  mnuClose.Enabled            := lstSubtitles.RootNodeCount > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuEditClick(Sender: TObject);
begin
  mnuInsertSubtitle.Enabled    := InterfaceEnabled;
  mnuInsertBefore.Enabled      := InterfaceEnabled;
  mnuRemoveSelected.Enabled    := lstSubtitles.SelectedCount > 0;
  mnuCut.Enabled               := lstSubtitles.SelectedCount > 0;
  mnuCopy.Enabled              := lstSubtitles.SelectedCount > 0;
  mnuPaste.Enabled             := (ClipBoard.HasFormat(CF_TEXT)) and (InterfaceEnabled);
  mnuSelectAll.Enabled         := lstSubtitles.RootNodeCount > 0;
  // -------
  mnuTimings.Enabled           := lstSubtitles.RootNodeCount > 0;
  mnuShiftPlusMS.Caption       := Format(ShiftMS, ['+' + IntToStr(ShiftTime)]);
  mnuShiftLessMS.Caption       := Format(ShiftMS, ['-' + IntToStr(ShiftTime)]);  
  mnuTexts.Enabled             := lstSubtitles.RootNodeCount > 0;
  mnuDivideLines.Enabled       := (lstSubtitles.RootNodeCount > 0) and (lstSubtitles.SelectedCount = 1) and (Assigned(lstSubtitles.FocusedNode)) and ((Length(GetSubText(lstSubtitles.FocusedNode)) > BreakLineAfter) or (Pos(#13#10, GetSubText(lstSubtitles.FocusedNode)) > 0));
  mnuFastDivideLines.Enabled   := mnuDivideLines.Enabled;
  mnuSubtitles.Enabled         := lstSubtitles.RootNodeCount > 0;
  mnuEffects.Enabled           := GetSubText(frmMain.lstSubtitles.FocusedNode) <> '';
  mnuCombineSubtitles.Enabled  := lstSubtitles.SelectedCount > 1;
  mnuSort.Enabled              := lstSubtitles.RootNodeCount > 1;
  // -------
  mnuSwap.Enabled := (mnuTranslatorMode.Checked) and (InterfaceEnabled);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSearchClick(Sender: TObject);
begin
  mnuSubSearch.Enabled        := lstSubtitles.RootNodeCount > 0;
  mnuFindNext.Enabled         := (lstSubtitles.RootNodeCount > 0) and (SearchWord <> '');
  mnuSearchAndReplace.Enabled := lstSubtitles.RootNodeCount > 0;
  mnuGoToLineNumber.Enabled   := lstSubtitles.RootNodeCount > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuToolsClick(Sender: TObject);
begin
  mnuSpellCheck.Enabled           := lstSubtitles.RootNodeCount > 0;
  mnuSplitSubtitle.Enabled        := lstSubtitles.RootNodeCount > 1;
  mnuInformationAndErrors.Enabled := lstSubtitles.RootNodeCount > 0;
  mnuVariousInformation.Enabled   := InterfaceEnabled;
  mnuRecheckErrors.Enabled        := (MarkErrorsInList) and (lstSubtitles.RootNodeCount > 0);
  mnuFixAllErrors.Enabled         := lstSubtitles.RootNodeCount > 0;
  mnuFixErrorsInSelSubs.Enabled   := lstSubtitles.SelectedCount > 0;
  mnuJumpToNextError.Enabled      := lstSubtitles.RootNodeCount > 0;
  mnuExternalPreview.Enabled      := lstSubtitles.RootNodeCount > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  frmMain.Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuClearListClick(Sender: TObject);
begin
  RecentFiles.Clear;
  UpdateRFMenus;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DeleteSelectedNodes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuOpenMovieClick(Sender: TObject);
begin
  dlgLoadFile.Filter := AllSupportedFiles + '|*.asf;*.avi;*.mp4;*.mkv;*.divx;*.mp3;*.mpg;*.mpeg;*.m1v;*.ogm;*.ogg;*.qt;*.vob;*.wav;*.wmv|' + 'ASF (*.asf)|*.asf|AVI (*.avi)|*.avi|OGM (*.ogm)|*.ogm|OGG (*.ogg)|*.ogg|Matroska (*.mkv)|*.mkv|DivX (*.mp4; *.divx)|*.mp4; ' + '*.divx|MP3 (*.mp3)|*.mp3|MPEG (*.mpg; *.mpeg; *.m1v)|*.mpg; *.mpeg; *.m1v|QuickTime 2.0 (*.qt)|*.qt|VOB (*.vob)|*.vob|WAV (*.wav)|*.wmv|WMV (*.wmv)|*.wmv';
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
  begin
    if LoadMovie(dlgLoadFile.FileName) = False then
      MsgBox(Format(ErrorMsg[05], [dlgLoadFile.FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain) else
    begin
      if mnuVideoPreviewMode.Checked = False then
        SetVideoPreviewMode(True);
    end;
  end;
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuCloseMovieClick(Sender: TObject);
begin
  FreeFile;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tmrVideoTimer(Sender: TObject);
var
  Node         : PVirtualNode;
  Data         : PSubtitleItem;
  LastNodeData : PSubtitleItem;
  CurrTime     : Integer;
  CurrentPos   : Int64;
  SubtitleText : String;
begin
  if (Player.Initialized) and (mnuVideoPreviewMode.Checked = True) then
  begin
    Player.MediaSeeking.GetCurrentPosition(CurrentPos);
    CurrTime := CurrentPos div 10000;

    if lstSubtitles.RootNodeCount > 0 then
    begin
      Node := lstSubtitles.GetFirst;
      LastNodeData := lstSubtitles.GetNodeData(lstSubtitles.GetLast);
      while Assigned(Node) do
      begin
        Data := lstSubtitles.GetNodeData(Node);
        if (CurrTime >= Data.InitialTime) and (CurrTime <= Data.FinalTime) then
        begin
          if mnuTranslatorMode.Checked then
          begin
            if mnuDisplayOriginal.Checked then
              SubtitleText := Data.Text else
              SubtitleText := Data.Translation;
          end else
            SubtitleText := Data.Text;

          if subSubtitle.Text <> SubtitleText then
          begin
            subSubtitle.Hide;
            subSubtitle.Font.Charset := OrgCharset;
            subSubtitle.Text := SubtitleText;
            UpdateSubtitlesPos;
            if ScrollList then
            begin
              UnSelectAll(lstSubtitles);
              lstSubtitles.ScrollIntoView(Node, True);
              lstSubtitles.Selected[Node] := True;
              lstSubtitles.FocusedNode    := Node;
            end;
            if mnuShowSubtitles.Checked then subSubtitle.Show;
          end;
          Break;
        end 
        else if (CurrTime < Data.FinalTime) or (CurrTime > LastNodeData.FinalTime) then
        begin
          subSubtitle.Visible := False;
          subSubtitle.Text := '';
          Break;
        end;  
        Node := Node.NextSibling;
      end;
    end;

    if VideoDuration > 0 then
    begin
      if CurrentPos = (VideoDuration * 10000) then
      begin
        CurrentPos := 0;
        Player.MediaSeeking.SetPositions(CurrentPos, AM_SEEKING_ABSOLUTEPOSITIONING, CurrentPos, AM_SEEKING_NOPOSITIONING);
        Player.MediaControl.Stop;
        Playing := False;
        tmrVideo.Enabled := False;
        btnPlay.Show;
        btnPause.Hide;
        sbSeekBar.Position := 0;
      end else
      begin
        sbSeekBar.Position := CurrentPos div 10000;
        SetTimeCounter(CurrentPos div 10000);
      end;
    end;

  end;  
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnPlayClick(Sender: TObject);
begin
  Pause;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuPlayPauseClick(Sender: TObject);
begin
  Pause;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  Stop;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tmrSeekRewFFTimer(Sender: TObject);
begin
  while (Seeking) do
  begin
    Application.ProcessMessages;
    if btnRew.State = sPressed then
      SetVideoPos(GetCurrentPos - RewFFTime) else
    if btnForward.State = sPressed then
      SetVideoPos(GetCurrentPos + RewFFTime);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnRewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetVideoPos(GetCurrentPos - RewFFTime);
  Seeking := True;
  tmrSeekRewFF          := TTimer.Create(Application);
  tmrSeekRewFF.Interval := 500;
  tmrSeekRewFF.OnTimer  := tmrSeekRewFFTimer;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnRewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Seeking := False;
  tmrSeekRewFF.Enabled := False;
  tmrSeekRewFF.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnForwardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetVideoPos(GetCurrentPos + RewFFTime);
  Seeking := True;
  tmrSeekRewFF          := TTimer.Create(Application);
  tmrSeekRewFF.Interval := 500;
  tmrSeekRewFF.OnTimer  := tmrSeekRewFFTimer;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnForwardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Seeking := False;
  tmrSeekRewFF.Enabled := False;
  tmrSeekRewFF.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuStopClick(Sender: TObject);
begin
  Stop;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuMovieClick(Sender: TObject);
begin
  mnuCloseMovie.Enabled := Player.Initialized;
  mnuMovieInfo.Enabled  := Player.Initialized;
  // ------
  mnuPlayback.Enabled   := Player.Initialized;
  mnuPlayPause.Enabled  := Player.Initialized;
  mnuStop.Enabled       := Player.Initialized;
  mnuBack5Sec.Enabled   := Player.Initialized;
  mnuFwd5Sec.Enabled    := Player.Initialized;
  // ------
  mnuPlaybackRate.Enabled := Player.Initialized;
  mnu50P.Enabled          := Player.Initialized;
  mnu60P.Enabled          := Player.Initialized;
  mnu70P.Enabled          := Player.Initialized;
  mnu80P.Enabled          := Player.Initialized;
  mnu90P.Enabled          := Player.Initialized;
  mnu100P.Enabled         := Player.Initialized;
  // ------
  mnuVidSubtitles.Enabled  := Player.Initialized;
  mnuSetStartTime.Enabled  := Player.Initialized;
  mnuSetFinalTime.Enabled  := Player.Initialized;
  mnuStartSubtitle.Enabled := Player.Initialized;
  mnuEndSubtitle.Enabled   := Player.Initialized;
  // ------
  mnuSynchronization.Enabled := (Player.Initialized) and (lstSubtitles.RootNodeCount > 0);
  mnuFirstSyncPoint.Enabled  := (Player.Initialized) and (lstSubtitles.RootNodeCount > 0);
  mnuLastSyncPoint.Enabled   := (Player.Initialized) and (lstSubtitles.RootNodeCount > 0);
  mnuAddSyncPoint.Enabled    := (Player.Initialized) and (lstSubtitles.RootNodeCount > 0);
  // ------
  mnuSubtitleToDisplay.Visible    := mnuTranslatorMode.Checked;
  mnuSaveMediaStartupFile.Enabled := (OrgFile <> '') and (MovieFile <> '');
  mnuSaveASX.Enabled              := (OrgFile <> '') and (MovieFile <> '');
  mnuSaveSMIL.Enabled             := (OrgFile <> '') and (MovieFile <> '');   
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSubSettingsClick(Sender: TObject);
begin
  frmSettings := TfrmSettings.Create(Application);
  frmSettings.ShowModal;
  // Apply always on top
  if frmSettings.chkAlwaysOnTop.Checked then
    SetWindowPos(frmMain.Handle, HWND_TOPMOST, frmMain.Left, frmMain.Top, frmMain.Width, frmMain.Height, SWP_NOMOVE + SWP_NOSIZE) else
    SetWindowPos(frmMain.Handle, HWND_NOTOPMOST, frmMain.Left, frmMain.Top, frmMain.Width, frmMain.Height, SWP_NOMOVE + SWP_NOSIZE);
  frmSettings.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuOutputSettingsClick(Sender: TObject);
begin
  frmOutputSettings := TfrmOutputSettings.Create(Application);
  frmOutputSettings.ShowModal;
  frmOutputSettings.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbModeSelect(Sender: TObject);
begin
  if cmbMode.ItemIndex = 0 then
    FormatType := ftTime else
    FormatType := ftFrames;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.rdoDurationClick(Sender: TObject);
begin
  if InterfaceEnabled then
  begin
    lblHide.Enabled     := False;
    tmeHide.Enabled     := False;
    lblDuration.Enabled := True;
    tmeDuration.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.rdoFinalTimeClick(Sender: TObject);
begin
  if InterfaceEnabled then
  begin
    lblHide.Enabled     := True;
    tmeHide.Enabled     := True;
    lblDuration.Enabled := False;
    tmeDuration.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.rdoBothClick(Sender: TObject);
begin
  if InterfaceEnabled then
  begin
    lblHide.Enabled     := True;
    tmeHide.Enabled     := True;
    lblDuration.Enabled := True;
    tmeDuration.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tmeShowTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
var
  UndoAction: PUndoAction;
begin
  if (lstSubtitles.SelectedCount = 1) then
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    New(UndoAction);
    UndoAction^.UndoActionType                 := uaTimeChange;
    UndoAction^.BufferSize                     := SizeOf(TTimeChange);
    UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node                           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber                     := UndoAction^.Node.Index;
    UndoAction^.BindToNext                     := False;
    PTimeChange(UndoAction^.Buffer)^.StartTime := GetStartTime(lstSubtitles.FocusedNode);
    PTimeChange(UndoAction^.Buffer)^.FinalTime := -1;
    UndoList.Add(UndoAction);
    mnuUndo.Enabled := True;

    SetStartTime(lstSubtitles.FocusedNode, NewTime);
    lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tmeHideTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
var
  UndoAction: PUndoAction;
begin
  if (lstSubtitles.SelectedCount = 1) then
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    New(UndoAction);
    UndoAction^.UndoActionType                 := uaTimeChange;
    UndoAction^.BufferSize                     := SizeOf(TTimeChange);
    UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node                           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber                     := UndoAction^.Node.Index;
    UndoAction^.BindToNext                     := False;
    PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
    PTimeChange(UndoAction^.Buffer)^.FinalTime := GetFinalTime(lstSubtitles.FocusedNode);
    UndoList.Add(UndoAction);
    mnuUndo.Enabled := True;

    SetFinalTime(lstSubtitles.FocusedNode, NewTime);
    lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tmeDurationTimeChangeFromEditOnly(Sender: TObject; NewTime: Cardinal);
var
  UndoAction: PUndoAction;
begin
  if (lstSubtitles.SelectedCount = 1) then
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    New(UndoAction);
    UndoAction^.UndoActionType                 := uaTimeChange;
    UndoAction^.BufferSize                     := SizeOf(TTimeChange);
    UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node                           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber                     := UndoAction^.Node.Index;
    UndoAction^.BindToNext                     := False;
    PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
    PTimeChange(UndoAction^.Buffer)^.FinalTime := GetFinalTime(lstSubtitles.FocusedNode);
    UndoList.Add(UndoAction);
    mnuUndo.Enabled := True;
    
    SetFinalTime(lstSubtitles.FocusedNode, Cardinal(GetStartTime(lstSubtitles.FocusedNode)) + NewTime);
    lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveProjectClick(Sender: TObject);
var
  dlgSaveProject : TSaveDialog;
  Ini            : TIniFile;
  OriginalFile   : String;
  TranslatedFile : String;
  MMovieFile     : String;
begin
  dlgSaveProject := TSaveDialog.Create(Application);
  try
    if OrgFile <> '' then
    begin
      UpdateArray;
      if SaveFile(OrgFile, OrgFormat, GetFPS) then  // We save original file in it's original format
        OrgModified := False;
      SubtitleAPI.ClearSubtitles;
    end else
      mnuSaveFileAsClick(Sender);

    if TransFile <> '' then
    begin
      UpdateArray(True);
      if SaveFile(TransFile, TransFormat, GetFPS) then // We save translated file in it's original format
        TransModified := False;
      SubtitleAPI.ClearSubtitles;
    end else // If file doesn't exist then, save as...
      mnuSaveTranslatedAsClick(Sender);

    if (OrgFile <> '') and (TransFile <> '') then
    begin
      dlgSaveProject.Filter := ID_STPROJECT + ' (*' + ID_STPEXT + ')|*' + ID_STPEXT;
      if (dlgSaveProject.Execute) and (dlgSaveProject.FileName <> '') then
      begin
        if ExtractFileExt(dlgSaveProject.FileName) = '' then
          dlgSaveProject.FileName := dlgSaveProject.FileName + ID_STPEXT;
        if (ExtractFilePath(OrgFile) = ExtractFilePath(TransFile)) and
           (ExtractFilePath(OrgFile) = ExtractFilePath(dlgSaveProject.FileName)) then
        begin
          OriginalFile   := ExtractFileName(OrgFile);
          TranslatedFile := ExtractFileName(TransFile);
        end else
        begin
          OriginalFile   := OrgFile;
          TranslatedFile := TransFile;
        end;
        if (ExtractFilePath(dlgSaveProject.FileName)) = (ExtractFilePath(MovieFile)) then
          MMovieFile := ExtractFileName(MovieFile) else
          MMovieFile := MovieFile;

        Ini := TIniFile.Create(dlgSaveProject.FileName);
        try
          Ini.WriteString('Subtitle files', 'Original', OriginalFile);
          Ini.WriteString('Subtitle files', 'Translated', TranslatedFile);
          Ini.WriteString('Movie file', 'Movie', MMovieFile);
          Ini.WriteInteger('Other', 'Focused node', lstSubtitles.FocusedNode.Index);
          Ini.WriteInteger('Movie file', 'Position', GetCurrentPos);
        finally
          Ini.Free;
        end;
      end;
    end;
  finally
    dlgSaveProject.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveFileClick(Sender: TObject);
begin
  if OrgFile <> '' then
  begin
    UpdateArray;
    if SaveFile(OrgFile, OrgFormat, GetFPS) then  // We save original file in it's original format
      OrgModified := False;
    SubtitleAPI.ClearSubtitles;
  end else
    mnuSaveFileAsClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.SetTranslationCtrlsPositions;
begin
  if mnuShowLeftPanel.Checked then
  begin
    pnlControl.Visible := True;
    pnlParent2.Left    := pnlControl.Left + pnlControl.Width + 7;
    pnlParent2.Width   := frmMain.ClientWidth - (pnlControl.Left + pnlControl.Width) - 15;

    lblShow.Left     := pnlParent2.Left;
    tmeShow.Left     := pnlParent2.Left;

    lblHide.Left     := tmeShow.Left + tmeShow.Width + 7;
    tmeHide.Left     := lblHide.Left;

    lblDuration.Left := pnlParent2.Left;
    tmeDuration.Left := pnlParent2.Left;
  end else
  begin
    pnlControl.Visible := False;
    pnlParent2.Left    := pnlControl.Left;
    pnlParent2.Width   := frmMain.ClientWidth - 16;

    lblShow.Left     := pnlControl.Left;
    tmeShow.Left     := pnlControl.Left;

    lblHide.Left     := tmeShow.Left + tmeShow.Width + 21;
    tmeHide.Left     := lblHide.Left;

    lblDuration.Left := pnlControl.Left;
    tmeDuration.Left := pnlControl.Left;
  end;

  if mnuShowTimeControls.Checked then
    mmoSubtitleText.Left := tmeHide.Left + tmeHide.Width + 8 else
    mmoSubtitleText.Left := tmeShow.Left;

  if mnuTranslatorMode.Checked then
  begin
    mmoSubtitleText.Width := ((frmMain.ClientWidth - mmoSubtitleText.Left) div 2) - 7;
    mmoTranslation.Width  := mmoSubtitleText.Width;
    mmoTranslation.Left   := mmoSubtitleText.Left + mmoSubtitleText.Width + 7;
    mmoTranslation.Height := mmoSubtitleText.Height;
  end else
    mmoSubtitleText.Width := (frmMain.ClientWidth - mmoSubtitleText.Left) - 8;
    
  mmoSubtitleText.Top := tmeHide.Top;
  mmoTranslation.Top  := mmoSubtitleText.Top;
  // Label...
  lblText.Left        := mmoSubtitleText.Left;
  lblTranslation.Top  := lblText.Top;
  lblTranslation.Left := mmoTranslation.Left;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuTranslatorModeClick(Sender: TObject);
begin
  SetTranslatorMode(not mnuTranslatorMode.Checked);
end;
// -----------------------------------------------------------------------------

procedure TfrmMain.mnuLoadProjectClick(Sender: TObject);
begin                 
  dlgLoadFile.Filter := ID_STPROJECT + ' (*' + ID_STPEXT + ')|*.stp';
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
    LoadProject(dlgLoadFile.FileName);
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuLoadOriginalClick(Sender: TObject);
begin
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
    LoadSubtitle(dlgLoadFile.FileName, GetInputFPS, dlgLoadFile.FilterIndex);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuLoadTranslatedClick(Sender: TObject);
begin
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
    LoadSubtitle(dlgLoadFile.FileName, GetInputFPS, dlgLoadFile.FilterIndex, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveTranslatedClick(Sender: TObject);
begin
  if TransFile <> '' then
  begin
    UpdateArray(True);
    if SaveFile(TransFile, TransFormat, GetFPS) then // We save translated file in it's original format
      TransModified := False;
    SubtitleAPI.ClearSubtitles
  end else // If file doesn't exist then, save as...
    mnuSaveTranslatedAsClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveFileAsClick(Sender: TObject);
begin
  frmSaveAs := TfrmSaveAs.Create(Application);
  frmSaveAs.SaveTranslation := False;
  frmSaveAs.ShowModal;
  frmSaveAs.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveTranslatedAsClick(Sender: TObject);
begin
  frmSaveAs := TfrmSaveAs.Create(Application);
  frmSaveAs.SaveTranslation := True;
  frmSaveAs.ShowModal;
  frmSaveAs.Free;
  SubtitleAPI.ClearSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mmoSubtitleTextChange(Sender: TObject);
var
  OldString: String;
begin
  if (lstSubtitles.SelectedCount = 1) and (mmoSubtitleText.Text <> ReplaceString(GetSubText(lstSubtitles.FocusedNode), '|', #13#10)) and (GetFocus <> lstSubtitles.Handle) then
  begin
    OldString := GetSubText(lstSubtitles.FocusedNode);
    SetText(lstSubtitles.FocusedNode, mmoSubtitleText.Text);
    DetectChangesForUndo(OldString, GetSubText(lstSubtitles.FocusedNode), True);
    lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
    OrgModified := True;
  end;
  lblText.Caption := Format(TextOrTransLength, [LabelText, GetLengthForEachLine(mmoSubtitleText.Text)]);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mmoTranslationChange(Sender: TObject);
var
  OldString: String;
begin
  if mnuTranslatorMode.Checked = True then
  begin
    if (lstSubtitles.SelectedCount = 1) and (mmoTranslation.Text <> GetSubTranslation(lstSubtitles.FocusedNode)) and (GetFocus <> lstSubtitles.Handle) then
    begin
      OldString := GetSubTranslation(lstSubtitles.FocusedNode);
      SetTranslation(lstSubtitles.FocusedNode, mmoTranslation.Text);
      DetectChangesForUndo(OldString, GetSubTranslation(lstSubtitles.FocusedNode), False);
      lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
      TransModified := True;
    end;
  end;
  lblTranslation.Caption := Format(TextOrTransLength, [LabelTranslation, GetLengthForEachLine(mmoTranslation.Text)]);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesDblClick(Sender: TObject);
begin
  if (lstSubtitles.Enabled) and (mmoSubtitleText.Enabled) and (mmoTranslation.Enabled) then
  begin
    if (Player.Initialized) and (mnuVideoPreviewMode.Checked = True) then
    begin
      if (HiWord(GetKeyState(VK_SHIFT)) <> 0) then
      begin
        UnSelectAll(lstSubtitles);
        case OnShiftDblClick of
          0:
            begin
              if mnuTranslatorMode.Checked then
                mmoTranslation.SetFocus else
                mmoSubtitleText.SetFocus;
            end;
          1: SetVideoPos(GetStartTime(lstSubtitles.FocusedNode));
          2: SetVideoPos(GetStartTime(lstSubtitles.FocusedNode) - (SecsToJump2 * 1000));
        end;
        lstSubtitles.Selected[lstSubtitles.FocusedNode] := True;
      end else
      begin
        case OnDoubleClick of
          0:
            begin
              if mnuTranslatorMode.Checked then
                mmoTranslation.SetFocus else
                mmoSubtitleText.SetFocus;
            end;
          1: SetVideoPos(GetStartTime(lstSubtitles.FocusedNode));
          2: SetVideoPos(GetStartTime(lstSubtitles.FocusedNode) - (SecsToJump1 * 1000));
        end;
      end;  

    end else
    begin
      if mnuTranslatorMode.Checked then
        mmoTranslation.SetFocus else
        mmoSubtitleText.SetFocus;
    end;  
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuDisplayOriginalClick(Sender: TObject);
begin
  mnuDisplayOriginal.Checked    := True;
  mnuDisplayTranslation.Checked := False;
  subSubtitle.Font.Charset      := OrgCharset;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuDisplayTranslationClick(Sender: TObject);
begin
  mnuDisplayOriginal.Checked    := False;
  mnuDisplayTranslation.Checked := True;
  subSubtitle.Font.Charset      := TransCharset;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data     : PSubtitleItem;
  Color    : Integer;
  ItemText : String;
begin
  Data := Sender.GetNodeData(Node);

  if Column = 3 then
    ItemText := Data.Text else
  if Column = 4 then
    ItemText := Data.Translation;

  if (((Data.ErrorType <> []) and (MarkErrorsInList)) or (Data.Marked)) and (Column <> 4) then
  begin
    if (Column = 1) or (Column = 2) or (Column = 3) then
    begin
      TargetCanvas.Font.Color := MarkWithColor;
      TargetCanvas.Font.Style := [];
      if MarkBold      then TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if MarkItalic    then TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic];
      if MarkUnderline then TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];
    end;
  end else
  if (Column = 3) or (Column = 4) then
  begin
    Color := GetSubColor(ItemText);
    if ApplyStyleInList then
    begin
      if (Pos('<i>', ItemText) > 0) then
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic];
      if (Pos('<b>', ItemText) > 0) then
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if (Pos('<u>', ItemText) > 0) then
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];
      if (Color > -1) then
        TargetCanvas.Font.Color := Color;
    end;
    if (mnuTranslatorMode.Checked) and (MarkUntransSubs) and (Column = 4) then
    begin
      // We mark items that are not translated...
      if (Data.Text = Data.Translation) or (Data.Translation = '') or (Data.Translation = UntranslatedSub) then
        TargetCanvas.Font.Color := UnTransSubsColor else
        begin
          if (ApplyStyleInList) and (Color > -1) then
            TargetCanvas.Font.Color := Color;
        end;
    end;
  end;
end;


// -----------------------------------------------------------------------------

procedure TfrmMain.btnMoveSubtitleClick(Sender: TObject);
var
  CurrTime : Integer;
  DiffTime : Integer;
begin
  if (Player.Initialized) and (mnuVideoPreviewMode.Checked) and (lstSubtitles.SelectedCount >= 1) then
  begin
    CurrTime := GetCurrentPos;
    DiffTime := CurrTime - GetStartTime(lstSubtitles.GetFirstSelected);
    SetDelay(DiffTime, True);
  end;  
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnSetStartTimeClick(Sender: TObject);
var
  UndoAction: PUndoAction;
begin
  if (Player.Initialized) and (mnuVideoPreviewMode.Checked) and (Assigned(lstSubtitles.FocusedNode)) and (lstSubtitles.SelectedCount = 1) then
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    New(UndoAction);
    UndoAction^.UndoActionType := uaTimeChange;
    UndoAction^.BufferSize     := SizeOf(TTimeChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
    UndoAction^.BindToNext     := False;
    PTimeChange(UndoAction^.Buffer)^.StartTime := GetStartTime(lstSubtitles.FocusedNode);
    PTimeChange(UndoAction^.Buffer)^.FinalTime := -1;
    UndoList.Add(UndoAction);

    mnuUndo.Enabled := True;
    SetStartTime(lstSubtitles.FocusedNode, GetCurrentPos);
    lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
    RefreshTimes;
  end; 
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnSetFinalTimeClick(Sender: TObject);
var
  UndoAction  : PUndoAction;
  UndoAction2 : PUndoAction;
  CurrPos     : Integer;
begin
  if (Player.Initialized) and (mnuVideoPreviewMode.Checked) and (Assigned(lstSubtitles.FocusedNode)) and (lstSubtitles.SelectedCount = 1) then
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
      
    New(UndoAction);
    UndoAction^.UndoActionType := uaTimeChange;
    UndoAction^.BufferSize     := SizeOf(TTimeChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := lstSubtitles.FocusedNode;
    UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
    UndoAction^.BindToNext     := False;
    PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
    PTimeChange(UndoAction^.Buffer)^.FinalTime := GetFinalTime(lstSubtitles.FocusedNode);
    UndoList.Add(UndoAction);

    CurrPos := GetCurrentPos;
    SetFinalTime(lstSubtitles.FocusedNode, CurrPos);
    if Assigned(lstSubtitles.FocusedNode.NextSibling) then
    begin
      if GetStartTime(lstSubtitles.FocusedNode.NextSibling) <= CurrPos then
      begin
        PUndoAction(UndoList.Last)^.BindToNext := True;

        New(UndoAction2);
        UndoAction2^.UndoActionType := uaTimeChange;
        UndoAction2^.BufferSize     := SizeOf(TTimeChange);
        UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);
        UndoAction2^.Node           := lstSubtitles.FocusedNode.NextSibling;
        UndoAction2^.LineNumber     := lstSubtitles.FocusedNode.NextSibling.Index;
        UndoAction2^.BindToNext     := False;
        PTimeChange(UndoAction2^.Buffer)^.StartTime := GetStartTime(lstSubtitles.FocusedNode.NextSibling);
        PTimeChange(UndoAction2^.Buffer)^.FinalTime := -1;
        UndoList.Add(UndoAction2);

        if FormatType = ftTime then
          SetStartTime(lstSubtitles.FocusedNode.NextSibling, CurrPos + ID_TIMEOVERLAPPRECISION) else
          SetStartTime(lstSubtitles.FocusedNode.NextSibling, CurrPos + Round(GetFPS));
      end;
      lstSubtitles.Selected[lstSubtitles.FocusedNode] := False;
      lstSubtitles.FocusedNode := lstSubtitles.FocusedNode.NextSibling;
    end;
    lstSubtitles.Selected[lstSubtitles.FocusedNode] := True;
    RefreshTimes;
    mnuUndo.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnStartSubtitleClick(Sender: TObject);
begin
  if (Player.Initialized) then
    StartSubTime := GetCurrentPos; 
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnEndSubtitleClick(Sender: TObject);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  if (Player.Initialized) and (StartSubTime <> -1) and (lstSubtitles.Enabled) then
  begin
    Node := InsertNode;
    if Assigned(lstSubtitles.FocusedNode) then
      lstSubtitles.Selected[lstSubtitles.FocusedNode] := False;
    lstSubtitles.FocusedNode := Node;
    lstSubtitles.Selected[Node] := True;
    SetStartTime(Node, StartSubtime);
    SetFinalTime(Node, GetCurrentPos);

    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    New(UndoAction);
    UndoAction^.UndoActionType := uaInsertLine;
    UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
    UndoAction^.Node           := lstSubtitles.FocusedNode;
    UndoAction^.BindToNext     := False;
    UndoAction^.Buffer         := nil;
    UndoAction^.BufferSize     := 0;
    UndoList.Add(UndoAction);
    mnuUndo.Enabled := True;

    mmoSubtitleText.SetFocus;
    RefreshTimes;
  end;  
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuInsertSubtitleClick(Sender: TObject);
var
  UndoAction: PUndoAction;
begin
  InsertNode;
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  lstSubtitles.Refresh;
  RefreshTimes;

  New(UndoAction);
  UndoAction^.UndoActionType := uaInsertLine;
  UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
  UndoAction^.Node           := lstSubtitles.FocusedNode;
  UndoAction^.BindToNext     := False;
  UndoAction^.Buffer         := nil;
  UndoAction^.BufferSize     := 0;
  UndoList.Add(UndoAction);
  mnuUndo.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuInsertBeforeClick(Sender: TObject);
var
  UndoAction: PUndoAction;
begin
  InsertNode(False);
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  lstSubtitles.Refresh;
  RefreshTimes;

  New(UndoAction);
  UndoAction^.UndoActionType := uaInsertLine;
  UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
  UndoAction^.Node           := lstSubtitles.FocusedNode;
  UndoAction^.BindToNext     := False;
  UndoAction^.Buffer         := nil;
  UndoAction^.BufferSize     := 0;
  UndoList.Add(UndoAction);
  mnuUndo.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuRemoveSelectedClick(Sender: TObject);
begin
  DeleteSelectedNodes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if AskToSaveFile = False then
    CanClose := False else
    CanClose := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuCutClick(Sender: TObject);
begin
  if GetFocus = lstSubtitles.Handle then
  begin
    CopyNodesToClipBoard;
    DeleteSelectedNodes;
  end else
  if GetFocus = mmoSubtitleText.Handle then
    mmoSubtitleText.CutToClipboard else
  if (mnuTranslatorMode.Checked) and (GetFocus = mmoTranslation.Handle) then
    mmoTranslation.CutToClipboard;  
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuCopyClick(Sender: TObject);
var
  CurrFocus: HWND;
begin
  CurrFocus := GetFocus;
  if CurrFocus = lstSubtitles.Handle then
    CopyNodesToClipBoard else
  if CurrFocus = mmoSubtitleText.Handle then
    mmoSubtitleText.CopyToClipboard else
  if (mnuTranslatorMode.Checked) and (CurrFocus = mmoTranslation.Handle) then
    mmoTranslation.CopyToClipboard else
  if CurrFocus = tmeShow.Handle then
    ClipBoard.SetTextBuf(PChar(TimeToString(tmeShow.Time))) else
  if CurrFocus = tmeHide.Handle then
    ClipBoard.SetTextBuf(PChar(TimeToString(tmeHide.Time))) else
  if CurrFocus = tmeDuration.Handle then
    ClipBoard.SetTextBuf(PChar(TimeToString(tmeDuration.Time)));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuPasteClick(Sender: TObject);
var
  CurrFocus : HWND;
  Text      : String;
  Time      : Integer;
begin
  CurrFocus := GetFocus;
  if GetFocus = lstSubtitles.Handle then
    PasteNodesFromClipBoard else
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    if GetFocus = mmoSubtitleText.Handle then
      mmoSubtitleText.PasteFromClipboard else
    if (mnuTranslatorMode.Checked) and (GetFocus = mmoTranslation.Handle) then
      mmoTranslation.PasteFromClipboard else
    begin
      if (CurrFocus = tmeShow.Handle) or (CurrFocus = tmeHide.Handle) or (CurrFocus = tmeDuration.Handle) then
      begin
        Text := Clipboard.AsText;
        if Text <> '' then
        begin
          if IsInteger(Text) then
            Time := FramesToTime(StrToInt(Text), GetFPS) else
            Time := StringToTime(Text);
          if Time = -1 then exit;

          if GetFocus = tmeShow.Handle then
            SetStartTime(lstSubtitles.FocusedNode, Time);
          if GetFocus = tmeHide.Handle then
            SetFinalTime(lstSubtitles.FocusedNode, Time);
          if GetFocus = tmeDuration.Handle then
            SetFinalTime(lstSubtitles.FocusedNode, GetStartTime(lstSubtitles.FocusedNode) + Time);

          lstSubtitles.RepaintNode(lstSubtitles.FocusedNode);
          RefreshTimes;
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuJumpToNextLineClick(Sender: TObject);
var
  a: HWND;
  i: Integer;
begin
  a := GetFocus;
  if (mnuTranslatorMode.Checked) and (a = mmoTranslation.Handle) then
    MemoKeyPress(mmoTranslation, lstSubtitles, True) else
  if (a = mmoSubtitleText.Handle) or (a = lstSubtitles.Handle) then
    MemoKeyPress(mmoSubtitleText, lstSubtitles, True) else
  if a = tmeShow.Handle then
  begin
    i := tmeShow.SelStart;
    MemoKeyPress(tmeShow, lstSubtitles, True);
    if tmeShow.SelStart <> i then
      tmeShow.SelStart := i;
  end else
  if a = tmeHide.Handle then
  begin
    i := tmeHide.SelStart;
    MemoKeyPress(tmeHide, lstSubtitles, True);
    if tmeHide.SelStart <> i then
      tmeHide.SelStart := i;
  end else
  if a = tmeDuration.Handle then
  begin
    i := tmeDuration.SelStart;
    MemoKeyPress(tmeDuration, lstSubtitles, True);
    if tmeDuration.SelStart <> i then
      tmeDuration.SelStart := i;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuJumpToPrevLineClick(Sender: TObject);
var
  a: HWND;
  i: Integer;
begin
  a := GetFocus;
  if (mnuTranslatorMode.Checked) and (a = mmoTranslation.Handle) then
    MemoKeyPress(mmoTranslation, lstSubtitles, False) else
  if (a = mmoSubtitleText.Handle) or (a = lstSubtitles.Handle) then
    MemoKeyPress(mmoSubtitleText, lstSubtitles, False) else
  if a = tmeShow.Handle then
  begin
    i := tmeShow.SelStart;
    MemoKeyPress(tmeShow, lstSubtitles, False);
    if tmeShow.SelStart <> i then
      tmeShow.SelStart := i;
  end else
  if a = tmeHide.Handle then
  begin
    i := tmeHide.SelStart;
    MemoKeyPress(tmeHide, lstSubtitles, False);
    if tmeHide.SelStart <> i then
      tmeHide.SelStart := i;
  end else
  if a = tmeDuration.Handle then
  begin
    i := tmeDuration.SelStart;
    MemoKeyPress(tmeDuration, lstSubtitles, False);
    if tmeDuration.SelStart <> i then
      tmeDuration.SelStart := i;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuJumpToNextSubClick(Sender: TObject);
begin
  btnNextSub.OnClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuJumpToPrevSubClick(Sender: TObject);
begin
  btnPrevSub.OnClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuZeroFunctionClick(Sender: TObject);
begin
  if (GetFocus = mmoSubtitleText.Handle) then
    mmoSubtitleText.Undo else
  if (mnuTranslatorMode.Checked) and (GetFocus = mmoTranslation.Handle) then
    mmoTranslation.Undo else
  ZeroFunction;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSubDblClickClick(Sender: TObject);
begin
  lstSubtitlesDblClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSortClick(Sender: TObject);
begin
  SortSubtitles;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuReverseTextClick(Sender: TObject);
begin
  TreeViewHandle.ReverseText(KeepOrderOfLines);
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFixPunctuationClick(Sender: TObject);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaRTLFix;
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    UndoAction^.BufferSize     := SizeOf(TRTLFix);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PRTLFix(UndoAction^.Buffer)^.ReverseText    := False;
    PRTLFix(UndoAction^.Buffer)^.FixPunctuation := True;
    PRTLFix(UndoAction^.Buffer)^.Original       := True;
      
    if mnuTranslatorMode.Checked then
    begin
      PRTLFix(UndoAction^.Buffer)^.Original := False;
      if GetSubTranslation(Node) <> UntranslatedSub then
        SetTranslation(Node, FixRTLPunctuation(GetSubTranslation(Node)));
    end else
      SetText(Node, FixRTLPunctuation(GetSubText(Node)));

    Node := lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);
    UndoList.Add(UndoAction);     
  end;

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuMoveSubtitleClick(Sender: TObject);
begin
  btnMoveSubtitleClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSetStartTimeClick(Sender: TObject);
begin
  btnSetStartTimeClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSetFinalTimeClick(Sender: TObject);
begin
  btnSetFinalTimeClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuStartSubtitleClick(Sender: TObject);
begin
  btnStartSubtitleClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuEndSubtitleClick(Sender: TObject);
begin
  btnEndSubtitleClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuCombineSubtitlesClick(Sender: TObject);
var
  UpdNode    : PVirtualNode;
  Node       : PVirtualNode;
  FinalTime  : Integer;
  NewText    : String;
  NewTrans   : String;
  UndoAction : PUndoAction;
begin
  if lstSubtitles.SelectedCount > 1 then
  begin
    UpdNode := lstSubtitles.GetFirstSelected;
    FinalTime := GetFinalTime(UpdNode);
    NewText  := GetSubText(UpdNode);
    NewTrans := GetSubTranslation(UpdNode);

    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.Node           := UpdNode;
    UndoAction^.LineNumber     := UpdNode.Index;
    UndoAction^.BindToNext     := True;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;
    PFullTextChange(UndoAction^.Buffer)^.OldText := NewText;
    PFullTextChange(UndoAction^.Buffer)^.OldTrans := NewTrans;
    UndoList.Add(UndoAction); 

    Node := lstSubtitles.GetNextSelected(UpdNode);
    while Assigned(Node) do
    begin
      FinalTime := GetFinalTime(Node);
      if GetSubText(Node) <> '' then
        NewText := NewText + #13#10 + GetSubText(Node);
      if GetSubTranslation(Node) <> '' then
        NewTrans := NewTrans + #13#10 + GetSubTranslation(Node);
      Node := lstSubtitles.GetNextSelected(Node);
    end;

    lstSubtitles.Selected[UpdNode] := False;
    DeleteSelectedWithUndo;
    lstSubtitles.FocusedNode := UpdNode;
    lstSubtitles.Selected[UpdNode] := True;

    SetFinalTime(UpdNode, FinalTime);
    SetText(UpdNode, NewText);
    SetTranslation(UpdNode, NewTrans);

    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSmartLineAdjustClick(Sender: TObject);
var
  Node       : PVirtualNode;
  tmp        : String;
  adj1       : String;
  adj2       : String;
  adj3       : String;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    tmp := GetSubText(Node);

    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
    PFullTextChange(UndoAction^.Buffer)^.OldText := tmp;

    adj1 := AdjustLines(tmp, True, True);
    adj2 := AdjustLines(tmp, True, False);
    adj3 := AdjustLines(tmp, False, False);

    if ToggleBreakPoint then
    begin
      if (tmp = adj2) or (tmp = adj3) then
      begin
      if tmp = adj2 then
        SetText(Node, adj3) else
      if tmp = adj3 then
        SetText(Node, adj2)
      end else
        SetText(Node, adj1);
    end else
      SetText(Node, adj1);

    if mnuTranslatorMode.Checked then
    begin
      Tmp := GetSubTranslation(Node);
      if Tmp <> UntranslatedSub then
      begin
        PFullTextChange(UndoAction^.Buffer)^.OldTrans := tmp;      
        PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;

        adj1 := AdjustLines(tmp, True, True);
        adj2 := AdjustLines(tmp, True, False);
        adj3 := AdjustLines(tmp, False, False);

        if ToggleBreakPoint then
        begin
          if (tmp = adj2) or (tmp = adj3) then
          begin
          if tmp = adj2 then
            SetTranslation(Node, adj3) else
          if tmp = adj3 then
            SetTranslation(Node, adj2)
          end else
            SetTranslation(Node, adj1);
        end else
          SetTranslation(Node, adj1);
      end;
    end;

    if (GetSubText(Node) <> PFullTextChange(UndoAction^.Buffer)^.OldText) or
       ((mnuTranslatorMode.Checked) and (GetSubTranslation(Node) <> PFullTextChange(UndoAction^.Buffer)^.OldTrans)) then
      UndoList.Add(UndoAction);

    Node := lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);
  end;

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
  frmMain.OrgModified := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuConvertCaseClick(Sender: TObject);
begin
  frmConvertCase := TfrmConvertCase.Create(Application);
  frmConvertCase.ShowModal;
  frmConvertCase.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuUnbreakSubtitlesClick(Sender: TObject);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := mnuTranslatorMode.Checked;
    PFullTextChange(UndoAction^.Buffer)^.OldText := GetSubText(Node);

    SetText(Node, ReplaceEnters(GetSubText(Node), ' '));
    if mnuTranslatorMode.Checked then
    begin
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := GetSubTranslation(Node);
      SetTranslation(Node, ReplaceEnters(GetSubTranslation(Node), ' '));
    end;
    
    Node := lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);

    UndoList.Add(UndoAction);
  end;

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
  frmMain.OrgModified := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuDivideLinesClick(Sender: TObject);
begin
  frmDivideLines := TfrmDivideLines.Create(Application);
  frmDivideLines.ShowModal;
  frmDivideLines.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFastDivideLinesClick(Sender: TObject);
var
  Ini                 : TIniFile;
  BreaksArray         : TOpenIntegerArray;
  AdjustAutomatically : Boolean;
  Part1, Part2        : String;
  ExtraVar            : Integer;
  Data1, Data2        : PSubtitleItem;
  Time1               : Integer;
  DurPerChar          : Single;
  UndoAction          : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Ini := TIniFile.Create(IniRoot);
  try
    AdjustAutomatically := Ini.ReadBool('Advanced', 'Smart line adjust automatically', True);
  finally
    Ini.Free;
  end;
  Data1 := lstSubtitles.GetNodeData(lstSubtitles.FocusedNode);
  ProcessStringToDivide(Data1.Text, BreaksArray, AdjustAutomatically, Part1, Part2, ExtraVar);

  lstSubtitles.InsertNode(lstSubtitles.FocusedNode, amInsertAfter);
  Data2 := lstSubtitles.GetNodeData(lstSubtitles.GetNextSibling(lstSubtitles.FocusedNode));

  New(UndoAction);
  UndoAction^.UndoActionType := uaFullTextChange;
  UndoAction^.Node           := lstSubtitles.FocusedNode;
  UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
  UndoAction^.BindToNext     := True;
  UndoAction^.BufferSize     := SizeOf(TFullTextChange);
  UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
  PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
  PFullTextChange(UndoAction^.Buffer)^.OldText := Data1.Text;
  UndoList.Add(UndoAction);

  New(UndoAction);
  UndoAction^.UndoActionType := uaTimeChange;
  UndoAction^.Node           := lstSubtitles.FocusedNode;
  UndoAction^.LineNumber     := lstSubtitles.FocusedNode.Index;
  UndoAction^.BindToNext     := True;
  UndoAction^.BufferSize     := SizeOf(TTimeChange);
  UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
  PTimeChange(UndoAction^.Buffer)^.StartTime := Data1.InitialTime;
  PTimeChange(UndoAction^.Buffer)^.FinalTime := Data1.FinalTime;
  UndoList.Add(UndoAction);

  New(UndoAction);
  UndoAction^.UndoActionType := uaInsertLine;
  UndoAction^.Node           := lstSubtitles.GetNextSibling(lstSubtitles.FocusedNode);
  UndoAction^.LineNumber     := UndoAction^.Node.Index;
  UndoAction^.BindToNext     := False;
  UndoAction^.Buffer         := nil;
  UndoAction^.BufferSize     := 0;
  UndoList.Add(UndoAction);  

  TrimParts(Part1, Part2);

  if Length(Data1.Text) <> 0 then
    DurPerChar := (Data1.FinalTime - Data1.InitialTime) / Length(Data1.Text) else // Milliseconds
    DurPerChar := 0;
  Time1 := Round(DurPerChar * Length(Part1));

  Data1.Text      := Part1;
  ExtraVar        := Data1.FinalTime;
  Data1.FinalTime := Time1 + Data1.InitialTime;

  Data2.Text        := Part2;
  Data2.InitialTime := Data1.FinalTime + 1;
  Data2.FinalTime   := ExtraVar;

  lstSubtitles.Selected[lstSubtitles.FocusedNode] := False;
  lstSubtitles.FocusedNode := lstSubtitles.FocusedNode.NextSibling;
  lstSubtitles.Selected[lstSubtitles.FocusedNode] := True;  

  mnuUndo.Enabled := True;
  RefreshTimes;
  lstSubtitles.Refresh;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSetMaxLineLengthClick(Sender: TObject);
  function BreakInLines(Text: String; MaximumLineLength: Integer): String;
  var
    i: Integer;
    // Tags
    Bold        : Boolean;
    Italic      : Boolean;
    Underline   : Boolean;
    Color       : Integer;
  begin
    Bold      := Pos('<b>', Text) > 0;
    Italic    := Pos('<i>', Text) > 0;
    Underline := Pos('<u>', Text) > 0;
    Color     := GetSubColor(Text);
    // Remove tags
    Text := RemoveSWTags(Text, True, True, True, True);
    Text := WrapText(ReplaceEnters(Text, ' '), MaximumLineLength);

    i := Pos(' ' + #13#10, Text);
    while i > 0 do
    begin
      Delete(Text, i, 1);
      i := Pos(' ' + #13#10, Text);
    end;  

    if SubtitleAPI.NoInteractionWithTags = False then
    begin
      // Restore tags
      if Underline = True then Text := '<u>' + Text;
      if Bold      = True then Text := '<b>' + Text;
      if Italic    = True then Text := '<i>' + Text;
      if Color > -1 then
        Text := SetColorTag(Text, Color);
    end;

    Result := Text;
  end;
var
  Node       : PVirtualNode;
  OldText    : String;
  NewText    : String;
  OldTrans   : String;
  NewTrans   : String;
  UndoAction : PUndoAction;
begin
  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;

    OldText := GetSubText(Node);
    NewText := BreakInLines(OldText, MaxLineLength);

    PFullTextChange(UndoAction^.Buffer)^.OldText := OldText;

    SetText(Node, NewText);
    if mnuTranslatorMode.Checked then
    begin
      OldTrans := GetSubTranslation(Node);
      NewTrans := BreakInLines(OldTrans, MaxLineLength);

      PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := OldTrans;
      SetTranslation(Node, NewTrans);
    end;

    Node := lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);

    if ((mnuTranslatorMode.Checked = False) and (OldText <> NewText)) or
       ((mnuTranslatorMode.Checked = True) and (OldTrans <> NewTrans)) then
      UndoList.Add(UndoAction);
  end;
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbOrgCharsetSelect(Sender: TObject);
begin
  if cmbOrgCharset.ItemIndex <> -1 then
  begin
    OrgCharset := StrCharsetToInt(cmbOrgCharset.Items[cmbOrgCharset.ItemIndex]);
    mmoSubtitleText.Font.Charset := OrgCharset;
    lstSubtitles.Refresh;
    if (Player.Initialized) and (mnuDisplayOriginal.Checked) then
      subSubtitle.Font.Charset := OrgCharset;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbTransCharsetSelect(Sender: TObject);
begin
  if cmbTransCharset.ItemIndex <> -1 then
  begin
    TransCharset := StrCharsetToInt(cmbTransCharset.Items[cmbTransCharset.ItemIndex]);
    mmoTranslation.Font.Charset := TransCharset;
    lstSubtitles.Refresh;
    if (Player.Initialized) and (mnuDisplayTranslation.Checked) then
      subSubtitle.Font.Charset := TransCharset;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuNewSubtitleClick(Sender: TObject);
begin
  if CloseSub then
  begin
    SubtitleAPI.CreateNewSubtitle;
    EnableCtrls(True);
    InsertNode;
    RefreshTimes;
    OrgModified   := False;
    TransModified := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFirstSyncPointClick(Sender: TObject);
begin
  btnSyncPoint1Click(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuLastSyncPointClick(Sender: TObject);
begin
  btnSyncPoint2Click(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuAddSyncPointClick(Sender: TObject);
begin
  btnAddSyncPointClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnSyncPoint1Click(Sender: TObject);
begin
  if (Player.Initialized) and (lstSubtitles.RootNodeCount > 1) and (lstSubtitles.Enabled) then
  begin
    SyncPoint.Point1Sub   := GetStartTime(lstSubtitles.GetFirstSelected);
    SyncPoint.Point1Movie := GetCurrentPos;
  end;  
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnSyncPoint2Click(Sender: TObject);
begin
  if (Player.Initialized) and (SyncPoint.Point1Sub <> -1) and (SyncPoint.Point1Movie <> -1) and (lstSubtitles.RootNodeCount > 1) and (lstSubtitles.Enabled) then
  begin
    SyncPoint.Point2Sub   := GetStartTime(lstSubtitles.GetFirstSelected);
    SyncPoint.Point2Movie := GetCurrentPos;

    if MsgBox(Format(QuestionMsg[06], [TimeToString(SyncPoint.Point1Sub),
                                      TimeToString(SyncPoint.Point1Movie),
                                      TimeToString(SyncPoint.Point2Sub),
                                      TimeToString(SyncPoint.Point2Movie)]),
              BTN_YES, BTN_NO, '', MB_ICONQUESTION, frmMain) = mrOk then
    begin
      AdjustSubtitles(SyncPoint);
      SyncPoint.Point1Sub   := -1;
      SyncPoint.Point1Movie := -1;
      SyncPoint.Point2Sub   := -1;
      SyncPoint.Point2Movie := -1;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnAddSyncPointClick(Sender: TObject);
begin
  if (lstSubtitles.RootNodeCount > 1) and (Assigned(lstSubtitles.FocusedNode)) and (lstSubtitles.SelectedCount = 1) then
  begin
    if AdjustFormOpened = False then
    begin
      frmAdjustSubtitles := TfrmAdjustSubtitles.Create(Application);
      frmAdjustSubtitles.pgeMode.ActivePageIndex := 1;
      frmAdjustSubtitles.Show;
      AdjustFormOpened := True;
    end else
      frmAdjustSubtitles.BringToFront;
    frmAdjustSubtitles.AddSyncPoint(GetStartTime(lstSubtitles.GetFirstSelected), GetCurrentPos, lstSubtitles.GetFirstSelected.Index + 1);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSelectAllClick(Sender: TObject);
begin
  lstSubtitles.SelectAll(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuBack5SecClick(Sender: TObject);
begin
  SetVideoPos(GetCurrentPos - 5000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFwd5SecClick(Sender: TObject);
begin
  SetVideoPos(GetCurrentPos + 5000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSetDurationLimitsClick(Sender: TObject);
begin
  frmDurationLimits := TfrmDurationLimits.Create(Application);
  frmDurationLimits.ShowModal;
  frmDurationLimits.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSetDelayClick(Sender: TObject);
begin
  frmSetDelay := TfrmSetDelay.Create(Application);
  frmSetDelay.ShowModal;
  frmSetDelay.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuAdjustSubtitlesClick(Sender: TObject);
begin
  if AdjustFormOpened = False then
  begin
    frmAdjustSubtitles := TfrmAdjustSubtitles.Create(Application);
    frmAdjustSubtitles.pgeMode.ActivePageIndex := 0;    
    frmAdjustSubtitles.Show;
    AdjustFormOpened := True;
  end else
    frmAdjustSubtitles.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSubSearchClick(Sender: TObject);
begin
  frmSearchAndReplace := TfrmSearchAndReplace.Create(Application);
  frmSearchAndReplace.pgeCtrl.ActivePageIndex := 0;
  frmSearchAndReplace.ShowModal;
  frmSearchAndReplace.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFindNextClick(Sender: TObject);
begin
  if FindInNode(SearchWord, CaseSensitive, MatchWholeWords, True) = nil then
    MsgBox(Format(InfoMsg[01], [SearchWord]), BTN_OK, '', '', MB_ICONINFORMATION, frmMain) else
    RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSearchAndReplaceClick(Sender: TObject);
begin
  frmSearchAndReplace := TfrmSearchAndReplace.Create(Application);
  frmSearchAndReplace.pgeCtrl.ActivePageIndex := 1;
  frmSearchAndReplace.ShowModal;
  frmSearchAndReplace.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuGoToLineNumberClick(Sender: TObject);
var
  S    : String;
  Line : Integer;
  Node : PVirtualNode;
  RootNodeCount: Integer;
begin
  S := '';
  if QueryInput(GoToLineNum, EnterLineNum, S, frmMain) = mrOk then
  begin
    if (IsInteger(S) = False) or (Length(S) > 8) then
      MsgBox(ErrorMsg[06], BTN_OK, '', '', MB_ICONERROR, frmMain) else
    begin
      Line := StrToInt(S) - 1;
      RootNodeCount := lstSubtitles.RootNodeCount;

      if Line < 0 then
        Line := 0;
      if Line > RootNodeCount then
        Line := RootNodeCount-1;

      Node := GetNodeWithIndex(lstSubtitles, Line);
      UnSelectAll(lstSubtitles);
      lstSubtitles.ScrollIntoView(Node, True);
      lstSubtitles.Selected[Node] := True;
      lstSubtitles.FocusedNode    := Node;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuBatchConvertClick(Sender: TObject);
begin
  frmBatchConvert := TfrmBatchConvert.Create(Application);
  frmBatchConvert.ShowModal;
  frmBatchConvert.Free; 
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSwapClick(Sender: TObject);
begin
  SwapOrgTrans(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuExtendLengthClick(Sender: TObject);
var
  Node       : PVirtualNode;
  FinalTime  : Integer;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    FinalTime := GetFinalTime(Node);
    New(UndoAction);
    UndoAction^.UndoActionType                 := uaTimeChange;
    UndoAction^.BufferSize                     := SizeOf(TTimeChange);
    UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node                           := Node;
    UndoAction^.LineNumber                     := Node.Index;
    PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
    PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;

    if Node <> lstSubtitles.GetLast then
      SetFinalTime(Node, GetStartTime(Node.NextSibling)-1);

    if FinalTime <> GetFinalTime(Node) then
    begin
      UndoAction^.BindToNext := Assigned(lstSubtitles.GetNextSelected(Node));
      UndoList.Add(UndoAction);
    end;

    Node := lstSubtitles.GetNextSelected(Node);
  end;

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuAutomaticDurationsClick(Sender: TObject);
begin
  frmAutomaticDurations := TfrmAutomaticDurations.Create(Application);
  frmAutomaticDurations.ShowModal;
  frmAutomaticDurations.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSplitSubtitleClick(Sender: TObject);
begin
  frmSplit := TfrmSplit.Create(Application);
  frmSplit.ShowModal;
  frmSplit.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuJoinSubtitleClick(Sender: TObject);
begin
  frmJoin := TfrmJoin.Create(Application);
  frmJoin.ShowModal;
  frmJoin.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuInformationAndErrorsClick(Sender: TObject);
begin
  frmInfoErrors := TfrmInfoErrors.Create(Application);
  frmInfoErrors.ShowModal;
  frmInfoErrors.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuVariousInformationClick(Sender: TObject);
begin
  frmVariousInfo := TfrmVariousInfo.Create(Application);
  frmVariousInfo.ShowModal;
  frmVariousInfo.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuAddFPSfromAVIClick(Sender: TObject);
var
  FPS      : Single;
  Duration : Integer;
begin
  dlgLoadFile.Filter := 'AVI Files (*.avi)|*.avi';
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
  begin
    if GetVideoInfo(dlgLoadFile.FileName, FPS, Duration) = True then
      AddFPSItem(FPS, False, True, False) else
      MsgBox(Format(ErrorMsg[05], [dlgLoadFile.FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain);
  end;
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuItalicClick(Sender: TObject);
var
  Text         : String;
  Node         : PVirtualNode;
  UndoAction   : PUndoAction;
  OriginalOnly : Boolean; // True for original, false for both (translation and original)
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  
  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := UndoAction^.Node.Index;
    UndoAction^.BindToNext     := lstSubtitles.SelectedCount > 1;

    Text := GetSubText(Node);
    PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

    if mnuItalic.Checked = False then
    begin
      if Pos('<i>', Text) = 0 then
        Text := '<i>' + Text;
    end else
      Text := RemoveSWTags(Text, False, True, False, False, True);
    SetText(Node, Text);

    if mnuTranslatorMode.Checked then
    begin
      OriginalOnly := False;

      Text := GetSubTranslation(Node);
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := Text;
            
      if mnuItalic.Checked = False then
      begin
        if Pos('<i>', Text) = 0 then
          Text := '<i>' + Text;
      end else
        Text := RemoveSWTags(Text, False, True, False, False, True);
      SetTranslation(Node, Text);
    end else
      OriginalOnly := True;

    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := OriginalOnly;

    Node := lstSubtitles.GetNextSelected(Node);

    if Assigned(Node) = False then UndoAction^.BindToNext := False;

    UndoList.Add(UndoAction);
  end;
  
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuBoldClick(Sender: TObject);
var
  Text         : String;
  Node         : PVirtualNode;
  UndoAction   : PUndoAction;
  OriginalOnly : Boolean; // True for original, false for both (translation and original)
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := UndoAction^.Node.Index;
    UndoAction^.BindToNext     := lstSubtitles.SelectedCount > 1;
    
    Text := GetSubText(Node);
    PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

    if mnuBold.Checked = False then
    begin
      if Pos('<b>', Text) = 0 then
        Text := '<b>' + Text;
    end else
      Text := RemoveSWTags(Text, False, True, False, False, True);
    SetText(Node, Text);

    if mnuTranslatorMode.Checked then
    begin
      OriginalOnly := False;
      Text := GetSubTranslation(Node);
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := Text;
      if mnuBold.Checked = False then
      begin
        if Pos('<b>', Text) = 0 then
          Text := '<b>' + Text;
      end else
        Text := RemoveSWTags(Text, False, True, False, False, True);
      SetTranslation(Node, Text);
    end else
      OriginalOnly := True;

    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := OriginalOnly;

    Node := lstSubtitles.GetNextSelected(Node);

    if Assigned(Node) = False then UndoAction^.BindToNext := False;

    UndoList.Add(UndoAction);
  end;
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuUnderlineClick(Sender: TObject);
var
  Text         : String;
  Node         : PVirtualNode;
  UndoAction   : PUndoAction;
  OriginalOnly : Boolean; // True for original, false for both (translation and original)
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := UndoAction^.Node.Index;
    UndoAction^.BindToNext     := lstSubtitles.SelectedCount > 1;
    
    Text := GetSubText(Node);
    PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

    if mnuUnderline.Checked = False then
    begin
      if Pos('<u>', Text) = 0 then
        Text := '<u>' + Text;
    end else
      Text := RemoveSWTags(Text, False, True, False, False, True);
    SetText(Node, Text);

    if mnuTranslatorMode.Checked then
    begin
      OriginalOnly := False;
      Text := GetSubTranslation(Node);
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := Text;
      if mnuUnderline.Checked = False then
      begin
        if Pos('<u>', Text) = 0 then
          Text := '<u>' + Text;
      end else
        Text := RemoveSWTags(Text, False, True, False, False, True);
      SetTranslation(Node, Text);
    end else
      OriginalOnly := True;

    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := OriginalOnly;

    Node := lstSubtitles.GetNextSelected(Node);

    if Assigned(Node) = False then UndoAction^.BindToNext := False;

    UndoList.Add(UndoAction);
  end;

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSetColorClick(Sender: TObject);
var
  Node         : PVirtualNode;
  Text         : String;
  UndoAction   : PUndoAction;
  OriginalOnly : Boolean; // True for original, false for both (translation and original)
begin
  if (dlgColor.Execute) then
  begin
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    Node := lstSubtitles.GetFirstSelected;
    while Assigned(Node) do
    begin
      New(UndoAction);
      UndoAction^.UndoActionType := uaFullTextChange;
      UndoAction^.BufferSize     := SizeOf(TFullTextChange);
      UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := UndoAction^.Node.Index;
      UndoAction^.BindToNext     := lstSubtitles.SelectedCount > 1;

      Text := GetSubText(Node);
      PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

      SetText(Node, SetColorTag(Text, dlgColor.Color));
      if mnuTranslatorMode.Checked then
      begin
        OriginalOnly := False;
        Text := GetSubTranslation(Node);
        PFullTextChange(UndoAction^.Buffer)^.OldTrans := Text;
        SetTranslation(Node, SetColorTag(Text, dlgColor.Color));
      end else
        OriginalOnly := True;

      PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := OriginalOnly;

      Node := lstSubtitles.GetNextSelected(Node);

      if Assigned(Node) = False then UndoAction^.BindToNext := False;

      UndoList.Add(UndoAction);
    end;
    
    mnuUndo.Enabled := True;
    lstSubtitles.Refresh;
    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuRemoveColorTagsClick(Sender: TObject);
var
  Node         : PVirtualNode;
  Text         : String;
  UndoAction   : PUndoAction;
  OriginalOnly : Boolean; // True for original, false for both (translation and original)
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  
  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaFullTextChange;
    UndoAction^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := UndoAction^.Node.Index;
    UndoAction^.BindToNext     := lstSubtitles.SelectedCount > 1;

    Text := GetSubText(Node);
    PFullTextChange(UndoAction^.Buffer)^.OldText := Text;

    SetText(Node, RemoveSWTags(Text, False, False, False, True));
    if mnuTranslatorMode.Checked then
    begin
      OriginalOnly := False;
      Text := GetSubTranslation(Node);
      PFullTextChange(UndoAction^.Buffer)^.OldTrans := Text;
      SetTranslation(Node, RemoveSWTags(Text, False, False, False, True, True));
    end else
      OriginalOnly := True;

    PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := OriginalOnly;

    Node := lstSubtitles.GetNextSelected(Node);

    if Assigned(Node) = False then UndoAction^.BindToNext := False;

    UndoList.Add(UndoAction);
  end;
  
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuStylePopupPopup(Sender: TObject);
begin
  mnuBold.Checked      := Pos('<b>', GetSubText(lstSubtitles.FocusedNode)) > 0;
  mnuItalic.Checked    := Pos('<i>', GetSubText(lstSubtitles.FocusedNode)) > 0;
  mnuUnderline.Checked := Pos('<u>', GetSubText(lstSubtitles.FocusedNode)) > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tmrSaveWorkTimer(Sender: TObject);
begin
  if lstSubtitles.RootNodeCount > 0 then
  begin
    // ------------------ //
    //    Save original   //
    // ------------------ //
    if OrgFile <> '' then
    begin
      UpdateArray;
      if SaveAsBackup = False then
      begin
        if SaveFile(OrgFile, OrgFormat, GetFPS) then // We save original file in it's original format
          OrgModified := False;
      end else
        SaveFile(OrgFile + '.bak', OrgFormat, GetFPS);
      SubtitleAPI.ClearSubtitles;
    end else
      mnuSaveFileAsClick(Sender);

    // -------------------- //
    //    Save translated   //
    // -------------------- //
    if (mnuTranslatorMode.Checked) then
    begin
      if (TransFile <> '') then
      begin
        if SaveAsBackup = False then
        begin
          UpdateArray(True);
          if SaveFile(TransFile, TransFormat, GetFPS) then // We save translated file in it's original format
            TransModified := False;
        end else
          SaveFile(TransFile + '.bak', TransFormat, GetFPS);
        SubtitleAPI.ClearSubtitles;
      end else
        mnuSaveTranslatedAsClick(Sender);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbInputFPSKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(VK_RETURN)) and (IsFloat(cmbInputFPS.Text)) then
    AddFPSItem(StrToFloat(cmbInputFPS.Text), True, True, True) else
  if (Key in ['0'..'9', DecimalSeparator, Chr(VK_RETURN), Chr(VK_BACK)]) = False then
    Key := #0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbFPSKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(VK_RETURN)) and (IsFloat(cmbFPS.Text)) then
    AddFPSItem(StrToFloat(cmbFPS.Text), False, True, False) else
  if (Key in ['0'..'9', DecimalSeparator, Chr(VK_RETURN), Chr(VK_BACK)]) = False then
    Key := #0;
end;


// -----------------------------------------------------------------------------

procedure TfrmMain.mnuAdjustToSyncSubsClick(Sender: TObject);
var
  Sync: TClassicSyncPoints;
begin
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles);
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
  begin
    if SubtitleAPI.LoadSubtitle(dlgLoadFile.FileName, GetInputFPS, dlgLoadFile.FilterIndex - 1) then
    begin
      Sync.Point1Sub   := GetStartTime(lstSubtitles.GetFirst);
      Sync.Point2Sub   := GetStartTime(lstSubtitles.GetLast);
      Sync.Point1Movie := SubtitleAPI.GetInitialTime(0);
      Sync.Point2Movie := SubtitleAPI.GetInitialTime(SubtitleAPI.SubtitleCount-1);
      AdjustSubtitles(Sync);
    end else
    begin
      if ((dlgLoadFile.FilterIndex - 1) = 0) then
        MsgBox(Format(ErrorMsg[03],[dlgLoadFile.FileName]), BTN_OK, '', '', MB_ICONERROR, frmMain) else
        MsgBox(Format(ErrorMsg[04],[dlgLoadFile.FileName, SubtitleAPI.GetFormatName(dlgLoadFile.FilterIndex - 1)]), BTN_OK, '', '', MB_ICONERROR, frmMain);
    end;
  end;
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuTimeExpanderReducerClick(Sender: TObject);
begin
  frmTimeExpanderReducer := TfrmTimeExpanderReducer.Create(Application);
  frmTimeExpanderReducer.ShowModal;
  frmTimeExpanderReducer.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuReadTimesFromFileClick(Sender: TObject);
begin
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles);
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
    ReadFromFile(dlgLoadFile.FileName, dlgLoadFile.FilterIndex - 1, True, False);
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuReadTextsFromFileClick(Sender: TObject);
begin
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles);
  if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
    ReadFromFile(dlgLoadFile.FileName, dlgLoadFile.FilterIndex -1, False, True);
  dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbInputFPSSelect(Sender: TObject);
var
  Node: PVirtualNode;
  Time1, Time2: Integer;
  UndoAction: PUndoAction;
begin
  if InterfaceEnabled = False then
    cmbFPS.ItemIndex := cmbInputFPS.ItemIndex;
  if SubtitleAPI.IsFrameBased(OrgFormat) = False then
  begin
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

    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;
    New(UndoAction);
    UndoAction^.UndoActionType                 := uaFPSChange;
    UndoAction^.BindToNext                     := False;
    UndoAction^.BufferSize                     := SizeOf(TFPSChange);
    UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
    PFPSChange(UndoAction^.Buffer)^.InputOrFPS := True;
    PFPSChange(UndoAction^.Buffer)^.OldValue   := OldInputFPS;
    UndoList.Add(UndoAction);
    mnuUndo.Enabled := True;

    OldInputFPS := GetInputFPS;
    lstSubtitles.Refresh;
    RefreshTimes;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbFPSSelect(Sender: TObject);
var
  Node: PVirtualNode;
  Time1, Time2: Integer;
  UndoAction: PUndoAction;
begin
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

  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  New(UndoAction);
  UndoAction^.UndoActionType                 := uaFPSChange;
  UndoAction^.BindToNext                     := False;
  UndoAction^.BufferSize                     := SizeOf(TFPSChange);
  UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
  PFPSChange(UndoAction^.Buffer)^.InputOrFPS := False;
  PFPSChange(UndoAction^.Buffer)^.OldValue   := OldFPS;
  UndoList.Add(UndoAction);
  mnuUndo.Enabled := True;

  OldFPS          := GetFPS;
  tmeShow.FPS     := OldFPS;
  tmeHide.FPS     := OldFPS;
  tmeDuration.FPS := OldFPS;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

  // removed by BDZL
{
  procedure TfrmMain.lstSubtitlesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

  function CountUnTranslated: Integer;
  var
    Node  : PVirtualNode;
    Text  : String;
    Trans : String;
  begin
    Result := 0;
    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      Text  := GetSubText(Node);
      Trans := GetSubTranslation(Node);
      if (Text = Trans) or (Trans = '') or (Trans = UntranslatedSub) then
        Inc(Result);
      Node := Node.NextSibling;
    end;
  end;

var
  Data: PSubtitleItem;
begin
  Data     := lstSubtitles.GetNodeData(Node);
  CellText := '';

  if Data.ErrorType = [] then
  begin
    if Data.Marked = True then
      CellText := ErrorReports[20] else
      CellText := '';
  end;

  if etLinesWithoutLetters  in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[01];
  if etEmptySubtitle        in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[02];
  // ---
  if etOverlapping          in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[03];
  if etBadValues            in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[04];
  if etTooLongDuration      in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[05];
  if etTooShortDuration     in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[06];
  if etTooLongLine          in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[07];
  if etOverTwoLines         in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[08];
  // ---
  if etHearingImpaired      in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[09];
  if etTextBeforeColon      in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[10];
  if etUnnecessaryDots      in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[11];
  if etProhibitedCharacter  in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[12];
  if etRepeatedCharacter    in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[13];
  if etRepeatedSubtitle     in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[14];
  if etOCRErrors            in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[15];
  // ---
  if etOpnDlgSubsOneLine    in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[16];
  if etSpaceAfterCustChars  in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[17];
  if etSpaceBeforeCustChars in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[18];
  if etUnnecessarySpaces    in Data.ErrorType then  CellText := CellText + #13#10 + ErrorReports[19];

  if (Data.ErrorType = []) and (Data.Marked = False) then
  begin
    if mnuTranslatorMode.Checked then
      CellText := Format(TransLeftLines, [CountUnTranslated]) else
      lstSubtitles.Hint := '';
  end;
  CellText := StringToWideStringEx(Trim(CellText), CharsetToCodePage(frmMain.Font.Charset));
end;
}

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuAboutSubtitleWorkshopClick(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Application);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuExternalPreviewClick(Sender: TObject);
var
  Ini      : TIniFile;
  Params   : String;
  Video    : String;
  Ext      : String;
  FileName : String;
  Temp     : array[0..MAX_PATH]of Char;
  Format   : Integer;
begin
  Ini := TIniFile.Create(IniRoot);
  try
    if (FileExists(Ini.ReadString('External Preview','Video player', '')) = False) then
    begin
      if MsgBox(QuestionMsg[07], BTN_YES, BTN_NO, '', MB_ICONWARNING, frmMain) = 1 then
      begin
        frmSettings := TfrmSettings.Create(Application);
        frmSettings.pgeCtrl.ActivePage:= frmSettings.pgeExternalPreviewGeneral;
        frmSettings.tvSettings.Items[9].Selected := True;
        frmSettings.ShowModal;
        frmSettings.Free;
      end;
      exit
    end;

    Params := Ini.ReadString('External Preview','Parameter','');

    if Ini.ReadBool('External Preview','Ask for different video',True) then
    begin
      dlgLoadFile.Filter := AllSupportedFiles + '|*.asf;*.avi;*.mp4;*.divx;*.mpg;*.mpeg;*.m1v;*.qt;*.wmv|ASF (*.asf)|*.asf|AVI (*.avi)|*.avi|DivX (*.mp4; *.divx)|*.mp4; *.divx|MPEG (*.mpg; *.mpeg; *.m1v)|*.mpg; *.mpeg; *.m1v|QuickTime 2.0 (*.qt)|*.qt|WMV (*.wmv)|*.wmv';
      if (dlgLoadFile.Execute) and (dlgLoadFile.FileName <> '') then
        Video := dlgLoadFile.FileName;
      dlgLoadFile.Filter := SubtitleAPI.FillDialogFilter(AllSupportedFiles) + ID_SRF + '|' + ID_PLAINTEXT;
    end else
      Video := Ini.ReadString('External Preview','Video to test','');

    if Video = '' then exit;
    Params := ReplaceString(Params, 'VIDEO_FILE', Video);

    if Ini.ReadBool('External Preview', 'Save in original format', True) then
      Format := OrgFormat else
      Format := SubtitleAPI.GetFormatIndex(Ini.ReadString('External Preview', 'Custom format', 'SubRip'));

    Ext := GetFormatExt(Format);

    GetTempPath(MAX_PATH, Temp);

    if mnuTranslatorMode.Checked = False then
      FileName := ExtractFileName(OrgFile) else
      FileName := ExtractFileName(TransFile);
    if FileName = '' then
      FileName := 'TmpSubFile' + Ext else
      FileName := Copy(FileName, 1, LastDelimiter('.', ExtractFileName(FileName))-1) + Ext;

    UpdateArray(mnuTranslatorMode.Checked);
    SubtitleAPI.SaveSubtitle(String(Temp) + FileName, Format, GetFPS);
    SubtitleAPI.ClearSubtitles;

    Params := ReplaceString(Params, 'SUBT_FILE', String(Temp) + FileName);
    ShellExecute(handle,'open', PChar(Ini.ReadString('External Preview','Video player','')), PCHar(Params), nil, SW_SHOW);

  finally
    Ini.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSAMILangExtractorClick(Sender: TObject);
begin
  frmSAMILangExtractor := TfrmSAMILangExtractor.Create(Application);
  frmSAMILangExtractor.ShowModal;
  frmSAMILangExtractor.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.pnlVideoDisplayClick(Sender: TObject);
begin
  Pause;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.subSubtitleClick(Sender: TObject);
begin
  Pause;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu10PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.10, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu20PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.20, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu30PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.30, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu40PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.40, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu50PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.50, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu60PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.60, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu70PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.70, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu80PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.80, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu90PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(0.90, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu100PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(1, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu150PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(1.50, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu200PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(2, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu300PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(3, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnu400PClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  SetPlayBackRate(4, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuRecheckErrorsClick(Sender: TObject);
begin
  CheckMarkErrors;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFixAllErrorsClick(Sender: TObject);
begin
  FixErrors;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFixErrorsInSelSubsClick(Sender: TObject);
begin
  FixErrors(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuJumpToNextErrorClick(Sender: TObject);
var
  Node : PVirtualNode;
  Data : PSubtitleItem;
begin
  if (Assigned(lstSubtitles.FocusedNode)) and (lstSubtitles.FocusedNode <> lstSubtitles.GetLast) then
    Node := lstSubtitles.FocusedNode.NextSibling else
    Node := lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    Data := lstSubtitles.GetNodeData(Node);
    if Data.ErrorType <> [] then
    begin
      lstSubtitles.ScrollIntoView(Node, True);
      lstSubtitles.Selected[lstSubtitles.FocusedNode] := False;
      lstSubtitles.FocusedNode := Node;
      lstSubtitles.Selected[Node] := True;
      RefreshTimes;
      Break;
    end;
    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuInfoErrorsSettingsClick(Sender: TObject);
begin
  frmInfoErrorsSettings := TfrmInfoErrorsSettings.Create(Application);
  frmInfoErrorsSettings.ShowModal;
  frmInfoErrorsSettings.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuDeleteUnnecessaryLinksClick(Sender: TObject);
var
  Node       : PVirtualNode;
  Text1      : String;
  Text2      : String;
  // Tags in first & second text
  Bold1      : Boolean;
  Italic1    : Boolean;
  Underline1 : Boolean;
  Color1     : Integer;
  // --
  Bold2       : Boolean;
  Italic2     : Boolean;
  Underline2  : Boolean;
  Color2      : Integer;
  UndoAction1 : PUndoAction;
  UndoAction2 : PUndoAction;  
begin
  ClearUndoList(RedoList);
  frmMain.mnuRedo.Enabled := False;
  
  Node := lstSubtitles.GetFirst.NextSibling;
  while Assigned(Node) do
  begin
    Text1 := GetSubText(Node.PrevSibling);
    Text2 := GetSubText(Node);

    New(UndoAction1);
    UndoAction1^.UndoActionType := uaFullTextChange;
    UndoAction1^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction1^.Buffer         := AllocMem(UndoAction1^.BufferSize);
    UndoAction1^.Node           := Node.PrevSibling;
    UndoAction1^.LineNumber     := Node.PrevSibling.Index;
    UndoAction1^.BindToNext     := True;
    PFullTextChange(UndoAction1^.Buffer)^.OriginalOnly := True;
    PFullTextChange(UndoAction1^.Buffer)^.OldText := Text1;

    New(UndoAction2);
    UndoAction2^.UndoActionType := uaFullTextChange;
    UndoAction2^.BufferSize     := SizeOf(TFullTextChange);
    UndoAction2^.Buffer         := AllocMem(UndoAction2^.BufferSize);
    UndoAction2^.Node           := Node;
    UndoAction2^.LineNumber     := Node.Index;
    UndoAction2^.BindToNext     := True;
    PFullTextChange(UndoAction2^.Buffer)^.OriginalOnly := True;
    PFullTextChange(UndoAction2^.Buffer)^.OldText := Text2;    

    Bold1      := Pos('<b>', Text1) > 0;
    Italic1    := Pos('<i>', Text1) > 0;
    Underline1 := Pos('<u>', Text1) > 0;
    Color1     := GetSubColor(Text1);
    Text1      := RemoveSWTags(Text1, True, True, True, True);

    Bold2      := Pos('<b>', Text2) > 0;
    Italic2    := Pos('<i>', Text2) > 0;
    Underline2 := Pos('<u>', Text2) > 0;
    Color2     := GetSubColor(Text2);
    Text2      := RemoveSWTags(Text2, True, True, True, True);

    if (Copy(Text1, Length(Text1)-2, 3) = '...') and
       (
       (Pos('...', Text2) = 1) or
       (Pos('-...', Text2) = 1) or
       (Pos('- ...', Text2) = 1)
       ) then
    begin
      Text1 := Copy(Text1, 1, Length(Text1)-3);
      if (Pos('...', Text2) = 1) then
        Text2 := Trim(Copy(Text2, 4, Length(Text2))) else
      if (Pos('-...', Text2) = 1) then
        Text2 := Trim(Copy(Text2, 5, Length(Text2))) else
      if (Pos('- ...', Text2) = 1) then
        Text2 := Trim(Copy(Text2, 6, Length(Text2)));

      if SubtitleAPI.NoInteractionWithTags = False then
      begin
        // Restore tags
        if Underline1 = True then Text1 := '<u>' + Text1;
        if Bold1      = True then Text1 := '<b>' + Text1;
        if Italic1    = True then Text1 := '<i>' + Text1;
        if Color1 > -1 then
          Text1 := SetColorTag(Text1, Color);

        // Restore tags
        if Underline2 = True then Text2 := '<u>' + Text2;
        if Bold2      = True then Text2 := '<b>' + Text2;
        if Italic2    = True then Text2 := '<i>' + Text2;
        if Color2 > -1 then
          Text2 := SetColorTag(Text2, Color);
      end;

      if mnuTranslatorMode.Checked = False then
      begin
        UndoList.Add(UndoAction1);
        UndoList.Add(UndoAction2);
      end;
      SetText(Node.PrevSibling, Text1);
      SetText(Node, Text2);
    end;
    if mnuTranslatorMode.Checked then
    begin
      Text1 := GetSubTranslation(Node.PrevSibling);
      Text2 := GetSubTranslation(Node);

      PFullTextChange(UndoAction1^.Buffer)^.OriginalOnly := False;
      PFullTextChange(UndoAction1^.Buffer)^.OldTrans     := Text1;
      PFullTextChange(UndoAction2^.Buffer)^.OriginalOnly := False;
      PFullTextChange(UndoAction2^.Buffer)^.OldTrans     := Text2;      

      Bold1      := Pos('<b>', Text1) > 0;
      Italic1    := Pos('<i>', Text1) > 0;
      Underline1 := Pos('<u>', Text1) > 0;
      Color1     := GetSubColor(Text1);
      Text1      := RemoveSWTags(Text1, True, True, True, True);

      Bold2      := Pos('<b>', Text2) > 0;
      Italic2    := Pos('<i>', Text2) > 0;
      Underline2 := Pos('<u>', Text2) > 0;
      Color2     := GetSubColor(Text2);
      Text2      := RemoveSWTags(Text2, True, True, True, True);

      if (Copy(Text1, Length(Text1)-2, 3) = '...') and
         (
         (Pos('...', Text2) = 1) or
         (Pos('-...', Text2) = 1) or
         (Pos('- ...', Text2) = 1)
         ) then
      begin
        Text1 := Copy(Text1, 1, Length(Text1)-3);
        if (Pos('...', Text2) = 1) then
          Text2 := Trim(Copy(Text2, 4, Length(Text2))) else
        if (Pos('-...', Text2) = 1) then
          Text2 := Trim(Copy(Text2, 5, Length(Text2))) else
        if (Pos('- ...', Text2) = 1) then
          Text2 := Trim(Copy(Text2, 6, Length(Text2)));

        if SubtitleAPI.NoInteractionWithTags = False then
        begin
          // Restore tags
          if Underline1 = True then Text1 := '<u>' + Text1;
          if Bold1      = True then Text1 := '<b>' + Text1;
          if Italic1    = True then Text1 := '<i>' + Text1;
          if Color1 > -1 then
            Text1 := SetColorTag(Text1, Color);

          // Restore tags
          if Underline2 = True then Text2 := '<u>' + Text2;
          if Bold2      = True then Text2 := '<b>' + Text2;
          if Italic2    = True then Text2 := '<i>' + Text2;
          if Color2 > -1 then
            Text2 := SetColorTag(Text2, Color);
        end;

        UndoList.Add(UndoAction1);
        UndoList.Add(UndoAction2);          

        SetTranslation(Node.PrevSibling, Text1);
        SetTranslation(Node, Text2);
      end;
    end;
    Node := Node.NextSibling;
  end;

  if UndoList.Count > 0 then
    PUndoAction(UndoList.Last)^.BindToNext := False;   

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;                 
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuCheckForNewVersionClick(Sender: TObject);
  function GetTempDir: String;
  var
    Temp: array[0..MAX_PATH] of Char;
  begin
    GetTempPath(MAX_PATH,Temp);
    Result := String(Temp);
  end;

  function DownloadFile(Source, Dest: String): Boolean;
  begin
    try
      Result := UrlDownloadToFile(nil, PChar(Source), PChar(Dest), 0, nil) = 0;
    except
      Result := False;
    end;
  end;
var
  Dir      : String;
  URL      : String;
  Language : String;
  Ini      : TIniFile;
begin
  Language := ExtractFileName(frmMain.ActualLangFile);
  Language := Copy(Language, 1, LastDelimiter('.', Language)-1);
  Dir      := GetTempDir + Copy(ID_UPDATEINI, LastDelimiter('/', ID_UPDATEINI)+1, Length(ID_UPDATEINI));
    
  if DownloadFile(ID_UPDATEINI, Dir) Then
  begin
    Ini := TIniFile.Create(Dir);
    try
      if Ini.ReadString('Update', 'CurrVer', ID_VERSION) <> ID_VERSION Then
      begin // New version!
        if MsgBox(QuestionMsg[08], BTN_YES, BTN_NO, '', MB_ICONQUESTION, frmMain) = 1 Then
        begin
          if Ini.ValueExists('Update', Language) then
            URL := Ini.ReadString('Update', Language, ID_WEBPAGE) else
            URL := Ini.ReadString('Update', 'English', ID_WEBPAGE);
          ShellExecute(GetDesktopWindow, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
        end;
      end else // No new version available...
        MsgBox(InfoMsg[02], BTN_OK, '', '', MB_ICONINFORMATION, frmMain);
    finally
      Ini.Free;
      DeleteFile(Dir + ExtractFileName(ID_UPDATEINI));
    end;
  end else // Error!
    MsgBox(ErrorMsg[11], BTN_OK, '', '', MB_ICONERROR, frmMain);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbSeekBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Player.Initialized) and (mnuVideoPreviewMode.Checked) then
    tmrVideo.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbSeekBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Player.Initialized) and (mnuVideoPreviewMode.Checked) then
  begin
    SetVideoPos(sbSeekBar.Position);
    if Playing = True then
      tmrVideo.Enabled := True;
  end; 
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbSeekBarMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    SetTimeCounter(sbSeekBar.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnAlterPlaybackRateClick(Sender: TObject);
begin
  if Player.Initialized then
  begin
    if GetPlaybackRate = 1 then
    begin
      mnu100P.Checked := False;
      SetPlaybackRate(DefAltPlayRate / 10);
      case DefAltPlayRate of
        1: mnu10P.Checked := True;
        2: mnu20P.Checked := True;
        3: mnu30P.Checked := True;
        4: mnu40P.Checked := True;
        5: mnu50P.Checked := True;
        6: mnu60P.Checked := True;
        7: mnu70P.Checked := True;
        8: mnu80P.Checked := True;
        9: mnu90P.Checked := True;
        10: mnu200P.Checked := True;
        11: mnu300P.Checked := True;
        12: mnu400P.Checked := True else
        mnu100P.Checked := True;
      end;
    end else
    begin
      SetPlaybackRate(1);
      mnu100P.Checked := True;
    end;
  end;   
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuRewindClick(Sender: TObject);
begin
  SetVideoPos(GetCurrentPos - RewFFTime);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuForwardClick(Sender: TObject);
begin
  SetVideoPos(GetCurrentPos + RewFFTime);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnScrollListClick(Sender: TObject);
begin
  ScrollList := not ScrollList;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSpellCheckClick(Sender: TObject);
var
  Node          : PVirtualNode;
  StringToCheck : TStringList;
  TextStrList   : TStringList;
  i, c          : Integer;
  Text          : String;
  UndoAction    : PUndoAction;
begin
  USSpellCheck.Connect;
  USSpellCheck.CheckGrammarWithSpelling := False;
  try
    ClearUndoList(RedoList);
    mnuRedo.Enabled := False;

    if USSpellCheck.Connected = True then
    begin
      StringToCheck := TStringList.Create;
      try
        Node := lstSubtitles.GetFirst;
        while Assigned(Node) do
        begin
          StringToCheck.Add(GetSubText(Node));
          StringToCheck.Add('');
          Node := Node.NextSibling;
        end;
        // Check spelling of the big string
        USSpellCheck.CheckSpelling(StringToCheck.Text);
        if Trim(USSpellCheck.ChangedText) <> '' then
        begin
          StringToCheck.Text := Trim(USSpellCheck.ChangedText);
          TextStrList := TStringList.Create;
          try
            for i := 0 to StringToCheck.Count-1 do
            begin
              if (StringToCheck[i] <> '') then
              begin
                Text := StringToCheck[i];
                if (i = 0) or ((i > 0) and (StringToCheck[i-1] = '')) then // Avoid lots of problems with subtitles that are more than one line
                begin
                  c := 1;
                  while (i+c < StringToCheck.Count) and (StringToCheck[i+c] <> '') do
                  begin
                    if (StringToCheck[i+c] <> '') then
                    begin
                      if Text <> '' then
                        Text := Text + #13#10 + StringToCheck[i+c] else
                        Text := StringToCheck[i+c];
                      Inc(c);
                    end else
                      break;
                  end;
                  if (Text <> '') then
                    TextStrList.Add(Text);
                end;
              end;
            end;

            // Update every node
            Node := lstSubtitles.GetFirst;
            while Assigned(Node) do
            begin
              if TextStrList[Node.Index] <> GetSubText(Node) then
              begin
                New(UndoAction);
                UndoAction^.UndoActionType := uaFullTextChange;
                UndoAction^.BufferSize     := SizeOf(TFullTextChange);
                UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
                UndoAction^.BindToNext     := True;
                PFullTextChange(UndoAction^.Buffer).OriginalOnly := True;
                UndoAction^.Node       := Node;
                UndoAction^.LineNumber := Node.Index;
                PFullTextChange(UndoAction^.Buffer)^.OldText := GetSubText(Node);
                SetText(Node, TextStrList[Node.Index]);
                UndoList.Add(UndoAction);
              end;
              Node := Node.NextSibling;
            end;
            mnuUndo.Enabled := True;
            lstSubtitles.Refresh;
            RefreshTimes;
          finally
            TextStrList.Free;
          end;
        end;
      finally
        MsgBox(Format(InfoMsg[03], [USSpellCheck.NumChanges]), BTN_OK, '', '', MB_ICONINFORMATION, frmMain);
        StringToCheck.Free;
      end;
    end else
      MsgBox(ErrorMsg[13], BTN_OK, '', '', MB_ICONERROR, frmMain);
  finally
    if UndoList.Count > 0 then
      PUndoAction(UndoList.Last)^.BindToNext := False;  
    USSpellCheck.Disconnect;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuHelpOfProgramClick(Sender: TObject);
var
  ManualFile: String;
begin
  ManualFile := ExtractFilePath(Application.ExeName) + 'Help\' + HelpFile;
  if FileExists(ManualFile) then
    ShellExecute(Handle, 'open', PChar(ManualFile), nil, nil, SW_MAXIMIZE) else
    ShellExecute(Handle, 'open', PChar(ExtractFilePath(Application.ExeName) + 'Help\Manual.html'), nil, nil, SW_MAXIMIZE);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuMarkSelectedSubsClick(Sender: TObject);
var
  Node       : PVirtualNode;
  Data       : PSubtitleItem;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    Data        := lstSubtitles.GetNodeData(Node);
    if Data.Marked <> True then
    begin
      Data.Marked := True;
      New(UndoAction);
      UndoAction^.UndoActionType := uaMarkSubtitle;
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := Node.Index;
      UndoAction^.BufferSize     := 0;
      UndoAction^.Buffer         := nil;
      UndoAction^.BindToNext     := Assigned(lstSubtitles.GetNextSelected(Node));
      UndoList.Add(UndoAction);
    end;
    Node := lstSubtitles.GetNextSelected(Node);
  end;
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuUnMarkSelectedSubsClick(Sender: TObject);
var
  Node       : PVirtualNode;
  Data       : PSubtitleItem;
  UndoAction : PUndoAction;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;
  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    Data        := lstSubtitles.GetNodeData(Node);
    if Data.Marked <> False then
    begin
      Data.Marked := False;
      New(UndoAction);
      UndoAction^.UndoActionType := uaMarkSubtitle;
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := Node.Index;
      UndoAction^.BufferSize     := 0;
      UndoAction^.Buffer         := nil;
      UndoAction^.BindToNext     := Assigned(lstSubtitles.GetNextSelected(Node));
      UndoList.Add(UndoAction);
    end;
    Node := lstSubtitles.GetNextSelected(Node);
  end;
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnPrevSubClick(Sender: TObject);
var
  Node     : PVirtualNode;
  CurrTime : Integer;
begin
  CurrTime := GetCurrentPos;
  Node := lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    if GetStartTime(Node) > CurrTime then
    begin
      if Node <> lstSubtitles.GetFirst then
        SetVideoPos(GetStartTime(Node.PrevSibling.PrevSibling));
      exit;
    end;
    if Node = lstSubtitles.GetLast then
    begin
      if (CurrTime >= GetStartTime(Node)) and (CurrTime <= GetFinalTime(Node)) then
        SetVideoPos(GetStartTime(Node.PrevSibling)) else
        SetVideoPos(GetStartTime(Node));        
    end;
    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnNextSubClick(Sender: TObject);
var
  Node     : PVirtualNode;
  CurrTime : Integer;
begin
  CurrTime := GetCurrentPos;
  Node := lstSubtitles.GetFirst;
  while Assigned(Node) do
  begin
    if GetStartTime(Node) > CurrTime then
    begin
      SetVideoPos(GetStartTime(Node));
      exit;
    end;
    Node := Node.NextSibling;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuShowLeftPanelClick(Sender: TObject);
begin
  SetTranslationCtrlsPositions;
  UpdateVideoPos;
  frmMain.Refresh;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuShowTimeControlsClick(Sender: TObject);
begin
  if mnuShowTimeControls.Checked then
  begin
    lblShow.Show;
    tmeShow.Show;
    lblHide.Show;
    tmeHide.Show;
    lblDuration.Show;
    tmeDuration.Show;
  end else
  begin
    lblShow.Hide;
    tmeShow.Hide;
    lblHide.Hide;
    tmeHide.Hide;
    lblDuration.Hide;
    tmeDuration.Hide;
  end;
  SetTranslationCtrlsPositions;
  frmMain.Refresh;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuUseInPlaceEditionClick(Sender: TObject);
begin
  if mnuUseInPlaceEdition.Checked = False then
    lstSubtitles.TreeOptions.MiscOptions := lstSubtitles.TreeOptions.MiscOptions - [toEditable] else
    lstSubtitles.TreeOptions.MiscOptions := lstSubtitles.TreeOptions.MiscOptions + [toEditable];    
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuTypeEffectClick(Sender: TObject);
begin
  TextEffect(EffectType, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuFastFlashClick(Sender: TObject);
begin
  TextEffect(EffectFlash, 250, 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuMediumFlashClick(Sender: TObject);
begin
  TextEffect(EffectFlash, 500, 500);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSlowFlashClick(Sender: TObject);
begin
  TextEffect(EffectFlash, 1000, 1000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuShiftPlusMSClick(Sender: TObject);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
  StartTime  : Integer;
  FinalTime  : Integer;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    New(UndoAction);
    UndoAction^.UndoActionType := uaTimeChange;
    UndoAction^.BufferSize     := SizeOf(TTimeChange);
    UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
    UndoAction^.Node           := Node;
    UndoAction^.LineNumber     := Node.Index;

    StartTime := GetStartTime(Node);
    FinalTime := GetFinalTime(Node);
    PTimeChange(UndoAction^.Buffer)^.StartTime := StartTime;
    PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;
    SetStartTime(Node, StartTime + ShiftTime);
    SetFinalTime(Node, FinalTime + ShiftTime);

    Node := lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);
    UndoList.Add(UndoAction);
  end;
  
  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuShiftLessMSClick(Sender: TObject);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
  StartTime  : Integer;
  FinalTime  : Integer;
begin
  ClearUndoList(RedoList);
  mnuRedo.Enabled := False;

  Node := lstSubtitles.GetFirstSelected;
  while Assigned(Node) do
  begin
    StartTime := GetStartTime(Node);
    FinalTime := GetFinalTime(Node);

    New(UndoAction);
    
    if (StartTime - ShiftTime) >= 0 then
    begin
      UndoAction^.UndoActionType := uaTimeChange;
      UndoAction^.BufferSize     := SizeOf(TTimeChange);
      UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
      UndoAction^.Node           := Node;
      UndoAction^.LineNumber     := Node.Index;

      PTimeChange(UndoAction^.Buffer)^.StartTime := StartTime;
      PTimeChange(UndoAction^.Buffer)^.FinalTime := FinalTime;

      SetStartTime(Node, StartTime - ShiftTime);
      SetFinalTime(Node, FinalTime - ShiftTime);
    end;
    
    Node := lstSubtitles.GetNextSelected(Node);
    UndoAction^.BindToNext := Assigned(Node);
    if (StartTime - ShiftTime) >= 0 then
      UndoList.Add(UndoAction);
  end;

  mnuUndo.Enabled := True;
  lstSubtitles.Refresh;
  RefreshTimes;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuMovieInfoClick(Sender: TObject);
const
  AviHeaderStart = 32; // Needed for positioning in the avi file
var
  f : File;
  // Temporary values
  AviHeaderSize  : Integer;
  TempFormatTest : array[0..2] of Char; // should be "AVI"
  TempVideoCodec : array[0..4] of Char;
  TempMicroSec   : Integer;
  VHeaderStart   : Integer;
  // -------------
  Size           : Double;
  LengthInFrames : Integer;
begin
  if (Player.Initialized) and (FileExists(MovieFile)) then
  begin
    Size := 0;
    FileMode := fmOpenRead;
    AssignFile(f, MovieFile);
    try
      {$I-}
      Reset(f, 1);
      {$I+}

      if IOResult = 0 then
      begin
        // Get file size...
        Size := FileSize(f) / 1024 / 1024;
        Seek(f, 8);
        BlockRead(f, TempFormatTest, SizeOf(TempFormatTest));
      
        if TempFormatTest = 'AVI' then
        begin
          Seek(f, AviHeaderStart);
          BlockRead(f, TempMicroSec, 4);

          // AVI header size (needed to locate audio part)
          Seek(f, 28);
          BlockRead(f, AviHeaderSize, 4);

          // Length of movie in frames
          Seek(f, AviHeaderStart + 16);
          BlockRead(f, LengthInFrames, 4);

          VHeaderStart := AviHeaderStart + AviHeaderSize + 20;

          // Video codec
          Seek(f, VHeaderStart + 4);
          BlockRead(f, TempVideoCodec, SizeOf(TempVideoCodec));

        end else
        begin
          LengthInFrames := TimeToFrames(VideoDuration, MovieFPS);
          TempVideoCodec := '-';
        end;
                   
      end;
    finally
      CloseFile(f);
    end;

    MsgBox(Format(InfoMsg[09],
                  [
                  MovieFile,                      // File name
                  FormatFloat('0.## MB', Size),   // File size
                  FormatFloat('#.###', MovieFPS), // FPS
                  TimeToString(VideoDuration),    // Duration
                  LengthInFrames,                 // Total frames
                  Player.VideoWidth,              // Resolution (width)
                  Player.VideoHeight,             // Resolution (height)
                  TempVideoCodec                  // FourCC
                  ]),
           BTN_OK,
           '',
           '',
           MB_ICONINFORMATION,
           frmMain);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TTreeEditLink.Create;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column <> 0) and (toEditable in lstSubtitles.TreeOptions.MiscOptions);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveASXClick(Sender: TObject);
var
  Dir : String;
  FN  : String;
  ASX : TStringList;
begin
  if (OrgFile <> '') and (MovieFile <> '') then
  begin
    if SelectDirectory(SelectOutputDir, '', Dir) then
    begin
      FN := Copy(ExtractFileName(OrgFile), 1, LastDelimiter('.', ExtractFileName(OrgFile))-1);

      UpdateArray;
      SaveFile(Dir + '\' + FN + '.smi', SubtitleAPI.GetFormatIndex('SAMI Captioning'), GetFPS);
      SubtitleAPI.ClearSubtitles;

      ASX := TStringList.Create;
      try
        ASX.Add('<ASX version ="3.0">');
        ASX.Add(Format('<Title>%s</Title>', [FN]));
        ASX.Add('  <Entry>');
        ASX.Add('    ' + Format('<Ref href="%s?sami=%s"/>', [MovieFile, Dir + '\' + FN + '.smi']));
        ASX.Add('  </Entry>');
        ASX.Add('</ASX>');

        ASX.SaveToFile(Dir + '\' + FN + '.asx');
      finally
        ASX.Free;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuSaveSMILClick(Sender: TObject);
var
  Dir  : String;
  FN   : String;
  SMIL : TStringList;
begin
  if (OrgFile <> '') and (MovieFile <> '') then
  begin
    if SelectDirectory(SelectOutputDir, '', Dir) then
    begin
      FN := Copy(ExtractFileName(OrgFile), 1, LastDelimiter('.', ExtractFileName(OrgFile))-1);

      UpdateArray;
      SaveFile(Dir + '\' + FN + '.rt', SubtitleAPI.GetFormatIndex('RealTime'), GetFPS);
      SubtitleAPI.ClearSubtitles;

      SMIL := TStringList.Create;
      try
        SMIL.Add('<smil>');
        SMIL.Add('  <head>');
        SMIL.Add('    <layout>');
        SMIL.Add('      <root-layout width="1080" height="350"/>');
        SMIL.Add('      <region id="video_left" width="480" height="304" left="0" top="0"/>');
        SMIL.Add('      <region id="video_center" width="480" height="304" left="480" top="0"/>');
        SMIL.Add('      <region id="video_right" width="480" height="304" left="960" top="0"/>');
        SMIL.Add('      <region id="text_subtitle" width="560" height="100" left="260" top="250"/>');
        SMIL.Add('    </layout>');
        SMIL.Add('  </head>');
        SMIL.Add('  <body>');
        SMIL.Add('    <par dur="7200">');
        SMIL.Add('      ' + Format('<video src="%s" begin="1s" clip-begin="0s" region="video_center"/>', [MovieFile]));
        SMIL.Add('      ' + Format('<textstream src="%s" region="text_subtitle"/>', [Dir + '\' + FN + '.rt']));
        SMIL.Add('    </par>');
        SMIL.Add('  </body>');
        SMIL.Add('</smil>');

        SMIL.SaveToFile(Dir + '\' + FN + '.smil');
      finally
        SMIL.Free;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuUndoClick(Sender: TObject);
begin
  if UndoList.Count > 0 then
    UndoActionSet(UndoList, RedoList);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuRedoClick(Sender: TObject);
begin
  if RedoList.Count > 0 then UndoActionSet(RedoList, UndoList);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuPascalScriptClick(Sender: TObject);
  procedure OutputMsg;
  var
    l          : LongInt;
    ErrorsText : String;
  begin
    ErrorsText := '';
    for l := 0 to psCompExec.CompilerMessageCount - 1 do
    begin
      ErrorsText := ErrorsText + #13#10 + psCompExec.CompilerErrorToStr(l);
    end;
    MsgBox('Compiler errors:' + #13#10#13#10 + ErrorsText, BTN_OK, '', '', MB_ICONERROR, Self);
  end;
begin
  psCompExec.Script.LoadFromFile(ExtractFilePath(Application.ExeName) + 'PascalScripts\' + (Sender as TMenuItem).Caption + '.pas');

  if psCompExec.Compile then
  begin
    if not psCompExec.Execute then
      OutputMsg else
    begin
      ClearUndoList(RedoList);
      mnuRedo.Enabled := False;
      mnuUndo.Enabled := True;
    end;
  end else
    OutputMsg;

  RefreshTimes;
  lstSubtitles.Refresh;
end;

// -----------------------------------------------------------------------------

function MyGetSubCount: Integer;
begin
  Result := frmMain.lstSubtitles.RootNodeCount;
end;

// -----------------------------------------------------------------------------

function MyIsSelSub(const Num: Integer): Boolean;
begin
  Result := frmMain.lstSubtitles.Selected[GetNodeWithIndex(frmMain.lstSubtitles, Num)]
end;

// -----------------------------------------------------------------------------

function MyGetSubText(const Num: Integer): String;
begin
  Result := GetSubText(GetNodeWithIndex(frmMain.lstSubtitles, Num));
end;

// -----------------------------------------------------------------------------

procedure MySetSubText(const Num: Integer; const Text: String);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  Node := GetNodeWithIndex(frmMain.lstSubtitles, Num);
  New(UndoAction);
  UndoAction^.UndoActionType := uaFullTextChange;
  UndoAction^.Node           := Node;
  UndoAction^.LineNumber     := Node.Index;
  UndoAction^.BindToNext     := True;
  UndoAction^.BufferSize     := SizeOf(TFullTextChange);
  UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
  PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := True;
  PFullTextChange(UndoAction^.Buffer)^.OldText      := GetSubText(Node);
  PFullTextChange(UndoAction^.Buffer)^.OldTrans     := '';
  UndoList.Add(UndoAction);
  SetText(Node, Text);
end;

// -----------------------------------------------------------------------------

function MyGetSubTrans(const Num: Integer): String;
begin
  Result := GetSubTranslation(GetNodeWithIndex(frmMain.lstSubtitles, Num));
end;

// -----------------------------------------------------------------------------

procedure MySetSubTrans(const Num: Integer; const Text: String);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  Node := GetNodeWithIndex(frmMain.lstSubtitles, Num);
  New(UndoAction);
  UndoAction^.UndoActionType := uaFullTextChange;
  UndoAction^.Node           := Node;
  UndoAction^.LineNumber     := Node.Index;
  UndoAction^.BindToNext     := True;
  UndoAction^.BufferSize     := SizeOf(TFullTextChange);
  UndoAction^.Buffer         := AllocMem(UndoAction^.BufferSize);
  PFullTextChange(UndoAction^.Buffer)^.OriginalOnly := False;
  PFullTextChange(UndoAction^.Buffer)^.OldText      := GetSubText(Node);
  PFullTextChange(UndoAction^.Buffer)^.OldTrans     := GetSubTranslation(Node);
  UndoList.Add(UndoAction);
  SetTranslation(Node, Text);
end;

// -----------------------------------------------------------------------------

function MyGetSubInitialTime(const Num: Integer): Integer;
begin
  Result := GetStartTime(GetNodeWithIndex(frmMain.lstSubtitles, Num));
end;

// -----------------------------------------------------------------------------

procedure MySetSubInitialTime(const Num: Integer; const InitialTime: Integer);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  Node := GetNodeWithIndex(frmMain.lstSubtitles, Num);
  New(UndoAction);
  UndoAction^.UndoActionType                 := uaTimeChange;
  UndoAction^.BufferSize                     := SizeOf(TTimeChange);
  UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
  UndoAction^.Node                           := Node;
  UndoAction^.LineNumber                     := Node.Index;
  UndoAction^.BindToNext                     := True;
  PTimeChange(UndoAction^.Buffer)^.StartTime := GetStartTime(Node);
  PTimeChange(UndoAction^.Buffer)^.FinalTime := -1;
  UndoList.Add(UndoAction);
  SetStartTime(Node, InitialTime);
end;

// -----------------------------------------------------------------------------

function MyGetSubFinalTime(const Num: Integer): Integer;
begin
  Result := GetFinalTime(GetNodeWithIndex(frmMain.lstSubtitles, Num));
end;

// -----------------------------------------------------------------------------

procedure MySetSubFinalTime(const Num: Integer; const FinalTime: Integer);
var
  Node       : PVirtualNode;
  UndoAction : PUndoAction;
begin
  Node := GetNodeWithIndex(frmMain.lstSubtitles, Num);
  New(UndoAction);
  UndoAction^.UndoActionType                 := uaTimeChange;
  UndoAction^.BufferSize                     := SizeOf(TTimeChange);
  UndoAction^.Buffer                         := AllocMem(UndoAction^.BufferSize);
  UndoAction^.Node                           := Node;
  UndoAction^.LineNumber                     := Node.Index;
  UndoAction^.BindToNext                     := True;
  PTimeChange(UndoAction^.Buffer)^.StartTime := -1;
  PTimeChange(UndoAction^.Buffer)^.FinalTime := GetFinalTime(Node);
  UndoList.Add(UndoAction);  
  SetFinalTime(Node, FinalTime);
end;

// -----------------------------------------------------------------------------

procedure MyInsSub(const Pos: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String);
var
  P          : Cardinal;
  Data       : PSubtitleItem;
  Node       : PVirtualNode;
  Node2      : PVirtualNode;
  UndoAction : PUndoAction;
begin
  if Pos > Integer(frmMain.lstSubtitles.RootNodeCount) then
    P := frmMain.lstSubtitles.RootNodeCount-1 else
  if Pos < 0 then P := 0 else
    P := Pos;
    
  if (frmMain.lstSubtitles.RootNodeCount = 0) or (P = 0) then
    Node2 := frmMain.lstSubtitles.InsertNode(frmMain.lstSubtitles.RootNode, amAddChildFirst) else
  if (P = frmMain.lstSubtitles.RootNodeCount) then
    Node2 := frmMain.lstSubtitles.InsertNode(frmMain.lstSubtitles.GetLast, amInsertAfter) else
  begin
    Node := frmMain.lstSubtitles.GetFirst;
    while Node.Index < P do
      Node := Node.NextSibling;
    Node2 := frmMain.lstSubtitles.InsertNode(Node.PrevSibling, amInsertAfter);
  end;

  if not (vsInitialized in Node2.States) then Include(Node2.States, vsInitialized);

  if Assigned(Node2) then
  begin
    with frmMain do
    begin
      New(UndoAction);
      UndoAction^.UndoActionType := uaInsertLine;
      UndoAction^.LineNumber     := Node2.Index;
      UndoAction^.Node           := Node2;
      UndoAction^.BindToNext     := True;
      UndoAction^.Buffer         := nil;
      UndoAction^.BufferSize     := 0;
      UndoList.Add(UndoAction);
      mnuUndo.Enabled := True;
    end;

    Data := frmMain.lstSubtitles.GetNodeData(Node2);
    Data^.InitialTime := InitialTime;
    Data^.FinalTime   := FinalTime;
    Data^.Text        := Text;
    Data^.Translation := Translation;
    Data^.Marked      := False;
    Data^.ErrorType   := [];
  end;

end;

// -----------------------------------------------------------------------------

procedure MyDelSub(const Num: Integer);
var
  Node       : PVirtualNode;
  Data       : PSubtitleItem;
  UndoAction : PUndoAction;
begin
  Node := GetNodeWithIndex(frmMain.lstSubtitles, Num);
  Data := frmMain.lstSubtitles.GetNodeData(Node);
  New(UndoAction);
  UndoAction^.BufferSize                        := SizeOf(TLineChange);
  UndoAction^.Buffer                            := AllocMem(UndoAction^.BufferSize);
  UndoAction^.UndoActionType                    := uaDeleteLine;
  UndoAction^.BindToNext                        := True;
  UndoAction^.LineNumber                        := Node.Index;
  PLineChange(UndoAction^.Buffer)^.SubtitleItem := Data^;
  UndoList.Add(UndoAction);  
  frmMain.lstSubtitles.DeleteNode(Node);
end;

// -----------------------------------------------------------------------------

function MyMsgBox(const AMsg, BCap1, BCap2, BCap3: String; const IconInd: Integer): Integer;
begin
  Result := MsgBox(AMsg, BCap1, BCap2, BCap3, IconInd, frmMain, frmMain.Font.Charset);
end;

// -----------------------------------------------------------------------------

function MyIsOriginalLoaded: Boolean;
begin
  Result := (frmMain.OrgFile <> '') or (frmMain.lstSubtitles.RootNodeCount > 0);
end;

// -----------------------------------------------------------------------------

function MyIsTranslatedLoaded: Boolean;
begin
  Result := (frmMain.TransFile <> '') or (frmMain.lstSubtitles.RootNodeCount > 0);
end;

// -----------------------------------------------------------------------------

procedure MyEnableWorkArea;
begin
  with frmMain do
  begin
    ClearUndoList(UndoList);
    ClearUndoList(RedoList);
    mnuUndo.Enabled := False;
    mnuRedo.Enabled := False;
    SetFormCaption;
    RefreshTimes;
    EnableCtrls(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure MyRandomize;
begin
  Randomize;
end;

// -----------------------------------------------------------------------------

function MyRandom(Range: Integer): Integer;
begin
  Result := Random(Range);
end;

// -----------------------------------------------------------------------------
// Date and time functions
// -----------------------------------------------------------------------------

function MyGetYear: Integer;
var
  YY, MM, DD: Word;
begin
  DecodeDate(Now, YY, MM, DD);
  Result := YY;
end;

// -----------------------------------------------------------------------------

function MyGetMonth: Integer;
var
  YY, MM, DD: Word;
begin
  DecodeDate(Now, YY, MM, DD);
  Result := MM;
end;

// -----------------------------------------------------------------------------

function MyGetDay: Integer;
var
  YY, MM, DD: Word;
begin
  DecodeDate(Now, YY, MM, DD);
  Result := DD;
end;

// -----------------------------------------------------------------------------

function MyGetHour: Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := HH;
end;

// -----------------------------------------------------------------------------

function MyGetMinute: Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := MM;
end;

// -----------------------------------------------------------------------------

function MyGetSecond: Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := SS;
end;

// -----------------------------------------------------------------------------

function MyGetDate: String;
begin
  Result := FormatDateTime('YYYY-MM-DD', Now);
end;

// -----------------------------------------------------------------------------

function MyGetTime: String;
begin
  Result := FormatDateTime('HH:MM', Now);
end;

// -----------------------------------------------------------------------------

function MyGetDateTime: String;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:MM', Now);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psCompExecCompile(Sender: TIFPS3CompExec);
begin
  Sender.AddFunction(@MyIsOriginalLoaded, 'function IsOriginalLoaded: Boolean;');
  Sender.AddFunction(@MyIsTranslatedLoaded, 'function IsTranslatedLoaded: Boolean;');
  Sender.AddFunction(@MyEnableWorkArea, 'function EnableWorkArea: Boolean;');
  Sender.AddFunction(@MyGetSubCount, 'function GetSubtitleCount: Integer;');
  Sender.AddFunction(@MyIsSelSub, 'function IsSubtitleSelected(const Num: Integer): Boolean;');
  Sender.AddFunction(@MyGetSubText, 'function GetSubtitleText(const Num: Integer): String;');
  Sender.AddFunction(@MySetSubText, 'procedure SetSubtitleText(const Num: Integer; const Text: String);');
  Sender.AddFunction(@MyGetSubTrans, 'function GetSubtitleTrans(const Num: Integer): String;');
  Sender.AddFunction(@MySetSubTrans, 'procedure SetSubtitleTrans(const Num: Integer; const Text: String);');
  Sender.AddFunction(@MyGetSubInitialTime, 'function GetSubtitleInitialTime(const Num: Integer): Integer;');
  Sender.AddFunction(@MySetSubInitialTime, 'procedure SetSubtitleInitialTime(const Num: Integer; const InitialTime: Integer);');
  Sender.AddFunction(@MyGetSubFinalTime, 'function GetSubtitleFinalTime(const Num: Integer): Integer;');
  Sender.AddFunction(@MySetSubFinalTime, 'procedure SetSubtitleFinalTime(const Num: Integer; const FinalTime: Integer);');
  Sender.AddFunction(@MyInsSub, 'procedure InsertSubtitle(const Pos: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String);');
  Sender.AddFunction(@MyDelSub, 'procedure DeleteSubtitle(const Num: Integer);');
  Sender.AddFunction(@MyMsgBox, 'function MsgBox(const AMsg, BCap1, BCap2, BCap3: String; const IconInd: Integer): Integer;');
  Sender.AddFunction(@MyRandomize, 'procedure Randomize;');
  Sender.AddFunction(@MyRandom, 'function Random(Range: Integer): Integer;');
  // Date and time functions
  Sender.AddFunction(@MyGetYear, 'function GetYear: Integer;');
  Sender.AddFunction(@MyGetMonth, 'function GetMonth: Integer;');
  Sender.AddFunction(@MyGetDay, 'function GetDay: Integer;');
  Sender.AddFunction(@MyGetHour, 'function GetHour: Integer;');
  Sender.AddFunction(@MyGetMinute, 'function GetMinute: Integer;');
  Sender.AddFunction(@MyGetSecond, 'function GetSecond: Integer;');
  Sender.AddFunction(@MyGetDate, 'function GetDate: String;');
  Sender.AddFunction(@MyGetTime, 'function GetTime: String;');
  Sender.AddFunction(@MyGetDateTime, 'function GetDateTime: String;');
  // ---
  Sender.AddRegisteredVariable('Application', 'TApplication');
  //Sender.AddRegisteredVariable('Self', 'TfrmMain');
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psCompExecExecute(Sender: TIFPS3CompExec);
begin
  psCompExec.SetVarToInstance('Application', Application);
  //psCompExec.SetVarToInstance('Self', Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psCompExecCompImport(Sender: TObject;
  x: TIFPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_extctrls(x);
  SIRegister_Forms(x);
  SIRegister_menus(x);
  RegisterDateTimeLibrary_C(x);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psCompExecExecImport(Sender: TObject; se: TIFPSExec;
  x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, true);
  RIRegister_Graphics(x);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_extctrls(x);
  RIRegister_Forms(x);
  RIRegister_menus(x);
  RegisterDateTimeLibrary_R(se);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psCompExecAfterExecute(Sender: TIFPS3CompExec);
begin
  if UndoList.Count > 0 then
    PUndoAction(UndoList.Last)^.BindToNext := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmbOCRScriptsSelect(Sender: TObject);
begin
  OCRDefFile := ExtractFilePath(Application.ExeName) + 'OCRScripts\' + cmbOCRScripts.Items[cmbOCRScripts.ItemIndex] + ID_OCREXT;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuShowInMainFormClick(Sender: TObject);
begin
  cmbOCRScripts.Visible := mnuShowInMainForm.Checked;
  OnResize(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mnuShowSubtitlesClick(Sender: TObject);
begin
  subSubtitle.Visible := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if MovieFile <> '' then
    FreeFile;
end;
// -----------------------------------------------------------------------------

procedure TfrmMain.lstSubtitlesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);

//  procedure TfrmMain.lstSubtitlesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);

  function CountUnTranslated: Integer;
  var
    Node  : PVirtualNode;
    Text  : String;
    Trans : String;
  begin
    Result := 0;
    Node := lstSubtitles.GetFirst;
    while Assigned(Node) do
    begin
      Text  := GetSubText(Node);
      Trans := GetSubTranslation(Node);
      if (Text = Trans) or (Trans = '') or (Trans = UntranslatedSub) then
        Inc(Result);
      Node := Node.NextSibling;
    end;
  end;

var
  Data: PSubtitleItem;
begin
  Data     := lstSubtitles.GetNodeData(Node);
  HintText := '';

  if Data.ErrorType = [] then
  begin
    if Data.Marked = True then
      HintText := ErrorReports[20] else
      HintText := '';
  end;

  if etLinesWithoutLetters  in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[01];
  if etEmptySubtitle        in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[02];
  // ---
  if etOverlapping          in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[03];
  if etBadValues            in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[04];
  if etTooLongDuration      in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[05];
  if etTooShortDuration     in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[06];
  if etTooLongLine          in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[07];
  if etOverTwoLines         in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[08];
  // ---
  if etHearingImpaired      in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[09];
  if etTextBeforeColon      in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[10];
  if etUnnecessaryDots      in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[11];
  if etProhibitedCharacter  in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[12];
  if etRepeatedCharacter    in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[13];
  if etRepeatedSubtitle     in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[14];
  if etOCRErrors            in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[15];
  // ---
  if etOpnDlgSubsOneLine    in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[16];
  if etSpaceAfterCustChars  in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[17];
  if etSpaceBeforeCustChars in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[18];
  if etUnnecessarySpaces    in Data.ErrorType then  HintText := HintText + #13#10 + ErrorReports[19];

  if (Data.ErrorType = []) and (Data.Marked = False) then
  begin
    if mnuTranslatorMode.Checked then
      HintText := Format(TransLeftLines, [CountUnTranslated]) else
      lstSubtitles.Hint := '';
  end;
  HintText := StringToWideStringEx(Trim(HintText), CharsetToCodePage(frmMain.Font.Charset));
end;

end.
