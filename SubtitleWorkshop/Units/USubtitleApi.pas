// URUSoft Subtitle API 1.0 WRAPPER for Delphi
// Copyright ® 2002-2003 URUSoft.

unit USubtitleApi;

// -----------------------------------------------------------------------------

interface

uses
  Windows;

type
  TTextAlign = (taLeft, taRight, taCenter);
  TTMPlayerFormat = (tfMultiline, tfTimeStruct1, tfTimeStruct2, tfPlusTimeStruct1, tfPlusTimeStruct2);

  TSubtitleApi = class
  private
    FInstance: Integer;
    function MyFileExists(const Name: String): Boolean;
    function GetModuleVersion: Integer;
    function GetModuleDescription: String;
    function GetSupportedFormatsCount: Integer;
    function GetSubtitleCount: Integer;
    function GetCurrentFormat(var Name: String; var Index: Integer): Boolean;
    function GetCurrentFormatName: String;
    function GetCurrentFormatIndex: Integer;
    function GetPlaybackDelay: Integer;
    procedure SetPlaybackDelay(Time: Integer);
    function GetNoInteractionWithTags: Boolean;
    procedure SetNoInteractionWithTags(Value: Boolean);
    function GetWorkWithTags: Boolean;
    procedure SetWorkWithTags(Value: Boolean);
    function GetInitialized: Boolean;
  public
    constructor Create(DLLFileName: String);
    destructor Destroy; override;
    function Initialize(DLLFileName: String): Boolean;
    function UnInitialize: Boolean;
    function LoadSubtitle(FileName: String; FPS: Single; FormatIndex: Integer = 0; Append: Boolean = False; ReCalcTimeValues: Boolean = True): Boolean;
    procedure CreateNewSubtitle;
    function GetFileFormat(FileName: String): Integer;
    function SaveSubtitle(FileName: String; FormatIndex: Integer; FPS: Single; FromIndex: Integer = -1; ToIndex: Integer = -1): Boolean;
    function CloseSubtitle: Boolean;
    function AddSubtitle(InitialTime, FinalTime: Integer; Text: String): Integer;
    function InsertSubtitle(Index, InitialTime, FinalTime: Integer; Text: String): Boolean;
    function MoveSubtitle(OldIndex, NewIndex: Integer): Boolean;
    function DeleteSubtitle(Index: Integer): Boolean;
    function ClearSubtitles: Boolean;
    function GetSubtitleText(Time: Integer): String;
    function GetSubtitle(Index: Integer; var InitialTime, FinalTime: Integer; var Text: String): Boolean;
    function GetInitialTime(Index: Integer): Integer;
    function GetFinalTime(Index: Integer): Integer;
    function GetText(Index: Integer): String;
    function SetSubtitle(Index: Integer; InitialTime, FinalTime: Integer; Text: String): Boolean;
    function GetFormatName(Index: Integer): String;
    function GetFormatIndex(Name: String): Integer;
    procedure SetAbsoluteDelay(Time: Integer; FromIndex: Integer = -1; ToIndex: Integer = -1);
    function ReverseSubtitleText(FromIndex: Integer = -1; ToIndex: Integer = -1; KeepLinesOrder: Boolean = True): Boolean;
    function GetFormatInfo(Index: Integer; var Description, Extensions: String): Boolean;
    function IsFrameBased(FormatIndex: Integer): Boolean;
    function FillDialogFilter(AllSupportedText: String = 'All supported files'): String;
    property ModuleVersion         : Integer read GetModuleVersion;
    property Description           : String  read GetModuleDescription;
    property FormatsCount          : Integer read GetSupportedFormatsCount;
    property SubtitleCount         : Integer read GetSubtitleCount;
    property CurrentFormatName     : String  read GetCurrentFormatName;
    property CurrentFormatIndex    : Integer read GetCurrentFormatIndex;
    property PlaybackDelay         : Integer read GetPlaybackDelay          write SetPlaybackDelay;
    property NoInteractionWithTags : Boolean read GetNoInteractionWithTags  write SetNoInteractionWithTags;
    property WorkWithTags          : Boolean read GetWorkWithTags           write SetWorkWithTags;
    property Initialized           : Boolean read GetInitialized;
    // -------------------------------------- //
    //             Output settings            //
    // -------------------------------------- //
    procedure SetOutputSettingsAdvancedSubStationAlpha(Assigned: Boolean; Collisions: String; PlayResX, PlayResY: Integer; Timer, FontName: String; FontSize, PrimaryColor, SecondaryColor, OutlineColour, BackColour: Integer; Bold, Italic, Underline, StrikeOut: Boolean; ScaleX, ScaleY, Spacing: Integer; Angle: Single; BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding: Integer);
    procedure SetOutputSettingsDVDSubtitle(Assigned: Boolean; DiskId, DVDTitle, Language, Author, Web, Info, License: String);
    procedure SetOutputSettingsSAMI(Assigned: Boolean; FontName: String; FontSize: Integer; Bold, Italic, Underline: Boolean; SubColor, BackgroundColor: Integer; Align: TTextAlign);
    procedure SetOutputSettingsSonicScenarist(Assigned, PAL, DropFrame: Boolean; Color0, Color1, Color2, Color3, Contrast0, Contrast1, Contrast2, Contrast3: Integer);
    procedure SetOutputSettingsSubViewer1(Assigned: Boolean; Title, Author, Source, vProgram, Path: String; Delay: Integer);
    procedure SetOutputSettingsSubViewer2(Assigned: Boolean; Title, Author, Source, vProgram, Path: String; Delay, CDTrack: Integer; Comment, FontName: String; FontSize, FontColor: Integer; Bold, Italic, Underline, StrikeOut: Boolean);
    procedure SetOutputSettingsSubStationAlpha(Assigned: Boolean; Title, Script, FontName: String; FontSize: Integer; Bold, Italic: Boolean; BorderStyle, PrimaryColor, SecondaryColor, TertiaryColor, ShadowColor, Outline, Shadow, Alignment, LeftMargin, RightMargin, VerticalMargin, Encoding : Integer);
    procedure SetOutputSettingsTMPlayer(Assigned: Boolean; TypeOfFormat: TTMPlayerFormat);
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

constructor TSubtitleApi.Create(DLLFileName: String);
begin
  FInstance := 0;
  Initialize(DLLFileName);
end;

// -----------------------------------------------------------------------------

destructor TSubtitleApi.Destroy;
begin
  UnInitialize;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.MyFileExists(const Name: String): Boolean;
var
  ErrorMode : UINT;
  Attrib    : Integer;
begin
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  Try
    Attrib := GetFileAttributes(PChar(Name));
    Result := (Attrib <> -1) And (Attrib And $00000010 = 0);
  Finally
    SetErrorMode(ErrorMode);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.Initialize(DLLFileName: String): Boolean;
begin
  Result := False;

  If (DLLFileName <> '') And (MyFileExists(DLLFileName) = True) Then
  Begin
    If FInstance <> 0 Then UnInitialize;
    FInstance := LoadLibrary(PChar(DLLFileName));

    If FInstance <> 0 Then
      Result := True;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.UnInitialize: Boolean;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    CloseSubtitle;

    If FreeLibrary(FInstance) = True Then
    Begin
      FInstance := 0;
      Result    := True;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetModuleVersion: Integer;
var
  GetModVersion: function: Integer; stdcall;
begin
  Result := 0;

  If FInstance <> 0 Then
  Begin
    GetModVersion := GetProcAddress(FInstance, 'GetModuleVersion');
    If @GetModVersion <> NIL Then
      Result := GetModVersion;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetModuleDescription: String;
var
  GetModDescription : procedure(Text: PChar; var BufferLen: Integer); stdcall;
  Len               : Integer;
begin
  Result := '';

  If FInstance <> 0 Then
  Begin
    GetModDescription := GetProcAddress(FInstance, 'GetModuleDescription');
    If @GetModDescription <> NIL Then
    Begin
      Len := 0;
      GetModDescription(PChar(Result), Len);
      If Len > 0 Then
      Begin
        SetLength(Result, Len);
        GetModDescription(PChar(Result), Len);
      End;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetSupportedFormatsCount: Integer;
var
  GetSuppFormatsCount: function: Integer; stdcall;
begin
  Result := 0;

  If FInstance <> 0 Then
  Begin
    GetSuppFormatsCount := GetProcAddress(FInstance, 'GetSupportedFormatsCount');
    If @GetSuppFormatsCount <> NIL Then
      Result := GetSuppFormatsCount;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.LoadSubtitle(FileName: String; FPS: Single; FormatIndex: Integer = 0; Append: Boolean = False; ReCalcTimeValues: Boolean = True): Boolean;
var
  LoadSubFile: function(FileName: PChar; FPS: Single; FormatIndex: Integer; Append, ReCalcTimeValues: LongBool): LongBool; stdcall;
begin
  Result := False;
  If (FInstance <> 0) And MyFileExists(FileName) = True Then
  Begin
    LoadSubFile := GetProcAddress(FInstance, 'LoadSubtitleFile');
    If @LoadSubFile <> NIL Then
      Result := LoadSubFile(PChar(FileName), FPS, FormatIndex, Append, ReCalcTimeValues);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.CreateNewSubtitle;
var
  CreateNewSub: procedure stdcall;
begin
  If (FInstance <> 0) Then
  Begin
    CreateNewSub := GetProcAddress(FInstance, 'CreateNewSubtitle');
    If @CreateNewSub <> NIL Then
      CreateNewSub;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetFileFormat(FileName: String): Integer;
var
  GetFFormat: function(FileName: PChar): Integer; stdcall;
begin
  Result := 0;
  
  If (FInstance <> 0) Then
  Begin
    GetFFormat := GetProcAddress(FInstance, 'GetFileFormat');
    If @GetFFormat <> NIL Then
      Result := GetFFormat(PChar(FileName));
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.SaveSubtitle(FileName: String; FormatIndex: Integer; FPS: Single; FromIndex: Integer = -1; ToIndex: Integer = -1): Boolean;
var
  SaveSubFile: function(FileName: PChar; FormatIndex: Integer; FPS: Single; FromIndex, ToIndex: Integer): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    SaveSubFile := GetProcAddress(FInstance, 'SaveSubtitleFile');
    If @SaveSubFile <> NIL Then
      Result := SaveSubFile(PChar(FileName), FormatIndex, FPS, FromIndex, ToIndex);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.CloseSubtitle: Boolean;
var
  CloseSubFile: procedure; stdcall;
begin
  Result := False;
  If FInstance <> 0 Then
  Begin
    CloseSubFile := GetProcAddress(FInstance, 'CloseSubtitleFile');
    If @CloseSubFile <> NIL Then
    Begin
      CloseSubFile;
      Result := True;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.AddSubtitle(InitialTime, FinalTime: Integer; Text: String): Integer;
var
  AddSub: function(InitialTime, FinalTime: Integer; Text: PChar): Integer; stdcall;
begin
  Result := -1;

  If FInstance <> 0 Then
  Begin
    AddSub := GetProcAddress(FInstance, 'AddSubtitle');
    If @AddSub <> NIL Then
      Result := AddSub(InitialTime, FinalTime, PChar(Text));
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.InsertSubtitle(Index, InitialTime, FinalTime: Integer; Text: String): Boolean;
var
  InsertSub: function(Index, InitialTime, FinalTime: Integer; Text: PChar): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    InsertSub := GetProcAddress(FInstance, 'InsertSubtitle');
    If @InsertSub <> NIL Then
      Result := InsertSub(Index, InitialTime, FinalTime, PChar(Text));
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.MoveSubtitle(OldIndex, NewIndex: Integer): Boolean;
var
  MoveSub: function(Index, NewIndex: Integer): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    MoveSub := GetProcAddress(FInstance, 'MoveSubtitle');
    If @MoveSub <> NIL Then
      Result := MoveSub(OldIndex, NewIndex);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.DeleteSubtitle(Index: Integer): Boolean;
var
  DeleteSub: function(Index: Integer): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    DeleteSub := GetProcAddress(FInstance, 'DeleteSubtitle');
    If @DeleteSub <> NIL Then
      Result := DeleteSub(Index);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.ClearSubtitles: Boolean;
var
  ClearSub: procedure; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    ClearSub := GetProcAddress(FInstance, 'ClearSubtitles');
    If @ClearSub <> NIL Then
    Begin
      ClearSub;
      Result := True;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetSubtitleText(Time: Integer): String;
var
  GetSubText : procedure(Time: Integer; Text: PChar; var BufferLen: Integer); stdcall;
  Len        : Integer;
begin
  Result := '';

  If FInstance <> 0 Then
  Begin
    GetSubText := GetProcAddress(FInstance, 'GetSubtitleText');
    If @GetSubText <> NIL Then
    Begin
      Len := 0;
      GetSubText(Time, PChar(Result), Len);
      If Len > 0 Then
      Begin
        SetLength(Result, Len);
        GetSubText(Time, PChar(Result), Len);
      End;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetSubtitle(Index: Integer; var InitialTime, FinalTime: Integer; var Text: String): Boolean;
var
  GetSub : function(Index: Integer; var InitialTime, FinalTime: Integer; Text: PChar; var BufferLen: Integer): LongBool; stdcall;
  Len    : Integer;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    GetSub := GetProcAddress(FInstance, 'GetSubtitle');
    If @GetSub <> NIL Then
    Begin
      Len := 0;
      If GetSub(Index, InitialTime, FinalTime, PChar(Text), Len) = True Then
      Begin
        SetLength(Text, Len);
        Result := GetSub(Index, InitialTime, FinalTime, PChar(Text), Len);
      End;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetInitialTime(Index: Integer): Integer;
var
  FinalTime : Integer;
  Text      : String;
begin
  GetSubtitle(Index, Result, FinalTime, Text);
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetFinalTime(Index: Integer): Integer;
var
  InitialTime : Integer;
  Text        : String;
begin
  GetSubtitle(Index, InitialTime, Result, Text);
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetText(Index: Integer): String;
var
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  GetSubtitle(Index, InitialTime, FinalTime, Result);
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.SetSubtitle(Index: Integer; InitialTime, FinalTime: Integer; Text: String): Boolean;
var
  SetSub: function(Index: Integer; InitialTime, FinalTime: Integer; Text: PChar): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    SetSub := GetProcAddress(FInstance, 'SetSubtitle');
    If @SetSub <> NIL Then
      Result := SetSub(Index, InitialTime, FinalTime, PChar(Text));
  End;
end;
// -----------------------------------------------------------------------------

function TSubtitleApi.GetSubtitleCount: Integer;
var
  GetSubCount: function: Integer; stdcall;
begin
  Result := 0;

  If FInstance <> 0 Then
  Begin
    GetSubCount := GetProcAddress(FInstance, 'GetSubtitleCount');
    If @GetSubCount <> NIL Then
      Result := GetSubCount;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetCurrentFormat(var Name: String; var Index: Integer): Boolean;
var
  GetCurrFormat : procedure(Name: PChar; var Index: Integer; var BufferLen: Integer); stdcall;
  Len           : Integer;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    GetCurrFormat := GetProcAddress(FInstance, 'GetCurrentFormat');
    If @GetCurrFormat <> NIL Then
    Begin
      Len := 0;
      GetCurrFormat(PChar(Name), Index, Len);
      If Len > 0 Then
      Begin
        SetLength(Name, Len);
        GetCurrFormat(PChar(Name), Index, Len);
        Result := True;
      End;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetCurrentFormatName: String;
var
  Num: Integer;
begin
  Result := '';
  GetCurrentFormat(Result, Num);
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetCurrentFormatIndex: Integer;
var
  Name: String;
begin
  Result := -1;
  GetCurrentFormat(Name, Result);
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetPlaybackDelay: Integer;
var
  GetPlayDelay: function: Integer; stdcall;
begin
  Result := 0;

  If FInstance <> 0 Then
  Begin
    GetPlayDelay := GetProcAddress(FInstance, 'GetPlaybackDelay');
    If @GetPlayDelay <> NIL Then
      Result := GetPlayDelay;
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.SetPlaybackDelay(Time: Integer);
var
  SetPlayDelay: procedure(Time: Integer); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetPlayDelay := GetProcAddress(FInstance, 'SetPlaybackDelay');
    If @SetPlayDelay <> NIL Then
      SetPlayDelay(Time);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.SetAbsoluteDelay(Time: Integer; FromIndex: Integer = -1; ToIndex: Integer = -1);
var
  SetAbsDelay: function(Time, FromIndex, ToIndex: Integer): LongBool; stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetAbsDelay := GetProcAddress(FInstance, 'SetAbsoluteDelay');
    If @SetAbsDelay <> NIL Then
      SetAbsDelay(Time, FromIndex, ToIndex);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetNoInteractionWithTags: Boolean;
var
  GetNoInterWithTags: function: LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    GetNoInterWithTags := GetProcAddress(FInstance, 'GetNoInteractionWithTags');
    If @GetNoInterWithTags <> NIL Then
      Result := GetNoInterWithTags;
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.SetNoInteractionWithTags(Value: Boolean);
var
  SetNoInterWithTags: procedure(Value: Boolean); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetNoInterWithTags := GetProcAddress(FInstance, 'SetNoInteractionWithTags');
    If @SetNoInterWithTags <> NIL Then
      SetNoInterWithTags(Value);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetWorkWithTags: Boolean;
var
  GetSubWorkWithTags: function: LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    GetSubWorkWithTags := GetProcAddress(FInstance, 'GetSubtitleWorkWithTags');
    If @GetSubWorkWithTags <> NIL Then
      Result := GetSubWorkWithTags;
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.SetWorkWithTags(Value: Boolean);
var
  SetSubWorkWithTags: procedure(Value: Boolean); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetSubWorkWithTags := GetProcAddress(FInstance, 'SetSubtitleWorkWithTags');
    If @SetSubWorkWithTags <> NIL Then
      SetSubWorkWithTags(Value);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.ReverseSubtitleText(FromIndex: Integer = -1; ToIndex: Integer = -1; KeepLinesOrder: Boolean = True): Boolean;
var
  ReverseSubText: function(FromIndex, ToIndex: Integer; KeepLinesOrder: LongBool): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    ReverseSubText := GetProcAddress(FInstance, 'ReverseSubtitleText');
    If @ReverseSubText <> NIL Then
      Result := ReverseSubText(FromIndex, ToIndex, KeepLinesOrder);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetFormatInfo(Index: Integer; var Description, Extensions: String): Boolean;
var
  GetFormatInformation : function(Index: Integer; Description: PChar; Extensions: PChar; var BufferLen1, BufferLen2: Integer): LongBool; stdcall;
  Len1, Len2           : Integer;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    GetFormatInformation := GetProcAddress(FInstance, 'GetFormatInformation');
    If @GetFormatInformation <> NIL Then
    Begin
      Len1 := 0;
      Len2 := 0;
      If GetFormatInformation(Index, PChar(Description), PChar(Extensions), Len1, Len2) = True Then
      Begin
        SetLength(Description, Len1);
        SetLength(Extensions, Len2);
        Result := GetFormatInformation(Index, PChar(Description), PChar(Extensions), Len1, Len2)
      End;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.IsFrameBased(FormatIndex: Integer): Boolean;
var
  FrameBased: function(FormatIndex: Integer): LongBool; stdcall;
begin
  Result := False;

  If FInstance <> 0 Then
  Begin
    FrameBased := GetProcAddress(FInstance, 'IsFrameBased');
    If @FrameBased <> NIL Then
      Result := FrameBased(FormatIndex);
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetFormatName(Index: Integer): String;
var
  GetFrmtName : procedure(Index: Integer; Name: PChar; var BufferLen: Integer); stdcall;
  Len         : Integer;
begin
  Result := '';

  If FInstance <> 0 Then
  Begin
    GetFrmtName := GetProcAddress(FInstance, 'GetFormatName');
    If @GetFrmtName <> NIL Then
    Begin
      Len := 0;
      GetFrmtName(Index, PChar(Result), Len);
      If Len > 0 Then
      Begin
        SetLength(Result, Len);
        GetFrmtName(Index, PChar(Result), Len);
      End;
    End;
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetFormatIndex(Name: String): Integer;
var
  GetFrmtNumber: function(Name: PChar): Integer; stdcall;
begin
  Result := -1;

  If FInstance <> 0 Then
  Begin
    GetFrmtNumber := GetProcAddress(FInstance, 'GetFormatIndex');
    If @GetFrmtNumber <> NIL Then
      Result := GetFrmtNumber(PChar(Name));
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.GetInitialized: Boolean;
begin
  Result := (FInstance <> 0);
end;

// -----------------------------------------------------------------------------

function TSubtitleApi.FillDialogFilter(AllSupportedText: String = 'All supported files'): String;
var
  Desc, Exts, AllExts : String;
  i, FCount           : Integer;
begin
  Result  := '';
  AllExts := '';

  If FInstance <> 0 Then
  Begin
    FCount := GetSupportedFormatsCount;
    If FCount = 0 Then Exit;

    For i := 1 To FCount Do
      If GetFormatInfo(i, Desc, Exts) = True Then
      Begin
        Result  := Result + Desc + ' (' + Exts + ')|' + Exts + '|';
        if Pos(Exts, AllExts) = 0 then
          AllExts := AllExts + Exts + ';';
      End;

    if AllSupportedText <> '' then
      Result := AllSupportedText + ' (' + AllExts + ')|' + AllExts + '|' + Result;
  End;
end;

// -------------------------------------------------------------------------- //
//                               Output settings                              //
// -------------------------------------------------------------------------- //

procedure TSubtitleApi.SetOutputSettingsAdvancedSubStationAlpha(Assigned: Boolean; Collisions: String; PlayResX, PlayResY: Integer; Timer, FontName: String; FontSize, PrimaryColor, SecondaryColor, OutlineColour, BackColour: Integer; Bold, Italic, Underline, StrikeOut: Boolean; ScaleX, ScaleY, Spacing: Integer; Angle: Single; BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding: Integer);
var
  SetOSAdvancedSubStationAlpha: procedure(Assigned: LongBool; Collisions: PChar;
                                          PlayResX, PlayResY: Integer; Timer, FontName: PChar;
                                          FontSize, PrimaryColor, SecondaryColor, OutlineColour,
                                          BackColour: Integer; Bold, Italic, Underline, StrikeOut: LongBool;
                                          ScaleX, ScaleY, Spacing: Integer; Angle: Single;
                                          BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR,
                                          MarginV, Encoding : Integer); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSAdvancedSubStationAlpha := GetProcAddress(FInstance, 'SetOutputSettingsDVDSubtitle');
    If @SetOSAdvancedSubStationAlpha <> NIL Then
      SetOSAdvancedSubStationAlpha(LongBool(Assigned), PChar(Collisions), PlayResX, PlayResY, PChar(Timer), PChar(FontName), FontSize, PrimaryColor, SecondaryColor, OutlineColour, BackColour, LongBool(Bold), LongBool(Italic), LongBool(Underline), LongBool(StrikeOut), ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.SetOutputSettingsDVDSubtitle(Assigned: Boolean; DiskId, DVDTitle, Language, Author, Web, Info, License: String);
var
  SetOSDVDSubtitle: procedure(Assigned: LongBool; DiskId, DVDTitle, Language,
                              Author, Web, Info, License : PChar); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSDVDSubtitle := GetProcAddress(FInstance, 'SetOutputSettingsDVDSubtitle');
    If @SetOSDVDSubtitle <> NIL Then
      SetOSDVDSubtitle(LongBool(Assigned), PChar(DiskId), PChar(DVDTitle), PChar(Language), PChar(Author), PChar(Web), PChar(Info), PChar(License));
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleApi.SetOutputSettingsSAMI(Assigned: Boolean; FontName: String; FontSize: Integer; Bold, Italic, Underline: Boolean; SubColor, BackgroundColor: Integer; Align: TTextAlign);
var
  SetOSSAMI: procedure(Assigned: LongBool; FontName: PChar; FontSize: Integer;
                       Bold, Italic, Underline: LongBool; SubColor,
                       BackgroundColor:Integer; Align: PChar); stdcall;
  AlignStr : PChar;
begin
  If FInstance <> 0 Then
  Begin
    SetOSSAMI := GetProcAddress(FInstance, 'SetOutputSettingsSAMI');
    If @SetOSSAMI <> NIL Then
    Begin
      Case Align Of
        taLeft  : AlignStr := 'Left';
        taRight : AlignStr := 'Right';
        Else
          AlignStr := 'Center';
      End;

      SetOSSAMI(LongBool(Assigned), PChar(FontName), FontSize, Bold, Italic, Underline, SubColor, BackgroundColor, AlignStr);
    End;
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleAPI.SetOutputSettingsSonicScenarist(Assigned, PAL, DropFrame: Boolean; Color0, Color1, Color2, Color3, Contrast0, Contrast1, Contrast2, Contrast3: Integer);
var
  SetOSSonicScenarist: procedure(Assigned, PAL, DropFrame: LongBool; Color0,
                                 Color1, Color2, Color3, Contrast0, Contrast1,
                                 Contrast2, Contrast3: Integer); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSSonicScenarist := GetProcAddress(FInstance, 'SetOutputSettingsSonicScenarist');
    If @SetOSSonicScenarist <> NIL Then
      SetOSSonicScenarist(LongBool(Assigned), LongBool(PAL), LongBool(DropFrame), Color0, Color1, Color2, Color3, Contrast0, Contrast1, Contrast2, Contrast3);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleAPI.SetOutputSettingsSubViewer1(Assigned: Boolean; Title, Author, Source, vProgram, Path: String; Delay: Integer);
var
  SetOSSubViewer1: procedure(Assigned: LongBool; Title, Author, Source,
                             vProgram, Path: PChar; Delay: Integer); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSSubViewer1 := GetProcAddress(FInstance, 'SetOutputSettingsSubViewer1');
    If @SetOSSubViewer1 <> NIL Then
      SetOSSubViewer1(LongBool(Assigned), PChar(Title), PChar(Author), PChar(Source), PChar(vProgram), PChar(Path), Delay);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleAPI.SetOutputSettingsSubViewer2(Assigned: Boolean; Title, Author, Source, vProgram, Path: String; Delay, CDTrack: Integer; Comment, FontName: String; FontSize, FontColor: Integer; Bold, Italic, Underline, StrikeOut: Boolean);
var
  SetOSSubViewer2: procedure(Assigned: LongBool; Title, Author, Source, vProgram,
                             Path: PChar; Delay, CDTrack: Integer; Comment,
                             FontName: PChar; FontSize, FontColor: Integer;
                             Bold, Italic, Underline, StrikeOut: LongBool); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSSubViewer2 := GetProcAddress(FInstance, 'SetOutputSettingsSubViewer2');
    If @SetOSSubViewer2 <> NIL Then
      SetOSSubViewer2(LongBool(Assigned), PChar(Title), PChar(Author), PChar(Source), PChar(vProgram), PChar(Path), Delay, CDTrack, PChar(Comment), PChar(FontName), FontSize, FontColor, LongBool(Bold), LongBool(Italic), LongBool(Underline), LongBool(StrikeOut));
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleAPI.SetOutputSettingsSubStationAlpha(Assigned: Boolean; Title, Script, FontName: String; FontSize: Integer; Bold, Italic: Boolean; BorderStyle, PrimaryColor, SecondaryColor, TertiaryColor, ShadowColor, Outline, Shadow, Alignment, LeftMargin, RightMargin, VerticalMargin, Encoding : Integer);
var
  SetOSSubStationAlpha: procedure(Assigned: LongBool; Title, Script, FontName: PChar;
                                  FontSize: Integer; Bold, Italic: LongBool; BorderStyle,
                                  PrimaryColor, SecondaryColor, TertiaryColor, ShadowColor,
                                  Outline, Shadow, Alignment, MarginL, MarginR, MarginV,
                                  Encoding : Integer); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSSubStationAlpha := GetProcAddress(FInstance, 'SetOutputSettingsSubStationAlpha');
    If @SetOSSubStationAlpha <> NIL Then
      SetOSSubStationAlpha(LongBool(Assigned), PChar(Title), PChar(Script), PChar(FontName), FontSize, LongBool(Bold), LongBool(Italic), BorderStyle, PrimaryColor, SecondaryColor, TertiaryColor, ShadowColor, Outline, Shadow, Alignment, LeftMargin, RightMargin, VerticalMargin, Encoding);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleAPI.SetOutputSettingsTMPlayer(Assigned: Boolean; TypeOfFormat: TTMPlayerFormat);
var
  SetOSTMPlayer: procedure(Assigned: LongBool; TypeOfFormat: Integer); stdcall;
begin
  If FInstance <> 0 Then
  Begin
    SetOSTMPlayer := GetProcAddress(FInstance, 'SetOutputSettingsTMPlayer');
    If @SetOSTMPlayer <> NIL Then
      SetOSTMPlayer(LongBool(Assigned), Integer(TypeOfFormat));
  End;
end;

// -----------------------------------------------------------------------------

end.
