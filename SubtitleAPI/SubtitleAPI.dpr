// URUSoft Subtitle API v1.06
// Copyright ® 2002-2005 URUSoft.

{

 Version  Changes
 -------------------------------------------------------------------------------
 1.00     Start.
 1.01     Added Support for FAB Subtitler, MAC DVD Studio Pro and MPSub.
          Fixed A bug when reading subtitle file in Spruce DVDMaestro format.
          ?
 1.02     Added Support for Inscriber CG, OVR Script and Panimation.
          Fixed AQTitle, Captions DAT, Captions DAT Text, DVDSubtitle,
          FAB Subtitler, MacSub, MPSub, PowerPixel, Sasami Script, SBT,
          SonicDVD, SpruceDVD, SubRip, SubViewer 1.0 and SubViewer 2.0
          Access Violation!
          Added "No interaction with tags" option
 1.03     Fixed a bug when loading some SubViewer 2 files created by SubRip
 1.04     Added support for Adobe Encore DVD format (no unicode)
          Fixed Sonic Scenarist, SonicDVD, Pinnacle Impression time formats
          read and write.
 1.05     Fixed bug reading SAMI files
          Added support for "Ulead DVD Workshop 2.0" format
 1.06     Fixed bug when reading and saving Softni format.

}

library SubtitleAPI;

// -----------------------------------------------------------------------------

{$IFDEF VER150}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses
  SysUtils,
  Windows,
  USubtitleFile in 'USubtitleFile.pas',
  UCheckFormat in 'UCheckFormat.pas',
  USubtitlesRead in 'USubtitlesRead.pas',
  USubtitlesSave in 'USubtitlesSave.pas',
  USubtitlesFunctions in 'USubtitlesFunctions.pas',
  FastStrings in 'FastStrings.pas';

var
  Subtitles: TSubtitles = NIL;

// -----------------------------------------------------------------------------

{$R *.res}

// -------------------------------------------------------------------------- //
//                              Module specific                               //
// -------------------------------------------------------------------------- //

function GetModuleVersion: Integer; stdcall;
begin
  Result := $106; // 1.06
end;

// -----------------------------------------------------------------------------

procedure GetModuleDescription(Text: PChar; var BufferLen: Integer); stdcall;
const
  Description = 'URUSoft Subtitle API';
begin
  if BufferLen > 0 then
    StrLCopy(Text, PChar(Description), BufferLen)
  else
    BufferLen := Length(Description);
end;

// -----------------------------------------------------------------------------

function GetSupportedFormatsCount: Integer; stdcall;
begin
  Result := High(TSubtitleFormatsName);
end;

// -------------------------------------------------------------------------- //
//                              Format specific                               //
// -------------------------------------------------------------------------- //

function GetFormatInformation(Index: Integer; Description: PChar; Extensions: PChar; var BufferLen1, BufferLen2: Integer): LongBool; stdcall;
var
  Desc, Exts: String;
begin
  Result := True;
  Desc   := '';
  Exts   := '';

  case TSubtitleformats(Index) Of
    sfAdobeEncoreDVD          : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfAdvancedSubStationAlpha : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.ass'; end;
    sfAQTitle                 : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.aqt'; end;
    sfCaptions32              : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfCaptionsDat             : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.dat'; end;
    sfCaptionsDatText         : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.dat'; end;
    sfCaptionsInc             : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfCheetah                 : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.asc'; end;
    sfCPC600                  : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfDKS                     : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.dks'; end;
    sfDVDJunior               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfDVDSubtitleSystem       : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfDVDSubtitle             : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfFABSubtitler            : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfIAuthor                 : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfInscriberCG             : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfJACOSub                 : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.js;*.jss'; end;
    sfKaraokeLyricsLRC        : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.lrc'; end;
    sfKaraokeLyricsVKT        : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.vkt'; end;
    sfMACDVDStudioPro         : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfMacSUB                  : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.scr'; end;
    sfMicroDVD                : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub;*.txt'; end;
    sfMPlayer                 : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.mpl'; end;
    sfMPlayer2                : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.mpl'; end;
    sfMPSub                   : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfOVRScript               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.ovr'; end;
    sfPanimator               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.pan'; end;
    sfPhilipsSVCD             : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfPhoenixJS               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.pjs'; end;
    sfPinnacleImpression      : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfPowerDivX               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.psb;*.txt'; end;
    sfPowerPixel              : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfQuickTimeText           : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfRealTime                : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.rt'; end;
    sfSAMI                    : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.smi;*.sami'; end;
    sfSasamiScript            : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.s2k'; end;
    sfSBT                     : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sbt'; end;
    sfSoftni                  : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfSoftitlerRTF            : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.rtf'; end;
    sfSonicDVD                : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfSonicScenarist          : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sst'; end;
    sfSpruceDVDMaestro        : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.son'; end;
    sfSpruceSubtitleFile      : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.stl'; end;
    sfSSTPlayer               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sst'; end;
    sfSSTScript               : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.ssts'; end;
    sfSubCreator              : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfSubRip                  : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.srt'; end;
    sfSubSonic                : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfSubStationAlpha         : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.ssa'; end;
    sfSubViewer1              : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfSubViewer2              : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.sub'; end;
    sfTMPlayer                : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt;*.sub'; end;
    sfTurboTitler             : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.tts'; end;
    sfUleadDVDWorkshop2       : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.txt'; end;
    sfViPlay                  : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.vsf'; end;
    sfZeroG                   : begin Desc := TSubtitleFormatsName[Index]; Exts := '*.zeg'; end;
    else
    begin
      Description := '';
      Extensions  := '';
      Result      := False;
    end;
  end;

  if (Result = True) and (Desc <> '') and (Exts <> '') then
  begin
    if BufferLen1 > 0 then
      StrLCopy(Description, PChar(Desc), BufferLen1)
    else
      BufferLen1 := Length(Desc);

    if BufferLen2 > 0 then
      StrLCopy(Extensions, PChar(Exts), BufferLen2)
    else
      BufferLen2 := Length(Exts);
  end;
end;

// -----------------------------------------------------------------------------

procedure GetFormatName(Index: Integer; Name: PChar; var BufferLen: Integer); stdcall;
var
  FName : String;
begin
  FName := IndexToName(Index);

  if BufferLen > 0 then
    StrLCopy(Name, PChar(FName), BufferLen)
  else
    BufferLen := Length(FName);
end;

// -----------------------------------------------------------------------------

function GetFormatIndex(Name: PChar): Integer; stdcall;
begin
  Result := NameToIndex(Name);
end;

// -----------------------------------------------------------------------------

function IsFrameBased(FormatIndex: Integer): LongBool; stdcall;
begin
  Result := False;

  case TSubtitleFormats(FormatIndex) of
    sfAdobeEncoreDVD,
    sfAQTitle,
    sfFABSubtitler,
    sfMACDVDStudioPro,
    sfMacSUB,
    sfMicroDVD,
    sfMPlayer,
    sfPhoenixJS,
    sfSonicDVD,
    sfSonicScenarist,
    sfSoftni,
    sfSpruceSubtitleFile: Result := True;
  end;
end;

// -------------------------------------------------------------------------- //
//                               File handling                                //
// -------------------------------------------------------------------------- //

function LoadSubtitleFile(FileName: PChar; FPS: Single; FormatIndex: Integer; Append, ReCalcTimeValues: LongBool): LongBool; stdcall;
begin
  if Append = False then
    Result := LongBool(LoadSubtitle(Subtitles, FileName, FPS, TSubtitleFormats(FormatIndex)))
  else
    Result := LongBool(LoadSubtitle(Subtitles, FileName, FPS, TSubtitleFormats(FormatIndex), False, ReCalcTimeValues));
end;

// -----------------------------------------------------------------------------

procedure CreateNewSubtitle; stdcall;
begin
  CloseSubtitle(Subtitles);
  Subtitles := TSubtitles.Create;
end;

// -----------------------------------------------------------------------------

function GetFileFormat(FileName: PChar): Integer; stdcall;
var
  tmpSubs: TSubtitleFile;
begin
  tmpSubs := TSubtitleFile.Create(FileName);
  try
    Result := Integer(CheckSubtitleFormat(tmpSubs));
  finally
    tmpSubs.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SaveSubtitleFile(FileName: PChar; FormatIndex: Integer; FPS: Single; FromIndex, ToIndex: Integer): LongBool; stdcall;
begin
  Result := LongBool(SubtitlesToFile(Subtitles, FileName, FPS, TSubtitleFormats(FormatIndex), FromIndex, ToIndex));
end;

// -----------------------------------------------------------------------------

procedure CloseSubtitleFile; stdcall;
begin
  CloseSubtitle(Subtitles);
end;

// -------------------------------------------------------------------------- //
//                              Subtitle editing                              //
// -------------------------------------------------------------------------- //

function AddSubtitle(InitialTime, FinalTime: Integer; Text: PChar): Integer; stdcall;
begin
  Result := -1;

  if Assigned(Subtitles) then
    Result := Subtitles.Add(InitialTime, FinalTime, String(Text));
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(Index, InitialTime, FinalTime: Integer; Text: PChar): LongBool; stdcall;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) and ((Index >= 0) and (Index < Subtitles.Count)) then
  begin
    Subtitles.Insert(Index, InitialTime, FinalTime, String(Text));
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function MoveSubtitle(Index, NewIndex: Integer): LongBool; stdcall;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) and ((Index >= 0) and (Index < Subtitles.Count)) then
  begin
    Subtitles.Move(Index, NewIndex);
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function DeleteSubtitle(Index: Integer): LongBool; stdcall;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) and ((Index >= 0) and (Index < Subtitles.Count)) then
  begin
    Subtitles.Delete(Index);
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure ClearSubtitles; stdcall;
begin
  if Assigned(Subtitles) then
    Subtitles.Clear;
end;

// -----------------------------------------------------------------------------

function SetSubtitle(Index: Integer; InitialTime, FinalTime: Integer; Text: PChar): LongBool; stdcall;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) and ((Index >= 0) and (Index < Subtitles.Count)) then
  begin
    Subtitles.InitialTime[Index] := InitialTime;
    Subtitles.FinalTime[Index]   := FinalTime;
    Subtitles.Text[Index]        := Text;
    Result                       := True;
  end;
end;

// -----------------------------------------------------------------------------

function ReverseSubtitleText(FromIndex, ToIndex: Integer; KeepLinesOrder: LongBool): LongBool; stdcall;
var
  i, IndexFrom, IndexTo: Integer;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) then
  begin
    if ((FromIndex >= 0) and (FromIndex < Subtitles.Count)) then
      IndexFrom := FromIndex
    else
      IndexFrom := 0;

    if ((ToIndex >= 0) and (ToIndex < Subtitles.Count)) then
      IndexTo := ToIndex
    else
      IndexTo := Subtitles.Count-1;

    if IndexFrom > IndexTo then Exit;

    for i := IndexFrom To IndexTo Do
      Subtitles.Text[i] := ReverseText(Subtitles[i].Text, Boolean(KeepLinesOrder));

    Result := True;
  end;
end;

// -------------------------------------------------------------------------- //
//                             Subtitle specific                              //
// -------------------------------------------------------------------------- //

procedure GetCurrentFormat(Name: PChar; var Index: Integer; var BufferLen: Integer); stdcall;
begin
  if Assigned(Subtitles) and (Subtitles.Format > 0) then
  begin
    if BufferLen > 0 then
      StrLCopy(Name, PChar(TSubtitleFormatsName[Subtitles.Format]), BufferLen)
    else
      BufferLen := Length(TSubtitleFormatsName[Subtitles.Format]);

    Index := Subtitles.Format;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleCount: Integer; stdcall;
begin
  Result := -1;

  if Assigned(Subtitles) then
    Result := Subtitles.Count;
end;

// -----------------------------------------------------------------------------

procedure GetSubtitleText(Time: Integer; Text: PChar; var BufferLen: Integer); stdcall;
var
  FText : String;
begin
  FText := DisplaySubtitle(Subtitles, Time);

  if BufferLen > 0 then
    StrLCopy(Text, PChar(FText), BufferLen)
  else
    BufferLen := Length(FText);
end;

// -----------------------------------------------------------------------------

function GetSubtitle(Index: Integer; var InitialTime, FinalTime: Integer; Text: PChar; var BufferLen: Integer): LongBool; stdcall;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) and ((Index >= 0) and (Index < Subtitles.Count)) then
  begin
    if BufferLen > 0 then
      StrLCopy(Text, PChar(Subtitles[Index].Text), BufferLen)
    else
      BufferLen := Length(Subtitles[Index].Text);

    InitialTime := Subtitles[Index].InitialTime;
    FinalTime   := Subtitles[Index].FinalTime;
    Result      := True;
  end;
end;

// -------------------------------------------------------------------------- //
//                                   Delay                                    //
// -------------------------------------------------------------------------- //

procedure SetPlaybackDelay(Time: Integer); stdcall;
begin
  PlaybackDelay := Time;
end;

// -----------------------------------------------------------------------------

function GetPlaybackDelay: Integer; stdcall;
begin
  Result := PlaybackDelay;
end;

// -----------------------------------------------------------------------------

function SetAbsoluteDelay(Time, FromIndex, ToIndex: Integer): LongBool; stdcall;
var
  i, IndexFrom, IndexTo: Integer;
begin
  Result := False;

  if Assigned(Subtitles) and (Subtitles.Count > 0) then
  begin
    if ((FromIndex >= 0) and (FromIndex < Subtitles.Count)) then
      IndexFrom := FromIndex
    else
      IndexFrom := 0;

    if ((ToIndex >= 0) and (ToIndex < Subtitles.Count)) then
      IndexTo := ToIndex
    else
      IndexTo := Subtitles.Count-1;

    if IndexFrom > IndexTo then Exit;

    for i := IndexFrom To IndexTo Do
    begin
      Subtitles.InitialTime[i] := Subtitles[i].InitialTime + Time;
      Subtitles.FinalTime[i]   := Subtitles[i].FinalTime + Time;
    end;

    Result := True;
  end;
end;

// -------------------------------------------------------------------------- //
//                             General variables                              //
// -------------------------------------------------------------------------- //
{$IFNDEF VIPLAY}

function GetNoInteractionWithTags: LongBool; stdcall;
begin
  Result := LongBool(NoInteractionWithTags);
end;

// -----------------------------------------------------------------------------

procedure SetNoInteractionWithTags(Value: LongBool); stdcall;
begin
  NoInteractionWithTags := Boolean(Value);
end;

{$ENDIF}
// -----------------------------------------------------------------------------

function GetSubtitleWorkWithTags: LongBool; stdcall;
begin
  Result := LongBool(WorkWithTags);
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleWorkWithTags(Value: LongBool); stdcall;
begin
  WorkWithTags := Boolean(Value);
end;

// -------------------------------------------------------------------------- //
//                              Output settings                               //
// -------------------------------------------------------------------------- //

procedure SetOutputSettingsAdvancedSubStationAlpha(Assigned: LongBool; Collisions: PChar;
                                                   PlayResX, PlayResY: Integer; Timer, FontName: PChar;
                                                   FontSize, PrimaryColor, SecondaryColor, OutlineColour,
                                                   BackColour: Integer; Bold, Italic, Underline, StrikeOut: LongBool;
                                                   ScaleX, ScaleY, Spacing: Integer; Angle: Single;
                                                   BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR,
                                                   MarginV, Encoding : Integer); stdcall;
begin
  ASSAttributes.Assigned       := Boolean(Assigned);
  ASSAttributes.Collisions     := Collisions;
  ASSAttributes.PlayResX       := PlayResX;
  ASSAttributes.PlayResY       := PlayResY;
  ASSAttributes.Timer          := Timer;
  ASSAttributes.FontName       := FontName;
  ASSAttributes.FontSize       := FontSize;
  ASSAttributes.PrimaryColor   := PrimaryColor;
  ASSAttributes.SecondaryColor := SecondaryColor;
  ASSAttributes.OutlineColour  := OutlineColour;
  ASSAttributes.BackColour     := BackColour;
  ASSAttributes.Bold           := Boolean(Bold);
  ASSAttributes.Italic         := Boolean(Italic);
  ASSAttributes.Underline      := Boolean(Underline);
  ASSAttributes.StrikeOut      := Boolean(StrikeOut);
  ASSAttributes.ScaleX         := ScaleX;
  ASSAttributes.ScaleY         := ScaleY;
  ASSAttributes.Spacing        := Spacing;
  ASSAttributes.Angle          := Angle;
  ASSAttributes.BorderStyle    := BorderStyle;
  ASSAttributes.Outline        := Outline;
  ASSAttributes.Shadow         := Shadow;
  ASSAttributes.Alignment      := Alignment;
  ASSAttributes.MarginL        := MarginL;
  ASSAttributes.MarginR        := MarginR;
  ASSAttributes.MarginV        := MarginV;
  ASSAttributes.Encoding       := Encoding;
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsDVDSubtitle(Assigned: LongBool; DiskId, DVDTitle,
                                       Language, Author, Web, Info, License : PChar); stdcall;
begin
  DVDSubtitleAttributes.Assigned := Boolean(Assigned);
  DVDSubtitleAttributes.DiskId   := DiskId;
  DVDSubtitleAttributes.DVDTitle := DVDTitle;
  DVDSubtitleAttributes.Language := Language;
  DVDSubtitleAttributes.Author   := Author;
  DVDSubtitleAttributes.Web      := Web;
  DVDSubtitleAttributes.Info     := Info;
  DVDSubtitleAttributes.License  := License;
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsSAMI(Assigned: LongBool; FontName: PChar; FontSize: Integer;
                                Bold, Italic, Underline: LongBool; SubColor,
                                BackgroundColor: Integer; Align: PChar);  stdcall;
  function ColorToHTML(Color: Integer): String;
  begin
    Result := Format('#%.2x%.2x%.2x', [GetRValue(Color), GetGValue(Color), GetBValue(Color)]);
  end;
begin
  SAMIAttributes.Assigned        := Boolean(Assigned);
  SAMIAttributes.FontName        := FontName;
  SAMIAttributes.FontSize        := FontSize;
  SAMIAttributes.Bold            := Boolean(Bold);
  SAMIAttributes.Italic          := Boolean(Italic);
  SAMIAttributes.Underline       := Boolean(Underline);
  SAMIAttributes.SubColor        := ColorToHTML(SubColor);
  SAMIAttributes.BackgroundColor := ColorToHTML(BackgroundColor);
  SAMIAttributes.Align           := Align;
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsSonicScenarist(Assigned, PAL, DropFrame: LongBool;
                                          Color0, Color1, Color2, Color3, Contrast0,
                                          Contrast1, Contrast2, Contrast3: Integer); stdcall;
begin
  SonicScenaristAttributes.Assigned  := Boolean(Assigned);
  SonicScenaristAttributes.PAL       := Boolean(PAL);
  SonicScenaristAttributes.DropFrame := Boolean(DropFrame);
  SonicScenaristAttributes.Color0    := Color0;
  SonicScenaristAttributes.Color1    := Color1;
  SonicScenaristAttributes.Color2    := Color2;
  SonicScenaristAttributes.Color3    := Color3;
  SonicScenaristAttributes.Contrast0 := Contrast0;
  SonicScenaristAttributes.Contrast1 := Contrast1;
  SonicScenaristAttributes.Contrast2 := Contrast2;
  SonicScenaristAttributes.Contrast3 := Contrast3;
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsSubViewer1(Assigned: LongBool; Title, Author, Source,
                                      vProgram, Path: PChar; Delay: Integer); stdcall;
begin
  SubViewer1Attributes.Assigned := Boolean(Assigned);
  SubViewer1Attributes.Title    := Title;
  SubViewer1Attributes.Author   := Author;
  SubViewer1Attributes.Source   := Source;
  SubViewer1Attributes.Programa := vProgram;
  SubViewer1Attributes.Path     := Path;
  SubViewer1Attributes.Delay    := Delay
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsSubViewer2(Assigned: LongBool; Title, Author, Source,
                                      vProgram, Path: PChar; Delay, CDTrack: Integer;
                                      Comment, FontName: PChar; FontSize, FontColor: Integer;
                                      Bold, Italic, Underline, StrikeOut: LongBool); stdcall;
begin
  SubViewer2Attributes.Assigned  := Boolean(Assigned);
  SubViewer2Attributes.Title     := Title;
  SubViewer2Attributes.Author    := Author;
  SubViewer2Attributes.Source    := Source;
  SubViewer2Attributes.Programa  := vProgram;
  SubViewer2Attributes.Path      := Path;
  SubViewer2Attributes.Delay     := Delay;
  SubViewer2Attributes.CDTrack   := CDTrack;
  SubViewer2Attributes.Comment   := Comment;
  SubViewer2Attributes.FontName  := FontName;
  SubViewer2Attributes.FontSize  := FontSize;
  SubViewer2Attributes.FontColor := FontColor;
  SubViewer2Attributes.Bold      := Boolean(Bold);
  SubViewer2Attributes.Italic    := Boolean(Italic);
  SubViewer2Attributes.Underline := Boolean(Underline);
  SubViewer2Attributes.StrikeOut := Boolean(StrikeOut);
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsSubStationAlpha(Assigned: LongBool; Title, Script, FontName: PChar;
                                           FontSize: Integer; Bold, Italic: LongBool; BorderStyle,
                                           PrimaryColor, SecondaryColor, TertiaryColor, ShadowColor,
                                           Outline, Shadow, Alignment, MarginL, MarginR, MarginV,
                                           Encoding : Integer); stdcall;
begin
  SSAAttributes.Assigned       := Boolean(Assigned);
  SSAAttributes.Title          := Title;
  SSAAttributes.Script         := Script;
  SSAAttributes.FontName       := FontName;
  SSAAttributes.FontSize       := FontSize;
  SSAAttributes.Bold           := Boolean(Bold);
  SSAAttributes.Italic         := Boolean(Italic);
  SSAAttributes.BorderStyle    := BorderStyle;
  SSAAttributes.PrimaryColor   := PrimaryColor;
  SSAAttributes.SecondaryColor := SecondaryColor;
  SSAAttributes.TertiaryColor  := TertiaryColor;
  SSAAttributes.ShadowColor    := ShadowColor;
  SSAAttributes.Outline        := Outline;
  SSAAttributes.Shadow         := Shadow;
  SSAAttributes.Alignment      := Alignment;
  SSAAttributes.MarginL        := MarginL;
  SSAAttributes.MarginR        := MarginR;
  SSAAttributes.MarginV        := MarginV;
  SSAAttributes.Encoding       := Encoding;
end;

// -----------------------------------------------------------------------------

procedure SetOutputSettingsTMPlayer(Assigned: LongBool; TypeOfFormat: Integer); stdcall;
begin
  TMPlayerAttributes.Assigned     := Boolean(Assigned);
  TMPlayerAttributes.TypeOfFormat := TypeOfFormat;
end;

// -----------------------------------------------------------------------------

exports
  // --------------------- //
  //    Module specific    //
  // --------------------- //
  GetModuleVersion,
  GetModuleDescription,
  GetSupportedFormatsCount,
  // --------------------- //
  //    Format specific    //
  // --------------------- //
  GetFormatInformation,
  GetFormatName,
  GetFormatIndex,
  IsFrameBased,
  // --------------------- //
  //     File handling     //
  // --------------------- //
  LoadSubtitleFile,
  CreateNewSubtitle,
  GetFileFormat,
  SaveSubtitleFile,
  CloseSubtitleFile,
  // --------------------- //
  //   Subtitle editing    //
  // --------------------- //
  AddSubtitle,
  InsertSubtitle,
  MoveSubtitle,
  DeleteSubtitle,
  ClearSubtitles,
  SetSubtitle,
  ReverseSubtitleText,
  // --------------------- //
  //   Subtitle specific   //
  // --------------------- //
  GetCurrentFormat,
  GetSubtitleCount,
  GetSubtitleText,
  GetSubtitle,
  // --------------------- //
  //         Delay         //
  // --------------------- //
  SetPlaybackDelay,
  GetPlaybackDelay,
  SetAbsoluteDelay,
  // --------------------- //
  //   General variables   //
  // --------------------- //
  {$IFNDEF VIPLAY}
  GetNoInteractionWithTags,
  SetNoInteractionWithTags,
  {$ENDIF}
  GetSubtitleWorkWithTags,
  SetSubtitleWorkWithTags,
  // --------------------- //
  //    Output settings    //
  // --------------------- //
  SetOutputSettingsAdvancedSubStationAlpha,
  SetOutputSettingsDVDSubtitle,
  SetOutputSettingsSAMI,
  SetOutputSettingsSonicScenarist,
  SetOutputSettingsSubViewer1,
  SetOutputSettingsSubViewer2,
  SetOutputSettingsSubStationAlpha,
  SetOutputSettingsTMPlayer;

// -----------------------------------------------------------------------------

begin
end.
