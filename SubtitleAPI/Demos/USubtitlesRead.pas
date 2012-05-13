// USubtitlesRead - Unit for subtitle handling
// Copyright © 2001-2003 URUSoft.
//
// -----------------------------------------------------------------------------
// FORMAT                                  EXTENSION                    BASE
// -----------------------------------------------------------------------------
// Advanced SubStation Alpha               *.ass                        Time
// AQTitle                                 *.aqt                        Frames
// Captions 32                             *.txt                        Time
// Captions DAT                            *.dat                        Time
// Captions DAT Text                       *.dat                        Time
// Captions Inc                            *.txt                        Time
// Cheetah                                 *.asc                        Time
// CPC-600                                 *.txt                        Time
// DKS Subtitle Format                     *.dks                        Time
// DVD Junior                              *.txt                        Time
// DVD Subtitle System                     *.txt                        Time
// DVDSubtitle                             *.sub                        Time
// I-Author Script                         *.txt                        Time
// JACOSub 2.7+                            *.jss                        Time
// Karaoke Lyrics LRC                      *.lrc                        Time
// Karaoke Lyrics VKT                      *.vkt                        Time
// MacSUB                                  *.scr                        Frames
// MicroDVD                                *.sub; *.txt                 Frames
// MPlayer                                 *.mpl                        Frames
// MPlayer2                                *.mpl                        Time
// Philips SVCD Designer                   *.sub                        Time
// Phoenix Japanimation Society            *.pjs                        Frames
// Pinnacle Impression                     *.txt                        Time
// PowerDivX                               *.psb; *.txt                 Time
// PowerPixel                              *.txt                        Time
// QuickTime Text                          *.txt                        Time
// RealTime                                *.rt                         Time
// SAMI Captioning                         *.smi; *.sami                Time
// Sasami Script                           *.s2k                        Time
// SBT                                     *.sbt                        Time
// Sofni                                   *.sub                        Time
// Softitler RTF                           *.rtf                        Time
// SonicDVD Creator                        *.sub                        Time
// Sonic Scenarist                         *.sst                        Time
// Spruce DVDMaestro                       *.son                        Time
// Spruce Subtitle File                    *.stl                        Time & Frames (25 or 30)
// SST Script                              *.ssts                       Time
// SubCreator 1.x                          *.txt                        Time
// SubRip                                  *.srt                        Time
// SubSonic                                *.sub                        Time
// SubStation Alpha                        *.ssa                        Time
// SubViewer 1.0                           *.sub                        Time
// SubViewer 2.0                           *.sub                        Time
// TMPlayer 1                              *.txt; *.sub                 Time
// TMPlayer 2                              *.txt; *.sub                 Time
// TMPlayer +1                             *.txt; *.sub                 Time
// TMPlayer +2                             *.txt; *.sub                 Time
// TMPlayer Multiline                      *.txt; *.sub                 Time
// KoalaPlayer (TMPlayer?)                 *.txt                        Time
// Turbo Titler                            *.tts                        Time
// ViPlay Subtitle File                    *.vsf                        Time
// ZeroG                                   *.zeg                        Time
// -----------------------------------------------------------------------------
// TOTAL: 52
// -----------------------------------------------------------------------------

unit USubtitlesRead;

//------------------------------------------------------------------------------

interface

uses
  USubtitleFile, USubtitlesFunctions, SysUtils, FastStrings {$IFDEF VIPLAY}, Forms{$ENDIF};

//------------------------------------------------------------------------------

type
  TSubtitleFormats = (sfInvalid,
                      sfAdvancedSubStationAlpha,
                      sfAQTitle,
                      sfCaptions32,
                      sfCaptionsDat,
                      sfCaptionsDatText,
                      sfCaptionsInc,
                      sfCheetah,
                      sfCPC600,
                      sfDKS,
                      sfDVDJunior,
                      sfDVDSubtitleSystem,
                      sfDVDSubtitle,
                      sfIAuthor,
                      sfJACOSub,
                      sfKaraokeLyricsLRC,
                      sfKaraokeLyricsVKT,
                      sfMacSUB,
                      sfMicroDVD,
                      sfMPlayer,
                      sfMPlayer2,
                      sfPhilipsSVCD,
                      sfPhoenixJS,
                      sfPinnacleImpression,
                      sfPowerDivX,
                      sfPowerPixel,
                      sfQuickTimeText,
                      sfRealTime,
                      sfSAMI,
                      sfSasamiScript,
                      sfSBT,
                      sfSofni,
                      sfSoftitlerRTF,
                      sfSonicDVD,
                      sfSonicScenarist,
                      sfSpruceDVDMaestro,
                      sfSpruceSubtitleFile,
                      sfSSTScript,
                      sfSubCreator,
                      sfSubRip,
                      sfSubSonic,
                      sfSubStationAlpha,
                      sfSubViewer1,
                      sfSubViewer2,
                      sfTMPlayer,
                      sfTurboTitler,
                      sfViPlay,
                      sfZeroG);

const
  TSubtitleFormatsName : array[1..47] of String =
  ('Advanced SubStation Alpha',
   'AQTitle',
   'Captions 32',
   'Captions DAT',
   'Captions DAT Text',
   'Captions Inc.',
   'Cheetah',
   'CPC-600',
   'DKS Subtitle Format',
   'DVD Junior',
   'DVD Subtitle System',
   'DVDSubtitle',
   'I-Author Script',
   'JACOSub 2.7+',
   'Karaoke Lyrics LRC',
   'Karaoke Lyrics VKT',
   'MacSUB',
   'MicroDVD',
   'MPlayer',
   'MPlayer2',
   'Philips SVCD Designer 1.5-2.0',
   'Phoenix Japanimation Society',
   'Pinnacle Impression',
   'PowerDivX',
   'PowerPixel',
   'QuickTime Text',
   'RealTime',
   'SAMI Captioning',
   'Sasami Script',
   'SBT',
   'Sofni',
   'Softitler RTF',
   'SonicDVD Creator',
   'Sonic Scenarist',
   'Spruce DVDMaestro',
   'Spruce Subtitle File',
   'Stream SubText Script',
   'SubCreator 1.x',
   'SubRip',
   'SubSonic',
   'SubStation Alpha',
   'SubViewer 1.0',
   'SubViewer 2.0',
   'TMPlayer',
   'Turbo Titler',
   'ViPlay Subtitle File',
   'ZeroG');

//------------------------------------------------------------------------------

function LoadSubtitle(var Subtitles: TSubtitles; const FileName: String; FPS: Single; SubtitleFormat: TSubtitleFormats = sfInvalid; Clear: Boolean = True; ReCalcTimeValues: Boolean = False): Boolean;
procedure CloseSubtitle(var Subtitles: TSubtitles);
function DisplaySubtitle(var Subtitles: TSubtitles; const Time: Integer): String;

function IndexToName(const FormatIndex: ShortInt): String;
function NameToIndex(const FormatName: String): ShortInt;
function MakeOneLine(const Source: TSubtitleFile; AddSpaces: Boolean = False): String;

//------------------------------------------------------------------------------

function FileToSubtitles                         (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; SubtitleFormat: TSubtitleFormats; ReCalcTimeValues: Boolean): Boolean;
function FileToSubtitles_ADVANCEDSUBSTATIONALPHA (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_AQTITLE                 (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
function FileToSubtitles_CAPTIONS32              (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_CAPTIONSDAT             (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_CAPTIONSDATTEXT         (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_CAPTIONSINC             (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_CHEETAH                 (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_CPC600                  (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_DKS                     (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_DVDJUNIOR               (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_DVDSUBTITLE             (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_DVDSUBTITLESYSTEM       (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_IAUTHOR                 (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_JACOSUB                 (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_KARAOKELYRICSLRC        (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_KARAOKELYRICSVKT        (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_MACSUB                  (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
function FileToSubtitles_MICRODVD                (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
function FileToSubtitles_MPLAYER                 (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
function FileToSubtitles_MPLAYER2                (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_PHILIPSSVCD             (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_PHOENIXJAPANIMATION     (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
function FileToSubtitles_PINNACLEIMPRESSION      (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_POWERDIVX               (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_POWERPIXEL              (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_QUICKTIMETEXT           (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_REALTIME                (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SAMI                    (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SASAMISCRIPT            (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SBT                     (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SOFNI                   (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SOFTITLERRTF            (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SONICDVD                (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SONICSCENARIST          (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SPRUCEDVDMAESTRO        (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SPRUCESUBTITLEFILE      (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
function FileToSubtitles_SSTSCRIPT               (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SUBCREATOR1X            (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SUBRIP                  (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SUBSONIC                (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SUBSTATIONALPHA         (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SUBVIEWER1              (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_SUBVIEWER2              (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_TMPLAYER                (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_TURBOTITLER             (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_VIPLAY                  (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
function FileToSubtitles_ZEROG                   (var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;

//------------------------------------------------------------------------------

var
  WorkWithTags  : Boolean = False;
  PlaybackDelay : Integer = 0;

//------------------------------------------------------------------------------

implementation

uses UCheckFormat;

//------------------------------------------------------------------------------

function LoadSubtitle(var Subtitles: TSubtitles; const FileName: String; FPS: Single; SubtitleFormat: TSubtitleFormats = sfInvalid; Clear: Boolean = True; ReCalcTimeValues: Boolean = False): Boolean;
var
  tmpSubFile: TSubtitleFile;
begin
  Result := False;

  if Clear = True then CloseSubtitle(Subtitles);
  if Assigned(Subtitles) = False then Subtitles := TSubtitles.Create;
  Subtitles.Format := Integer(SubtitleFormat);

  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.LoadFromFile(FileName);
    if Subtitles.Format <= Integer(sfInvalid) then
      Subtitles.Format := Integer(CheckSubtitleFormat(tmpSubFile));

    Result := FileToSubtitles(Subtitles, tmpSubFile, FPS, TSubtitleFormats(Subtitles.Format), ReCalcTimeValues);
  finally
    tmpSubFile.Free;
    if Result = False then
      CloseSubtitle(Subtitles);
  end;
end;

//------------------------------------------------------------------------------

procedure CloseSubtitle(var Subtitles: TSubtitles);
begin
  if Assigned(Subtitles) then
  begin
    Subtitles.Free;
    Subtitles := NIL;
  end;
end;

// -----------------------------------------------------------------------------

function DisplaySubtitle(var Subtitles: TSubtitles; const Time: Integer): String;
var
  i: Integer;
begin
  Result := '';

  if not Assigned(Subtitles) then Exit;

  for i := 0 to Subtitles.Count-1 do
  begin
    {$IFDEF VIPLAY}
    if (i mod 10) = 0 then Application.ProcessMessages;
    {$ENDIF}

    if (Time >= Subtitles[i].InitialTime+PlaybackDelay) and (Time <= Subtitles[i].FinalTime+PlaybackDelay) then
    begin
      Result := Subtitles[i].Text;
      Break;
    end
    else if Time < Subtitles[i].FinalTime + PlaybackDelay then
      Break;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; SubtitleFormat: TSubtitleFormats; ReCalcTimeValues: Boolean): Boolean;
var
  ExtraTime: Integer;
begin
  Result := False;

  if (ReCalcTimeValues = True) and Assigned(Subtitles) and (Subtitles.Count > 0) then
    ExtraTime := Subtitles[Subtitles.Count-1].FinalTime
  else
    ExtraTime := 0;

  case SubtitleFormat of
    sfAdvancedSubStationAlpha : Result := FileToSubtitles_ADVANCEDSUBSTATIONALPHA(Subtitles, tmpSubFile, ExtraTime);
    sfAQTitle                 : Result := FileToSubtitles_AQTITLE                (Subtitles, tmpSubFile, FPS, ExtraTime);
    sfCaptions32              : Result := FileToSubtitles_CAPTIONS32             (Subtitles, tmpSubFile, ExtraTime);
    sfCaptionsDat             : Result := FileToSubtitles_CAPTIONSDAT            (Subtitles, tmpSubFile, ExtraTime);
    sfCaptionsDatText         : Result := FileToSubtitles_CAPTIONSDATTEXT        (Subtitles, tmpSubFile, ExtraTime);
    sfCaptionsInc             : Result := FileToSubtitles_CAPTIONSINC            (Subtitles, tmpSubFile, ExtraTime);
    sfCheetah                 : Result := FileToSubtitles_CHEETAH                (Subtitles, tmpSubFile, ExtraTime);
    sfCPC600                  : Result := FileToSubtitles_CPC600                 (Subtitles, tmpSubFile, ExtraTime);
    sfDKS                     : Result := FileToSubtitles_DKS                    (Subtitles, tmpSubFile, ExtraTime);
    sfDVDJunior               : Result := FileToSubtitles_DVDJUNIOR              (Subtitles, tmpSubFile, ExtraTime);
    sfDVDSubtitleSystem       : Result := FileToSubtitles_DVDSUBTITLESYSTEM      (Subtitles, tmpSubFile, ExtraTime);
    sfDVDSubtitle             : Result := FileToSubtitles_DVDSUBTITLE            (Subtitles, tmpSubFile, ExtraTime);
    sfIAuthor                 : Result := FileToSubtitles_IAUTHOR                (Subtitles, tmpSubFile, ExtraTime);
    sfJACOSub                 : Result := FileToSubtitles_JACOSUB                (Subtitles, tmpSubFile, ExtraTime);
    sfKaraokeLyricsLRC        : Result := FileToSubtitles_KARAOKELYRICSLRC       (Subtitles, tmpSubFile, ExtraTime);
    sfKaraokeLyricsVKT        : Result := FileToSubtitles_KARAOKELYRICSVKT       (Subtitles, tmpSubFile, ExtraTime);
    sfMacSUB                  : Result := FileToSubtitles_MACSUB                 (Subtitles, tmpSubFile, FPS, ExtraTime);
    sfMicroDVD                : Result := FileToSubtitles_MICRODVD               (Subtitles, tmpSubFile, FPS, ExtraTime);
    sfMPlayer                 : Result := FileToSubtitles_MPLAYER                (Subtitles, tmpSubFile, FPS, ExtraTime);
    sfMPlayer2                : Result := FileToSubtitles_MPLAYER2               (Subtitles, tmpSubFile, ExtraTime);
    sfPhilipsSVCD             : Result := FileToSubtitles_PHILIPSSVCD            (Subtitles, tmpSubFile, ExtraTime);
    sfPhoenixJS               : Result := FileToSubtitles_PHOENIXJAPANIMATION    (Subtitles, tmpSubFile, FPS, ExtraTime);
    sfPinnacleImpression      : Result := FileToSubtitles_PINNACLEIMPRESSION     (Subtitles, tmpSubFile, ExtraTime);
    sfPowerDivX               : Result := FileToSubtitles_POWERDIVX              (Subtitles, tmpSubFile, ExtraTime);
    sfPowerPixel              : Result := FileToSubtitles_POWERPIXEL             (Subtitles, tmpSubFile, ExtraTime);
    sfQuickTimeText           : Result := FileToSubtitles_QUICKTIMETEXT          (Subtitles, tmpSubFile, ExtraTime);
    sfRealTime                : Result := FileToSubtitles_REALTIME               (Subtitles, tmpSubFile, ExtraTime);
    sfSAMI                    : Result := FileToSubtitles_SAMI                   (Subtitles, tmpSubFile, ExtraTime);
    sfSasamiScript            : Result := FileToSubtitles_SASAMISCRIPT           (Subtitles, tmpSubFile, ExtraTime);
    sfSBT                     : Result := FileToSubtitles_SBT                    (Subtitles, tmpSubFile, ExtraTime);
    sfSofni                   : Result := FileToSubtitles_SOFNI                  (Subtitles, tmpSubFile, ExtraTime);
    sfSoftitlerRTF            : Result := FileToSubtitles_SOFTITLERRTF           (Subtitles, tmpSubFile, ExtraTime);
    sfSonicDVD                : Result := FileToSubtitles_SONICDVD               (Subtitles, tmpSubFile, ExtraTime);
    sfSonicScenarist          : Result := FileToSubtitles_SONICSCENARIST         (Subtitles, tmpSubFile, ExtraTime);
    sfSpruceDVDMaestro        : Result := FileToSubtitles_SPRUCEDVDMAESTRO       (Subtitles, tmpSubFile, ExtraTime);
    sfSpruceSubtitleFile      : Result := FileToSubtitles_SPRUCESUBTITLEFILE     (Subtitles, tmpSubFile, FPS, ExtraTime);
    sfSSTScript               : Result := FileToSubtitles_SSTSCRIPT              (Subtitles, tmpSubFile, ExtraTime);
    sfSubCreator              : Result := FileToSubtitles_SUBCREATOR1X           (Subtitles, tmpSubFile, ExtraTime);
    sfSubRip                  : Result := FileToSubtitles_SUBRIP                 (Subtitles, tmpSubFile, ExtraTime);
    sfSubSonic                : Result := FileToSubtitles_SUBSONIC               (Subtitles, tmpSubFile, ExtraTime);
    sfSubStationAlpha         : Result := FileToSubtitles_SUBSTATIONALPHA        (Subtitles, tmpSubFile, ExtraTime);
    sfSubViewer1              : Result := FileToSubtitles_SUBVIEWER1             (Subtitles, tmpSubFile, ExtraTime);
    sfSubViewer2              : Result := FileToSubtitles_SUBVIEWER2             (Subtitles, tmpSubFile, ExtraTime);
    sfTMPlayer                : Result := FileToSubtitles_TMPLAYER               (Subtitles, tmpSubFile, ExtraTime);
    sfTurboTitler             : Result := FileToSubtitles_TURBOTITLER            (Subtitles, tmpSubFile, ExtraTime);
    sfViPlay                  : Result := FileToSubtitles_VIPLAY                 (Subtitles, tmpSubFile, ExtraTime);
    sfZeroG                   : Result := FileToSubtitles_ZEROG                  (Subtitles, tmpSubFile, ExtraTime);
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_ADVANCEDSUBSTATIONALPHA(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, a        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      a := Pos(',', tmpSubFile[i]);
      InitialTime := StringToTime(Trim(Copy(tmpSubFile[i], a + 1, SmartPos(',', tmpSubFile[i], True, a + 1) - (a + 1))));
      a := SmartPos(',', tmpSubFile[i], True, a + 1);
      FinalTime   := StringToTime(Trim(Copy(tmpSubFile[i], a + 1, SmartPos(',', tmpSubFile[i], True, a + 1) - (a + 1))));

      if (InitialTime > -1) and (FinalTime > -1) then
      begin
        Text := tmpSubFile[i];

        for a := 1 to 9 do
          Delete(Text, 1, Pos(',', Text));

        Text := ReplaceString(Trim(Text), '\N', #13#10);
        Text := ReplaceString(Text, '{\i0}', '');
        Text := ReplaceString(Text, '{\b0}', '');

        if WorkWithTags = False then
        begin
          Text := ReplaceString(Text, '{\i1}', '');
          Text := ReplaceString(Text, '{\b1}', '');
        end
        else
        begin
          if SmartPos('{\b1}', Text, False) <> 0 then
          begin
            Text := ReplaceString(Text, '{\b1}', '');
            Text := '<b>' + Text;
          end;
          if SmartPos('{\i1}', Text, False) <> 0 then
          begin
            Text := ReplaceString(Text, '{\i1}', '');
            Text := '<i>' + Text;
          end;
        end;

        while (Pos('{', Text) > 0) and (Pos('}',Text) > (Pos('{', Text))) do
          Delete(Text, Pos('{', Text), Pos('}', Text));

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_AQTITLE(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (Pos('-->> ', tmpSubFile[i]) = 1) and
         (IsInteger(Copy(tmpSubFile[i], 6, Length(tmpSubFile[i])))) and
         (Length(tmpSubFile[i]) = 11) then
      begin
        c    := 1;
        Text := '';
        InitialTime := FramesToTime(StrToIntDef(Copy(tmpSubFile[i], 6, 6), -1), FPS);
        while (i+c < tmpSubFile.Count) and (Copy(tmpSubFile[i+c], 1, 5) <> '-->> ') do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        If (IsInteger(Copy(tmpSubFile[i+c], 6, 2))) then
          FinalTime := FramesToTime(StrToIntDef(Copy(tmpSubFile[i+c], 6, 6), -1), FPS)
        else
          FinalTime := InitialTime + 2000;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_CAPTIONS32(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 15, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
      begin
        Text := Trim(Copy(tmpSubFile[i], 29, 33));
        if Length(tmpSubFile[i]) > 62 then
          Text := Text + #13#10 + Trim(Copy(tmpSubFile[i], 63, Length(tmpSubFile[i])-62));

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_CAPTIONSDAT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, a, c     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  BigStr      : String;
begin
  Result := False;
  try
    BigStr := MakeOneLine(tmpSubFile);

    // Add #13#10 as needed
    for i := Length(BigStr)-1 downto 0 do
    begin
      Text := Copy(BigStr, i, 4);
      if (Text = 'Bð é') or (Copy(Text, 1, 2) = '#S') then
        Insert(#13#10, BigStr, i);
    end;

    tmpSubFile.Text := BigStr;
    BigStr          := '';

    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (Pos('#S', tmpSubFile[i]) = 1) and (IsInteger(Copy(tmpSubFile[i], Pos('#S', tmpSubFile[i]) + 2, 14))) then
      begin
        Text := '';
        c    := 1;
        while (i+c < tmpSubFile.Count) and (Pos('Bð é', tmpSubFile[i+c]) = 1) do
        begin
          a := SmartPos('     ', tmpSubFile[i+c], False);

          if Text <> '' then
            Text := Text + #13#10 + Copy(tmpSubFile[i+c], a+5, Length(tmpSubFile[i+c])-(a+4))
          else
            Text := Copy(tmpSubFile[i+c], a+5, Length(tmpSubFile[i+c])-(a+4));

          Inc(c);
        end;
        // The format of the time is:
        // #S00023010032100
        //   hmmsszzz+++++?

        InitialTime := StrToInt(Copy(tmpSubFile[i], 3, 8));
        FinalTime   := InitialTime + StrToInt(Copy(tmpSubFile[i], 11, 5));

        Subtitles.Add(InitialTime, FinalTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_CAPTIONSDATTEXT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, a, c     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  BigStr      : String;
begin
  Result := False;
  try
    BigStr := MakeOneLine(tmpSubFile);

    // Add #13#10 as needed
    for i := Length(BigStr)-1 downto 0 do
    begin
      Text := Copy(BigStr, i, 4);
      if (Text = 'BG @') or (Copy(Text, 1, 2) = '#T') then
        Insert(#13#10, BigStr, i);
    end;

    tmpSubFile.Text := BigStr;
    BigStr          := '';

    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (Pos('#T', tmpSubFile[i]) = 1) and (IsInteger(Copy(tmpSubFile[i], Pos('#T', tmpSubFile[i]) + 2, 14))) then
      begin
        Text := '';
        c    := 1;
        while (i+c < tmpSubFile.Count) and (Pos('BG @', tmpSubFile[i+c]) = 1) do
        begin
          a := SmartPos(' ', tmpSubFile[i+c], True, SmartPos(' ', tmpSubFile[i+c], True, Pos(' ', tmpSubFile[i+c]) + 1) + 1) + 1;

          if Text <> '' then
            Text := Text + #13#10 + Copy(tmpSubFile[i+c], a, Length(tmpSubFile[i+c])-(a-1))
          else
            Text := Copy(tmpSubFile[i+c], a, Length(tmpSubFile[i+c])-(a-1));

          Inc(c);
        end;
        // The format of the time is:
        // #T00023010032100
        //   hmmsszzz+++++?

        InitialTime := StrToInt(Copy(tmpSubFile[i], 3, 8));
        FinalTime   := InitialTime + StrToInt(Copy(tmpSubFile[i], 11, 5));

        Subtitles.Add(InitialTime, FinalTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_CAPTIONSINC(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-2 do
    begin
      if (TimeInFormat(Copy(tmpSubFile[i], 1, 11), 'hh:mm:ss:zz') and
         (TimeInFormat(Copy(tmpSubFile[i], 13, 11), 'hh:mm:ss:zz') and
         (Pos('{0 [1 ', tmpSubFile[i+1]) = 1))) then
        Subtitles.Add(StringToTime(Copy(tmpSubFile[i], 1, 11)) + ExtraTime, StringToTime(Copy(tmpSubFile[i], 13, 11)) + ExtraTime, Copy(tmpSubFile[i+1], 7, Length(tmpSubFile[i+1])));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_CHEETAH(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c : Integer;
  Text : String;
begin
  Result := False;
  try
    for i := tmpSubFile.Count-1 downto 0 do
      if (Pos('*', tmpSubFile[i]) = 1) and (SmartPos('*t ', tmpSubFile[i], False) <> 1) then
        tmpSubFile.Delete(i);

    for i := 0 to tmpSubFile.Count-2 do
    begin
      If (TimeInFormat(Copy(tmpSubFile[i], 4, 11), 'hh:mm:ss:zz')) then
      begin
        c    := 1;
        Text := '';
        while (i+c < tmpSubFile.Count) and (Pos('*', tmpSubFile[i+c]) <> 1) do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        Subtitles.Add(StringToTime(Copy(tmpSubFile[i], 4, 11)) + ExtraTime, StringToTime(Copy(tmpSubFile[i+c], 4, 11)) + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_CPC600(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-2 do
    begin
      if (TimeInFormat(Copy(tmpSubFile[i], 1, 11), 'hh:mm:ss:zz')) and
         (TimeInFormat(Copy(tmpSubFile[i+1], 1, 11), 'hh:mm:ss:zz')) and
         (SmartPos('³0NEN³',tmpSubFile[i], False) = 12) and
         (SmartPos('³0NEN³',tmpSubFile[i+1], False) = 12) then
        Subtitles.Add(StringToTime(Copy(tmpSubFile[i], 1, 11)) + ExtraTime, StringToTime(Copy(tmpSubFile[i+1], 1, 11)) + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 18, Length(tmpSubFile[i])), '\', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_DKS(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result    := False;
  FinalTime := -1;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      If (Pos('[', tmpSubFile[i]) = 1) and
         (Pos(']', tmpSubFile[i]) = 10) and
         (TimeInFormat(Copy(tmpSubFile[i], 2, 8), 'hh:mm:ss')) then
      begin
        InitialTime := StringToTime(Copy(tmpSubFile[i], 2, 8));
        if (i <= tmpSubFile.Count-1) then
          FinalTime := StringToTime(Copy(tmpSubFile[i+1], 2, 8));
        if FinalTime = -1 then FinalTime := InitialTime + 2000;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 11, Length(tmpSubFile[i])), '[br]', #13#10));
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_DVDJUNIOR(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 25, Length(tmpSubFile[i])), '<', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_DVDSUBTITLESYSTEM(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 25, Length(tmpSubFile[i])), '//', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_DVDSUBTITLE(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (SmartPos('{T ', tmpSubFile[i], False) = 1) and
         (TimeInFormat(Copy(tmpSubFile[i], 4, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(tmpSubFile[i], 4, 11));
        c    := 1;
        Text := '';
        while (i+c < tmpSubFile.Count) and (tmpSubFile[i+c] <> '}') do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        c := 1;
        while (i+c < tmpSubFile.Count) and (SmartPos('{T ', tmpSubFile[i+c], False) = 0) do
          Inc(c);
        FinalTime := StringToTime(Copy(tmpSubFile[i+c], 4, 11));
        if FinalTime = -1 then FinalTime := InitialTime + 2000;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_IAUTHOR(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c, u     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  DecimalSep  : Char;
  Text        : String;
begin
  Result := False;

  c := 0;
  u := 0;
  DecimalSep := DecimalSeparator;
  DecimalSeparator := '.';

  try
    for i := tmpSubFile.Count-1 downto 0 do
    begin
      if (SmartPos('BMPFILE:', tmpSubFile[i], False) = 0) and
         ((SmartPos('TIME:', tmpSubFile[i], False) = 1) and ((SmartPos('DISABLE_OGT', tmpSubFile[i], False) = 0))) or
         (Pos('*', tmpSubFile[i]) = 1) then
        tmpSubFile.Delete(i);
    end;

    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (SmartPos('BMPFILE:', tmpSubFile[i], False) = 1) then
      begin
        Text := Trim(Copy(tmpSubFile[i], 9, Length(tmpSubFile[i])));
        if (SmartPos('STARTTIME:', tmpSubFile[i+1], False) = 1) then
        begin
          InitialTime := Round((StrToFloat(Trim(Copy(tmpSubFile[i+1], 11, Length(tmpSubFile[i+1]))))+(256*c))*1000);

          if InitialTime > u then
            u := InitialTime
          else
          begin
            u := InitialTime;
            Inc(c);
          end;
          if (SmartPos('TIME:', tmpSubFile[i+2], False) = 1) then
            FinalTime := Round((StrToFloat(Trim(Copy(tmpSubFile[i+2], 6, Pos(' ', tmpSubFile[i+2]))))+(256*c))*1000)
          else
            FinalTime := InitialTime + 2000;
          Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
        end;
      end;
    end;
  finally
    DecimalSeparator := DecimalSep;
    if Subtitles.Count > 0 then Result := True;
  end;   
end;

//------------------------------------------------------------------------------

function FileToSubtitles_JACOSUB(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 10));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 12, 10));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Trim(Copy(tmpSubFile[i], Pos('}', tmpSubFile[i]) + 2, Length(tmpSubFile[i]))), '\n', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_KARAOKELYRICSLRC(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 2, Pos(']', tmpSubFile[i]) - 2), True);
      if i+1 <= (tmpSubFile.Count-1) then
        FinalTime := StringToTime(Copy(tmpSubFile[i+1], 2, Pos(']', tmpSubFile[i+1]) - 2), True)
      else
        FinalTime := InitialTime + 2000;

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Copy(tmpSubFile[i], Pos(']', tmpSubFile[i]) + 1, Length(tmpSubFile[i])));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_KARAOKELYRICSVKT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
      if Copy(tmpSubFile[i], 1, 1) = '{' then
      begin
        InitialTime := StrToInt(Copy(tmpSubFile[i], 2, Pos(' ', tmpSubFile[i]) - 2))*10;
        if (i+1 <= (tmpSubFile.Count-1)) and (Copy(tmpSubFile[i+1], 1, 1) = '{') then
          FinalTime := StrToInt(Copy(tmpSubFile[i+1], 2, Pos(' ', tmpSubFile[i+1]) - 2))*10
        else
          FinalTime := InitialTime + 2000;

        if (InitialTime > -1) and (FinalTime > -1) then
          Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Copy(tmpSubFile[i], Pos(' ', tmpSubFile[i]) + 1, LastDelimiter('}', tmpSubFile[i]) - (Pos(' ', tmpSubFile[i]) + 1)));
      end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_MACSUB(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      If (Pos('/', tmpSubFile[i]) = 1) and
         (IsInteger(Copy(tmpSubFile[i], 2, Length(tmpSubFile[i])))) then
      begin
        InitialTime := FramesToTime(StrToInt(Copy(tmpSubFile[i], 2, Length(tmpSubFile[i]))), FPS);
        c    := 1;
        Text := '';
        while (i+c < tmpSubFile.Count) and (Pos('/', tmpSubFile[i+c]) <> 1) do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        If (IsInteger(Copy(tmpSubFile[i+c], 2, Length(tmpSubFile[i+c])))) then
          FinalTime := FramesToTime(StrToInt(Copy(tmpSubFile[i+c], 2, Length(tmpSubFile[i+c]))), FPS)
        else
          FinalTime := InitialTime + 2000;
    
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_MICRODVD(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  DecimalSep  : Char;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      // DivXG400 FPS Info tag
      if Copy(tmpSubFile[i], 0, 6) = '{1}{1}' then
      begin
        DecimalSep       := DecimalSeparator;
        DecimalSeparator := '.';
        FPS := StrToFloat(Copy(tmpSubFile[i], 7, Length(tmpSubFile[i])));
        DecimalSeparator := DecimalSep;
      end
      else if (Pos('{', tmpSubFile[i]) = 1) and
              (Pos('}', tmpSubFile[i]) > 1) and
              (StringCount('{',tmpSubFile[i], True) >= 2) and
              (StringCount('}',tmpSubFile[i], True) >= 2) and
              (IsInteger(Copy(tmpSubFile[i], 2, Pos('}', tmpSubFile[i]) - 2))) then
      begin
        InitialTime := FramesToTime(StrToInt(Copy(tmpSubFile[i], 2, Pos('}', tmpSubFile[i]) - 2)), FPS);
        Text := Copy(tmpSubFile[i], SmartPos('{', tmpSubFile[i], True, 2) + 1, SmartPos('}', tmpSubFile[i], True, Pos('}', tmpSubFile[i]) + 1) - (SmartPos('{', tmpSubFile[i], True, 2) + 1));
        if IsInteger(Text) then
          FinalTime := FramesToTime(StrToInt(Text), FPS) else
          FinalTime := InitialTime + 2000;
        Text := ReplaceString(Copy(tmpSubFile[i], SmartPos('}', tmpSubFile[i], True, Pos('}', tmpSubFile[i]) + 1) + 1, Length(tmpSubFile[i])), '|', #13#10);

        if WorkWithTags = True then
        begin
          // ESTILOS
          if (SmartPos('{y:', Text, False) <> 0) and (Pos('}',Text) <> 0) then
          begin
            tmpSubFile[i] := Copy(Text, SmartPos('{y:', Text, False) + 3, SmartPos('}', Text, True, SmartPos('{y:', Text, False) + 3) - SmartPos('{y:', Text, False) - 3);

            if SmartPos('u', tmpSubFile[i], False) > 0 then
              Text := '<u>' + Text;
            if SmartPos('b', tmpSubFile[i], False) > 0 then
              Text := '<b>' + Text;
            if SmartPos('i', tmpSubFile[i], False) > 0 then
              Text := '<i>' + Text;
          end;
          // COLORES
          if (SmartPos('{c:$', Text, False) > 0) and (SmartPos('}', Text, True, SmartPos('{c:$', Text, False)) = (SmartPos('{c:$', Text, False) + 10)) then
            Text := '<c:#' + InvertHTMLColor(Copy(Text, SmartPos('{c:$', Text, False) + 4, 6)) + '>' + Text;
        end;

        // BORRA COSAS DE FORMATTING QUE QUEDARON
        while (Pos('{', Text) <> 0) and (Pos('}', Text) <> 0) do
          Delete(Text, Pos('{', Text), Pos('}', Text) - Pos('{', Text) + 1);

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_MPLAYER(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (IsInteger(Copy(tmpSubFile[i], 1, Pos(',',tmpSubFile[i]) - 1))) and
         (IsInteger(Copy(tmpSubFile[i], Pos(',',tmpSubFile[i]) + 1, SmartPos(',', tmpSubFile[i], True, Pos(',',tmpSubFile[i]) + 1) - (Pos(',',tmpSubFile[i]) + 1)))) then
      begin
        InitialTime   := FramesToTime(StrToInt(Copy(tmpSubFile[i], 1, Pos(',',tmpSubFile[i]) - 1)), FPS);
        FinalTime     := FramesToTime(StrToInt(Copy(tmpSubFile[i], Pos(',',tmpSubFile[i]) + 1, SmartPos(',', tmpSubFile[i], True, Pos(',',tmpSubFile[i]) + 1) - (Pos(',',tmpSubFile[i]) + 1))), FPS);
        Text          := ReplaceString(Copy(tmpSubFile[i], SmartPos(',', tmpSubFile[i], True, SmartPos(',', tmpSubFile[i], True, SmartPos(',', tmpSubFile[i], True, Pos(',',tmpSubFile[i]) + 1)) + 1) + 1, Length(tmpSubFile[i])), '|', #13#10);
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_MPLAYER2(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (Pos('[', tmpSubFile[i]) = 1) and
         (Pos(']', tmpSubFile[i]) > 1) and
         (StringCount('[',tmpSubFile[i], True) >= 2) and
         (StringCount(']',tmpSubFile[i], True) >= 2) and
         (IsInteger(Copy(tmpSubFile[i], 2, Pos(']', tmpSubFile[i]) - 2))) then
      begin
        InitialTime := StrToInt(Copy(tmpSubFile[i], 2, Pos(']', tmpSubFile[i]) - 2));
        Text := Copy(tmpSubFile[i], SmartPos('[', tmpSubFile[i], True, 2) + 1, SmartPos(']', tmpSubFile[i], True, Pos(']', tmpSubFile[i]) + 1) - (SmartPos('[', tmpSubFile[i], True, 2) + 1));
        if IsInteger(Text) then
          FinalTime := StrToInt(Text)
        else
          FinalTime := InitialTime + 2000;

        Text := ReplaceString(Copy(tmpSubFile[i], SmartPos(']', tmpSubFile[i], True, Pos(']', tmpSubFile[i]) + 1) + 1, Length(tmpSubFile[i])), '|', #13#10);

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_PHILIPSSVCD(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i         : Integer;
  StartPos  : Integer;
  AfterList : Boolean;
  Text      : String;
begin
  Result := False;
  AfterList := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if AfterList then
      begin
        if StringCount(':', tmpSubFile[i]) >= 6 then
        begin
          StartPos := 1;
          while TimeInFormat(Copy(tmpSubFile[i], StartPos, 11), 'hh:mm:ss:zz') = False do
            Inc(StartPos);
          Text := Text + Copy(tmpSubFile[i], 1, StartPos - 2);
          Subtitles.Add(StringToTime(Copy(tmpSubFile[i], StartPos, 11)) + ExtraTime, StringToTime(Copy(tmpSubFile[i], SmartPos(' ', tmpSubFile[i], True, StartPos) + 1, 11)) + ExtraTime, Text);
          Text := '';
        end
        else
        begin
          if (i < tmpSubFile.Count-1) then
            Text := Text + tmpSubFile[i] + #13#10;
        end;
      end
      else
      if SmartPos('[list]', tmpSubFile[i], False) = 1 then
        AfterList := True;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_PHOENIXJAPANIMATION(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      If (IsInteger(Copy(tmpSubFile[i], 1, Pos(',',tmpSubFile[i]) - 1))) and
         (Pos('"', tmpSubFile[i]) > 0) and
         (StringCount(',', Copy(tmpSubFile[i], 1, Pos('"', tmpSubFile[i]) - 1)) = 2) and
         (StringCount('"', tmpSubFile[i]) >= 2) then
      begin
        InitialTime := FramesToTime(StrToInt(Copy(tmpSubFile[i], 0, Pos(',', tmpSubFile[i])-1)), FPS);
        FinalTime   := FramesToTime(StrToInt(Trim(Copy(tmpSubFile[i], Pos(',', tmpSubFile[i]) + 1, SmartPos(',', tmpSubFile[i], True, Pos(',', tmpSubFile[i]) + 1) - (Pos(',', tmpSubFile[i]) + 1)))), FPS);
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], Pos('"', tmpSubFile[i]) + 1, Length(tmpSubFile[i]) - (Pos('"', tmpSubFile[i]) + 1)), '|', #13#10));
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_PINNACLEIMPRESSION(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Copy(tmpSubFile[i], 25, Length(tmpSubFile[i])));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_POWERDIVX(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 2, Pos('}', tmpSubFile[i]) -2));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], SmartPos('{', tmpSubFile[i], True, 2) + 1, SmartPos('}', tmpSubFile[i], True, Pos('}', tmpSubFile[i]) + 1) - (SmartPos('{', tmpSubFile[i], True, 2) + 1)));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], SmartPos('}', tmpSubFile[i], True, Pos('}', tmpSubFile[i]) + 1) + 1, Length(tmpSubFile[i])), '|', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_POWERPIXEL(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      c    := 1;
      Text := '';
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
      begin
        while (i+c < tmpSubFile.Count) and
              (StringToTime(Copy(tmpSubFile[i+c], 1, 11)) = -1) and
              (StringToTime(Copy(tmpSubFile[i+c], 13, 11)) = -1) do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_QUICKTIMETEXT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 2, 11));
      if (i < tmpSubFile.Count-2) and
         (InitialTime > -1) then
      begin
        FinalTime := StringToTime(Copy(tmpSubFile[i+2], 2, 11));
        if FinalTime = -1 then FinalTime := InitialTime + 2000;
        if (InitialTime > -1) and (FinalTime > -1) and (Pos('[', tmpSubFile[i+1]) <> 1) and (Pos(']', tmpSubFile[i+1]) <> 13) then
          Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, tmpSubFile[i+1])
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_REALTIME(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i,a         : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  BigStr      : String;
  tmpCad      : String;
  Text        : String;
  Bold, Italic, Underline: Boolean;
begin
  Result := False;

  try
    // Put all in one line
    BigStr := MakeOneLine(tmpSubFile);

    // Add #13#10 as needed
    for i := Length(BigStr)-3 downto 2 do
    begin
      if SmartPos('<time', BigStr, False, i) = i then//(AnsiLowerCase(Copy(BigStr, i, 5)) = '<time') then
        Insert(#13#10, BigStr, i);
    end;

    tmpSubFile.Text := BigStr;
    BigStr          := '';

    for i := 0 to tmpSubFile.Count-1 do
    begin
      Text := '';
      tmpCad := Copy(tmpSubFile[i], SmartPos('begin="', tmpSubFile[i], False) + 7, SmartPos('"', tmpSubFile[i], True, SmartPos('begin="', tmpSubFile[i], False) + 7) - (SmartPos('begin="', tmpSubFile[i], False) + 7));
      if Length(tmpCad) = 8 then tmpCad := '00:' + tmpCad;
      InitialTime := StringToTime(tmpCad);
      tmpCad := Copy(tmpSubFile[i], SmartPos('end="', tmpSubFile[i], False) + 5, SmartPos('"', tmpSubFile[i], True, SmartPos('end="', tmpSubFile[i], False) + 5) - (SmartPos('end="', tmpSubFile[i], False) + 5));
      if Length(tmpCad) = 8 then tmpCad := '00:' + tmpCad;
      FinalTime := StringToTime(tmpCad);

      if (InitialTime > -1) and
         (FinalTime > -1) and
         (SmartPos('<time', tmpSubFile[i], False) > 0) and
         (SmartPos('begin="', tmpSubFile[i], False) > 0) and
         (SmartPos('end="', tmpSubFile[i], False) > 0) then
      begin
        Text := ReplaceString(Trim(Copy(tmpSubFile[i], SmartPos('<clear/>', tmpSubFile[i], False) + 8, Length(tmpSubFile[i]))), '<br>', #13#10);

        // Store tags
        Bold      := SmartPos('<b>', Text, False) > 0;
        Italic    := SmartPos('<i>', Text, False) > 0;
        Underline := SmartPos('<u>', Text, False) > 0;
        if SmartPos('<font color="', Text, False) > 0 then
        begin
          tmpCad := Copy(Text, SmartPos('<font color="', Text, False) + 13, 7);
          if Pos('#', tmpCad) <> 1 then tmpCad := '#' + tmpCad;
          tmpCad := '<c:' + tmpCad + '>';
        end
        else
          tmpCad := '';

        // Replace all opening and closing tags...
        while (Pos('<', Text) <> 0) and (Pos('>', Text) <> 0) do
        begin
          for a := Pos('<', Text) to Length(Text) do
            if Text[a] = '>' then break;
          Delete(Text, Pos('<', Text), a + 1 - Pos('<', Text));
        end;

        if WorkWithTags then
        begin
          if Underline then
            Text := '<u>' + Text;
          if Bold then
            Text := '<b>' + Text;
          if Italic then
            Text := '<i>' + Text;
          Text := tmpCad + Text;
        end;

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SAMI(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, a         : Integer;
  InitialTime  : Integer;
  FinalTime    : Integer;
  tmpCad, Text : String;
  BigStr       : String;
  Bold, Italic, Underline: Boolean;
begin
  Result := False;

  try
    // Put all in one line
    BigStr := MakeOneLine(tmpSubFile);

    // Delete comments
    while (Pos('<!--', BigStr) > 0) and (Pos('-->', BigStr) > 0) do
      BigStr := Copy(BigStr, 0, Pos('<!--', BigStr)-1) + Copy(BigStr, Pos('-->',BigStr) + 3, Length(BigStr));

    // Add #13#10 as needed
    for i := Length(BigStr)-3 downto 2 do
    begin
      if (BigStr[i] = '>') then
      begin
        for a := i downto 2 do
          if BigStr[a]  = '<' then break; // search for last "<" before the actual ">"
        if (AnsiLowerCase(Copy(BigStr, a, 2)) = '<p')     or
           (AnsiLowerCase(Copy(BigStr, i-3, 3)) = '</p>') then
        begin
          if (AnsiLowerCase(Copy(BigStr, a, 2)) = '<p') then
            Delete(BigStr, a, i - a + 1)
          else
            Insert(#13#10, BigStr, i + 1);
        end;
        if (AnsiLowerCase(Copy(BigStr, a, 12)) = '<sync start=') then
        begin
          Insert(#13#10, BigStr, a);
          Insert(#13#10, BigStr, i + 3);
        end;
      end;
    end;

    tmpSubFile.Text := BigStr;
    BigStr          := '';

    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (i <= tmpSubFile.Count-2) and
        (SmartPos('<sync start=', tmpSubFile[i], False) = 1) and
        (Pos('>', tmpSubFile[i]) > 13) then
      begin
        // ------------------ //
        //    Initial time    //
        // ------------------ //
        tmpCad := Trim(Copy(tmpSubFile[i], Pos('=', tmpSubFile[i]) + 1, Pos('>', tmpSubFile[i]) - 13));

        if Pos('"',tmpCad) > 0 then
          tmpCad := Trim(Copy(tmpCad, Pos('"',tmpCad) + 1, SmartPos('"', tmpCad, True, Pos('"', tmpCad) + 1) - (Pos('"',tmpCad) + 1)));
        if IsInteger(tmpCad) then
          InitialTime := StrToInt(tmpCad)
        else
          InitialTime := 0;

        // ------------------ //
        //     Final time     //
        // ------------------ //
        if (i <= tmpSubFile.Count-3) and (SmartPos('<sync start=', tmpSubFile[i+2], False) > 0) then
        begin
          tmpCad := Trim(Copy(tmpSubFile[i+2], Pos('=', tmpSubFile[i+2]) + 1, Pos('>', tmpSubFile[i+2]) - 13));
          if Pos('"',tmpCad) > 0 then
            tmpCad := Trim(Copy(tmpCad, Pos('"',tmpCad) + 1, SmartPos('"', tmpCad, True, Pos('"', tmpCad) + 1) - (Pos('"',tmpCad) + 1)));
          if IsInteger(tmpCad) then
            FinalTime := StrToInt(tmpCad)
          else
            FinalTime := InitialTime + 2000;
        end
        else
          FinalTime := InitialTime + 2000;

        // ------------------ //
        //        Text        //
        // ------------------ //
        tmpCad := tmpSubFile[i+1];

        // Replace the enters, not just <br> because they may be some variants like <br />...
        while (SmartPos('<br', tmpCad, False) > 0) and (Pos('>',tmpCad) > 0) do
        begin
          for a := SmartPos('<br', tmpCad, False) to Length(tmpCad) do  // search for next ">" after "<br" ...
            if tmpCad[a] = '>' then break;
          tmpCad := ReplaceString(tmpCad, Copy(tmpCad, SmartPos('<br', tmpCad, False), a - SmartPos('<br', tmpCad, False) + 1), #13#10);
        end;

        tmpCad := ReplaceString(tmpCad, '&nbsp;', ' ');
        tmpCad := ReplaceString(tmpCad, '&quote;', '"');
        tmpCad := ReplaceString(tmpCad, '&quot;', '"');

        Text := Trim(tmpCad);

        // Store tags
        Bold      := SmartPos('<b>', Text, False) > 0;
        Italic    := SmartPos('<i>', Text, False) > 0;
        Underline := SmartPos('<u>', Text, False) > 0;

        if SmartPos('<font color="', Text, False) > 0 then
        begin
          tmpCad := Copy(Text, SmartPos('<font color="', Text, False) + 13, 7);
          if Pos('#', tmpCad) <> 1 then tmpCad := '#' + tmpCad;
          tmpCad := '<c:' + tmpCad + '>';
        end
        else
          tmpCad := '';

        // Replace all opening and closing tags...
        while (Pos('<', Text) <> 0) and (Pos('>', Text) <> 0) do
        begin
          for a := Pos('<', Text) to Length(Text) do
            if Text[a] = '>' then break;
          Delete(Text, Pos('<', Text), a + 1 - Pos('<', Text));
        end;

        if WorkWithTags then
        begin
          if Underline then
            Text := '<u>' + Text;
          if Bold then
            Text := '<b>' + Text;
          if Italic then
            Text := '<i>' + Text;
          Text := tmpCad + Text;
        end;

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;

  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SASAMISCRIPT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i,c         : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    // Delete garbage lines...
    for i := tmpSubFile.Count-1 downto 0 do
      if ((Pos(';', tmpSubFile[i]) = 1) and (AnsiLowerCase(Copy(tmpSubFile[i], 1, 16)) <> ';set.time.start=')) or
         (Copy(tmpSubFile[i], 0, 2) = '//') then
        tmpSubFile.Delete(i);
             
    for i := 0 to tmpSubFile.Count-2 do
    begin
      if (Pos(';', tmpSubFile[i]) = 1) then
      begin
        InitialTime := StrToInt(Copy(tmpSubFile[i], 17, Length(tmpSubFile[i])));
        Text := '';
        c    := 1;
        while (i+c < tmpSubFile.Count) and (tmpSubFile[i+c][1] <> ';') do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        If Pos(';', tmpSubFile[i+c]) = 1 then
          FinalTime := StrToInt(Copy(tmpSubFile[i+c], 17, Length(tmpSubFile[i])))
        else
          FinalTime := InitialTime + 2000;

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;

  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SBT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i,c         : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-2 do
    begin
      InitialTime := StringToTime(tmpSubFile[i]);
      if (InitialTime > -1) then
      begin
        FinalTime := StringToTime(tmpSubFile[i+1]);
        if FinalTime = -1 then FinalTime := InitialTime + 2000;
        Text := '';
        c    := 2;
        while (i+c < tmpSubFile.Count) and (StringToTime(tmpSubFile[i+c]) = -1) do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SOFNI(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 11));
      if (InitialTime = -1) and
         (FinalTime = -1) then
      begin
        if Text = '' then
          Text := tmpSubFile[i]
        else
          Text := Text + #13#10 + tmpSubFile[i];
      end
      else if (InitialTime > -1) and (FinalTime > -1) and
              (Pos('\', tmpSubFile[i]) = 12) then
      begin
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
        Text := '';
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SOFTITLERRTF(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-2 do
    begin
      if (Copy(tmpSubFile[i], 1, 5) = '\par ') and
         (Copy(tmpSubFile[i+1], 1, 5) = '\par ') then
      begin
        InitialTime := StringToTime(Copy(tmpSubFile[i], 6, 11));
        FinalTime   := StringToTime(Copy(tmpSubFile[i+1], 6, 11));

        if (InitialTime > -1) and
           (FinalTime > -1) then
        begin
          Text := Copy(tmpSubFile[i+2], 6, Length(tmpSubFile[i+2]));
          c := 3;
          while (i+c < tmpSubFile.Count) and
                ((Copy(tmpSubFile[i+c], 6, 1) <> '[') and
                (Copy(tmpSubFile[i+c], Length(tmpSubFile[i+c]), 1) <> ']')) do
          begin
            Text := Text + #13#10 + Copy(tmpSubFile[i+c], 6, Length(tmpSubFile[i+c]));
            Inc(c);
          end;

          Text := ReplaceString(Text, '\''e1', 'á', True, False);
          Text := ReplaceString(Text, '\''e9', 'é', True, False);
          Text := ReplaceString(Text, '\''ed', 'í', True, False);
          Text := ReplaceString(Text, '\''f3', 'ó', True, False);
          Text := ReplaceString(Text, '\''fa', 'ú', True, False);
          Text := ReplaceString(Text, '\''f1', 'ñ', True, False);
          Text := ReplaceString(Text, '\''c1', 'Á', True, False);
          Text := ReplaceString(Text, '\''c9', 'É', True, False);
          Text := ReplaceString(Text, '\''cd', 'Í', True, False);
          Text := ReplaceString(Text, '\''d3', 'Ó', True, False);
          Text := ReplaceString(Text, '\''da', 'Ú', True, False);
          Text := ReplaceString(Text, '\''d1', 'Ñ', True, False);
          Text := ReplaceString(Text, '\{', '{', True, False);
          Text := ReplaceString(Text, '\}', '{', True, False);
          Text := ReplaceString(Text, '\''a1', '¡', True, False);
          Text := ReplaceString(Text, '\''bf', '¿', True, False);
          Text := ReplaceString(Text, '\''94', '"', True, False);
          Text := ReplaceString(Text, '\''fb', 'û', True, False);
          Text := ReplaceString(Text, '\''e4', 'ä', True, False);
          Text := ReplaceString(Text, '\''ea', 'ê', True, False);     
          Text := ReplaceString(Text, '\''e7', 'ç', True, False);
          Text := ReplaceString(Text, '\''e0', 'à', True, False);    
          Text := ReplaceString(Text, '\''e8', 'è', True, False);
          Text := ReplaceString(Text, '\''f9', 'ù', True, False);
          Text := ReplaceString(Text, '\''a3', '£', True, False);
          Text := ReplaceString(Text, '\''f5', 'õ', True, False);
          Text := ReplaceString(Text, '\''fc', 'ü', True, False);
          Text := ReplaceString(Text, '\''f6', 'ö', True, False);
          Text := ReplaceString(Text, '\''d7', '×', True, False);
          Text := ReplaceString(Text, '\''e5', 'å', True, False);
          Text := ReplaceString(Text, '\''f2', 'ò', True, False);
          Text := ReplaceString(Text, '\''ee', 'î', True, False);

          if Copy(Text, Length(Text)-3, 4) = #13#10'}}' then
            Text := Copy(Text, 1, Length(Text)-6);

          Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
          Text := '';
        end;
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SONICDVD(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      c    := 1;
      Text := '';
      InitialTime := StringToTime(Copy(tmpSubFile[i], 7, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 20, 11));
      if (InitialTime > -1) and
         (FinalTime > -1) then
      begin
        Text := Copy(tmpSubFile[i], 33, Length(tmpSubFile[i]));
        while (i+c < tmpSubFile.Count) and
              (StringToTime(Copy(tmpSubFile[i+c], 7, 11)) = -1) and
              (StringToTime(Copy(tmpSubFile[i+c], 20, 11)) = -1) do
        begin
          Text := Text + #13#10 + tmpSubFile[i+c];
          Inc(c);
        end;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SONICSCENARIST(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      c           := Pos(#9#9, tmpSubFile[i]);
      InitialTime := StringToTime(Copy(tmpSubFile[i], c + 2, SmartPos(#9, tmpSubFile[i], True, c + 2) - (c+2)));
      c           := SmartPos(#9, tmpSubFile[i], True, c + 2) + 1;
      FinalTime   := StringToTime(Copy(tmpSubFile[i], c, SmartPos(#9, tmpSubFile[i], True, c) - c));
      Text        := Copy(tmpSubFile[i], SmartPos(#9, tmpSubFile[i], True, c) + 1, Length(tmpSubFile[i]));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SPRUCEDVDMAESTRO(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 10, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 25, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Copy(tmpSubFile[i], 39, Length(tmpSubFile[i])));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SPRUCESUBTITLEFILE(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; FPS: Single; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 8));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 8));
      if IsInteger(Copy(tmpSubFile[i], 10, 2)) then
        InitialTime := InitialTime + FramesToTime(StrToInt(Copy(tmpSubFile[i], 10, 2)), FPS);
      if IsInteger(Copy(tmpSubFile[i], 22, 2)) then
        FinalTime := FinalTime + FramesToTime(StrToInt(Copy(tmpSubFile[i], 22, 2)), FPS);
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 25, Length(tmpSubFile[i])), '|', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SSTSCRIPT(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i,c         : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (StringCount(#9, tmpSubFile[i]) = 5) then
      begin
        c := SmartPos(#9, tmpSubFile[i], True, Pos(#9, tmpSubFile[i]) + 1) + 1;
        InitialTime := StringToTime(Copy(tmpSubFile[i], c, SmartPos(#9, tmpSubFile[i], True, c) - c));
        c := SmartPos(#9, tmpSubFile[i], True, c + 1) + 1;
        FinalTime := StringToTime(Copy(tmpSubFile[i], c, SmartPos(#9, tmpSubFile[i], True, c) - c));
        c := SmartPos(#9, tmpSubFile[i], True, c + 1) + 1;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], c, SmartPos(#9, tmpSubFile[i], True, c) - c), '~', #13#10));
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SUBCREATOR1X(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 10));
      if i+1 <= (tmpSubFile.Count-1) then
        FinalTime := StringToTime(Copy(tmpSubFile[i+1], 1, 10))
      else
        FinalTime := InitialTime + 2000;

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 12, Length(tmpSubFile[i])), '|', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SUBRIP(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 12));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 18, 12));
      if (InitialTime > -1) and (FinalTime > -1) then
      begin
        c    := 1;
        Text := '';
        while (i+c < tmpSubFile.Count) and
              ((IsInteger(tmpSubFile[i+c]) = False) and
              (Pos(' --> ', tmpSubFile[i+c]) = 0)) do
        begin
          If Text <> '' then
            Text := Text + #13#10 + tmpSubFile[i+c]
          else
            Text := tmpSubFile[i+c];
          Inc(c);
        end;

        Text := ReplaceString(Text,'</i>','');
        Text := ReplaceString(Text,'</b>','');
        Text := ReplaceString(Text,'</u>','');

        if WorkWithTags = False then
        begin
          Text := ReplaceString(Text,'<i>','');
          Text := ReplaceString(Text,'<b>','');
          Text := ReplaceString(Text,'<u>','');
        end
        else
        begin
          if Pos('<u>',Text) <> 0 then
          begin
            Text := ReplaceString(Text,'<u>','');
            Text := '<u>' + Text;
          end;
          if Pos('<b>',Text) <> 0 then
          begin
            Text := ReplaceString(Text,'<b>','');
            Text := '<b>' + Text;
          end;
          if Pos('<i>',Text) <> 0 then
          begin
            Text := ReplaceString(Text,'<i>','');
            Text := '<i>' + Text;
          end;
        end;

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SUBSONIC(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, c, u     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  DecimalSep  : Char;
begin
  // *** COMO FUNCIONA ESTA #$&@! DE SUBSONIC E I-AUTHOR:
  // POR LO QUE DESCUBRI CON LA CALCULADORA DE WINDOWS SERIA ASI...
  // Resultado := (Tiempo+(256*Cantidad))*1000;
  Result := False;
  c := 0;
  u := 0;
  DecimalSep := DecimalSeparator;
  DecimalSeparator := '.';

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if Pos(' \ ~:\', tmpSubFile[i]) > 0 then
      begin
        InitialTime := Round((StrToFloat(Trim(Copy(tmpSubFile[i], 3, Pos('\ ~:\', tmpSubFile[i]) - 3)))+(256*c))*1000);
        if InitialTime > u then
          u := InitialTime
        else
        begin
          u := InitialTime;
          Inc(c);
        end;
        if Pos(' \ ~:\', tmpSubFile[i+1]) = 0 Then
          FinalTime := Round((StrToFloat(Trim(Copy(tmpSubFile[i+1], 3, Length(tmpSubFile[i+1]))))+(256*c))*1000)
        else
          FinalTime := InitialTime + 2000;
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Copy(tmpSubFile[i], Pos(' \ ~:\', tmpSubFile[i]) + 6, Length(tmpSubFile[i])));
      end;
    end;
  finally
    DecimalSeparator := DecimalSep;
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SUBSTATIONALPHA(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i, a        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  //BigStr      : String;
begin
  Result := False;

  try
    // Put all in one line
    {BigStr := MakeOneLine(tmpSubFile, True);

    // Add #13#10 as needed
    for i := Length(BigStr)-1 downto 0 do
    begin
      if (AnsiLowerCase(Copy(BigStr, i, 9)) = 'dialogue:') or
         (AnsiLowerCase(Copy(BigStr, i, 8)) = 'comment:') then
        Insert(#13#10, BigStr, i);
    end;

    tmpSubFile.Text := BigStr;
    BigStr          := '';    }

    for i := 0 to tmpSubFile.Count-1 do           
    begin
      a := Pos(',', tmpSubFile[i]);
      InitialTime := StringToTime(Trim(Copy(tmpSubFile[i], a + 1, SmartPos(',', tmpSubFile[i], True, a + 1) - (a + 1))));
      a := SmartPos(',', tmpSubFile[i], True, a + 1);
      FinalTime   := StringToTime(Trim(Copy(tmpSubFile[i], a + 1, SmartPos(',', tmpSubFile[i], True, a + 1) - (a + 1))));

      if (InitialTime > -1) and (FinalTime > -1) then
      begin
        // Always 9 commas before text
        for a := 1 to 9 do
          tmpSubFile[i] := Copy(tmpSubFile[i], Pos(',',tmpSubFile[i]) + 1, Length(tmpSubFile[i]));

        Text := ReplaceString(Trim(tmpSubFile[i]), '\N', #13#10);
        Text := ReplaceString(Text,'{\i0}','');
        Text := ReplaceString(Text,'{\b0}','');

        if WorkWithTags = False then
        begin
          Text := ReplaceString(Text,'{\i1}','');
          Text := ReplaceString(Text,'{\b1}','');
        end
        else
        begin
          if SmartPos('{\b1}', Text, False) <> 0 then
          begin
            Text := ReplaceString(Text,'{\b1}','');
            Text := '<b>' + Text;
          end;
          if SmartPos('{\i1}', Text, False) <> 0 then
          begin
            Text := ReplaceString(Text,'{\i1}','');
            Text := '<i>' + Text;
          end;
        end;

        // Removemos todas las cosas entre { }
        while (Pos('{', Text) > 0) and (Pos('}',Text) > (Pos('{', Text))) do
          Delete(Text, Pos('{', Text), Pos('}', Text));

        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SUBVIEWER1(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      if (Pos('[', tmpSubFile[i]) = 1) and (Pos(']',tmpSubFile[i]) = 10) then
        InitialTime := StringToTime(Copy(tmpSubFile[i], 2, 8))
      else
        InitialTime := StringToTime(tmpSubFile[i]);
      if (InitialTime > -1) then
      begin
        if (Pos('[', tmpSubFile[i+2]) = 1) and (Pos(']',tmpSubFile[i+2]) = 10) then
          FinalTime := StringToTime(Copy(tmpSubFile[i+2], 2, 8))
        else
          FinalTime := StringToTime(tmpSubFile[i+2]);
        if FinalTime = -1 then FinalTime := InitialTime + 2000;
        if (StringToTime(Copy(tmpSubFile[i+1], 2, 8)) = -1) and
           (SmartPos('[end]', tmpSubFile[i+1], False) <> 1) then
          Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(tmpSubFile[i+1], '|', #13#10));
      end;
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_SUBVIEWER2(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 11));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 13, 11));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(tmpSubFile[i+1], '[br]', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_TMPLAYER(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;

  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := 0;
      FinalTime   := 0;
      Text        := '';

      // TMPlayer MultiLine format
      If ((StringCount(':', tmpSubFile[i]) >= 2) and
         (Pos(',', tmpSubFile[i]) = 9) and
         (Pos('=', tmpSubFile[i]) = 11) and
         (TimeInFormat(Copy(tmpSubFile[i], 1, 8), 'hh:mm:ss'))) then
         begin
           InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 8));

           If (i+2 < tmpSubFile.Count) and (TimeInFormat(Copy(tmpSubFile[i+2], 1, 8), 'hh:mm:ss')) and (Copy(tmpSubFile[i+2],9,2) = ',1') then
             FinalTime := StringToTime(Copy(tmpSubFile[i+2], 1, 8))
           else
             FinalTime := InitialTime + 2000;

           if Copy(tmpSubFile[i], 9, 2) = ',1' then
             Text := Copy(tmpSubFile[i], 12, Length(tmpSubFile[i]));

           If (i+1 <= tmpSubFile.Count-1) and
              (Copy(tmpSubFile[i+1], 1, 8) = Copy(tmpSubFile[i],0,8)) and
              (Copy(tmpSubFile[i+1], 9, 2) = ',2') and
              (Copy(tmpSubFile[i+1], 12, Length(tmpSubFile[i+1])) <> '') then
              Text := Text + #13#10 + Copy(tmpSubFile[i+1], 12, Length(tmpSubFile[i+1]));

        end
        else

      // TMPlayer Time Structure 1
      if ((StringCount(':', tmpSubFile[i]) >= 3) and
         (Pos(':', tmpSubFile[i]) = 3) and
         (TimeInFormat(Copy(tmpSubFile[i], 1, 8), 'hh:mm:ss'))) then
         begin
           InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 8));

           If (i+1 <= tmpSubFile.Count-1) and (TimeInFormat(Copy(tmpSubFile[i+1], 1, 8), 'hh:mm:ss')) then
             FinalTime := StringToTime(Copy(tmpSubFile[i+1], 1, 8))
           else
             FinalTime := InitialTime + 2000;

           Text := ReplaceString(Copy(tmpSubFile[i], 10, Length(tmpSubFile[i])), '|', #13#10);
         end
         else

      // TMPlayer Time Structure 2
      if ((StringCount(':', tmpSubFile[i]) >= 3) and
         (Pos(':', tmpSubFile[i]) = 2) and
         (TimeInFormat(Copy(tmpSubFile[i], 1, 7), 'h:mm:ss'))) then
         begin
           InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 7));

           If (i+1 <= tmpSubFile.Count-1) and (TimeInFormat(Copy(tmpSubFile[i+1], 1, 7), 'h:mm:ss')) then
             FinalTime := StringToTime(Copy(tmpSubFile[i+1], 1, 7))
           else
             FinalTime := InitialTime + 2000;

           Text := ReplaceString(Copy(tmpSubFile[i], 9, Length(tmpSubFile[i])), '|', #13#10);
         end
         else

      // TMPlayer+ Time Structure 1
      if ((StringCount(':', tmpSubFile[i]) >= 2) and
         (Pos(':', tmpSubFile[i]) = 3) and
         (Pos('=',tmpSubFile[i]) = 9) and
         (TimeInFormat(Copy(tmpSubFile[i], 1, 8), 'hh:mm:ss'))) then
         begin
           InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 8));

           If (i+1 <= tmpSubFile.Count-1) and (TimeInFormat(Copy(tmpSubFile[i+1], 1, 8), 'hh:mm:ss')) then
             FinalTime := StringToTime(Copy(tmpSubFile[i+1], 1, 8))
           else
             FinalTime := InitialTime + 2000;

           Text := ReplaceString(Copy(tmpSubFile[i], 10, Length(tmpSubFile[i])), '|', #13#10);
         end
         else
         
      // TMPlayer+ Time Structure 2
      if ((StringCount(':', tmpSubFile[i]) >= 2) and
         (Pos(':', tmpSubFile[i]) = 2) and
         (TimeInFormat(Copy(tmpSubFile[i], 1, 7), 'h:mm:ss'))) then
         begin
           InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 7));

           If (i+1 <= tmpSubFile.Count-1) and (TimeInFormat(Copy(tmpSubFile[i+1], 1, 7), 'h:mm:ss')) then
             FinalTime := StringToTime(Copy(tmpSubFile[i+1], 1, 7))
           else
             FinalTime := InitialTime + 2000;

           Text := ReplaceString(Copy(tmpSubFile[i], 9, Length(tmpSubFile[i])), '|', #13#10);
         end;
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, Text);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_TURBOTITLER(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 10));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 12, 10));
      if (InitialTime > -1) and (FinalTime > -1) and (Pos(' ', tmpSubFile[i]) = 26) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 27, Length(tmpSubFile[i])), '|', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_VIPLAY(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 1, 12));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 14, 12));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], 27, Length(tmpSubFile[i])), '|', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function FileToSubtitles_ZEROG(var Subtitles: TSubtitles; tmpSubFile: TSubtitleFile; ExtraTime: Integer): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
begin
  Result := False;
  try
    for i := 0 to tmpSubFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(tmpSubFile[i], 5, 10));
      FinalTime   := StringToTime(Copy(tmpSubFile[i], 16, 10));
      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime + ExtraTime, FinalTime + ExtraTime, ReplaceString(Copy(tmpSubFile[i], SmartPos('NTP ', tmpSubFile[i], False) + 4, Length(tmpSubFile[i])), '\n', #13#10));
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

//------------------------------------------------------------------------------

function IndexToName(const FormatIndex: ShortInt): String;
begin
  Result := '';
  if (FormatIndex >= Low(TSubtitleFormatsName)) and (FormatIndex <= High(TSubtitleFormatsName)) Then
    Result := TSubtitleFormatsName[FormatIndex];
end;

//------------------------------------------------------------------------------

function NameToIndex(const FormatName: String): ShortInt;
var
  i: ShortInt;
begin
  Result := 0;
  for i := Low(TSubtitleFormatsName) to High(TSubtitleFormatsName) do
    if AnsiLowerCase(FormatName) = AnsiLowerCase(TSubtitleFormatsName[i]) Then
      Result := i;
end;

//------------------------------------------------------------------------------

function MakeOneLine(const Source: TSubtitleFile; AddSpaces: Boolean = False): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Source.Count-1 do
    if AddSpaces = False then
      Result := Result + Source[i]
    else
      Result := Result + ' ' + Source[i];
end;

//------------------------------------------------------------------------------

end.
