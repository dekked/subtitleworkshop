// USubtitlesSave - Unit to save subtitles
// Copyright © 2001-2003 URUSoft.

unit USubtitlesSave;

// -----------------------------------------------------------------------------

interface

uses
  USubtitlesRead, USubtitleFile, USubtitlesFunctions, FastStrings, SysUtils;

function RemoveSWTags                            (Text: String; Bold, Italic, Underline: Boolean; Color: Boolean = True): String;
function SubtitlesToFile                         (Subtitles: TSubtitles; const FileName: String; const FPS: Single; const SubtitleFormat: TSubtitleFormats; From: Integer = -1; UpTo: Integer = -1): Boolean;
function SubtitlesToFile_ADOBEENCOREDVD          (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_ADVANCEDSUBSTATIONALPHA (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_AQTITLE                 (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_CAPTIONS32              (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_CAPTIONSDAT             (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_CAPTIONSDATTEXT         (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_CAPTIONSINC             (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_CHEETAH                 (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_CPC600                  (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_DKS                     (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_DVDJUNIOR               (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_DVDSUBTITLESYSTEM       (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_DVDSUBTITLE             (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_FABSUBTITLER            (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_IAUTHOR                 (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_INSCRIBERCG             (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_JACOSUB                 (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_KARAOKELYRICSLRC        (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_KARAOKELYRICSVKT        (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_MACDVDSTUDIOPRO         (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_MACSUB                  (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_MICRODVD                (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_MPLAYER                 (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_MPLAYER2                (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_MPSUB                   (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_OVRSCRIPT               (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_PANIMATOR               (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_PHILIPSSVCD             (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_PHOENIXJAPANIMATION     (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_PINNACLEIMPRESSION      (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_POWERDIVX               (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_POWERPIXEL              (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_QUICKTIMETEXT           (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_REALTIME                (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SAMI                    (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SASAMISCRIPT            (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SBT                     (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SOFTNI                  (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_SOFTITLERRTF            (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SONICDVD                (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_SONICSCENARIST          (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_SPRUCEDVDMAESTRO        (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_SPRUCESUBTITLEFILE      (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_SSTPLAYER               (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SSTSCRIPT               (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SUBCREATOR1X            (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SUBRIP                  (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SUBSONIC                (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SUBSTATIONALPHA         (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SUBVIEWER1              (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_SUBVIEWER2              (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_TMPLAYER                (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_TURBOTITLER             (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_ULEADDVDWORKSHOP2       (Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
function SubtitlesToFile_VIPLAY                  (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;
function SubtitlesToFile_ZEROG                   (Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1)                    : Boolean;

type
  TASSAttributes = record
    Assigned       : Boolean;
    Collisions     : String;
    PlayResX       : Integer;
    PlayResY       : Integer;
    Timer          : String;
    FontName       : String;
    FontSize       : Integer;
    PrimaryColor   : Integer;
    SecondaryColor : Integer;
    OutlineColour  : Integer;
    BackColour     : Integer;
    Bold           : Boolean;
    Italic         : Boolean;
    Underline      : Boolean;
    StrikeOut      : Boolean;
    ScaleX         : Integer;
    ScaleY         : Integer;
    Spacing        : Integer;
    Angle          : Single;
    BorderStyle    : Integer;
    Outline        : Integer; // From 0 to 4
    Shadow         : Integer; // "         "
    Alignment      : Integer;
    // Margins
    MarginL        : Integer;
    MarginR        : Integer;
    MarginV        : Integer;
    Encoding       : Integer;
  end;

  TDVDSubtitleAttributes = record
    Assigned : Boolean;
    DiskId   : String;
    DVDTitle : String;
    Language : String;
    Author   : String;
    Web      : String;
    Info     : String;
    License  : String;
  end;

  TSAMIAttributes = record
    Assigned        : Boolean;
    FontName        : String;
    FontSize        : Integer;
    Bold            : Boolean;
    Italic          : Boolean;
    Underline       : Boolean;
    SubColor        : String;
    BackgroundColor : String;
    Align           : String;
  end;

  TSonicScenaristAttributes = record
    Assigned  : Boolean;
    PAL       : Boolean;
    DropFrame : Boolean;
    Color0    : Integer;
    Color1    : Integer;
    Color2    : Integer;
    Color3    : Integer;
    Contrast0 : Integer;
    Contrast1 : Integer;
    Contrast2 : Integer;
    Contrast3 : Integer;
  end;

  TSubViewer1Attributes = record
    Assigned : Boolean;
    Title    : String;
    Author   : String;
    Source   : String;
    Programa : String;
    Path     : String;
    Delay    : Integer;
  end;

  TSubViewer2Attributes = record
    Assigned  : Boolean;
    Title     : String;
    Author    : String;
    Source    : String;
    Programa  : String;
    Path      : String;
    Delay     : Integer;
    CDTrack   : Integer;
    Comment   : String;
    FontName  : String;
    FontSize  : Integer;
    Bold      : Boolean;
    Italic    : Boolean;
    Underline : Boolean;
    StrikeOut : Boolean;
    FontColor : Integer;
  end;

  TSSAAttributes = record
    Assigned       : Boolean;
    Title          : String;
    Script         : String;
    FontName       : String;
    FontSize       : Integer;
    Bold           : Boolean;
    Italic         : Boolean;
    BorderStyle    : Integer;
    PrimaryColor   : Integer;
    SecondaryColor : Integer;
    TertiaryColor  : Integer;
    ShadowColor    : Integer;
    Outline        : Integer; // From 0 to 4
    Shadow         : Integer; // "         "
    Alignment      : Integer;
    // Margins
    MarginL        : Integer;
    MarginR        : Integer;
    MarginV        : Integer;
    Encoding       : Integer;
  end;

  TTMplayerAttributes = record
    Assigned     : Boolean;
    TypeOfFormat : ShortInt;
  end;

var
  ASSAttributes            : TASSAttributes;
  DVDSubtitleAttributes    : TDVDSubtitleAttributes;
  SAMIAttributes           : TSAMIAttributes;
  SonicScenaristAttributes : TSonicScenaristAttributes;
  SubViewer1Attributes     : TSubViewer1Attributes;
  SubViewer2Attributes     : TSubViewer2Attributes;
  SSAAttributes            : TSSAAttributes;
  TMPlayerAttributes       : TTMPlayerAttributes;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function RemoveSWTags(Text: String; Bold, Italic, Underline: Boolean; Color: Boolean = True): String;
begin
  if Bold      = True then Text := ReplaceString(Text, '<b>', '');
  if Italic    = True then Text := ReplaceString(Text, '<i>', '');
  if Underline = True then Text := ReplaceString(Text, '<u>', '');
  if Color = True then
  begin
    while SmartPos('<c:#', Text, False) > 0 Do
      Delete(Text, SmartPos('<c:#', Text, False), Pos('>', Copy(Text, SmartPos('<c:#', Text, False), Length(Text))));
  end;
  Result := Text;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile(Subtitles: TSubtitles; const FileName: String; const FPS: Single; const SubtitleFormat: TSubtitleFormats; From: Integer = -1; UpTo: Integer = -1): Boolean;
begin
  Result := False;
  if not Assigned(Subtitles) then exit;

  if From = -1 then From := 0;
  if UpTo = -1 then UpTo := Subtitles.Count-1;

  if FileExists(FileName) and FileIsReadOnly(FileName) then
    Exit;

  if Subtitles.Count > 0 then
  begin
    case SubtitleFormat of
      sfAdobeEncoreDVD          : Result := SubtitlesToFile_ADOBEENCOREDVD(Subtitles, FileName, FPS, From, UpTo);
      sfAdvancedSubStationAlpha : Result := SubtitlesToFile_ADVANCEDSUBSTATIONALPHA(Subtitles, FileName, From, UpTo);
      sfAQTitle                 : Result := SubtitlesToFile_AQTITLE(Subtitles, FileName, FPS, From, UpTo);
      sfCaptions32              : Result := SubtitlesToFile_CAPTIONS32(Subtitles, FileName, From, UpTo);
      sfCaptionsDat             : Result := SubtitlesToFile_CAPTIONSDAT(Subtitles, FileName, From, UpTo);
      sfCaptionsDatText         : Result := SubtitlesToFile_CAPTIONSDATTEXT(Subtitles, FileName, From, UpTo);
      sfCaptionsInc             : Result := SubtitlesToFile_CAPTIONSINC(Subtitles, FileName, From, UpTo);
      sfCheetah                 : Result := SubtitlesToFile_CHEETAH(Subtitles, FileName, From, UpTo);
      sfCPC600                  : Result := SubtitlesToFile_CPC600(Subtitles, FileName, From, UpTo);
      sfDKS                     : Result := SubtitlesToFile_DKS(Subtitles, FileName, From, UpTo);
      sfDVDJunior               : Result := SubtitlesToFile_DVDJUNIOR(Subtitles, FileName, From, UpTo);
      sfDVDSubtitleSystem       : Result := SubtitlesToFile_DVDSUBTITLESYSTEM(Subtitles, FileName, From, UpTo);
      sfDVDSubtitle             : Result := SubtitlesToFile_DVDSUBTITLE(Subtitles, FileName, From, UpTo);
      sfFABSubtitler            : Result := SubtitlesToFile_FABSUBTITLER(Subtitles, FileName, FPS, From, UpTo);
      sfIAuthor                 : Result := SubtitlesToFile_IAUTHOR(Subtitles, FileName, From, UpTo);
      sfInscriberCG             : Result := SubtitlesToFile_INSCRIBERCG(Subtitles, FileName, From, UpTo);
      sfJACOSub                 : Result := SubtitlesToFile_JACOSUB(Subtitles, FileName, From, UpTo);
      sfKaraokeLyricsLRC        : Result := SubtitlesToFile_KARAOKELYRICSLRC(Subtitles, FileName, From, UpTo);
      sfKaraokeLyricsVKT        : Result := SubtitlesToFile_KARAOKELYRICSVKT(Subtitles, FileName, From, UpTo);
      sfMACDVDStudioPro         : Result := SubtitlesToFile_MACDVDSTUDIOPRO(Subtitles, FileName, FPS, From, UpTo);
      sfMacSUB                  : Result := SubtitlesToFile_MACSUB(Subtitles, FileName, FPS, From, UpTo);
      sfMicroDVD                : Result := SubtitlesToFile_MICRODVD(Subtitles, FileName, FPS, From, UpTo);
      sfMPlayer                 : Result := SubtitlesToFile_MPLAYER(Subtitles, FileName, FPS, From, UpTo);
      sfMPlayer2                : Result := SubtitlesToFile_MPLAYER2(Subtitles, FileName, From, UpTo);
      sfMPSub                   : Result := SubtitlesToFile_MPSUB(Subtitles, FileName, From, UpTo);
      sfOVRScript               : Result := SubtitlesToFile_OVRSCRIPT(Subtitles, FileName, From, UpTo);
      sfPanimator               : Result := SubtitlesToFile_PANIMATOR(Subtitles, FileName, From, UpTo);
      sfPhilipsSVCD             : Result := SubtitlesToFile_PHILIPSSVCD(Subtitles, FileName, From, UpTo);
      sfPhoenixJS               : Result := SubtitlesToFile_PHOENIXJAPANIMATION(Subtitles, FileName, FPS, From, UpTo);
      sfPinnacleImpression      : Result := SubtitlesToFile_PINNACLEIMPRESSION(Subtitles, FileName, FPS, From, UpTo);
      sfPowerDivX               : Result := SubtitlesToFile_POWERDIVX(Subtitles, FileName, From, UpTo);
      sfPowerPixel              : Result := SubtitlesToFile_POWERPIXEL(Subtitles, FileName, From, UpTo);
      sfQuickTimeText           : Result := SubtitlesToFile_QUICKTIMETEXT(Subtitles, FileName, From, UpTo);
      sfRealTime                : Result := SubtitlesToFile_REALTIME(Subtitles, FileName, From, UpTo);
      sfSAMI                    : Result := SubtitlesToFile_SAMI(Subtitles, FileName, From, UpTo);
      sfSasamiScript            : Result := SubtitlesToFile_SASAMISCRIPT(Subtitles, FileName, From, UpTo);
      sfSBT                     : Result := SubtitlesToFile_SBT(Subtitles, FileName, From, UpTo);
      sfSoftni                  : Result := SubtitlesToFile_SOFTNI(Subtitles, FileName, FPS, From, UpTo);
      sfSoftitlerRTF            : Result := SubtitlesToFile_SOFTITLERRTF(Subtitles, FileName, From, UpTo);
      sfSonicDVD                : Result := SubtitlesToFile_SONICDVD(Subtitles, FileName, FPS, From, UpTo);
      sfSonicScenarist          : Result := SubtitlesToFile_SONICSCENARIST(Subtitles, FileName, FPS, From, UpTo);
      sfSpruceDVDMaestro        : Result := SubtitlesToFile_SPRUCEDVDMAESTRO(Subtitles, FileName, FPS, From, UpTo);
      sfSpruceSubtitleFile      : Result := SubtitlesToFile_SPRUCESUBTITLEFILE(Subtitles, FileName, FPS, From, UpTo);
      sfSSTPlayer               : Result := SubtitlesToFile_SSTPLAYER(Subtitles, FileName, From, UpTo);      
      sfSSTScript               : Result := SubtitlesToFile_SSTSCRIPT(Subtitles, FileName, From, UpTo);
      sfSubCreator              : Result := SubtitlesToFile_SUBCREATOR1X(Subtitles, FileName, From, UpTo);
      sfSubRip                  : Result := SubtitlesToFile_SUBRIP(Subtitles, FileName, From, UpTo);
      sfSubSonic                : Result := SubtitlesToFile_SUBSONIC(Subtitles, FileName, From, UpTo);
      sfSubStationAlpha         : Result := SubtitlesToFile_SUBSTATIONALPHA(Subtitles, FileName, From, UpTo);
      sfSubViewer1              : Result := SubtitlesToFile_SUBVIEWER1(Subtitles, FileName, From, UpTo);
      sfSubViewer2              : Result := SubtitlesToFile_SUBVIEWER2(Subtitles, FileName, From, UpTo);
      sfTMPlayer                : Result := SubtitlesToFile_TMPLAYER(Subtitles, FileName, From, UpTo);
      sfTurboTitler             : Result := SubtitlesToFile_TURBOTITLER(Subtitles, FileName, From, UpTo);
      sfUleadDVDWorkshop2       : Result := SubtitlesToFile_ULEADDVDWORKSHOP2(Subtitles, FileName, FPS, From, UpTo);      
      sfViPlay                  : Result := SubtitlesToFile_VIPLAY(Subtitles, FileName, From, UpTo);
      sfZeroG                   : Result := SubtitlesToFile_ZEROG(Subtitles, FileName, From, UpTo);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_ADOBEENCOREDVD(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      tmpSubFile.Add(MSToHHMMSSFFTime(Subtitles.InitialTime[i], FPS) + ' ' +
                     MSToHHMMSSFFTime(Subtitles.FinalTime[i], FPS) + ' ' +
                     RemoveSWTags(Subtitles.Text[i], True, True, True, True)
                     );
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_ADVANCEDSUBSTATIONALPHA(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
const
  Format = 'Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding';

  function GetASSStyleStr: String;
  var
    DSep : Char;
  begin
    DSep := DecimalSeparator;
    DecimalSeparator := '.';

    Result := Format;
    Result := ReplaceString(Result, 'Format: Name',     'Style: Default');
    Result := ReplaceString(Result, ' FontName',        ASSAttributes.FontName);
    Result := ReplaceString(Result, ' FontSize',        IntToStr(ASSAttributes.FontSize));
    Result := ReplaceString(Result, ' PrimaryColour',   '&H'+IntToHex(ASSAttributes.PrimaryColor, 8));
    Result := ReplaceString(Result, ' SecondaryColour', '&H'+IntToHex(ASSAttributes.SecondaryColor, 8));
    Result := ReplaceString(Result, ' OutlineColour',   '&H'+IntToHex(ASSAttributes.OutlineColour, 8));
    Result := ReplaceString(Result, ' BackColour',      '&H'+IntToHex(ASSAttributes.BackColour, 8));
    Result := ReplaceString(Result, ' Bold',            BoolToStr(ASSAttributes.Bold));
    Result := ReplaceString(Result, ' Italic',          BoolToStr(ASSAttributes.Italic));
    Result := ReplaceString(Result, ' Underline',       BoolToStr(ASSAttributes.Underline));
    Result := ReplaceString(Result, ' StrikeOut',       BoolToStr(ASSAttributes.StrikeOut));
    Result := ReplaceString(Result, ' ScaleX',          IntToStr(ASSAttributes.ScaleX));
    Result := ReplaceString(Result, ' ScaleY',          IntToStr(ASSAttributes.ScaleY));
    Result := ReplaceString(Result, ' Spacing',         IntToStr(ASSAttributes.Spacing));
    Result := ReplaceString(Result, ' Angle',           SysUtils.Format('%.2f', [ASSAttributes.Angle]));
    Result := ReplaceString(Result, ' BorderStyle',     IntToStr(ASSAttributes.BorderStyle));
    Result := ReplaceString(Result, ' Outline',         IntToStr(ASSAttributes.Outline));
    Result := ReplaceString(Result, ' Shadow',          IntToStr(ASSAttributes.Shadow));
    Result := ReplaceString(Result, ' Alignment',       IntToStr(ASSAttributes.Alignment));
    Result := ReplaceString(Result, ' MarginL',         IntToStr(ASSAttributes.MarginL));
    Result := ReplaceString(Result, ' MarginR',         IntToStr(ASSAttributes.MarginR));
    Result := ReplaceString(Result, ' MarginV',         IntToStr(ASSAttributes.MarginV));
    Result := ReplaceString(Result, ' Encoding',        IntToStr(ASSAttributes.Encoding));

    DecimalSeparator := DSep;
  end;

var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  tmpCad     : String;
begin
  Result := True;

  if ASSAttributes.Assigned = False then
    with ASSAttributes do
    begin
      Collisions     := 'Normal';
      PlayResX       := 384;
      PlayResY       := 288;
      Timer          := '100.0000';
      FontName       := 'Tahoma';
      FontSize       := 24;
      PrimaryColor   := 16777215;
      SecondaryColor := 16777215;
      OutlineColour  := 16777215;
      BackColour     := 12632256;
      Bold           := True;
      Italic         := False;
      Underline      := False;
      StrikeOut      := False;
      ScaleX         := 100;
      ScaleY         := 100;
      Spacing        := 0;
      Angle          := 0.00;
      BorderStyle    := 1;
      Outline        := 2;
      Shadow         := 3;
      Alignment      := 2;
      MarginL        := 20;
      MarginR        := 20;
      MarginV        := 20;
      Encoding       := 1;
    end;

  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('[Script Info]', False);
    tmpSubFile.Add('ScriptType: v4.00+', False);
    tmpSubFile.Add('Collisions: ' + ASSAttributes.Collisions, False);
    tmpSubFile.Add('PlayResX: ' + IntToStr(ASSAttributes.PlayResX), False);
    tmpSubFile.Add('PlayResY: ' + IntToStr(ASSAttributes.PlayResY), False);
    tmpSubFile.Add('Timer: ' + ASSAttributes.Timer, False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('[V4+ Styles]', False);
    tmpSubFile.Add(Format, False);
    tmpSubFile.Add(GetASSStyleStr);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('[Events]', False);
    tmpSubFile.Add('Format: Layer, Start, End, Style, Actor, MarginL, MarginR, MarginV, Effect, Text', False);

    for i := From to UpTo do
    begin
      tmpCad := Subtitles[i].Text;

      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        if WorkWithTags then
        begin
          if Pos('<b>', tmpCad) <> 0 then
            tmpCad := '{\b1}' + tmpCad + '{\b0}';
          if Pos('<i>', tmpCad) <> 0 then
            tmpCad := '{\i1}' + tmpCad + '{\i0}';
          if Pos('<u>', tmpCad) <> 0 then
            tmpCad := '{\u1}' + tmpCad + '{\u0}';
        end;
        tmpCad := RemoveSWTags(tmpCad, True, True, True);
      end;
      
      tmpSubFile.Add('Dialogue: 0,' + TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ',' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ',Default,,0000,0000,0000,,'+ ReplaceEnters(tmpCad,'\N'), False);
    {$IFNDEF VIPLAY}
    end;
    {$ENDIF}

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    
    if ASSAttributes.Assigned = False then
      with ASSAttributes do
      begin
        Collisions     := '';
        PlayResX       := 0;
        PlayResY       := 0;
        Timer          := '';
        FontName       := '';
        FontSize       := 0;
        PrimaryColor   := 0;
        SecondaryColor := 0;
        OutlineColour  := 0;
        BackColour     := 0;
        Bold           := False;
        Italic         := False;
        Underline      := False;
        StrikeOut      := False;
        ScaleX         := 0;
        ScaleY         := 0;
        Spacing        := 0;
        Angle          := 0;
        BorderStyle    := 0;
        Outline        := 0;
        Shadow         := 0;
        Alignment      := 0;
        MarginL        := 0;
        MarginR        := 0;
        MarginV        := 0;
        Encoding       := 0;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_AQTITLE(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1) : Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  Text       : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      tmpSubFile.Add('-->> ' + PadLeft(IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)),'0', 6), False);

      if StringCount(#13#10, Subtitles[i].Text) = 0 then
        Text := Subtitles[i].Text + #13#10
      else if StringCount(#13#10, Subtitles[i].Text) = 1 then
        Text := Subtitles[i].Text
      else if StringCount(#13#10, Subtitles[i].Text) > 1 then
        Text := Copy(Subtitles[i].Text, 0, Pos(#13#10, Subtitles[i].Text) + 2) + ReplaceEnters(Copy(Subtitles[i].Text, Pos(#13#10, Subtitles[i].Text) + 2, Length(Subtitles[i].Text)), ' ');

      tmpSubFile.Add(Text, False);
      tmpSubFile.Add('-->> ' + PadLeft(IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)),'0', 6), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add('', False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_CAPTIONS32(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i, c       : Integer;
  Text       : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := ReplaceEnters(RemoveSWTags(Subtitles.Text[i], True, True, True), '|');

      c := StringCount('|', Subtitles.Text[i], False);
      if c = 0 then
      begin
        if Length(Subtitles.Text[i]) > 33 then
          Text := Copy(Subtitles.Text[i], 1, 33) else
          Text := PadRight(Subtitles.Text[i], ' ', 33, False);
        Text := PadRight(Text + '|', ' ', 67, False);
      end else
      begin
        if c > 1 then
        begin
          c := Pos('|', Subtitles.Text[i]);
          Subtitles.Text[i] := Copy(Subtitles.Text[i], 1, SmartPos('|', Subtitles.Text[i], False, c+1)-1);
        end;

        c := Pos('|', Subtitles.Text[i]);
        Text := Copy(Subtitles.Text[i], 1, c-1);
        if Length(Text) > 33 then
          Text := Copy(Text, 1, 33) else
          Text := PadRight(Text, ' ', 33, False);

        Text := Text + '|' + Copy(Subtitles.Text[i], c+1, Length(Subtitles.Text[i])-c);

        if Length(Text) > 67 then
          Text := Copy(Text, 1, 67)
        else
          Text := PadRight(Text, ' ', 67, False);
      end;

      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + ' , ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + ' , ' + Text, False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_CAPTIONSDAT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i, a       : Integer;
  BigStr     : String;
  Text       : String;
begin
  Result := True;
  BigStr := '';

  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Text := RemoveSWTags(Subtitles.Text[i], True, True, True);

      BigStr := BigStr + '#S' + TimeToString(Subtitles[i].InitialTime, 'hmmsszzz') + '0' + IntToStr(Subtitles[i].FinalTime-Subtitles[i].InitialTime) + '0' + #13;
      a := Pos(#13#10, Text);
      while a > 0 do
      begin
        BigStr := BigStr + 'Bð é 015 Aô     ' + Copy(Text, 1, a - 1) + #13;
        Text := Copy(Text, a + 2, Length(Text));
        a := Pos(#13#10, Text);
      end;

      BigStr := BigStr + 'Bð é 015 Aô     ' + Text + #13;
    end;

    tmpSubFile.Add(#0#13 + BigStr + #26, False);
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_CAPTIONSDATTEXT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i, a       : Integer;
  BigStr     : String;
  Text       : String;
begin
  Result := True;
  BigStr := '';

  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Text := RemoveSWTags(Subtitles.Text[i], True, True, True);

      BigStr := BigStr + '#T' + TimeToString(Subtitles[i].InitialTime, 'hmmsszzz') + '0' + IntToStr(Subtitles[i].FinalTime-Subtitles[i].InitialTime) + '0' + #13;
      a := Pos(#13#10, Text);
      while a > 0 do
      begin
        BigStr := BigStr + 'BG @015 A ' + Copy(Text, 1, a - 1) + #13;
        Text := Copy(Text, a + 2, Length(Text));
        a := Pos(#13#10, Text);
      end;

      BigStr := BigStr + 'BG @015 A ' + Text + #13;
    end;

    tmpSubFile.Add(#0#13 + BigStr + #26, False);
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_CAPTIONSINC(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('*Timecode type: PAL/EBU', False);
    tmpSubFile.Add('', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + ' ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add('{0 [1 ' + ReplaceEnters(Subtitles[i].Text, ' '), False);
      tmpSubFile.Add('', False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_CHEETAH(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  SubIndex   : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('*NonDropFrame', False);
    tmpSubFile.Add('*Width 32', False);
    tmpSubFile.Add('', False);
    SubIndex := 1;
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);;
      
      tmpSubFile.Add('** Caption Number '+ IntToStr(SubIndex), False);
      tmpSubFile.Add('*PopOn', False);
      tmpSubFile.Add('*T ' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add('*BottomUp', False);
      tmpSubFile.Add('*Lf01', False);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('', False);
      Inc(SubIndex);

      tmpSubFile.Add('** Caption Number '+ IntToStr(SubIndex), False);
      tmpSubFile.Add('*PopOn', False);
      tmpSubFile.Add('*T ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add('*BottomUp', False);
      tmpSubFile.Add('*Lf01', False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add('', False);
      Inc(SubIndex);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_CPC600(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('~CPCC6.38~;UpperLower;PopOn;01;', False);
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);;
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + '³0NEN³' + ReplaceString(Subtitles[i].Text,#13#10,'\'), False);
      tmpSubFile.Add(TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + '³0NEN³');
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_DKS(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      if (Subtitles[i].InitialTime = Subtitles[i].FinalTime) then
        Subtitles.FinalTime[i] := Subtitles[i].InitialTime + 1000;
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss') + ']' + ReplaceEnters(Subtitles[i].Text,'[br]'), False);
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss') + ']', False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_DVDJUNIOR(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('Subtitle File Mark:2C:\Untitled.avi', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + '&' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + '#' + ReplaceEnters(Subtitles[i].Text,'<'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_DVDSUBTITLESYSTEM(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + ' ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + ' ' + ReplaceEnters(Subtitles[i].Text,'//'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_DVDSUBTITLE(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('{HEAD', False);
    tmpSubFile.Add('DISCID=' + DVDSubtitleAttributes.DiskId, False);
    tmpSubFile.Add('DVDTITLE=' + DVDSubtitleAttributes.DVDTitle, False);
    tmpSubFile.Add('CODEPAGE=1250', False);
    tmpSubFile.Add('FORMAT=ASCII', False);
    tmpSubFile.Add('LANG=' + DVDSubtitleAttributes.Language, False);
    tmpSubFile.Add('TITLE=1', False);
    tmpSubFile.Add('ORIGINAL=ORIGINAL', False);
    tmpSubFile.Add('AUTHOR=' + DVDSubtitleAttributes.Author, False);
    tmpSubFile.Add('WEB=' + DVDSubtitleAttributes.Web, False);
    tmpSubFile.Add('INFO=' + DVDSubtitleAttributes.Info, False);
    tmpSubFile.Add('LICENSE=' + DVDSubtitleAttributes.License, False);
    tmpSubFile.Add('}', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      tmpSubFile.Add('{T ' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('}', False);
      tmpSubFile.Add('{T ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add('}', False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_FABSUBTITLER(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile  : TSubtitleFile;
  InitialTime : String;
  FinalTime   : String;
  i           : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      // Time format is hh:mm:ss:ff
      InitialTime := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:') +
                     PadLeft(IntToStr(GetMSInFrames(Subtitles[i].InitialTime, FPS)), '0', 2, False);

      FinalTime := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:') +
                   PadLeft(IntToStr(GetMSInFrames(Subtitles[i].FinalTime, FPS)), '0', 2, False);

      tmpSubFile.Add(InitialTime + '  ' + FinalTime);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('', False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_IAUTHOR(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  tmpTiempo  : Single;
  i, c       : Integer;
  tmpNum     : String;
  dc         : Char;
begin
  Result := True;

  dc := DecimalSeparator;
  tmpSubFile := TSubtitleFile.Create;
  try
    c                := 1;
    DecimalSeparator := '.';

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      tmpTiempo := Subtitles[i].InitialTime / 1000;
      If tmpTiempo > (256*c) Then inc(c);

      tmpTiempo := tmpTiempo - (256*(c-1));
      tmpNum    := LimitDecimals(tmpTiempo, 2);

      if Pos('.',tmpNum) = 0 then
        tmpNum := tmpNum + '.00';
      if Length(Copy(tmpNum,Pos('.',tmpNum)+1,Length(tmpNum))) < 2 then
        tmpNum := tmpNum + '0';

      tmpSubFile.Add('BMPFILE: ' + ReplaceEnters(Subtitles[i].Text,' '), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add('STARTTIME: ' + tmpNum, False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Primary 0, 16, 128, 128',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Primary 1, 234, 128, 128',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Primary 2, 16, 128, 128',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Primary 3, 125, 128, 128',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Highlight 0, 16, 128, 128',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Highlight 1, 209, 146, 17',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Highlight 2, 81, 239, 91',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETCOLOR Highlight 3, 144, 35, 54',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s region 207, 170 to 432, 190',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETBLEND Primary 0, 15, 15, 15',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s SETBLEND Hightlight 0, 15, 15, 15',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s FIELDINDEX 0, 1',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add(Format('TIME: %s ENABLE_OGT',[tmpNum]), False);
      tmpSubFile.Add('', False);

      tmpTiempo := Subtitles[i].FinalTime / 1000;
      tmpTiempo := tmpTiempo - (256*(c-1));

      tmpNum    := LimitDecimals(tmpTiempo, 2);

      if Pos('.',tmpNum) = 0 then
        tmpNum := tmpNum + '.00';
      if Length(Copy(tmpNum,Pos('.',tmpNum)+1,Length(tmpNum))) < 2 then
        tmpNum := tmpNum + '0';

      tmpSubFile.Add(Format('TIME: %s DISABLE_OGT',[tmpNum]), False);
      tmpSubFile.Add('', False);
      tmpSubFile.Add('************ Page #' + IntToStr(i+1) + ' Finished ***************', False);
      tmpSubFile.Add('', False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    DecimalSeparator := dc;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_INSCRIBERCG(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('@@9 Title', False);
    tmpSubFile.Add('@@9 Created by URUSoft Subtitle API (http://www.urusoft.net)', False);
    tmpSubFile.Add('@@9 STORY:', False);
    tmpSubFile.Add('@@9 LANG: ENG', False);
    for i := 1 to 4 do
      tmpSubFile.Add('@@9', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      tmpSubFile.Add('@@9 ' + ReplaceEnters(Subtitles[i].Text, #13#10+'@@9 ') +
                     '<' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + '><'  +
                     TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + '>');

      tmpSubFile.Add('', False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_JACOSUB(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('#T100', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('# Directive entries', False);
    tmpSubFile.Add('#D', False);
    for i := 1 to 29 do
      tmpSubFile.Add('#D'+IntToStr(i), False);
    tmpSubFile.Add('', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ' ' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ' {NTP} ' + ReplaceEnters(Subtitles[i].Text,'\n'), False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_KARAOKELYRICSLRC(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('[ti:Project title]', False);
    tmpSubFile.Add('[ar:Project author]', False);
    tmpSubFile.Add('[la:af]', False);
    tmpSubFile.Add('Project title', False);
    tmpSubFile.Add('', False);    

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].InitialTime, 'mm:ss.zz') + ']' + ReplaceEnters(Subtitles[i].Text, ''), False);
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].FinalTime, 'mm:ss.zz') + ']');
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_KARAOKELYRICSVKT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  DateSep    : Char;
  DateFor    : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('# <HEAD>', False);
    tmpSubFile.Add('# FRAME RATE=MP3', False);
    tmpSubFile.Add('# CREATOR=Project author', False);
    tmpSubFile.Add('# VIDEO SOURCE=C:\Untitled.avi', False);
    // DATE
    DateSep         := DateSeparator;
    DateFor         := ShortDateFormat;
    DateSeparator   := '-';
    ShortDateFormat := 'yyyy/mm/dd';
    tmpSubFile.Add('# DATE=' + DateToStr(Date), False);
    DateSeparator   := DateSep;
    ShortDateFormat := DateFor;
    //
    tmpSubFile.Add('# </HEAD>', False);
    tmpSubFile.Add('#', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('{' + PadLeft(IntToStr(Subtitles[i].InitialTime div 10), '0', 5){TimeToString(Subtitles[i].InitialTime, 'msszz')} + ' ' + ReplaceEnters(Subtitles[i].Text, '') + '}', False);
      tmpSubFile.Add('{' + PadLeft(IntToStr(Subtitles[i].FinalTime div 10), '0', 5){TimeToString(Subtitles[i].FinalTime, 'msszz')} + ' }');
    end;

    tmpSubFile.Add('', False);
    tmpSubFile.Add('#', False);
    tmpSubFile.Add('# THE END.', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_MACDVDSTUDIOPRO(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile  : TSubtitleFile;
  InitialTime : String;
  FinalTime   : String;
  i           : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      // Time format is hh:mm:ss:ff
      InitialTime := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:') +
                     PadLeft(IntToStr(GetMSInFrames(Subtitles[i].InitialTime, FPS)), '0', 2, False);

      FinalTime := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:') +
                   PadLeft(IntToStr(GetMSInFrames(Subtitles[i].FinalTime, FPS)), '0', 2, False);

      tmpSubFile.Add(InitialTime + #9 + FinalTime + #9 + ReplaceString(Subtitles[i].Text, #13#10, '<P>'));
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_MACSUB(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('/' + IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)), False);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('/' + IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)), False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_MICRODVD(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  tmpStyle   : String;
  i          : Integer;
  DecimalSep : Char;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    // DivXG400 FPS Info tag
    DecimalSep       := DecimalSeparator;
    DecimalSeparator := '.';
    tmpSubFile.Add(Format('{1}{1}%.3f', [FPS]), False);
    DecimalSeparator := DecimalSep;

    for i := From to UpTo do
    begin
      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        if WorkWithTags = True then
        begin
          // Styles
          tmpStyle := '';
          if Pos('<u>',Subtitles[i].Text) <> 0 then
            tmpStyle := tmpStyle + 'u';
          if Pos('<b>',Subtitles[i].Text) <> 0 then
            tmpStyle := tmpStyle + 'b';
          if Pos('<i>',Subtitles[i].Text) <> 0 then
            tmpStyle := tmpStyle + 'i';

          if tmpStyle <> '' then
            Subtitles.Text[i] := '{Y:' + tmpStyle + '}' + Subtitles[i].Text;

          // Colors
          if Pos('<c:#', Subtitles[i].Text) > 0 then
            Subtitles.Text[i] := '{C:$' + InvertHTMLColor(Copy(Subtitles.Text[i], Pos('<c:#', Subtitles[i].Text) + 4, 6)) + '}' + Subtitles[i].Text;
        end;
        Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      end;
      tmpSubFile.Add('{' + IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)) + '}{' + IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)) + '}' + ReplaceEnters(Subtitles[i].Text,'|'), False);
    {$IFNDEF VIPLAY}
    end;
    {$ENDIF}
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_MPLAYER(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile := TSubtitleFile.Create;

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)) + ',' + IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)) + ',0,' + ReplaceEnters(Subtitles[i].Text,'|'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_MPLAYER2(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('[' + IntToStr(Subtitles[i].InitialTime div 100) + '][' + IntToStr(Subtitles[i].FinalTime div 100) + ']' + ReplaceEnters(Subtitles[i].Text,'|'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_MPSUB(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  DSep       : Char;
  BigStr     : String;
begin
  Result     := True;
  tmpSubFile := TSubtitleFile.Create;
  DSep       := DecimalSeparator;
  try
    DecimalSeparator := '.';
    BigStr           := '';

    BigStr := BigStr + 'TITLE='#10;
    BigStr := BigStr + 'FILE='#10;
    BigStr := BigStr + 'AUTHOR='#10;
    BigStr := BigStr + 'TYPE=VIDEO'#10;
    BigStr := BigStr + 'FORMAT=TIME'#10;
    BigStr := BigStr + 'NOTE='#10#10;

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      if i = 0 then
        BigStr := BigStr + LimitDecimals(Subtitles[i].InitialTime / 1000, 2) + ' ' + LimitDecimals((Subtitles[i].FinalTime / 1000) - (Subtitles[i].InitialTime / 1000), 2) + #10 else
        BigStr := BigStr + LimitDecimals((Subtitles[i].InitialTime / 1000) - (Subtitles[i-1].FinalTime / 1000), 2) + ' ' + LimitDecimals((Subtitles[i].FinalTime / 1000) - (Subtitles[i].InitialTime / 1000), 2) + #10;

      BigStr := BigStr + ReplaceEnters(Subtitles[i].Text, #10) + #10#10;
    end;

    tmpSubFile.Add(BigStr, False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    DecimalSeparator := DSep;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_OVRSCRIPT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result     := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      if (Subtitles[i].InitialTime = Subtitles[i].FinalTime) then
        Subtitles.FinalTime[i] := Subtitles[i].InitialTime + 1000;
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + ' ' + ReplaceEnters(Subtitles[i].Text,'//'), False);
      tmpSubFile.Add(TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz'), False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_PANIMATOR(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('/d ' + TimeToString(Subtitles[i].InitialTime, 'ss zz') + ' ', False);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('/d ' + TimeToString(Subtitles[i].FinalTime, 'ss zz') + ' ', False);
      tmpSubFile.Add('/c');
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_PHILIPSSVCD(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('# PHILIPS SVCD DESIGNER 1.5 - 2.0 SUBTITLES FILE', False);
    tmpSubFile.Add('#', False);
    tmpSubFile.Add('[HEADER]', False);
    tmpSubFile.Add('TITLE'{+#9+#9+''}, False);             // TITULO!
    tmpSubFile.Add('FRAMERATE'+#9+'PAL', False);           // PAL o NTSC
// LA SIGUIENTE LINEA VA SOLO SI EL FRAMERATE ES NTSC!
//    tmpSubFile.Add('DROPFRAME'+#9+'NO', False);          // YES o NO
    tmpSubFile.Add('CONTRAST'+#9+'(0 15 15 15)', False);
    {tmpSubFile.Add('COLOR'+#9+#9+'(0 1 2 3)', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('[PALETTE]', False);
    tmpSubFile.Add('0 (0, 0, 0)', False);
    tmpSubFile.Add('1 (255, 255, 255)', False);
    tmpSubFile.Add('2 (0, 0, 0)', False);
    tmpSubFile.Add('3 (128, 128, 128)', False);        }
    tmpSubFile.Add('', False);
    tmpSubFile.Add('[LIST]', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(Subtitles[i].Text + ' ' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + ' ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + ' 001 001 000 000', False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_PHOENIXJAPANIMATION(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(PadLeft(IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)), ' ', 7) + ',' + PadLeft(IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)), ' ', 7) + ', "' + ReplaceEnters(Subtitles[i].Text, '|') + '"', False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_PINNACLEIMPRESSION(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('-------------------------------------------------', False);
    tmpSubFile.Add('#INPOINT OUTPOINT PATH', False);
    tmpSubFile.Add('-------------------------------------------------', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(MSToHHMMSSFFTime(Subtitles[i].InitialTime, FPS) + ' ' + MSToHHMMSSFFTime(Subtitles[i].FinalTime, FPS) + ' '+ ReplaceEnters(Subtitles[i].Text,' '), False);
    end;
    tmpSubFile.Add('-------------------------------------------------', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_POWERDIVX(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      if (Subtitles[i].InitialTime = Subtitles[i].FinalTime) then
        Subtitles.FinalTime[i] := Subtitles[i].InitialTime + 1000;
      tmpSubFile.Add('{' + TimeToString(Subtitles[i].InitialTime, 'h:mm:ss') + '}{' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss') + '}'+ ReplaceEnters(Subtitles[i].Text,'|'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_POWERPIXEL(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + #9 + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('', False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_QUICKTIMETEXT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('{QTtext}', False);
    tmpSubFile.Add('{font:Tahoma}', False);
    tmpSubFile.Add('{plain}', False);
    tmpSubFile.Add('{size:10}', False);
    tmpSubFile.Add('{timeScale:10}', False);
    tmpSubFile.Add('{height:40}', False);
    tmpSubFile.Add('{width:320}', False);
    tmpSubFile.Add('{useMovieBackColor:ON}', False);
    tmpSubFile.Add('{timeStamps:absolute}', False);
    tmpSubFile.Add('', False);
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss.zz') + ']');
      tmpSubFile.Add({ReplaceEnters(}Subtitles[i].Text{, #13#10)});
      if i = UpTo then
        tmpSubFile.Add('[' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss.zz') + ']');
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_REALTIME(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  Text       : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('<Window', False);
    tmpSubFile.Add('  Width    = "640"', False);
    tmpSubFile.Add('  Height   = "480"', False);
    tmpSubFile.Add('  WordWrap = "true"', False);
    tmpSubFile.Add('  Loop     = "true"', False);
    tmpSubFile.Add('  bgcolor  = "black"', False);
    tmpSubFile.Add('>', False);
    tmpSubFile.Add('<Font', False);
    tmpSubFile.Add('  Color = "white"', False);
    tmpSubFile.Add('  Face  = "Arial"', False);
    tmpSubFile.Add('  Size  = "+2"', False);
    tmpSubFile.Add('>', False);
    tmpSubFile.Add('<center>', False);
    tmpSubFile.Add('<b>', False);
    tmpSubFile.Add('', False);

    for i := From to UpTo do
    begin
      Text := ReplaceEnters(Subtitles[i].Text,'<br>');
      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        if WorkWithTags = False then
          Text := RemoveSWTags(Text, True, True, True) else
        begin
          if Pos('<u>',Text) > 0 then
            Text := Text + '</u>';
          if Pos('<b>',Text) > 0 then
            Text := Text + '</b>';
          if Pos('<i>',Text) > 0 then
            Text := Text + '</i>';
          if Pos('<c:#', Text) > 0 then
          begin
            Text := Text + '</font>';
            Text := Copy(Text, 0, Pos('<c:#', Text) - 1) + '<font color="' + Copy(Text, Pos('<c:#', Text) + 3, 7) + '">' + Copy(Text, Pos('<c:#', Text) + 11, Length(Text));
          end;
        end;
      end;
      tmpSubFile.Add('<Time begin="' + TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.z') + '" end="' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.z') + '" /><clear/>'+ Text, False);
    {$IFNDEF VIPLAY}
    end;
    {$ENDIF}
    tmpSubFile.Add('</b>', False);
    tmpSubFile.Add('</center>', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SAMI(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  tmpStr     : String;
begin

  // Default values
  if SAMIAttributes.Assigned = False then
    with SAMIAttributes do
    begin
      FontName        := 'Tahoma';
      FontSize        := 24;
      Bold            := True;
      Italic          := False;
      Underline       := False;
      SubColor        := '#FFFFFF';
      BackgroundColor := '#000000';
      Align           := 'center';
    end;

  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    if SAMIAttributes.Bold then
    begin
      if SAMIAttributes.Italic then
      begin
        if SAMIAttributes.Underline then
          tmpStr := 'font-weight: bold; font-style: italic; text-decoration: underline; '
        else
          tmpStr := 'font-weight: bold; font-style: italic; ';
      end
      else
        tmpStr := 'font-weight: bold; ';
    end
    else if SAMIAttributes.Italic then
    begin
      if SAMIAttributes.Underline then
        tmpStr := 'font-style: italic; text-decoration: underline; '
      else
        tmpStr := 'font-style: italic; ';
    end
    else if SAMIAttributes.Underline then
      tmpStr := 'text-decoration: underline; '
    else
      tmpStr := '';

    tmpSubFile.Add('<SAMI>', False);
    tmpSubFile.Add('<HEAD>', False);
    tmpSubFile.Add('   <STYLE TYPE="Text/css">', False);
    tmpSubFile.Add('   <!--', False);
    tmpSubFile.Add('      P {margin-left: 29pt; margin-right: 29pt; font-size: ' + IntToStr(SAMIAttributes.FontSize) + 'pt; text-align: ' + AnsiLowerCase(SAMIAttributes.Align) + '; font-family: ' + SAMIAttributes.FontName + '; '+ tmpStr + 'color: ' + SAMIAttributes.SubColor + '; background-color: ' + SAMIAttributes.BackgroundColor + ';}', False);
    tmpSubFile.Add('      .SUBTTL {Name: ''Subtitles''; Lang: en-US; SAMIType: CC;}', False);
    tmpSubFile.Add('   -->', False);
    tmpSubFile.Add('   </STYLE>', False);
    tmpSubFile.Add('</HEAD>', False);
    tmpSubFile.Add('<BODY>', False);

    for i := From to UpTo do
    begin

      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        if WorkWithTags = False then
          Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True) else
        begin
          if Pos('<u>',Subtitles[i].Text) <> 0 then
            Subtitles.Text[i] := Subtitles[i].Text + '</u>';
          if Pos('<b>',Subtitles[i].Text) <> 0 then
            Subtitles.Text[i] := Subtitles[i].Text + '</b>';
          if Pos('<i>',Subtitles[i].Text) <> 0 then
            Subtitles.Text[i] := Subtitles[i].Text + '</i>';
          Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], False, False, False);
        end;
      end;

      tmpSubFile.Add('   <SYNC START=' + IntToStr(Subtitles[i].InitialTime) + '>', False);
      tmpSubFile.Add('      <P CLASS=SUBTTL>' + ReplaceEnters(Subtitles[i].Text,'<br>'), False);

      tmpSubFile.Add('   <SYNC START=' + IntToStr(Subtitles[i].FinalTime) + '>', False);
      tmpSubFile.Add('      <P CLASS=SUBTTL>&nbsp;', False);
    {$IFNDEF VIPLAY}
    end;
    {$ENDIF}

    tmpSubFile.Add('</BODY>', False);
    tmpSubFile.Add('</SAMI>', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    if SAMIAttributes.Assigned = False then
      with SAMIAttributes do
      begin
        FontName        := '';
        FontSize        := 0;
        Bold            := False;
        Italic          := False;
        Underline       := False;
        SubColor        := '';
        BackgroundColor := '';
        Align           := '';
      end;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SASAMISCRIPT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile  : TSubtitleFile;
  tmpLines    : TSubtitleFile;
  i,a         : Integer;
  tmpText,Aux : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  tmpLines   := TSubtitleFile.Create;
  try
    tmpSubFile.Add(';Env.Movie.Width=320', False);
    tmpSubFile.Add(';Env.Movie.Height=240', False);
    tmpSubFile.Add(';Set.Slot=1', False);
    tmpSubFile.Add(';Set.Time.Delay=15000', False);
    tmpSubFile.Add(';Set.Alpha.End=256', False);
    tmpSubFile.Add(';Set.Alpha.Start=256', False);
    tmpSubFile.Add(';Set.Alpha.Step=-300', False);
    tmpSubFile.Add(';Set.Font.Style.Bold=0', False);
    tmpSubFile.Add(';Set.Font.Color=#FFFFFF', False);
    tmpSubFile.Add(';Set.Font.Outline.Color=#00101010', False);
    tmpSubFile.Add(';Set.Font.Outline2.Color=#01101010', False);
    tmpSubFile.Add(';Set.Font.Size=20', False);
    tmpSubFile.Add(';Set.Font.Face=Tahoma', False);
    tmpSubFile.Add(';Buffer.Push=1', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpText := Subtitles.Text[i];

      tmpLines.Clear;

      if StringCount(#13#10, tmpText) > 0 then
      begin
        for a := 0 to StringCount(#13#10, tmpText) do
        begin
          if Pos(#13#10,tmpText) <> 0 then
            Aux := Copy(tmpText, 1, Pos(#13#10,tmpText)-1)
          else
            Aux := tmpText;

          tmpText := Copy(tmpText, Pos(#13#10,tmpText)+1,Length(tmpText));
          if Assigned(tmpLines) then
            tmpLines.Add(Aux);
        end;
      end
      else
        tmpLines.Add(tmpText);

      tmpSubFile.Add(';Buffer.Pop=1', False);
      tmpSubFile.Add(';Set.Time.Start=' + IntToStr(Subtitles.InitialTime[i]), False);
      
      for a := 0 to tmpLines.Count-1 do
      begin
        tmpSubFile.Add(';Set.Start.Position.x=160', False);
        tmpSubFile.Add(';Set.End.Position.x=160', False);
        tmpSubFile.Add(';Set.Start.Position.y=' + IntToStr(221 - (18 * (tmpLines.Count - a))), False);
        tmpSubFile.Add(';Set.End.Position.y=' + IntToStr(221 - (18 * (tmpLines.Count - a))), False);
        tmpSubFile.Add(tmpLines[a], False);
      end;

      tmpSubFile.Add(';Buffer.Pop=1', False);
      tmpSubFile.Add(';Set.Time.Start=' + IntToStr(Subtitles.FinalTime[i]), False);
      tmpSubFile.Add(';Set.Start.Position.x=160', False);
      tmpSubFile.Add(';Set.End.Position.x=160', False);
      tmpSubFile.Add(';Set.Start.Position.y=221', False);
      tmpSubFile.Add(';Set.End.Position.y=221', False);
      tmpSubFile.Add(' ', False);

    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    tmpLines.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SBT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  Text       : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      if StringCount(#13#10, Subtitles[i].Text) = 0 then
        Text := #13#10 + Subtitles[i].Text
      else if StringCount(#13#10, Subtitles[i].Text) = 1 then
        Text := Subtitles[i].Text
      else if StringCount(#13#10, Subtitles[i].Text) > 1 then
        Text := Copy(Subtitles[i].Text, 0, Pos(#13#10, Subtitles[i].Text) + 2) + ReplaceEnters(Copy(Subtitles[i].Text, Pos(#13#10, Subtitles[i].Text)+2, Length(Subtitles[i].Text)), ' ');

      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss'), False);
      tmpSubFile.Add(TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss'), False);
      tmpSubFile.Add(Text, False);
    end;
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SOFTNI(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add(MSToHHMMSSFFTime(Subtitles[i].InitialTime, FPS, ':', '.') + '\' + MSToHHMMSSFFTime(Subtitles[i].FinalTime, FPS, ':', '.'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SOFTITLERRTF(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile  : TSubtitleFile;
  i, Index, a : Integer;
  Text        : String;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 MS Sans Serif;}}', False);
    tmpSubFile.Add('{\colortbl\red0\green0\blue0;\red255\green255\blue255;}', False);
    tmpSubFile.Add('\deflang2058\pard\plain\f2\fs40\cf1 [1]', False);
    Index := 2;
    for i := From to UpTo do
    begin
      Text := RemoveSWTags(Subtitles.Text[i], True, True, True);
      Text := ReplaceString(Text, 'á', '\''e1', True, False);
      Text := ReplaceString(Text, 'é', '\''e9', True, False);
      Text := ReplaceString(Text, 'í', '\''ed', True, False);
      Text := ReplaceString(Text, 'ó', '\''f3', True, False);
      Text := ReplaceString(Text, 'ú', '\''fa', True, False);
      Text := ReplaceString(Text, 'ñ', '\''f1', True, False);
      Text := ReplaceString(Text, 'Á', '\''c1', True, False);
      Text := ReplaceString(Text, 'É', '\''c9', True, False);
      Text := ReplaceString(Text, 'Í', '\''cd', True, False);
      Text := ReplaceString(Text, 'Ó', '\''d3', True, False);
      Text := ReplaceString(Text, 'Ú', '\''da', True, False);
      Text := ReplaceString(Text, 'Ñ', '\''d1', True, False);
      Text := ReplaceString(Text, '{', '\{', True, False);
      Text := ReplaceString(Text, '{', '\}', True, False);
      Text := ReplaceString(Text, '¡', '\''a1', True, False);
      Text := ReplaceString(Text, '¿', '\''bf', True, False);
      Text := ReplaceString(Text, '"', '\''94', True, False);
      Text := ReplaceString(Text, 'û', '\''fb', True, False);
      Text := ReplaceString(Text, 'ä', '\''e4', True, False);
      Text := ReplaceString(Text, 'ù', '\''f9', True, False);       
      Text := ReplaceString(Text, 'û', '\''fb', True, False);
      Text := ReplaceString(Text, 'ä', '\''e4', True, False);
      Text := ReplaceString(Text, 'ê', '\''ea', True, False);
      Text := ReplaceString(Text, 'ç', '\''e7', True, False);
      Text := ReplaceString(Text, 'à', '\''e0', True, False);
      Text := ReplaceString(Text, 'è', '\''e8', True, False);
      Text := ReplaceString(Text, 'ù', '\''f9', True, False);
      Text := ReplaceString(Text, '£', '\''a3', True, False);
      Text := ReplaceString(Text, 'õ', '\''f5', True, False);
      Text := ReplaceString(Text, 'ü', '\''fc', True, False);
      Text := ReplaceString(Text, 'ö', '\''f6', True, False);
      Text := ReplaceString(Text, '×', '\''d7', True, False);
      Text := ReplaceString(Text, 'å', '\''e5', True, False);
      Text := ReplaceString(Text, 'ò', '\''f2', True, False);
      Text := ReplaceString(Text, 'î', '\''ee', True, False);

      if (i = 0) and (Subtitles[i].InitialTime <> 0) then
      begin
        tmpSubFile.Add('\par 00:00:00:00', False);
        tmpSubFile.Add('\par ' + TimeToString(Subtitles[i].InitialTime - 10, 'hh:mm:ss:zz'), False);
        tmpSubFile.Add('\par ', False);
        tmpSubFile.Add('\par [2]', False);
        Inc(Index);
      end;
      if i > 0 then
      begin
        tmpSubFile.Add('\par [' + IntToStr(Index) + ']', False);
        Inc(Index);
        tmpSubFile.Add('\par ' + TimeToString(Subtitles[i-1].FinalTime + 10, 'hh:mm:ss:zz'), False);
        tmpSubFile.Add('\par ' + TimeToString(Subtitles[i].InitialTime - 10, 'hh:mm:ss:zz'), False);
        tmpSubFile.Add('\par ', False);
        tmpSubFile.Add('\par [' + IntToStr(Index) + ']', False);
        Inc(Index);
      end;

      tmpSubFile.Add('\par ' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz'), False);
      tmpSubFile.Add('\par ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz'), False);

      a := Pos(#13#10, Text);
      while a > 0 do
      begin
        tmpSubFile.Add('\par ' + Copy(Text, 1, a-1), False);
        Text := Copy(Text, a+2, Length(Text));
        a := Pos(#13#10, Text);
      end;
      tmpSubFile.Add('\par ' + Text, False);

    end;

    tmpSubFile.Add('\par ', False);
    tmpSubFile.Add('\par }}', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SONICDVD(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  Indice     : String;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      Indice := IntToStr(i+1);
      while Length(Indice) < 4 do
        Insert('0', Indice, 0);
      tmpSubFile.Add(Indice + '  ' + MSToHHMMSSFFTime(Subtitles[i].InitialTime, FPS) + '  ' + MSToHHMMSSFFTime(Subtitles[i].FinalTime, FPS) + '  ' + Subtitles[i].Text, False);
      tmpSubFile.Add('', False);
    end;
    
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SONICSCENARIST(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  Indice     : String;
  sFPS       : String;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;

  if SonicScenaristAttributes.Assigned = False then
    with SonicScenaristAttributes do
    begin
      PAL       := True;
      Color0    := 3;
      Color1    := 4;
      Color2    := 3;
      Color3    := 9;
      Contrast0 := 0;
      Contrast1 := 15;
      Contrast2 := 15;
      Contrast3 := 15;
    end;

  if SonicScenaristAttributes.PAL = True then
    sFPS := 'PAL' else
    sFPS := 'NTSC';

  try
    tmpSubFile.Add('st_format 2', False);
    tmpSubFile.Add('Display_Start'+#9+'non_forced', False);
    tmpSubFile.Add('TV_Type'+#9#9+ sFPS, False);             // PAL o NTSC

    if (SonicScenaristAttributes.PAL = False) and
       (SonicScenaristAttributes.DropFrame = True) then
      tmpSubFile.Add('Tape_Type'+#9+'DROP', False) // NON_DROP o DROP
    else
      tmpSubFile.Add('Tape_Type'+#9+'NON_DROP', False);

    if SonicScenaristAttributes.PAL = True then
    begin
      tmpSubFile.Add('Pixel_Area'+#9+'(2 576)', False);         // 576 en PAL o 479 en NTSC
      tmpSubFile.Add('Display_Area'+#9+'(0 2 719 574)', False); // 574 en PAL o 479 en NTSC
    end
    else
    begin
      tmpSubFile.Add('Pixel_Area'+#9+'(2 479)', False);
      tmpSubFile.Add('Display_Area'+#9+'(0 2 719 479)', False);
    end;

    tmpSubFile.Add('Color'+#9+#9+ Format('(%d %d %d %d)',[SonicScenaristAttributes.Color3,SonicScenaristAttributes.Color2,SonicScenaristAttributes.Color1,SonicScenaristAttributes.Color0]), False);
    tmpSubFile.Add('Contrast'+#9+Format('(%d %d %d %d)',[SonicScenaristAttributes.Contrast3,SonicScenaristAttributes.Contrast2,SonicScenaristAttributes.Contrast1,SonicScenaristAttributes.Contrast0]), False);
    tmpSubFile.Add('E2'+#9+#9+'(0 0 255 ===)', False);
    tmpSubFile.Add('E1'+#9+#9+'(255 0 0 ===)', False);
    tmpSubFile.Add('PA'+#9+#9+'(0 0 0 ===)', False);
    tmpSubFile.Add('BG'+#9+#9+'(255 255 255 ===)', False);
    tmpSubFile.Add('Directory'+#9+'.', False);
    tmpSubFile.Add('#####################################################', False);
    tmpSubFile.Add('SP_NUMBER'+#9+'START'+#9#9+'END'+#9#9+'FILE_NAME', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      Indice := IntToStr(i+1);
      tmpSubFile.Add(Indice + #9#9 + MSToHHMMSSFFTime(Subtitles[i].InitialTime, FPS) + #9 + MSToHHMMSSFFTime(Subtitles[i].FinalTime, FPS) + #9 + ReplaceEnters(Subtitles[i].Text, ' '), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    if SonicScenaristAttributes.Assigned = False then
      with SonicScenaristAttributes do
      begin
        PAL       := False;
        Color0    := 0;
        Color1    := 0;
        Color2    := 0;
        Color3    := 0;
        Contrast0 := 0;
        Contrast1 := 0;
        Contrast2 := 0;
        Contrast3 := 0;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SPRUCEDVDMAESTRO(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
//const
//  SPFInfo : Array[0..63] of Byte =
//  ($80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $00,
//   $80, $80, $80, $00, $C0, $C0, $C0, $00, $00, $FF, $FF, $00, $FF, $00, $FF, $00,
//   $FF, $FF, $00, $00, $00, $00, $80, $00, $00, $80, $00, $00, $80, $00, $00, $00,
//   $00, $80, $80, $00, $80, $00, $80, $00, $80, $80, $00, $00, $FF, $FF, $FF, $00);
var
  tmpSubFile : TSubtitleFile;
  Indice     : String;
  i          : Integer;
//  Ext, FileNameSPF : String;
//  SPF              : TFileStream;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('st_format'+#9+'2', False);
    tmpSubFile.Add('Display_Start'+#9+'non_forced', False);
    tmpSubFile.Add('TV_Type'+#9+#9+'PAL', False);             // PAL o NTSC
    tmpSubFile.Add('Tape_Type'+#9+'NON_DROP', False);         // NON_DROP o DROP
    tmpSubFile.Add('Pixel_Area'+#9+'(0 573)', False);         // 573 en PAL o 477 en NTSC
    tmpSubFile.Add('Directory', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('SP_NUMBER' + #9 + 'START'+ #9#9 +'END'+ #9#9 +'FILE_NAME', False);
    tmpSubFile.Add('Color'+ #9 +'(0 1 2 3)', False);
    tmpSubFile.Add('Contrast'+ #9 +'(0 15 15 15)', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      Indice := PadLeft(IntToStr(i+1), '0', 4);

    // LA SIGUIENTE LINEA VA SOLO SI SE QUIERE GRABAR EL 'Extended Format'!
    //  tmpSubFile.Add('Display_Area	(000 000 719 573)', False); // 573 en PAL o 477 en NTSC
      tmpSubFile.Add(Indice + #9#9 + MSToHHMMSSFFTime(Subtitles[i].InitialTime, FPS) + #9 + MSToHHMMSSFFTime(Subtitles[i].FinalTime, FPS) + #9 + ReplaceString(Subtitles[i].Text, #13#10, ' '), False);
    end;

    // A CONTINUACION SE CREA EL ARCHIVO .SPF, PARA QUE? NPI!
{    Ext := ExtractFileExt(FileName);
    if  Ext <> '' then
      FileNameSPF := Copy(FileName, 1, Length(FileName)-Length(Ext))
    else
      FileNameSPF := FileName;

    FileNameSPF := FileNameSPF + '.spf';

    SPF := TFileStream.Create(FileNameSPF, fmCreate);
    try
      SPF.Seek(0, soFromBeginning);
      SPF.Write(SPFInfo, SizeOf(SPFInfo));
    finally
      SPF.Free;
    end;}

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SPRUCESUBTITLEFILE(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
//const
//  SPFInfo : Array[0..63] of Byte =
//  ($80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $00,
//   $80, $80, $80, $00, $C0, $C0, $C0, $00, $00, $FF, $FF, $00, $FF, $00, $FF, $00,
//   $FF, $FF, $00, $00, $00, $00, $80, $00, $00, $80, $00, $00, $80, $00, $00, $00,
//   $00, $80, $80, $00, $80, $00, $80, $00, $80, $80, $00, $00, $FF, $FF, $FF, $00);
var
  tmpSubFile         : TSubtitleFile;
  InitialTime        : String;
  FinalTime          : String;
  i                  : Integer;
//  Ext, FileNameSPF : String;
//  SPF              : TFileStream;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('//Font select and font size', False);
    tmpSubFile.Add('$FontName       = Arial', False);
    tmpSubFile.Add('$FontSize       = 30', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Character attributes (global)', False);
    tmpSubFile.Add('$Bold           = FALSE', False);
    tmpSubFile.Add('$UnderLined     = FALSE', False);
    tmpSubFile.Add('$Italic         = FALSE', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Position Control', False);
    tmpSubFile.Add('$HorzAlign      = Center', False);
    tmpSubFile.Add('$VertAlign      = Bottom', False);
    tmpSubFile.Add('$XOffset        = 0', False);
    tmpSubFile.Add('$YOffset        = 0', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Contrast Control', False);
    tmpSubFile.Add('$TextContrast           = 15', False);
    tmpSubFile.Add('$Outline1Contrast       = 8', False);
    tmpSubFile.Add('$Outline2Contrast       = 15', False);
    tmpSubFile.Add('$BackgroundContrast     = 0', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Effects Control', False);
    tmpSubFile.Add('$ForceDisplay   = FALSE', False);
    tmpSubFile.Add('$FadeIn         = 0', False);
    tmpSubFile.Add('$FadeOut        = 0', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Other Controls', False);
    tmpSubFile.Add('$TapeOffset          = FALSE', False);
    tmpSubFile.Add('//$SetFilePathToken  = <<:>>', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Colors', False);
    tmpSubFile.Add('$ColorIndex1    = 0', False);
    tmpSubFile.Add('$ColorIndex2    = 1', False);
    tmpSubFile.Add('$ColorIndex3    = 2', False);
    tmpSubFile.Add('$ColorIndex4    = 3', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('//Subtitles', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      // Time format is hh:mm:ss:ff
      InitialTime := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:') +
                     PadLeft(IntToStr(GetMSInFrames(Subtitles[i].InitialTime, FPS)), '0', 2, False);

      FinalTime := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:') +
                   PadLeft(IntToStr(GetMSInFrames(Subtitles[i].FinalTime, FPS)), '0', 2, False);

      tmpSubFile.Add(InitialTime + ',' + FinalTime + ',' + ReplaceEnters(Subtitles[i].Text, '|'), False);
    end;

{    // A CONTINUACION SE CREA EL ARCHIVO .SPF, PARA QUE? NPI!
    Ext := ExtractFileExt(FileName);
    if  Ext <> '' then
      FileNameSPF := Copy(FileName, 1, Length(FileName)-Length(Ext))
    else
      FileNameSPF := FileName;

    FileNameSPF := FileNameSPF + '.spf';

    SPF := TFileStream.Create(FileNameSPF, fmCreate);
    try
      SPF.Seek(0, soFromBeginning);
      SPF.Write(SPFInfo, SizeOf(SPFInfo));
    finally
      SPF.Free;
    end;}

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SSTPLAYER(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
  
    tmpSubFile.Add('SST 2.0.73', False);
    tmpSubFile.Add(';', False);
    tmpSubFile.Add(';This is a subtitles file.', False);
    tmpSubFile.Add(';Get the Stream Subtext Player at', False);
    tmpSubFile.Add(';http://hem.passagen.se/jmorones/Pages/StreamSubText.htm', False);
    tmpSubFile.Add(';', False);
    tmpSubFile.Add('[PROPERTIES]', False);
    tmpSubFile.Add('Title=Project title', False);
    tmpSubFile.Add('Author=Project author', False);
    tmpSubFile.Add('Copyright=Copyright', False);
    tmpSubFile.Add('Description=Project description', False);
    tmpSubFile.Add('Web=Web page', False);
    tmpSubFile.Add('MovieFile=Default.avi', False);
    tmpSubFile.Add('Width=640', False);
    tmpSubFile.Add('Height=480', False);
    tmpSubFile.Add('FrameRate=25.000', False);
    tmpSubFile.Add('Aspect=4:3', False);
    tmpSubFile.Add('PixelAspect=1.0000', False);
    tmpSubFile.Add('VideoStandard=PC', False);
    tmpSubFile.Add('BackColor=0,0,0', False);
    tmpSubFile.Add('Language=English', False);
    tmpSubFile.Add('Language2=', False);
    tmpSubFile.Add('Volume=0%', False);
    tmpSubFile.Add('FontSizePercent=0%', False);
    tmpSubFile.Add('BoxHeight=0', False);
    tmpSubFile.Add('TimeOffset=0', False);
    tmpSubFile.Add('AutoScaleText=0', False);
    tmpSubFile.Add('[SCRIPT]', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(IntToStr(Subtitles[i].InitialTime) + #9 + IntToStr(Subtitles[i].FinalTime) + #9 +
                     '{\rtf1\ansi\ansicpg1252\deff0\deflang14346{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil\fcharset0 MS Sans Serif;}}~{\colortbl ;\red255\green255\blue255;}~\viewkind4\uc1\pard\cf1\lang1252\fs24 ' +
                     ReplaceEnters(Subtitles.Text[i], '\cf0\f1\fs17 ~\par \cf1\f0\fs24 ') +
                     '\cf0\f1\fs17 ~\par \pard\lang14346\fs17 ~\par }~'
                     , False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SSTSCRIPT(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
  
    tmpSubFile.Add('SST 2.0.73', False);
    tmpSubFile.Add('[TITLES]', False);
    tmpSubFile.Add('fps=25.00', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(IntToStr(i+1) + #9 + 'Default' + #9 + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss.zz') + #9 + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss.zz') + #9 + ReplaceEnters(Subtitles[i].Text,'~') + #9 + 'SubRip', False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SUBCREATOR1X(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try
    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss.z') + ':' + ReplaceEnters(Subtitles[i].Text,'|'), False);
      tmpSubFile.Add(TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss.z') + ':');
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SUBRIP(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i, Count   : Integer;
begin
  Count  := 1;
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      tmpSubFile.Add(IntToStr(Count), False);
      Inc(Count);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss,zzz') + ' --> ' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss,zzz'), False);

      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        if WorkWithTags = False then
          Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True) else
        begin
          if Pos('<u>', Subtitles[i].Text) <> 0 then
            Subtitles.Text[i] := Subtitles[i].Text + '</u>';
          if Pos('<b>', Subtitles[i].Text) <> 0 then
            Subtitles.Text[i] := Subtitles[i].Text + '</b>';
          if Pos('<i>', Subtitles[i].Text) <> 0 then
            Subtitles.Text[i] := Subtitles[i].Text + '</i>';
          Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], False, False, False, True);
        end;
      {$IFNDEF VIPLAY}
      end;
      {$ENDIF}

      tmpSubFile.Add(Subtitles[i].Text, False);
      tmpSubFile.Add('', False);
    end;
    
    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SUBSONIC(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  tmpTiempo  : Single;
  i, c       : Integer;
  tmpNum     : String;
  dc         : Char;
begin
  c          := 1;
  Result     := True;
  dc         := DecimalSeparator;
  tmpSubFile := TSubtitleFile.Create;

  try
    DecimalSeparator := '.';

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      tmpTiempo := Subtitles[i].InitialTime / 1000;
      If tmpTiempo > (256*c) Then inc(c);

      tmpTiempo := tmpTiempo - (256*(c-1));
      tmpNum    := LimitDecimals(tmpTiempo, 2);

      if Pos('.',tmpNum) = 0 then
        tmpNum := tmpNum + '.00';
      if Length(Copy(tmpNum,Pos('.',tmpNum)+1,Length(tmpNum))) < 2 then
        tmpNum := tmpNum + '0';

      tmpSubFile.Add('1 ' + tmpNum + Format(' \ ~:\%s', [ReplaceEnters(Subtitles[i].Text, ' ')]), False);

      tmpTiempo := Subtitles[i].FinalTime / 1000;
      tmpTiempo := tmpTiempo - (256*(c-1));
      tmpNum    := LimitDecimals(tmpTiempo, 2);

      if Pos('.',tmpNum) = 0 then
        tmpNum := tmpNum + '.00';
      if Length(Copy(tmpNum,Pos('.',tmpNum) + 1, Length(tmpNum))) < 2 then
        tmpNum := tmpNum + '0';

      tmpSubFile.Add('1 ' + tmpNum, False);

    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    DecimalSeparator := dc;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SUBSTATIONALPHA(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
  const
    Format = 'Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding';

  function GetSSAStyleStr: String;
  begin
    Result := Format;
    Result := ReplaceString(Result, 'Format: Name',     'Style: Default');
    Result := ReplaceString(Result, ' Fontname',        SSAAttributes.FontName);
    Result := ReplaceString(Result, ' Fontsize',        IntToStr(SSAAttributes.FontSize));
    Result := ReplaceString(Result, ' PrimaryColour',   IntToStr(SSAAttributes.PrimaryColor));
    Result := ReplaceString(Result, ' SecondaryColour', IntToStr(SSAAttributes.SecondaryColor));
    Result := ReplaceString(Result, ' TertiaryColour',  IntToStr(SSAAttributes.TertiaryColor));
    Result := ReplaceString(Result, ' BackColour',      IntToStr(SSAAttributes.ShadowColor));
    Result := ReplaceString(Result, ' Bold',            BoolToStr(SSAAttributes.Bold));
    Result := ReplaceString(Result, ' Italic',          BoolToStr(SSAAttributes.Italic));
    Result := ReplaceString(Result, ' BorderStyle',     IntToStr(SSAAttributes.BorderStyle));
    Result := ReplaceString(Result, ' Outline',         IntToStr(SSAAttributes.Outline));
    Result := ReplaceString(Result, ' Shadow',          IntToStr(SSAAttributes.Shadow));
    Result := ReplaceString(Result, ' Alignment',       IntToStr(SSAAttributes.Alignment));
    Result := ReplaceString(Result, ' MarginL',         IntToStr(SSAAttributes.MarginL));
    Result := ReplaceString(Result, ' MarginR',         IntToStr(SSAAttributes.MarginR));
    Result := ReplaceString(Result, ' MarginV',         IntToStr(SSAAttributes.MarginV));
    Result := ReplaceString(Result, ' AlphaLevel',      '0');
    Result := ReplaceString(Result, ' Encoding',        IntToStr(SSAAttributes.Encoding));
  end;

var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  tmpCad     : String;
begin
  Result := True;

  if SSAAttributes.Assigned = False then
    with SSAAttributes do
    begin
      Title          := '<untitled>';
      Script         := '<unknown>';
      FontName       := 'Tahoma';
      FontSize       := 24;
      PrimaryColor   := 16777215;
      SecondaryColor := 16777215;
      TertiaryColor  := 16777215;
      ShadowColor    := 12632256;
      Bold           := True;
      Italic         := False;
      BorderStyle    := 1;
      Outline        := 1;
      Shadow         := 1;
      Alignment      := 6;
      MarginL        := 30;
      MarginR        := 30;
      MarginV        := 30;
      Encoding       := 0;
    end;

  tmpSubFile := TSubtitleFile.Create;
  try
    tmpSubFile.Add('[Script Info]', False);
    tmpSubFile.Add('Title: ' + SSAAttributes.Title, False);
    tmpSubFile.Add('Original Script: ' + SSAAttributes.Script, False);
    tmpSubFile.Add('ScriptType: v4.00', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('[V4 Styles]', False);
    tmpSubFile.Add(Format, False);
    tmpSubFile.Add(GetSSAStyleStr);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('[Events]', False);
    tmpSubFile.Add('Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text', False);

    for i := From to UpTo do
    begin
      tmpCad := Subtitles[i].Text;
      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        // No hay estilo para subrayado
        if WorkWithTags then
        begin
          if Pos('<b>',tmpCad) <> 0 then
            tmpCad := '{\b1}' + tmpCad + '{\b0}';
          if Pos('<i>',tmpCad) <> 0 then
            tmpCad := '{\i1}' + tmpCad + '{\i0}';
        end;
        tmpCad := RemoveSWTags(tmpCad, True, True, True);
      {$IFNDEF VIPLAY}
      end;
      {$ENDIF}

      tmpSubFile.Add('Dialogue: Marked=0,' + TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ',' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ',Default,NTP,0000,0000,0000,!Effect,'+ ReplaceEnters(tmpCad,'\N'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;

    if SSAAttributes.Assigned = False then
      with SSAAttributes do
      begin
        Title          := '';
        Script         := '';
        FontName       := '';
        FontSize       := 0;
        PrimaryColor   := 0;
        SecondaryColor := 0;
        TertiaryColor  := 0;
        ShadowColor    := 0;
        Bold           := False;
        Italic         := False;
        BorderStyle    := 0;
        Outline        := 0;
        Shadow         := 0;
        Alignment      := 0;
        MarginL        := 0;
        MarginR        := 0;
        MarginV        := 0;
        Encoding       := 0;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SUBVIEWER1(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  if SubViewer1Attributes.Assigned = False then
    with SubViewer1Attributes do
    begin
      Title    := '';
      Author   := '';
      Source   := '';
      Programa := '';
      Path     := '';
      Delay    := 0;
    end;
    
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('[TITLE]', False);
    tmpSubFile.Add(SubViewer1Attributes.Title, False);
    tmpSubFile.Add('[AUTHOR]', False);
    tmpSubFile.Add(SubViewer1Attributes.Author, False);
    tmpSubFile.Add('[SOURCE]', False);
    tmpSubFile.Add(SubViewer1Attributes.Source, False);
    tmpSubFile.Add('[PRG]', False);
    tmpSubFile.Add(SubViewer1Attributes.Programa, False);
    tmpSubFile.Add('[FILEPATH]', False);
    tmpSubFile.Add(SubViewer1Attributes.Path, False);
    tmpSubFile.Add('[DELAY]', False);
    tmpSubFile.Add(IntToStr(SubViewer1Attributes.Delay), False);
    tmpSubFile.Add('[CD TRACK]', False);
    tmpSubFile.Add('0', False);
    tmpSubFile.Add('[BEGIN]', False);
    tmpSubFile.Add('******** START SCRIPT ********', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      if (Subtitles[i].InitialTime = Subtitles[i].FinalTime) then
        Subtitles.FinalTime[i] := Subtitles[i].InitialTime + 1000;
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss') + ']', False);
      tmpSubFile.Add(ReplaceEnters(Subtitles[i].Text,'|'), False);
      tmpSubFile.Add('[' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss') + ']', False);
      tmpSubFile.Add('', False);
    end;

    tmpSubFile.Add('[end]', False);
    tmpSubFile.Add('******** END SCRIPT ********', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_SUBVIEWER2(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
  function GetSubViewer2FontStr(FontName: String; FontSize: Integer; Bold, Italic, Underline, StrikeOut: Boolean; Color: Integer): String;
  var
    StCol, StStyle, StSize, StFont : String;
  begin
    StCol := IntToHex((Color and $FFFFFF), 6);
    StStyle := 'no';
    If StrikeOut = True then StStyle := 'st';
    If Underline then StStyle := 'ud';
    If Bold = True then StStyle := 'bd';
    If Italic then StStyle := 'it';
    StSize := IntToStr(FontSize);
    StFont := FontName;
    Result := '[COLF]&H' + StCol + ',[STYLE]' + StStyle + ',[SIZE]' + StSize + ',[FONT]' + StFont;
  end;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
  T1,T2      : String;
begin
  if SubViewer2Attributes.Assigned = False then
    with SubViewer2Attributes do
    begin
      Title     := '';
      Author    := '';
      Source    := '';
      Programa  := '';
      Path      := '';
      Delay     := 0;
      CDTrack   := 0;
      Comment   := '';
      FontName  := 'Tahoma';
      FontSize  := 24;
      FontColor := 0;
      Bold      := True;
      Italic    := False;
      Underline := False;
      StrikeOut := False;
    end;

  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('[INFORMATION]', False);
    tmpSubFile.Add('[TITLE]' + SubViewer2Attributes.Title, False);
    tmpSubFile.Add('[AUTHOR]' + SubViewer2Attributes.Author, False);
    tmpSubFile.Add('[SOURCE]' + SubViewer2Attributes.Source, False);
    tmpSubFile.Add('[PRG]' + SubViewer2Attributes.Programa, False);
    tmpSubFile.Add('[FILEPATH]' + SubViewer2Attributes.Path, False);
    tmpSubFile.Add('[DELAY]' + IntToStr(SubViewer2Attributes.Delay), False);
    tmpSubFile.Add('[CD TRACK]' + IntToStr(SubViewer2Attributes.CDTrack), False);
    tmpSubFile.Add('[COMMENT]' + SubViewer2Attributes.Comment, False);
    tmpSubFile.Add('[END INFORMATION]', False);
    tmpSubFile.Add('[SUBTITLE]', False);
    tmpSubFile.Add(GetSubViewer2FontStr(SubViewer2Attributes.FontName,
                                        SubViewer2Attributes.FontSize,
                                        SubViewer2Attributes.Bold,
                                        SubViewer2Attributes.Italic,
                                        SubViewer2Attributes.Underline,
                                        SubViewer2Attributes.StrikeOut,
                                        SubViewer2Attributes.FontColor), False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      T1 := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss.zz');
      T2 := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss.zz');
      tmpSubFile.Add(T1 + ',' + T2, False);
      tmpSubFile.Add(ReplaceEnters(Subtitles[i].Text,'[br]'), False);
      tmpSubFile.Add('', False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;

    if SubViewer2Attributes.Assigned = False then
      with SubViewer2Attributes do
      begin
        Assigned  := True;
        Title     := '';
        Author    := '';
        Source    := '';
        Programa  := '';
        Path      := '';
        Delay     := 0;
        CDTrack   := 0;
        Comment   := '';
        FontName  := '';
        FontSize  := 0;
        FontColor := 0;
        Bold      := False;
        Italic    := False;
        Underline := False;
        StrikeOut := False;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_TMPLAYER(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile          : TSubtitleFile;
  i                   : Integer;
  T1,T2, Line1, Line2 : String;
begin
  if TMPlayerAttributes.Assigned = False then
    TMPlayerAttributes.TypeOfFormat := 0;

  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);

      if TMPlayerAttributes.TypeOfFormat = 0 then
      begin
        //********************************//
        //    TMPlayer Multiline Format   //
        //********************************//
        Line1 := '';
        Line2 := '';
        T1 := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss');
        T2 := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss');

        if Pos(#13#10,Subtitles[i].Text) <> 0 then
        begin
          Line1 := Copy(Subtitles[i].Text,0,Pos(#13#10,Subtitles[i].Text)-1);
          Line2 := ReplaceEnters(Copy(Subtitles[i].Text, Pos(#13#10,Subtitles[i].Text) + 2, Length(Subtitles[i].Text)),' ');
        end
        else
        begin
          Line1 := Subtitles[i].Text;
          Line2 := '';
        end;

        // Tiempo inicial
        tmpSubFile.Add(T1 + ',1=' + Line1, False);
        tmpSubFile.Add(T1 + ',2=' + Line2, False);
        // Tiempo final
        tmpSubFile.Add(T2 + ',1=', False);
        tmpSubFile.Add(T2 + ',2=', False);
      end
      else
      begin
        //********************************//
        //    TMPlayer Time Structure 1   //
        //********************************//
        if TMPlayerAttributes.TypeOfFormat = 1 then
        begin
          T1 := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss') + ':';
          T2 := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss') + ':';
        end
        else
        //********************************//
        //    TMPlayer Time Structure 2   //
        //********************************//
        if TMPlayerAttributes.TypeOfFormat = 2 then
        begin
          T1 := TimeToString(Subtitles[i].InitialTime, 'h:mm:ss') + ':';
          T2 := TimeToString(Subtitles[i].FinalTime, 'h:mm:ss') + ':';
        end
        else
        //********************************//
        //    TMPlayer+ Time Structure 1  //
        //********************************//
        if TMPlayerAttributes.TypeOfFormat = 3 then
        begin
          T1 := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss') + '=';
          T2 := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss') + '=';
        end
        else
        //********************************//
        //    TMPlayer+ Time Structure 2  //
        //********************************//
        if TMPlayerAttributes.TypeOfFormat = 4 then
        begin
          T1 := TimeToString(Subtitles[i].InitialTime, 'h:mm:ss') + '=';
          T2 := TimeToString(Subtitles[i].FinalTime, 'h:mm:ss') + '=';
        end;
        tmpSubFile.Add(T1 + ReplaceEnters(Subtitles[i].Text,'|'), False);
        tmpSubFile.Add(T2, False);
      end;
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
    if TMPlayerAttributes.Assigned then
      TMPlayerAttributes.TypeOfFormat := -1;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_TURBOTITLER(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ',' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ',NTP '+ ReplaceEnters(Subtitles[i].Text,'|'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_ULEADDVDWORKSHOP2(Subtitles: TSubtitles; const FileName: String; const FPS: Single; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile         : TSubtitleFile;
  i, count           : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('#Ulead subtitle format', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('#Subtitle stream attribute begin', False);
    tmpSubFile.Add('#FR:' + Format('%.2f', [FPS]), False);
    tmpSubFile.Add('#Subtitle stream attribute end', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('#Subtitle text begin', False);

    count := 0;
    for i := From to UpTo do
    begin
      // Time format is hh;mm;ss;ff
      tmpSubFile.Add('#' + IntToStr(count) + ' ' +
                     MSToHHMMSSFFTime(Subtitles.InitialTime[i], FPS, ';', ';') + ' ' +
                     MSToHHMMSSFFTime(Subtitles.FinalTime[i], FPS, ';', ';') + #13#10 + 
                     RemoveSWTags(Subtitles.Text[i], True, True, True)
                     );
      Inc(count);
    end;

    tmpSubFile.Add('#Subtitle text end', False);
    tmpSubFile.Add('', False);
    tmpSubFile.Add('#Subtitle text attribute begin', False);
    tmpSubFile.Add('#/R:1,' + IntToStr(count) + ' /FP:8  /FS:24', False);
    tmpSubFile.Add('#Subtitle text attribute end', False);

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_VIPLAY(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('{* VIPLAY SUBTITLE FILE *}', False);

    for i := From to UpTo do
    begin
      {$IFNDEF VIPLAY}
      if NoInteractionWithTags = False then
      begin
      {$ENDIF}
        if WorkWithTags = False then
          Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      {$IFNDEF VIPLAY}
      end;
      {$ENDIF}
      tmpSubFile.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss,zzz') + '-' + TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss,zzz') + '=' + ReplaceEnters(Subtitles[i].Text,'|'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function SubtitlesToFile_ZEROG(Subtitles: TSubtitles; const FileName: String; From: Integer = -1; UpTo: Integer = -1): Boolean;
var
  tmpSubFile : TSubtitleFile;
  i          : Integer;
begin
  Result := True;
  tmpSubFile := TSubtitleFile.Create;
  try

    tmpSubFile.Add('% Zero G 1.0', False);
    tmpSubFile.Add('', False);

    for i := From to UpTo do
    begin
      Subtitles.Text[i] := RemoveSWTags(Subtitles.Text[i], True, True, True);
      tmpSubFile.Add('E 1 ' + TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ' ' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ' Default NTP '+ ReplaceEnters(Subtitles[i].Text,'\n'), False);
    end;

    try
      tmpSubFile.SaveToFile(FileName);
    except
      Result := False;
    end;
  finally
    tmpSubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
