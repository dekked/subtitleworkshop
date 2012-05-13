VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CSubtitleApi"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
' URUSoft Subtitle API 1.0 WRAPPER for Visual Basic
' Copyright ® 2002-2003 URUSoft.

Option Explicit
Private Declare Function GetModuleVersion Lib "SubtitleAPI.dll" () As Long
Private Declare Sub GetModuleDescription Lib "SubtitleAPI.dll" (ByVal Text As String, ByRef BufferLen As Long)
Private Declare Function GetSupportedFormatsCount Lib "SubtitleAPI.dll" () As Long
Private Declare Function GetFormatInformation Lib "SubtitleAPI.dll" (ByVal Index As Long, ByVal Description As String, ByVal Extensions As String, ByRef BufferSize1 As Long, ByRef BufferSize2 As Long) As Boolean
Private Declare Sub GetFormatNameEx Lib "SubtitleAPI.dll" Alias "GetFormatName" (ByVal Index As Long, ByVal Name As String, ByRef BufferLen As Long)
Private Declare Function GetFormatIndexEx Lib "SubtitleAPI.dll" Alias "GetFormatIndex" (ByVal Name As String) As Long
Private Declare Function IsFrameBasedEx Lib "SubtitleAPI.dll" Alias "IsFrameBased" (ByVal FormatIndex As Long) As Boolean
Private Declare Function LoadSubtitleFile Lib "SubtitleAPI.dll" (ByVal FileName As String, ByVal FPS As Single, ByVal FormatIndex As Long, ByVal Append As Boolean, ByVal ReCalcTimeValues As Boolean) As Boolean
Private Declare Sub CreateNewSubtitleEx Lib "SubtitleAPI.dll" Alias "CreateNewSubtitle" ()
Private Declare Function GetFileFormatEx Lib "SubtitleAPI.dll" Alias "GetFileFormat" (ByVal FileName As String) As Long
Private Declare Function SaveSubtitleFile Lib "SubtitleAPI.dll" (ByVal FileName As String, ByVal FormatIndex As Long, ByVal FPS As Single, ByVal FromIndex As Long, ByVal ToIndex As Long) As Boolean
Private Declare Sub CloseSubtitleFile Lib "SubtitleAPI.dll" ()
Private Declare Function AddSubtitleEx Lib "SubtitleAPI.dll" Alias "AddSubtitle" (ByVal InitialTime As Long, ByVal FinalTime As Long, ByVal Text As String) As Long
Private Declare Function InsertSubtitleEx Lib "SubtitleAPI.dll" Alias "InsertSubtitle" (ByVal Index As Long, ByVal InitialTime As Long, ByVal FinalTime As Long, ByVal Text As String) As Boolean
Private Declare Function MoveSubtitleEx Lib "SubtitleAPI.dll" Alias "MoveSubtitle" (ByVal Index As Long, ByVal NewIndex As Long) As Boolean
Private Declare Function DeleteSubtitleEx Lib "SubtitleAPI.dll" Alias "DeleteSubtitle" (ByVal Index As Long) As Boolean
Private Declare Sub ClearSubtitlesEx Lib "SubtitleAPI.dll" Alias "ClearSubtitles" ()
Private Declare Function SetSubtitleEx Lib "SubtitleAPI.dll" Alias "SetSubtitle" (ByVal Index As Long, ByVal InitialTime As Long, ByVal FinalTime As Long, ByVal Text As String) As Boolean
Private Declare Function ReverseSubtitleTextEx Lib "SubtitleAPI.dll" Alias "ReverseSubtitleText" (ByVal FromIndex As Long, ByVal ToIndex As Long, ByVal KeepLinesOrder As Boolean) As Boolean
Private Declare Sub GetCurrentFormatEx Lib "SubtitleAPI.dll" Alias "GetCurrentFormat" (ByVal Name As String, ByRef Index As Long, ByRef BufferLen As Long)
Private Declare Function GetSubtitleCount Lib "SubtitleAPI.dll" () As Long
Private Declare Sub GetSubtitleTextEx Lib "SubtitleAPI.dll" Alias "GetSubtitleText" (ByVal Time As Long, ByVal Text As String, ByRef BuffeLen As Long)
Private Declare Function GetSubtitleEx Lib "SubtitleAPI.dll" Alias "GetSubtitle" (ByVal Index As Long, ByRef InitialTime As Long, ByRef FinalTime As Long, ByVal Text As String, ByRef BufferLen As Long) As Boolean
Private Declare Sub SetPlaybackDelay Lib "SubtitleAPI.dll" (ByVal Time As Long)
Private Declare Function GetPlaybackDelay Lib "SubtitleAPI.dll" () As Long
Private Declare Function SetAbsoluteDelayEx Lib "SubtitleAPI.dll" Alias "SetAbsoluteDelay" (ByVal Time As Long, ByVal FromIndex As Long, ByVal ToIndex As Long) As Boolean
Private Declare Function GetSubtitleWorkWithTags Lib "SubtitleAPI.dll" () As Boolean
Private Declare Sub SetSubtitleWorkWithTags Lib "SubtitleAPI.dll" (ByVal Value As Boolean)
Private Declare Sub SetOutputSettingsAdvancedSubStationAlphaEx Lib "SubtitleAPI.dll" Alias "SetOutputSettingsAdvancedSubStationAlpha" (ByVal Assigned As Boolean, ByVal Collisions As String, ByVal PlayResX As Long, ByVal PlayResY As Long, ByVal Timer As String, ByVal FontName As String, ByVal FontSize As Long, ByVal PrimaryColor As Long, ByVal SecundaryColor As Long, ByVal OutlineColor As Long, ByVal BackColor As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal StrikeOut As Boolean, ByVal ScaleX As Long, ByVal ScaleY As Long, ByVal Spacing As Long, ByVal Angle As Single, ByVal BorderStyle As Long, ByVal Outline As Long, ByVal Shadow As Long, ByVal Alignment As Long, ByVal MarginL As Long, ByVal MarginR As Long, ByVal MarginV As Long, ByVal Encoding As Long)
Private Declare Sub SetOutputSettingsDVDSubtitleEx Lib "SubtitleAPI.dll" Alias "SetOutputSettingsDVDSubtitle" (ByVal Assigned As Boolean, ByVal DiskId As String, ByVal DVDTitle As String, ByVal Language As String, ByVal Author As String, ByVal Web As String, ByVal Info As String, ByVal License As String)
Private Declare Sub SetOutputSettingsSAMIEx Lib "SubtitleAPI.dll" Alias "SetOutputSettingsSAMI" (ByVal Assigned As Boolean, ByVal FontName As String, ByVal FontSize As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal SubColor As Long, ByVal BackgroundColor As Long, ByVal Align As String)
Private Declare Sub SetOutputSettingsSonicScenaristEx Lib "SubtitleAPI.dll" Alias "SetOutputSettingsSonicScenarist" (ByVal Assigned As Boolean, ByVal PAL As Boolean, ByVal DropFrame As Boolean, ByVal Color0 As Long, ByVal Color1 As Long, ByVal Color2 As Long, ByVal Color3 As Long, ByVal Contrast0 As Long, ByVal Contrast1 As Long, ByVal Contrast2 As Long, ByVal Contrast3 As Long)
Private Declare Sub SetOutputSettingsSubViewer1Ex Lib "SubtitleAPI.dll" Alias "SetOutputSettingsSubViewer1" (ByVal Assigned As Boolean, ByVal Title As String, ByVal Author As String, ByVal Source As String, ByVal vProgram As String, ByVal Path As String, ByVal Delay As Long)
Private Declare Sub SetOutputSettingsSubViewer2Ex Lib "SubtitleAPI.dll" Alias "SetOutputSettingsSubViewer2" (ByVal Assigned As Boolean, ByVal Title As String, ByVal Author As String, ByVal Source As String, ByVal vProgram As String, ByVal Path As String, ByVal Delay As Long, ByVal CDTrack As Long, ByVal Comment As String, ByVal FontName As String, ByVal FontSize As Long, ByVal FontColor As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal StrikeOut As Boolean)
Private Declare Sub SetOutputSettingsSubStationAlphaEx Lib "SubtitleAPI.dll" Alias "SetOutputSettingsSubStationAlpha" (ByVal Assigned As Boolean, ByVal Title As String, ByVal Script As String, ByVal PrimaryColor As Long, ByVal SecundaryColor As Long, ByVal TertiaryColor As Long, ByVal ShadowColor As Long, ByVal VerticalMargin As Long, ByVal BottomMargin As Long, ByVal Border As Long, ByVal Shadow As Long, ByVal FontName As String, ByVal FontSize As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal StrikeOut As Boolean, ByVal Encoding As Long)
Private Declare Sub SetOutputSettingsTMPlayerEx Lib "SubtitleAPI.dll" Alias "SetOutputSettingsTMPlayer" (ByVal Assigned As Boolean, ByVal TypeOfFormat As Long)

Enum ETextAlign
  taLeft
  taRight
  taCenter
End Enum

Enum ETMPlayerFormat
  tfMultiline
  tfTimeStruct1
  tfTimeStruct2
  tfPlusTimeStruct1
  tfPlusTimeStruct2
End Enum

Private Function MyFileExists(ByVal Name As String) As Boolean
  On Error Resume Next
  MyFileExists = GetAttr(Name) And Not vbDirectory
End Function

Property Get ModuleVersion() As Long
  ModuleVersion = GetModuleVersion
End Property

Property Get Description() As String
  Dim l As Long
  l = 0
  
  GetModuleDescription Description, l
  If l > 0 Then
    Description = Space(l)
    GetModuleDescription Description, l
  End If
End Property

Property Get FormatsCount() As Long
  FormatsCount = GetSupportedFormatsCount
End Property

Function LoadSubtitle(ByVal FileName As String, ByVal FPS As Single, Optional ByVal FormatIndex As Long = 0, Optional ByVal Append As Boolean = False, Optional ByVal ReCalcTimeValues As Boolean = True) As Boolean
  LoadSubtitle = False
  
  If MyFileExists(FileName) = True Then
    LoadSubtitle = LoadSubtitleFile(FileName, FPS, FormatIndex, Append, ReCalcTimeValues)
  End If
End Function

Sub CreateNewSubtitle()
  CreateNewSubtitleEx
End Sub

Function GetFileFormat(ByVal FileName As String) As Long
  GetFileFormat = GetFileFormatEx(FileName)
End Function

Function SaveSubtitle(ByVal FileName As String, ByVal Format As Long, ByVal FPS As Single, Optional ByVal FromIndex As Long = -1, Optional ByVal ToIndex As Long = -1) As Boolean
  SaveSubtitle = SaveSubtitleFile(FileName, Format, FPS, FromIndex, ToIndex)
End Function

Function CloseSubtitle() As Boolean
  CloseSubtitleFile
  CloseSubtitle = True
End Function

Function AddSubtitle(ByVal InitialTime As Long, ByVal FinalTime As Long, ByVal Text As String) As Long
  AddSubtitle = AddSubtitleEx(InitialTime, FinalTime, Text)
End Function

Function InsertSubtitle(ByVal Index As Long, ByVal InitialTime As Long, ByVal FinalTime As Long, ByVal Text As String) As Boolean
  InsertSubtitle = InsertSubtitleEx(Index, InitialTime, FinalTime, Text)
End Function

Function MoveSubtitle(ByVal OldIndex As Long, ByVal NewIndex As Long) As Boolean
  MoveSubtitle = MoveSubtitleEx(OldIndex, NewIndex)
End Function

Function DeleteSubtitle(ByVal Index As Long) As Boolean
  DeleteSubtitle = DeleteSubtitleEx(Index)
End Function

Function ClearSubtitles() As Boolean
  ClearSubtitlesEx
  ClearSubtitles = True
End Function

Function GetSubtitleText(ByVal Time As Long) As String
  Dim l As Long
  l = 0

  GetSubtitleTextEx Time, GetSubtitleText, l
  If l > 0 Then
    GetSubtitleText = Space(l)
    GetSubtitleTextEx Time, GetSubtitleText, l
  End If
End Function

Function GetSubtitle(ByVal Index As Long, ByRef InitialTime As Long, ByRef FinalTime As Long, ByRef Text As String) As Boolean
  Dim l As Long
  l = 0

  If GetSubtitleEx(Index, InitialTime, FinalTime, Text, l) = True Then
    Text = Space(l)
    GetSubtitle = GetSubtitleEx(Index, InitialTime, FinalTime, Text, l) = True
  End If
End Function

Function GetInitialTime(ByVal Index As Long) As Long
  Dim FinalTime As Long, Text As String

  GetSubtitle Index, GetInitialTime, FinalTime, Text
End Function

Function GetFinalTime(ByVal SubNumber As Long) As Long
  Dim InitialTime As Long, Text As String

  GetSubtitle SubNumber, InitialTime, GetFinalTime, Text
End Function

Function GetText(ByVal Index As Long) As String
  Dim InitialTime As Long, FinalTime As Long

  GetSubtitle Index, InitialTime, FinalTime, GetText
End Function

Function SetSubtitle(ByVal Index As Long, ByVal InitialTime As Long, ByVal FinalTime As Long, ByVal Text As String) As Boolean
  SetSubtitle = SetSubtitleEx(Index, InitialTime, FinalTime, Text)
End Function

Property Get SubtitleCount() As Long
  SubtitleCount = GetSubtitleCount
End Property

Private Function GetCurrentFormat(ByRef Name As String, ByRef Index As Long) As Boolean
  Dim l As Long
  l = 0

  GetCurrentFormat = False

  GetCurrentFormatEx Name, Index, l
  If l > 0 Then
    Name = Space(l)
    GetCurrentFormatEx Name, Index, l
    GetCurrentFormat = True
  End If
End Function

Property Get CurrentFormatName() As String
  Dim Index As Long
  GetCurrentFormat CurrentFormatName, Index
End Property

Property Get CurrentFormatIndex() As Long
  Dim Name As String
  GetCurrentFormat Name, CurrentFormatIndex
End Property

Property Get PlaybackDelay() As Long
  PlaybackDelay = GetPlaybackDelay
End Property

Property Let PlaybackDelay(ByVal Time As Long)
  SetPlaybackDelay Time
End Property

Function SetAbsoluteDelay(ByVal Time As Long, Optional ByVal FromIndex As Long = -1, Optional ByVal ToIndex As Long = -1) As Boolean
  SetAbsoluteDelay = SetAbsoluteDelayEx(Time, FromIndex, ToIndex)
End Function

Property Get WorkWithTags() As Boolean
  WorkWithTags = GetSubtitleWorkWithTags
End Property

Property Let WorkWithTags(ByVal Value As Boolean)
  SetSubtitleWorkWithTags Value
End Property

Function ReverseSubtitleText(Optional ByVal FromIndex As Long = -1, Optional ByVal ToIndex As Long = -1, Optional ByVal KeepLinesOrder As Boolean = True) As Boolean
  ReverseSubtitleText = ReverseSubtitleTextEx(FromIndex, ToIndex, KeepLinesOrder)
End Function

Function GetFormatInfo(ByVal Index As Long, ByRef Description As String, ByRef Extensions As String) As Boolean
  Dim l1 As Long, l2 As Long
  l1 = 0
  l2 = 0
  
  If GetFormatInformation(Index, Description, Extensions, l1, l2) = True Then
    Description = Space(l1)
    Extensions = Space(l2)
    GetFormatInfo = GetFormatInformation(Index, Description, Extensions, l1, l2)
  End If
End Function

Function IsFrameBased(ByVal FormatIndex As Long) As Boolean
  IsFrameBased = IsFrameBasedEx(FormatIndex)
End Function

Function GetFormatName(ByVal Index As Long) As String
  Dim l As Long
  l = 0
  
  GetFormatNameEx Index, GetFormatName, l
  If l > 0 Then
    GetFormatName = Space(l)
    GetFormatNameEx Index, GetFormatName, l
  End If
End Function

Function GetFormatIndex(ByVal Name As String) As Long
  GetFormatIndex = GetFormatIndexEx(Name)
End Function

Function FillDialogFilter(Optional ByVal AllSupportedText As String = "All supported files") As String
  Dim Desc As String, Exts As String, AllExts As String
  Dim i As Long, FCount As Long
  
  FillDialogFilter = ""
  AllExts = ""
  FCount = GetSupportedFormatsCount
  If FCount = 0 Then Exit Function
  
  For i = 1 To FCount
    If GetFormatInfo(i, Desc, Exts) = True Then
      FillDialogFilter = FillDialogFilter & Desc & " (" & Exts & ")|" & Exts & "|"
      If InStr(1, AllExts, Exts) = 0 Then AllExts = AllExts & Exts & ";"
    End If
  Next i
  
  If AllSupportedText <> "" Then
    FillDialogFilter = AllSupportedText & " (" & AllExts & ")|" & AllExts & "|" & FillDialogFilter
  End If
End Function

Sub SetOutputAdvancedSettingsSubStationAlpha(ByVal Assigned As Boolean, ByVal Collisions As String, ByVal PlayResX As Long, ByVal PlayResY As Long, ByVal Timer As String, ByVal FontName As String, ByVal FontSize As Long, ByVal PrimaryColor As Long, ByVal SecundaryColor As Long, ByVal OutlineColor As Long, ByVal BackColor As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal StrikeOut As Boolean, ByVal ScaleX As Long, ByVal ScaleY As Long, ByVal Spacing As Long, ByVal Angle As Single, ByVal BorderStyle As Long, ByVal Outline As Long, ByVal Shadow As Long, ByVal Alignment As Long, ByVal MarginL As Long, ByVal MarginR As Long, ByVal MarginV As Long, ByVal Encoding As Long)
  SetOutputSettingsAdvancedSubStationAlphaEx Assigned, Collisions, PlayResX, PlayResY, Timer, FontName, FontSize, PrimaryColor, SecundaryColor, OutlineColor, BackColor, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
End Sub

Sub SetOutputSettingsDVDSubtitle(ByVal Assigned As Boolean, ByVal DiskId As String, ByVal DVDTitle As String, ByVal Language As String, ByVal Author As String, ByVal Web As String, ByVal Info As String, ByVal License As String)
  SetOutputSettingsDVDSubtitleEx Assigned, DiskId, DVDTitle, Language, Author, Web, Info, License
End Sub

Sub SetOutputSettingsSAMI(ByVal Assigned As Boolean, ByVal FontName As String, ByVal FontSize As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal SubColor As Long, ByVal BackgroundColor As Long, ByVal Align As ETextAlign)
  Dim AlignStr As String

  Select Case Align
    Case taLeft
      AlignStr = "Left"
    Case taRight
      AlignStr = "Right"
    Case Else
      AlignStr = "Center"
  End Select

  SetOutputSettingsSAMIEx Assigned, FontName, FontSize, Bold, Italic, Underline, SubColor, BackgroundColor, AlignStr
End Sub

Sub SetOutputSettingsSonicScenarist(ByVal Assigned As Boolean, ByVal PAL As Boolean, ByVal DropFrame As Boolean, ByVal Color0 As Long, ByVal Color1 As Long, ByVal Color2 As Long, ByVal Color3 As Long, ByVal Contrast0 As Long, ByVal Contrast1 As Long, ByVal Contrast2 As Long, ByVal Contrast3 As Long)
  SetOutputSettingsSonicScenaristEx Assigned, PAL, DropFrame, Color0, Color1, Color2, Color3, Contrast0, Contrast1, Contrast2, Contrast3
End Sub

Sub SetOutputSettingsSubViewer1(ByVal Assigned As Boolean, ByVal Title As String, ByVal Author As String, ByVal Source As String, ByVal vProgram As String, ByVal Path As String, ByVal Delay As Long)
  SetOutputSettingsSubViewer1Ex Assigned, Title, Author, Source, vProgram, Path, Delay
End Sub

Sub SetOutputSettingsSubViewer2(ByVal Assigned As Boolean, ByVal Title As String, ByVal Author As String, ByVal Source As String, ByVal vProgram As String, ByVal Path As String, ByVal Delay As Long, ByVal CDTrack As Long, ByVal Comment As String, ByVal FontName As String, ByVal FontSize As Long, ByVal FontColor As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal StrikeOut As Boolean)
  SetOutputSettingsSubViewer2Ex Assigned, Title, Author, Source, vProgram, Path, Delay, CDTrack, Comment, FontName, FontSize, FontColor, Bold, Italic, Underline, StrikeOut
End Sub

Sub SetOutputSettingsSubStationAlpha(ByVal Assigned As Boolean, ByVal Title As String, ByVal Script As String, ByVal PrimaryColor As Long, ByVal SecundaryColor As Long, ByVal TertiaryColor As Long, ByVal ShadowColor As Long, ByVal VerticalMargin As Long, ByVal BottomMargin As Long, ByVal Border As Long, ByVal Shadow As Long, ByVal FontName As String, ByVal FontSize As Long, ByVal Bold As Boolean, ByVal Italic As Boolean, ByVal Underline As Boolean, ByVal StrikeOut As Boolean, ByVal Encoding As Long)
  SetOutputSettingsSubStationAlphaEx Assigned, Title, Script, PrimaryColor, SecundaryColor, TertiaryColor, ShadowColor, VerticalMargin, BottomMargin, Border, Shadow, FontName, FontSize, Bold, Italic, Underline, StrikeOut, Encoding
End Sub

Sub SetOutputSettingsTMPlayer(ByVal Assigned As Boolean, ByVal TypeOfFormat As ETMPlayerFormat)
  SetOutputSettingsTMPlayerEx Assigned, TypeOfFormat
End Sub

Private Sub Class_Terminate()
  CloseSubtitle
End Sub
