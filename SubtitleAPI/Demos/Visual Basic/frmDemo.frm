VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomctl.ocx"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "URUSoft Subtitle API Test"
   ClientHeight    =   5085
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   9360
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   339
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   624
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtShowAt 
      Height          =   315
      Left            =   2640
      TabIndex        =   14
      Top             =   4560
      Width           =   5055
   End
   Begin VB.ComboBox cboFPS 
      Height          =   315
      ItemData        =   "frmDemo.frx":0000
      Left            =   7800
      List            =   "frmDemo.frx":0016
      Style           =   2  'Dropdown List
      TabIndex        =   13
      Top             =   3120
      Width           =   1455
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Close"
      Height          =   495
      Left            =   7800
      TabIndex        =   11
      Top             =   1920
      Width           =   1455
   End
   Begin VB.CommandButton btnSaveAs 
      Caption         =   "Save as..."
      Height          =   495
      Left            =   7800
      TabIndex        =   10
      Top             =   1320
      Width           =   1455
   End
   Begin VB.CommandButton cmdShowFormats 
      Caption         =   "Show supported formats"
      Height          =   375
      Left            =   4560
      TabIndex        =   8
      Top             =   120
      Width           =   3135
   End
   Begin VB.CommandButton cmdShowAt 
      Caption         =   "Show At:"
      Height          =   375
      Left            =   120
      TabIndex        =   3
      Top             =   4560
      Width           =   1095
   End
   Begin VB.TextBox txtTime 
      Height          =   315
      Left            =   1320
      TabIndex        =   2
      Text            =   "500"
      Top             =   4560
      Width           =   1215
   End
   Begin VB.CommandButton cmdOpen 
      Caption         =   "Open..."
      Height          =   495
      Left            =   7800
      TabIndex        =   1
      Top             =   720
      Width           =   1455
   End
   Begin MSComctlLib.ListView lsvSubtitles 
      Height          =   2415
      Left            =   120
      TabIndex        =   0
      Top             =   720
      Width           =   7575
      _ExtentX        =   13361
      _ExtentY        =   4260
      View            =   3
      LabelEdit       =   1
      LabelWrap       =   -1  'True
      HideSelection   =   -1  'True
      FullRowSelect   =   -1  'True
      GridLines       =   -1  'True
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      NumItems        =   3
      BeginProperty ColumnHeader(1) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         Text            =   "Show"
         Object.Width           =   1720
      EndProperty
      BeginProperty ColumnHeader(2) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   1
         Text            =   "Hide"
         Object.Width           =   1720
      EndProperty
      BeginProperty ColumnHeader(3) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   2
         Text            =   "Text"
         Object.Width           =   9260
      EndProperty
   End
   Begin MSComDlg.CommonDialog dlgOpen 
      Left            =   7800
      Top             =   120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin MSComDlg.CommonDialog dlgSave 
      Left            =   8280
      Top             =   120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label lblFPS 
      AutoSize        =   -1  'True
      Caption         =   "FPS:"
      Height          =   195
      Left            =   7800
      TabIndex        =   12
      Top             =   2880
      Width           =   330
   End
   Begin VB.Label lblCurrentSubtitle 
      Alignment       =   2  'Center
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   855
      Left            =   240
      TabIndex        =   9
      Top             =   3480
      Width           =   7320
   End
   Begin VB.Line Line4 
      BorderColor     =   &H00808080&
      X1              =   512
      X2              =   8
      Y1              =   296
      Y2              =   296
   End
   Begin VB.Line Line3 
      BorderColor     =   &H00808080&
      X1              =   512
      X2              =   512
      Y1              =   216
      Y2              =   296
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      X1              =   8
      X2              =   8
      Y1              =   216
      Y2              =   296
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00FFFFFF&
      X1              =   512
      X2              =   8
      Y1              =   216
      Y2              =   216
   End
   Begin VB.Label lblCount 
      AutoSize        =   -1  'True
      Caption         =   "Lines count:"
      Height          =   195
      Left            =   2160
      TabIndex        =   7
      Top             =   360
      Width           =   870
   End
   Begin VB.Label lblFormat 
      AutoSize        =   -1  'True
      Caption         =   "Format:"
      Height          =   195
      Left            =   2160
      TabIndex        =   6
      Top             =   120
      Width           =   570
   End
   Begin VB.Label lblFormats 
      AutoSize        =   -1  'True
      Caption         =   "Supported formats:"
      Height          =   195
      Left            =   120
      TabIndex        =   5
      Top             =   360
      Width           =   1410
   End
   Begin VB.Label lblModuleVersion 
      AutoSize        =   -1  'True
      Caption         =   "Module version:"
      Height          =   195
      Left            =   120
      TabIndex        =   4
      Top             =   120
      Width           =   1140
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'----------------------------------------------------------------------------'
'                                                                            '
'          URUSoft Subtitle API - Visual Basic Example Application           '
'                                                                            '
'                       Copyright ® 2002-2003 URUSoft.                       '
'                                                                            '
'    This example is not intended to correctly load and save all subtitle    '
'    files but only to test the power and speed of URUSoft Subtitle API.     '
'                                                                            '
'----------------------------------------------------------------------------'
Option Explicit

' Subtitle API Variable
Dim SubtitleAPI As CSubtitleApi

Private Sub Form_Load()
  Dim APIVersion As String

  ' We initialize SubtitleAPI
  Set SubtitleAPI = New CSubtitleApi

  ' Set the filter of the open dialogue to all the supported formats, by using
  ' a function which is in CSubtitleAPI.cls
  dlgOpen.Filter = SubtitleAPI.FillDialogFilter
  dlgSave.Filter = SubtitleAPI.FillDialogFilter("") ' This time we don't have "all supported files" text

  ' Get the module version and add the corresponding dot to display it...
  APIVersion = Hex(SubtitleAPI.ModuleVersion)
  APIVersion = Left(APIVersion, 1) & "." & Right(APIVersion, 2)
  lblModuleVersion.Caption = "Module version: " & APIVersion

  ' We display the number of supported formats
  lblFormats.Caption = "Supported formats: " & SubtitleAPI.FormatsCount

  cboFPS.ListIndex = 3
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  Set SubtitleAPI = Nothing ' Free SubtitleAPI variable
End Sub

Private Sub cmdOpen_Click()
  Dim i As Long
  Dim SubtitleItem As ListItem

  On Error GoTo Err

  dlgOpen.ShowOpen

  ' First of all, clear the list...
  lsvSubtitles.ListItems.Clear

  ' By default we are not going to use the extension of the subtitle file to check
  ' it's format.
  ' If you specify a FormatNumber parameter the DLL will interpret that the subtitle
  ' file you are about to load is in the specified format, so no format checking
  ' will be performed. Useful if the automatic format recognition doesn't work for
  ' certain file.
  If SubtitleAPI.LoadSubtitle(dlgOpen.FileName, cboFPS.List(cboFPS.ListIndex)) = True Then
    ' Reverse the text of all subtitles, keeping lines order...
    SubtitleAPI.ReverseSubtitleText

    ' We have now the file in memory. Now we are going to play with it a bit...
    
    ' Insert subtitles...
    SubtitleAPI.InsertSubtitle 0, 100, 200, "TEST: INSERT FUNCTION"
    SubtitleAPI.InsertSubtitle 1, 300, 400, "WE HAVE REVERSED EACH SUBTITLE'S TEXT"
    SubtitleAPI.InsertSubtitle 2, 500, 600, "AND WE HAVE DELETED FOURTH SUBTITLE"

    SubtitleAPI.DeleteSubtitle (3) ' Delete fourth subtitle (zero-based)

    lblCount.Caption = "Lines count: " & SubtitleAPI.SubtitleCount
    lblFormat.Caption = "Format: " & SubtitleAPI.CurrentFormatName

    ' Add subtitles to the list...
    For i = 0 To SubtitleAPI.SubtitleCount - 1
      Set SubtitleItem = lsvSubtitles.ListItems.Add
      SubtitleItem.Text = SubtitleAPI.GetInitialTime(i) ' Get start time (milliseconds)
      SubtitleItem.SubItems(1) = SubtitleAPI.GetFinalTime(i) ' Get final time (milliseconds)
      SubtitleItem.SubItems(2) = SubtitleAPI.GetText(i) ' Get text
    Next i
  Else
    MsgBox "Unable to load: " & dlgOpen.FileName
  End If

Err:
End Sub

Private Sub btnSaveAs_Click()
  On Error GoTo Err

  dlgSave.ShowSave
  SubtitleAPI.SaveSubtitle dlgSave.FileName, dlgSave.FilterIndex, cboFPS.List(cboFPS.ListIndex)

Err:
End Sub

Private Sub cmdClose_Click()
  ' Close the current subtitle file (free it from memory)
  SubtitleAPI.CloseSubtitle
  lsvSubtitles.ListItems.Clear
  lblFormat.Caption = "Format:"
  lblCount.Caption = "Lines count:"
  lblCurrentSubtitle.Caption = ""
End Sub

Private Sub cmdShowFormats_Click()
  Dim i, x As Long
  Dim s As String
  
  i = SubtitleAPI.FormatsCount ' Total supported formats
  If i = 0 Then Exit Sub ' If we have no supported formats, exit sub
  
  s = ""
  For x = 1 To i
    If s = "" Then
      s = SubtitleAPI.GetFormatName(x)
    Else ' Get the format name of the current format index (x variable)
      s = s & ", " & SubtitleAPI.GetFormatName(x)
    End If
  Next x

  MsgBox s, vbInformation, "Supported formats"
End Sub

Private Sub cmdShowAt_Click()
  ' The GetSubtitleText function gets the text of the subtitle that is
  ' going to be displayed in certain time, this is very useful for movie
  ' players.
  txtShowAt.Text = SubtitleAPI.GetSubtitleText(txtTime.Text)
End Sub

Private Sub lsvSubtitles_ItemClick(ByVal Item As MSComctlLib.ListItem)
  lblCurrentSubtitle.Caption = Item.SubItems(2)
End Sub
