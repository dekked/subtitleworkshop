object frmTimeExpanderReducer: TfrmTimeExpanderReducer
  Left = 222
  Top = 138
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmTimeExpanderReducer'
  ClientHeight = 337
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTimeExpander: TPanel
    Left = 8
    Top = 64
    Width = 241
    Height = 233
    TabOrder = 0
    object lblModifyDuration: TLabel
      Left = 8
      Top = 8
      Width = 120
      Height = 13
      Caption = 'Expand/reduce duration:'
    end
    object lblSecOrFrames: TLabel
      Left = 62
      Top = 28
      Width = 74
      Height = 13
      Caption = 'lblSecOrFrames'
    end
    object lblChars: TLabel
      Left = 88
      Top = 94
      Width = 38
      Height = 13
      Caption = 'lblChars'
    end
    object bvlSeparator: TBevel
      Left = 8
      Top = 56
      Width = 225
      Height = 9
      Shape = bsTopLine
    end
    object lblSecOrFrames2: TLabel
      Left = 86
      Top = 142
      Width = 74
      Height = 13
      Caption = 'lblSecOrFrames'
    end
    object edtTimeToExpand: TMaskEdit
      Left = 8
      Top = 24
      Width = 46
      Height = 21
      TabOrder = 0
      OnKeyPress = edtTimeToExpandKeyPress
    end
    object chkOnlyIfLongerThan: TCheckBox
      Left = 8
      Top = 72
      Width = 225
      Height = 17
      Caption = 'Only if subtitle is longer than:'
      TabOrder = 1
      OnClick = chkOnlyIfLongerThanClick
    end
    object edtChars: TEdit
      Left = 32
      Top = 90
      Width = 33
      Height = 21
      TabOrder = 2
      Text = '40'
    end
    object udChars: TUpDown
      Left = 65
      Top = 90
      Width = 15
      Height = 21
      Associate = edtChars
      Min = 10
      Max = 200
      Position = 40
      TabOrder = 3
    end
    object rdoSelSubs: TRadioButton
      Left = 8
      Top = 208
      Width = 225
      Height = 17
      Caption = 'For the selected subtitles'
      TabOrder = 4
    end
    object rdoAllSubs: TRadioButton
      Left = 8
      Top = 192
      Width = 225
      Height = 17
      Caption = 'For all the subtitles'
      Checked = True
      TabOrder = 5
      TabStop = True
    end
    object chkOnlyIfDuration: TCheckBox
      Left = 8
      Top = 120
      Width = 225
      Height = 17
      Caption = 'Only if duration is longer/shorter than:'
      TabOrder = 6
      OnClick = chkOnlyIfDurationClick
    end
    object edtMinMaxDuration: TMaskEdit
      Left = 32
      Top = 138
      Width = 46
      Height = 21
      TabOrder = 7
      OnKeyPress = edtMinMaxDurationKeyPress
    end
    object chkPreventOverlapping: TCheckBox
      Left = 8
      Top = 168
      Width = 225
      Height = 17
      Caption = 'Prevent overlapping'
      TabOrder = 8
    end
  end
  object btnApply: TButton
    Left = 80
    Top = 304
    Width = 81
    Height = 25
    Caption = '&Apply'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 168
    Top = 304
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 241
    Height = 49
    TabOrder = 3
    object rdoExpandDuration: TRadioButton
      Left = 8
      Top = 8
      Width = 225
      Height = 17
      Caption = 'Expand duration'
      TabOrder = 0
      OnClick = rdoExpandDurationClick
    end
    object rdoReduceDuration: TRadioButton
      Left = 8
      Top = 24
      Width = 225
      Height = 17
      Caption = 'Reduce duration'
      TabOrder = 1
      OnClick = rdoExpandDurationClick
    end
  end
end
