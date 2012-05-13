object frmDivideLines: TfrmDivideLines
  Left = 274
  Top = 204
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Divide lines'
  ClientHeight = 352
  ClientWidth = 554
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
  object btnDivide: TButton
    Left = 376
    Top = 320
    Width = 81
    Height = 25
    Caption = '&Divide!'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = btnDivideClick
  end
  object btnCancel: TButton
    Left = 464
    Top = 320
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlDivideLines: TPanel
    Left = 8
    Top = 8
    Width = 541
    Height = 305
    TabOrder = 2
    object lblDivideAfterLineNumber: TLabel
      Left = 8
      Top = 8
      Width = 118
      Height = 13
      Caption = 'Divide after line number:'
    end
    object Bevel1: TBevel
      Left = 8
      Top = 192
      Width = 525
      Height = 3
    end
    object lblShowSub1: TLabel
      Left = 8
      Top = 89
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Show:'
    end
    object lblHideSub1: TLabel
      Left = 8
      Top = 113
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Hide:'
    end
    object lblShowSub2: TLabel
      Left = 8
      Top = 225
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Show:'
    end
    object lblHideSub2: TLabel
      Left = 8
      Top = 249
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Hide:'
    end
    object lblDuration1: TLabel
      Left = 8
      Top = 137
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Duration:'
    end
    object lblDuration2: TLabel
      Left = 8
      Top = 273
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Duration:'
    end
    object lblLength1: TLabel
      Left = 184
      Top = 72
      Width = 49
      Height = 13
      Caption = 'lblLength1'
    end
    object lblLength2: TLabel
      Left = 184
      Top = 208
      Width = 49
      Height = 13
      Caption = 'lblLength2'
    end
    object edtDivideAfterBreakNum: TEdit
      Left = 16
      Top = 24
      Width = 33
      Height = 21
      ReadOnly = True
      TabOrder = 2
      Text = '1'
    end
    object udDivideAfterBreakNum: TUpDown
      Left = 49
      Top = 24
      Width = 15
      Height = 21
      Associate = edtDivideAfterBreakNum
      Min = 1
      Position = 1
      TabOrder = 3
      OnChangingEx = udDivideAfterBreakNumChangingEx
    end
    object btn31: TButton
      Left = 422
      Top = 8
      Width = 33
      Height = 41
      Caption = '3:1'
      TabOrder = 10
      OnClick = btn31Click
    end
    object btn13: TButton
      Left = 389
      Top = 8
      Width = 33
      Height = 41
      Caption = '1:3'
      TabOrder = 9
      OnClick = btn13Click
    end
    object btn32: TButton
      Left = 348
      Top = 8
      Width = 33
      Height = 41
      Caption = '3:2'
      TabOrder = 8
      OnClick = btn32Click
    end
    object btn23: TButton
      Left = 315
      Top = 8
      Width = 33
      Height = 41
      Caption = '2:3'
      TabOrder = 7
      OnClick = btn23Click
    end
    object btn21: TButton
      Left = 274
      Top = 8
      Width = 33
      Height = 41
      Caption = '2:1'
      TabOrder = 6
      OnClick = btn21Click
    end
    object btn12: TButton
      Left = 241
      Top = 8
      Width = 33
      Height = 41
      Caption = '1:2'
      TabOrder = 5
      OnClick = btn12Click
    end
    object btn11: TButton
      Left = 184
      Top = 8
      Width = 49
      Height = 41
      Caption = '1:1'
      TabOrder = 4
      OnClick = btn11Click
    end
    object chkContinueDirectly: TCheckBox
      Left = 8
      Top = 168
      Width = 449
      Height = 17
      Caption = 'Continue directly'
      TabOrder = 12
      OnClick = chkContinueDirectlyClick
    end
    object mmoSub1: TMemo
      Left = 184
      Top = 88
      Width = 349
      Height = 70
      TabOrder = 0
      OnChange = mmoSub1Change
      OnKeyDown = mmoSub1KeyDown
    end
    object mmoSub2: TMemo
      Left = 184
      Top = 224
      Width = 349
      Height = 70
      TabOrder = 1
      OnChange = mmoSub2Change
      OnKeyDown = mmoSub2KeyDown
    end
    object chkUseAutoDur: TCheckBox
      Left = 8
      Top = 56
      Width = 449
      Height = 17
      Caption = 'Use automatic duration'
      TabOrder = 11
      OnClick = chkUseAutoDurClick
    end
    object tmeShowSub1: TTimeMaskEdit
      Left = 88
      Top = 88
      Width = 89
      Height = 22
      ChangeTimeOnModify = False
      Enabled = False
      FPS = 25.000000000000000000
      MinTime = 0
      ReadOnly = True
      TabOrder = 13
      Time = 0
      TimeMode = tmTime
    end
    object tmeHideSub1: TTimeMaskEdit
      Left = 88
      Top = 112
      Width = 89
      Height = 22
      ChangeTimeOnModify = False
      FPS = 25.000000000000000000
      MinTime = 0
      TabOrder = 14
      Time = 0
      TimeMode = tmTime
      OnTimeChangeFromEditOnly = tmeHideSub1TimeChangeFromEditOnly
    end
    object tmeDuration1: TTimeMaskEdit
      Left = 88
      Top = 136
      Width = 89
      Height = 22
      ChangeTimeOnModify = False
      FPS = 25.000000000000000000
      MinTime = 0
      TabOrder = 15
      Time = 0
      TimeMode = tmTime
      OnTimeChangeFromEditOnly = tmeDuration1TimeChangeFromEditOnly
    end
    object tmeShowSub2: TTimeMaskEdit
      Left = 88
      Top = 224
      Width = 89
      Height = 22
      ChangeTimeOnModify = False
      FPS = 25.000000000000000000
      MinTime = 0
      TabOrder = 16
      Time = 0
      TimeMode = tmTime
      OnTimeChangeFromEditOnly = tmeShowSub2TimeChangeFromEditOnly
    end
    object tmeHideSub2: TTimeMaskEdit
      Left = 88
      Top = 248
      Width = 89
      Height = 22
      ChangeTimeOnModify = False
      Enabled = False
      FPS = 25.000000000000000000
      MinTime = 0
      ReadOnly = True
      TabOrder = 17
      Time = 0
      TimeMode = tmTime
    end
    object tmeDuration2: TTimeMaskEdit
      Left = 88
      Top = 272
      Width = 89
      Height = 22
      ChangeTimeOnModify = False
      Enabled = False
      FPS = 25.000000000000000000
      MinTime = 0
      ReadOnly = True
      TabOrder = 18
      Time = 0
      TimeMode = tmTime
    end
  end
end
