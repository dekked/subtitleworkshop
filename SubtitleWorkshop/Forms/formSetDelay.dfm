object frmSetDelay: TfrmSetDelay
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmSetDelay'
  ClientHeight = 128
  ClientWidth = 232
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
  object bvlDelay: TBevel
    Left = 8
    Top = 8
    Width = 217
    Height = 81
  end
  object btnApply: TButton
    Left = 8
    Top = 96
    Width = 129
    Height = 25
    Caption = '&Apply'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = btnApplyClick
  end
  object cmbDelayType: TComboBox
    Left = 16
    Top = 16
    Width = 71
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = '+'
    Items.Strings = (
      '+'
      '-')
  end
  object rdoAllSubs: TRadioButton
    Left = 16
    Top = 48
    Width = 199
    Height = 17
    Caption = 'For all the subtitles'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object rdoSelSubs: TRadioButton
    Left = 16
    Top = 64
    Width = 199
    Height = 17
    Caption = 'For the selected subtitles'
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 144
    Top = 96
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object tmeDelay: TTimeMaskEdit
    Left = 96
    Top = 16
    Width = 121
    Height = 22
    ChangeTimeOnModify = True
    FPS = 25.000000000000000000
    MinTime = 0
    TabOrder = 1
    Time = 0
    TimeMode = tmTime
  end
end
