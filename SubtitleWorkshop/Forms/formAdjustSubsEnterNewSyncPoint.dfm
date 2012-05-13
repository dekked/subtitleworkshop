object frmEnterNewSyncPoint: TfrmEnterNewSyncPoint
  Left = 526
  Top = 197
  Width = 191
  Height = 178
  BorderIcons = []
  Caption = 'Enter new sync point'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlEnterTimes: TPanel
    Left = 8
    Top = 8
    Width = 169
    Height = 105
    TabOrder = 0
    object lblOldTime: TLabel
      Left = 8
      Top = 8
      Width = 43
      Height = 13
      Caption = 'Old time:'
    end
    object lblNewTime: TLabel
      Left = 8
      Top = 56
      Width = 48
      Height = 13
      Caption = 'New time:'
    end
    object tmeOldTime: TTimeMaskEdit
      Left = 8
      Top = 24
      Width = 105
      Height = 22
      ChangeTimeOnModify = True
      FPS = 25.000000000000000000
      MinTime = 0
      TabOrder = 0
      Time = 0
      TimeMode = tmTime
    end
    object tmeNewTime: TTimeMaskEdit
      Left = 8
      Top = 72
      Width = 105
      Height = 22
      ChangeTimeOnModify = True
      FPS = 25.000000000000000000
      MinTime = 0
      TabOrder = 1
      Time = 0
      TimeMode = tmTime
    end
  end
  object btnOk: TButton
    Left = 8
    Top = 120
    Width = 81
    Height = 25
    Caption = '&Ok'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 96
    Top = 120
    Width = 81
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
