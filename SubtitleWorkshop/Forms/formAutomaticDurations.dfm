object frmAutomaticDurations: TfrmAutomaticDurations
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmAutomaticDurations'
  ClientHeight = 280
  ClientWidth = 336
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
  object btnOk: TButton
    Left = 160
    Top = 248
    Width = 81
    Height = 25
    Caption = '&Ok'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = btnOkClick
  end
  object gbMilliseconds: TGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 105
    Caption = 'Milliseconds'
    TabOrder = 2
    object lblPerCharacter: TLabel
      Left = 88
      Top = 32
      Width = 65
      Height = 13
      Caption = 'per character'
    end
    object lblPerWord: TLabel
      Left = 88
      Top = 56
      Width = 43
      Height = 13
      Caption = 'per word'
    end
    object lblPerLine: TLabel
      Left = 88
      Top = 80
      Width = 35
      Height = 13
      Caption = 'per line'
    end
    object edtMSPerCharacter: TEdit
      Left = 24
      Top = 24
      Width = 41
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object udMSPerCharacter: TUpDown
      Left = 65
      Top = 24
      Width = 15
      Height = 21
      Associate = edtMSPerCharacter
      Max = 200
      TabOrder = 1
    end
    object edtMSPerWord: TEdit
      Left = 24
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 2
      Text = '0'
    end
    object udMSPerWord: TUpDown
      Left = 65
      Top = 48
      Width = 15
      Height = 21
      Associate = edtMSPerWord
      Max = 200
      TabOrder = 3
    end
    object edtMSPerLine: TEdit
      Left = 24
      Top = 72
      Width = 41
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object udMSPerLine: TUpDown
      Left = 65
      Top = 72
      Width = 15
      Height = 21
      Associate = edtMSPerLine
      Max = 200
      TabOrder = 5
    end
  end
  object btnCancel: TButton
    Left = 248
    Top = 248
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlWhere: TPanel
    Left = 8
    Top = 192
    Width = 321
    Height = 49
    TabOrder = 4
    object rdoSelectedSubs: TRadioButton
      Left = 8
      Top = 24
      Width = 305
      Height = 17
      Caption = 'Selected subtitles only'
      TabOrder = 1
    end
    object rdoAllSubs: TRadioButton
      Left = 8
      Top = 8
      Width = 305
      Height = 17
      Caption = 'All subtitles'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 120
    Width = 321
    Height = 65
    TabOrder = 3
    object rdoNewDurGreaterOrg: TRadioButton
      Left = 8
      Top = 24
      Width = 305
      Height = 17
      Caption = 'Only if new duration is greater than original'
      TabOrder = 1
    end
    object rdoAllCases: TRadioButton
      Left = 8
      Top = 8
      Width = 305
      Height = 17
      Caption = 'Apply new duration in all cases'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rdoNewDurSmallerOrg: TRadioButton
      Left = 8
      Top = 40
      Width = 305
      Height = 17
      Caption = 'Only if new duration is smaller than original'
      TabOrder = 2
    end
  end
end
