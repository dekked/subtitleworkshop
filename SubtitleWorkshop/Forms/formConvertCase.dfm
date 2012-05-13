object frmConvertCase: TfrmConvertCase
  Left = 312
  Top = 150
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmConvertCase'
  ClientHeight = 242
  ClientWidth = 256
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
  object btnOk: TButton
    Left = 79
    Top = 209
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
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 168
    Top = 209
    Width = 80
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlConvertCase: TPanel
    Left = 8
    Top = 8
    Width = 241
    Height = 137
    TabOrder = 2
    object rdoInverseType: TRadioButton
      Left = 8
      Top = 112
      Width = 225
      Height = 17
      Caption = 'iNVERSE tYPE'
      TabOrder = 4
      OnClick = rdoSentenceTypeClick
    end
    object rdoTitleType: TRadioButton
      Left = 8
      Top = 96
      Width = 225
      Height = 17
      Caption = 'Title Type'
      TabOrder = 3
      OnClick = rdoSentenceTypeClick
    end
    object rdoUpperCase: TRadioButton
      Left = 8
      Top = 80
      Width = 225
      Height = 17
      Caption = 'UPPERCASE'
      TabOrder = 2
      OnClick = rdoSentenceTypeClick
    end
    object rdoLowerCase: TRadioButton
      Left = 8
      Top = 64
      Width = 225
      Height = 17
      Caption = 'lowercase'
      TabOrder = 1
      OnClick = rdoSentenceTypeClick
    end
    object rdoSentenceType: TRadioButton
      Left = 8
      Top = 8
      Width = 225
      Height = 17
      Caption = 'Sentence type.'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rdoSentenceTypeClick
    end
    object chkDotsDetection: TCheckBox
      Left = 24
      Top = 40
      Width = 209
      Height = 17
      Caption = '"..." detection'
      TabOrder = 5
    end
    object chkOnlyFirstLetterOfFirstWord: TCheckBox
      Left = 24
      Top = 24
      Width = 209
      Height = 17
      Caption = 'Only first letter of first word'
      TabOrder = 6
    end
  end
  object pnlWhere: TPanel
    Left = 8
    Top = 152
    Width = 241
    Height = 49
    TabOrder = 3
    object rdoSelectedSubs: TRadioButton
      Left = 8
      Top = 24
      Width = 225
      Height = 17
      Caption = 'Selected subtitles only'
      TabOrder = 1
    end
    object rdoAllSubs: TRadioButton
      Left = 8
      Top = 8
      Width = 225
      Height = 17
      Caption = 'All subtitles'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
  end
end
