object frmSAMILangExtractor: TfrmSAMILangExtractor
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmSAMILangExtractor'
  ClientHeight = 353
  ClientWidth = 424
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
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 409
    Height = 297
    TabOrder = 0
    object lblTip: TLabel
      Left = 8
      Top = 160
      Width = 199
      Height = 13
      Caption = 'The selected languages will be extracted:'
    end
    object btnBrowse1: TButton
      Left = 8
      Top = 48
      Width = 105
      Height = 25
      Caption = 'Browse...'
      TabOrder = 0
      OnClick = btnBrowse1Click
    end
    object edtSAMIFile: TLabeledEdit
      Left = 8
      Top = 24
      Width = 393
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'SAMI file:'
      TabOrder = 1
    end
    object edtOutputDir: TLabeledEdit
      Left = 8
      Top = 96
      Width = 393
      Height = 21
      EditLabel.Width = 84
      EditLabel.Height = 13
      EditLabel.Caption = 'Output directory:'
      TabOrder = 2
    end
    object btnBrowse2: TButton
      Left = 8
      Top = 120
      Width = 105
      Height = 25
      Caption = 'Browse...'
      TabOrder = 3
      OnClick = btnBrowse2Click
    end
    object btnAutoDetect: TButton
      Left = 24
      Top = 264
      Width = 177
      Height = 25
      Caption = 'Auto detect all'
      TabOrder = 4
      OnClick = btnAutoDetectClick
    end
    object lstLanguages: TListView
      Left = 8
      Top = 176
      Width = 393
      Height = 81
      Columns = <
        item
          Caption = 'Class'
          Width = 120
        end
        item
          Caption = 'Language'
          Width = 250
        end>
      GridLines = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 5
      ViewStyle = vsReport
    end
    object btnAddManually: TButton
      Left = 208
      Top = 264
      Width = 177
      Height = 25
      Caption = 'Add manually'
      TabOrder = 6
      OnClick = btnAddManuallyClick
    end
    object pnlPleaseWait: TPanel
      Left = 264
      Top = -8
      Width = 241
      Height = 81
      Caption = 'Please wait...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
    end
  end
  object btnExtract: TButton
    Left = 8
    Top = 312
    Width = 313
    Height = 33
    Caption = 'Extract!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnExtractClick
  end
  object btnCancel: TButton
    Left = 328
    Top = 312
    Width = 89
    Height = 33
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object dlgLoadFile: TOpenDialog
    Filter = 'SAMI Files (*.smi;*.sami)|*.smi;*.sami'
    Left = 376
    Top = 64
  end
end
