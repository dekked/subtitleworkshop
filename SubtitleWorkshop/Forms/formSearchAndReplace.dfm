object frmSearchAndReplace: TfrmSearchAndReplace
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmSearchAndReplace'
  ClientHeight = 409
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 248
    Top = 192
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object pgeCtrl: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 180
    ActivePage = pgeSearch
    TabOrder = 0
    object pgeSearch: TTabSheet
      Caption = 'Search'
      object bvlSearch: TBevel
        Left = 8
        Top = 8
        Width = 297
        Height = 105
      end
      object edtTextToFind: TLabeledEdit
        Left = 16
        Top = 32
        Width = 281
        Height = 21
        EditLabel.Width = 60
        EditLabel.Height = 13
        EditLabel.Caption = 'Text to find:'
        TabOrder = 0
      end
      object btnSearch: TButton
        Left = 216
        Top = 120
        Width = 89
        Height = 25
        Caption = '&Search!'
        Default = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = btnSearchClick
      end
    end
    object pgeReplace: TTabSheet
      Caption = 'Search && Replace'
      ImageIndex = 1
      object bvlReplace: TBevel
        Left = 8
        Top = 8
        Width = 297
        Height = 105
      end
      object edtTextToFind2: TLabeledEdit
        Left = 16
        Top = 32
        Width = 281
        Height = 21
        EditLabel.Width = 60
        EditLabel.Height = 13
        EditLabel.Caption = 'Text to find:'
        TabOrder = 0
      end
      object btnReplace: TButton
        Left = 104
        Top = 120
        Width = 89
        Height = 25
        Caption = '&Replace'
        TabOrder = 3
        OnClick = btnReplaceClick
      end
      object btnReplaceAll: TButton
        Left = 200
        Top = 120
        Width = 105
        Height = 25
        Caption = 'Replace &all'
        Default = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = btnReplaceAllClick
      end
      object edtReplaceBy: TLabeledEdit
        Left = 16
        Top = 80
        Width = 281
        Height = 21
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'Replace by:'
        TabOrder = 1
      end
      object btnFindNext: TButton
        Left = 8
        Top = 120
        Width = 89
        Height = 25
        Caption = '&Find next'
        TabOrder = 2
        OnClick = btnFindNextClick
      end
    end
  end
  object btnMoreLess: TButton
    Left = 8
    Top = 192
    Width = 89
    Height = 25
    Caption = 'More'
    TabOrder = 1
    OnClick = btnMoreLessClick
  end
  object pnlExpand: TPanel
    Left = 8
    Top = 232
    Width = 321
    Height = 169
    BevelOuter = bvLowered
    TabOrder = 3
    object lblSearchForTextIn: TLabel
      Left = 8
      Top = 64
      Width = 88
      Height = 13
      Caption = 'Search for text in:'
    end
    object lblCharset: TLabel
      Left = 8
      Top = 120
      Width = 42
      Height = 13
      Caption = 'Charset:'
    end
    object rdoFromSelItem: TRadioButton
      Left = 8
      Top = 96
      Width = 305
      Height = 17
      Caption = 'From the selected item'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object rdoAllTheSubtitle: TRadioButton
      Left = 8
      Top = 80
      Width = 305
      Height = 17
      Caption = 'All the subtitle'
      TabOrder = 2
    end
    object chkCaseSensitive: TCheckBox
      Left = 8
      Top = 8
      Width = 305
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
    end
    object chkWholeWords: TCheckBox
      Left = 8
      Top = 24
      Width = 305
      Height = 17
      Caption = 'Match whole words'
      TabOrder = 1
    end
    object cmbCharset: TComboBox
      Left = 8
      Top = 136
      Width = 201
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      OnChange = cmbCharsetChange
    end
    object chkPreserveCase: TCheckBox
      Left = 8
      Top = 40
      Width = 305
      Height = 17
      Caption = 'Preserve case on replace'
      TabOrder = 5
    end
  end
end
