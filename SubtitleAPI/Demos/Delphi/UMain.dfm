object frmMain: TfrmMain
  Left = 190
  Top = 105
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'URUSoft Subtitle API Test'
  ClientHeight = 337
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblFormat: TLabel
    Left = 144
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Format:'
  end
  object lblCount: TLabel
    Left = 144
    Top = 24
    Width = 60
    Height = 13
    Caption = 'Lines Count:'
  end
  object lblModuleVersion: TLabel
    Left = 8
    Top = 8
    Width = 76
    Height = 13
    Caption = 'Module version:'
  end
  object lblFormats: TLabel
    Left = 8
    Top = 24
    Width = 94
    Height = 13
    Caption = 'Supported formats:'
  end
  object lblFPS: TLabel
    Left = 520
    Top = 192
    Width = 22
    Height = 13
    Caption = 'FPS:'
  end
  object txtShowAt: TEdit
    Left = 176
    Top = 304
    Width = 337
    Height = 21
    TabOrder = 6
  end
  object cmdOpen: TButton
    Left = 520
    Top = 48
    Width = 97
    Height = 33
    Caption = 'Open...'
    TabOrder = 2
    OnClick = cmdOpenClick
  end
  object cmdClose: TButton
    Left = 520
    Top = 128
    Width = 97
    Height = 33
    Caption = 'Close'
    TabOrder = 3
    OnClick = cmdCloseClick
  end
  object cmdShowAt: TButton
    Left = 8
    Top = 304
    Width = 73
    Height = 25
    Caption = 'Show At:'
    TabOrder = 4
    OnClick = cmdShowAtClick
  end
  object txtTime: TEdit
    Left = 88
    Top = 304
    Width = 81
    Height = 21
    TabOrder = 5
    Text = '500'
  end
  object lsvSubtitles: TListView
    Left = 8
    Top = 48
    Width = 505
    Height = 161
    Columns = <
      item
        Caption = 'Show'
        Width = 65
      end
      item
        Caption = 'Hide'
        Width = 65
      end
      item
        Caption = 'Text'
        Width = 350
      end>
    ColumnClick = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = lsvSubtitlesSelectItem
  end
  object pnlCurrentSubtitle: TPanel
    Left = 8
    Top = 216
    Width = 505
    Height = 81
    TabOrder = 1
    object lblCurrentSubtitle: TLabel
      Left = 9
      Top = 16
      Width = 488
      Height = 57
      Alignment = taCenter
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object cmdShowFormats: TButton
    Left = 304
    Top = 8
    Width = 209
    Height = 25
    Caption = 'Show supported formats'
    TabOrder = 7
    OnClick = cmdShowFormatsClick
  end
  object cboFPS: TComboBox
    Left = 520
    Top = 208
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 8
    Text = '25'
    Items.Strings = (
      '15'
      '20'
      '23,976'
      '25'
      '29,97'
      '30')
  end
  object btnSaveAs: TButton
    Left = 520
    Top = 88
    Width = 97
    Height = 33
    Caption = 'Save as...'
    TabOrder = 9
    OnClick = btnSaveAsClick
  end
  object dlgOpen: TOpenDialog
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 520
    Top = 8
  end
  object dlgSave: TSaveDialog
    Left = 552
    Top = 8
  end
end
