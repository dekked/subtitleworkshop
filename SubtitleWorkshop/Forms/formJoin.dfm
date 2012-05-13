object frmJoin: TfrmJoin
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmJoin'
  ClientHeight = 320
  ClientWidth = 560
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
  object btnJoin: TButton
    Left = 352
    Top = 288
    Width = 97
    Height = 25
    Caption = 'Join!'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnJoinClick
  end
  object btnCancel: TButton
    Left = 456
    Top = 288
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlJoin: TPanel
    Left = 8
    Top = 8
    Width = 545
    Height = 273
    TabOrder = 2
    object lblOutputFormat: TLabel
      Left = 8
      Top = 184
      Width = 73
      Height = 13
      Caption = 'Output format:'
    end
    object lblAddFiles: TLabel
      Left = 8
      Top = 8
      Width = 228
      Height = 13
      Caption = 'Add the subtitle files you wish to join (in order):'
    end
    object lblOutputFPS: TLabel
      Left = 224
      Top = 184
      Width = 59
      Height = 13
      Caption = 'Output FPS:'
    end
    object btnAdd: TButton
      Left = 8
      Top = 144
      Width = 81
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 96
      Top = 144
      Width = 81
      Height = 25
      Caption = 'Remove'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object chkLoadFile: TCheckBox
      Left = 8
      Top = 232
      Width = 369
      Height = 17
      Caption = 'Load file after joining and saving'
      TabOrder = 7
    end
    object cmbOutputFormat: TComboBox
      Left = 8
      Top = 200
      Width = 209
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 5
      OnChange = cmbOutputFormatChange
    end
    object chkRecalculate: TCheckBox
      Left = 8
      Top = 248
      Width = 369
      Height = 17
      Caption = 'Recalculate time values'
      TabOrder = 8
    end
    object lstFiles: TListView
      Left = 8
      Top = 24
      Width = 529
      Height = 113
      Columns = <
        item
          Caption = 'File name'
          Width = 150
        end
        item
          Caption = 'Format'
          Width = 100
        end
        item
          Caption = 'FPS'
        end
        item
          Caption = 'Size'
          Width = 45
        end
        item
          Caption = 'Movie fragment (Optional)'
          Width = 160
        end>
      DragMode = dmAutomatic
      GridLines = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDragDrop = lstFilesDragDrop
      OnDragOver = lstFilesDragOver
      OnKeyDown = lstFilesKeyDown
    end
    object cmbOutputFPS: TComboBox
      Left = 224
      Top = 200
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      OnChange = cmbOutputFPSChange
    end
    object btnClear: TButton
      Left = 184
      Top = 144
      Width = 81
      Height = 25
      Caption = 'Clear'
      TabOrder = 3
      OnClick = btnClearClick
    end
    object cmbFPS: TComboBox
      Left = 272
      Top = 144
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      OnChange = cmbOutputFPSChange
    end
    object btnSetMovieFrag: TButton
      Left = 384
      Top = 144
      Width = 153
      Height = 25
      Caption = '&Set movie fragment'
      TabOrder = 9
      OnClick = btnSetMovieFragClick
    end
    object btnClearMovieFrag: TButton
      Left = 384
      Top = 176
      Width = 153
      Height = 25
      Caption = 'C&lear movie fragment'
      TabOrder = 10
      OnClick = btnClearMovieFragClick
    end
  end
  object opnDlg: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 8
    Top = 288
  end
  object dlgSave: TSaveDialog
    OnClose = dlgSaveClose
    Filter = 'All files (*.*)|*.*'
    Left = 40
    Top = 288
  end
end
