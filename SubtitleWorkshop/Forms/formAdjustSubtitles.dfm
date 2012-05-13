object frmAdjustSubtitles: TfrmAdjustSubtitles
  Left = 272
  Top = 141
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmAdjustSubtitles'
  ClientHeight = 327
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnAdjust: TButton
    Left = 40
    Top = 296
    Width = 137
    Height = 25
    Caption = '&Adjust!'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnAdjustClick
  end
  object btnCancel: TButton
    Left = 184
    Top = 296
    Width = 97
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object pgeMode: TPageControl
    Left = 8
    Top = 8
    Width = 305
    Height = 281
    ActivePage = pgeSimple
    TabOrder = 2
    object pgeSimple: TTabSheet
      Caption = 'Simple'
      object lblFirstSpokenLine: TLabel
        Left = 8
        Top = 8
        Width = 81
        Height = 13
        Caption = 'First spoken line:'
      end
      object lblLastSpokenLine: TLabel
        Left = 8
        Top = 56
        Width = 80
        Height = 13
        Caption = 'Last spoken line:'
      end
      object tmeFirstSpokenLine: TTimeMaskEdit
        Left = 8
        Top = 24
        Width = 97
        Height = 22
        ChangeTimeOnModify = True
        FPS = 25.000000000000000000
        MinTime = 0
        TabOrder = 0
        Time = 0
        TimeMode = tmTime
      end
      object tmeLastSpokenLine: TTimeMaskEdit
        Left = 8
        Top = 72
        Width = 97
        Height = 22
        ChangeTimeOnModify = True
        FPS = 25.000000000000000000
        MinTime = 0
        TabOrder = 1
        Time = 0
        TimeMode = tmTime
      end
    end
    object pgeAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object lblIfTimeOutsideScope: TLabel
        Left = 8
        Top = 184
        Width = 146
        Height = 13
        Caption = 'If time is outside points scope:'
      end
      object lstPoints: TVirtualStringTree
        Left = 8
        Top = 40
        Width = 281
        Height = 105
        Header.AutoSizeIndex = 0
        Header.Font.Charset = ANSI_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = 2
        Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
        Header.Style = hsPlates
        ScrollBarOptions.ScrollBars = ssVertical
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
        OnEditing = lstPointsEditing
        OnFreeNode = lstPointsFreeNode
        OnGetText = lstPointsGetText
        OnGetNodeDataSize = lstPointsGetNodeDataSize
        OnInitNode = lstPointsInitNode
        OnNewText = lstPointsNewText
        Columns = <
          item
            Position = 0
            Width = 33
            WideText = '#'
          end
          item
            Position = 1
            WideText = 'Line #'
          end
          item
            Position = 2
            Width = 97
            WideText = 'Old time'
          end
          item
            Position = 3
            Width = 97
            WideText = 'New time'
          end>
        WideDefaultText = '0'
      end
      object btnAdd: TButton
        Left = 8
        Top = 152
        Width = 81
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnRemove: TButton
        Left = 208
        Top = 152
        Width = 81
        Height = 25
        Caption = 'Remove'
        TabOrder = 2
        OnClick = btnRemoveClick
      end
      object rdoExtrapolate: TRadioButton
        Left = 8
        Top = 200
        Width = 289
        Height = 17
        Caption = 'Extrapolate'
        TabOrder = 3
      end
      object rdoReturnOrgTime: TRadioButton
        Left = 8
        Top = 216
        Width = 289
        Height = 17
        Caption = 'Return original time (no changes)'
        TabOrder = 4
      end
      object rdoNeighbour: TRadioButton
        Left = 8
        Top = 232
        Width = 289
        Height = 17
        Caption = 'Return displacement of neighbour point'
        TabOrder = 5
      end
      object btnLoadFromFile: TButton
        Left = 8
        Top = 8
        Width = 137
        Height = 25
        Caption = 'Load from file'
        TabOrder = 6
        OnClick = btnLoadFromFileClick
      end
      object btnSaveToFile: TButton
        Left = 152
        Top = 8
        Width = 137
        Height = 25
        Caption = 'Save to file'
        TabOrder = 7
        OnClick = btnSaveToFileClick
      end
      object btnAddFromMedia: TButton
        Left = 96
        Top = 152
        Width = 105
        Height = 25
        Caption = 'Add from media'
        TabOrder = 8
        OnClick = btnAddFromMediaClick
      end
    end
  end
  object dlgSaveToFile: TSaveDialog
    Filter = 'Sync Points File (*.spf)|*.spf'
    Left = 164
    Top = 120
  end
  object dlgLoadFromFile: TOpenDialog
    Filter = 'Sync Points File (*.spf)|*.spf'
    Left = 124
    Top = 120
  end
end
