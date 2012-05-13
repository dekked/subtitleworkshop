object frmInfoErrors: TfrmInfoErrors
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmInfoErrors'
  ClientHeight = 428
  ClientWidth = 503
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
  object bvlInfoErrors: TBevel
    Left = 8
    Top = 8
    Width = 489
    Height = 381
  end
  object btnCheck: TButton
    Left = 16
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Check!'
    Default = True
    TabOrder = 0
    OnClick = btnCheckClick
  end
  object btnFixErrors: TButton
    Left = 16
    Top = 356
    Width = 105
    Height = 25
    Caption = 'Fix errors!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnFixErrorsClick
  end
  object chkConfirm: TCheckBox
    Left = 128
    Top = 359
    Width = 361
    Height = 17
    Caption = 'Confirm each deletion'
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 408
    Top = 396
    Width = 89
    Height = 25
    Cancel = True
    Caption = '&Ok'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 3
  end
  object btnSettings: TButton
    Left = 8
    Top = 396
    Width = 105
    Height = 25
    Caption = '&Settings'
    TabOrder = 4
    OnClick = btnSettingsClick
  end
  object lstErrors: TVirtualStringTree
    Left = 16
    Top = 48
    Width = 473
    Height = 301
    DefaultPasteMode = amInsertAfter
    EditDelay = 100
    Header.AutoSizeIndex = 4
    Header.Font.Charset = ANSI_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoRestrictDrag, hoVisible]
    Header.Style = hsPlates
    HintMode = hmHintAndDefault
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnDblClick = lstErrorsDblClick
    OnFreeNode = lstErrorsFreeNode
    OnGetText = lstErrorsGetText
    OnPaintText = lstErrorsPaintText
    OnGetNodeDataSize = lstErrorsGetNodeDataSize
    OnInitNode = lstErrorsInitNode
    Columns = <
      item
        Position = 0
        WideText = 'Subtitle'
      end
      item
        Position = 1
        Width = 80
        WideText = 'Type'
      end
      item
        Position = 2
        Width = 310
        WideText = 'Description'
      end>
  end
end
