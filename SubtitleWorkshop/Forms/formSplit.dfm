object frmSplit: TfrmSplit
  Left = 206
  Top = 113
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmSplit'
  ClientHeight = 489
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnSplit: TButton
    Left = 248
    Top = 456
    Width = 81
    Height = 25
    Caption = 'Split!'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnSplitClick
  end
  object btnCancel: TButton
    Left = 336
    Top = 456
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object chkRecalculate: TCheckBox
    Left = 8
    Top = 458
    Width = 233
    Height = 17
    Caption = 'Recalculate time values'
    TabOrder = 2
  end
  object pgeSplitMode: TPageControl
    Left = 8
    Top = 8
    Width = 409
    Height = 329
    ActivePage = pgeSimple
    Style = tsFlatButtons
    TabOrder = 3
    object pgeSimple: TTabSheet
      Caption = 'Simple'
      object rdoSelectedItem: TRadioButton
        Left = 0
        Top = 8
        Width = 257
        Height = 17
        Caption = 'Selected item'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rdoSelectedItemClick
      end
      object rdoItemNumber: TRadioButton
        Left = 0
        Top = 32
        Width = 257
        Height = 17
        Caption = 'Item number:'
        TabOrder = 1
        OnClick = rdoSelectedItemClick
      end
      object rdoGivenTime: TRadioButton
        Left = 0
        Top = 80
        Width = 257
        Height = 17
        Caption = 'Given time:'
        TabOrder = 3
        OnClick = rdoSelectedItemClick
      end
      object rdoGivenFrame: TRadioButton
        Left = 0
        Top = 128
        Width = 257
        Height = 17
        Caption = 'Given frame:'
        TabOrder = 4
        OnClick = rdoSelectedItemClick
      end
      object edtItemNumber: TEdit
        Left = 16
        Top = 48
        Width = 87
        Height = 21
        Enabled = False
        TabOrder = 2
        Text = '0'
      end
      object edtGivenFrame: TEdit
        Left = 16
        Top = 144
        Width = 87
        Height = 21
        Enabled = False
        TabOrder = 5
        Text = '0'
      end
      object rdoEndOfVideo: TRadioButton
        Left = 0
        Top = 176
        Width = 257
        Height = 17
        Caption = 'End of video:'
        TabOrder = 6
        OnClick = rdoSelectedItemClick
      end
      object edtEndOfVideo: TEdit
        Left = 16
        Top = 192
        Width = 281
        Height = 21
        Enabled = False
        TabOrder = 7
      end
      object gbNaming1: TGroupBox
        Left = 0
        Top = 224
        Width = 385
        Height = 73
        Caption = 'Naming'
        TabOrder = 8
        object lblPart1Ext: TLabel
          Left = 293
          Top = 22
          Width = 20
          Height = 13
          Caption = '.Ext'
        end
        object lblPart2Ext: TLabel
          Left = 293
          Top = 46
          Width = 20
          Height = 13
          Caption = '.Ext'
        end
        object edtNameFile1: TLabeledEdit
          Left = 72
          Top = 18
          Width = 217
          Height = 21
          EditLabel.Width = 33
          EditLabel.Height = 13
          EditLabel.Caption = 'Part 1:'
          LabelPosition = lpLeft
          TabOrder = 0
        end
        object edtNameFile2: TLabeledEdit
          Left = 72
          Top = 42
          Width = 217
          Height = 21
          EditLabel.Width = 33
          EditLabel.Height = 13
          EditLabel.Caption = 'Part 2:'
          LabelPosition = lpLeft
          TabOrder = 1
        end
      end
      object btnBrowse1: TButton
        Left = 304
        Top = 192
        Width = 89
        Height = 25
        Caption = 'Browse'
        TabOrder = 9
        OnClick = btnBrowse1Click
      end
      object tmeGivenTime: TTimeMaskEdit
        Left = 16
        Top = 96
        Width = 89
        Height = 22
        ChangeTimeOnModify = True
        FPS = 25.000000000000000000
        MinTime = 0
        TabOrder = 10
        Time = 0
        TimeMode = tmTime
      end
    end
    object pgeAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object rdoEndOfVideos: TRadioButton
        Left = 0
        Top = 192
        Width = 241
        Height = 17
        Caption = 'At the ends of videos'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        OnClick = rdoEqualInLinesClick
      end
      object rdoEqualInLines: TRadioButton
        Left = 0
        Top = 176
        Width = 241
        Height = 16
        Caption = 'Parts equal in lines'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = rdoEqualInLinesClick
      end
      object rdoEqualInTime: TRadioButton
        Left = 0
        Top = 160
        Width = 241
        Height = 16
        Caption = 'Parts equal in time'
        TabOrder = 2
        OnClick = rdoEqualInLinesClick
      end
      object edtNumberOfParts: TLabeledEdit
        Left = 248
        Top = 176
        Width = 81
        Height = 21
        EditLabel.Width = 82
        EditLabel.Height = 13
        EditLabel.Caption = 'Number of parts:'
        TabOrder = 3
        Text = '2'
      end
      object udNumberOfParts: TUpDown
        Left = 329
        Top = 176
        Width = 15
        Height = 21
        Associate = edtNumberOfParts
        Min = 2
        Max = 2
        Position = 2
        TabOrder = 4
        Thousands = False
        OnChangingEx = udNumberOfPartsChangingEx
      end
      object gbNaming2: TGroupBox
        Left = 0
        Top = 217
        Width = 401
        Height = 80
        Caption = 'Naming'
        TabOrder = 5
        object lblPlus: TLabel
          Left = 256
          Top = 47
          Width = 8
          Height = 13
          Caption = '+'
        end
        object lblAutoExt: TLabel
          Left = 360
          Top = 48
          Width = 20
          Height = 13
          Caption = '.Ext'
        end
        object edtPrefixName: TEdit
          Left = 16
          Top = 44
          Width = 233
          Height = 21
          TabOrder = 0
          OnChange = cmbSuffixNameChange
        end
        object chkAutoName: TCheckBox
          Left = 16
          Top = 24
          Width = 353
          Height = 17
          Caption = 'Auto-name the parts:'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = chkAutoNameClick
        end
        object cmbSuffixName: TComboBox
          Left = 272
          Top = 44
          Width = 81
          Height = 21
          ItemHeight = 0
          Sorted = True
          TabOrder = 2
          OnChange = cmbSuffixNameChange
        end
      end
      object lstSplitParts: TVirtualStringTree
        Left = 0
        Top = 8
        Width = 401
        Height = 145
        Header.AutoSizeIndex = 0
        Header.Font.Charset = ANSI_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
        Header.Style = hsFlatButtons
        TabOrder = 6
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        OnDblClick = lstSplitPartsDblClick
        OnFreeNode = lstSplitPartsFreeNode
        OnGetText = lstSplitPartsGetText
        OnPaintText = lstSplitPartsPaintText
        OnGetNodeDataSize = lstSplitPartsGetNodeDataSize
        OnInitNode = lstSplitPartsInitNode
        OnKeyDown = lstSplitPartsKeyDown
        OnNewText = lstSplitPartsNewText
        Columns = <
          item
            Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 0
            Width = 251
            WideText = 'Filename'
          end
          item
            Alignment = taCenter
            Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 1
            Width = 90
            WideText = 'Length'
          end
          item
            Alignment = taCenter
            Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
            Position = 2
            Width = 60
            WideText = 'Lines'
          end>
      end
    end
  end
  object pnlOutput: TPanel
    Left = 8
    Top = 344
    Width = 409
    Height = 105
    TabOrder = 4
    object lblOutputFormat: TLabel
      Left = 16
      Top = 56
      Width = 73
      Height = 13
      Caption = 'Output format:'
    end
    object cmbOutputFormat: TComboBox
      Left = 16
      Top = 72
      Width = 273
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = cmbOutputFormatChange
    end
    object edtOutputDirectory: TLabeledEdit
      Left = 16
      Top = 24
      Width = 289
      Height = 21
      EditLabel.Width = 84
      EditLabel.Height = 13
      EditLabel.Caption = 'Output directory:'
      TabOrder = 1
    end
    object btnBrowse2: TButton
      Left = 312
      Top = 24
      Width = 89
      Height = 25
      Caption = 'Browse'
      TabOrder = 2
      OnClick = btnBrowse2Click
    end
  end
  object dlgOpenAVI: TOpenDialog
    Filter = 'AVI Files (*.avi)|*.avi'
    Left = 320
    Top = 408
  end
  object dlgSavePart: TSaveDialog
    Filter = 'Subrip(*.srt)|*.srt|All Files(*.*)|*.*'
    Left = 352
    Top = 408
  end
end
