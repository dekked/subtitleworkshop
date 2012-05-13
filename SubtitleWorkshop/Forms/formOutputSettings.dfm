object frmOutputSettings: TfrmOutputSettings
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmOutputSettings'
  ClientHeight = 360
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    567
    360)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlSeparate1: TBevel
    Left = 8
    Top = 320
    Width = 559
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object tvFormats: TTreeView
    Left = 8
    Top = 8
    Width = 169
    Height = 305
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HideSelection = False
    HotTrack = True
    Indent = 19
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    OnClick = tvFormatsClick
    OnKeyUp = tvFormatsKeyUp
  end
  object pgeFormats: TPageControl
    Left = 184
    Top = 40
    Width = 385
    Height = 273
    ActivePage = pgeDVDSubtitle
    Style = tsButtons
    TabOrder = 1
    object pgeDVDSubtitle: TTabSheet
      Caption = 'DVDSubtitle (*.sub)'
      TabVisible = False
      object edtDVDSubtitleDiskId: TLabeledEdit
        Left = 88
        Top = 0
        Width = 281
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Disk ID:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object edtDVDSubtitleDVDTitle: TLabeledEdit
        Left = 88
        Top = 24
        Width = 281
        Height = 21
        EditLabel.Width = 47
        EditLabel.Height = 13
        EditLabel.Caption = 'DVD Title:'
        LabelPosition = lpLeft
        TabOrder = 1
      end
      object edtDVDSubtitleLanguage: TLabeledEdit
        Left = 88
        Top = 48
        Width = 281
        Height = 21
        EditLabel.Width = 51
        EditLabel.Height = 13
        EditLabel.Caption = 'Language:'
        LabelPosition = lpLeft
        TabOrder = 2
      end
      object edtDVDSubtitleAuthor: TLabeledEdit
        Left = 88
        Top = 72
        Width = 281
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Author:'
        LabelPosition = lpLeft
        TabOrder = 3
      end
      object edtDVDSubtitleWeb: TLabeledEdit
        Left = 88
        Top = 96
        Width = 281
        Height = 21
        EditLabel.Width = 26
        EditLabel.Height = 13
        EditLabel.Caption = 'Web:'
        LabelPosition = lpLeft
        TabOrder = 4
      end
      object edtDVDSubtitleInfo: TLabeledEdit
        Left = 88
        Top = 120
        Width = 281
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Info:'
        LabelPosition = lpLeft
        TabOrder = 5
      end
      object edtDVDSubtitleLicense: TLabeledEdit
        Left = 88
        Top = 144
        Width = 281
        Height = 21
        EditLabel.Width = 39
        EditLabel.Height = 13
        EditLabel.Caption = 'License:'
        LabelPosition = lpLeft
        TabOrder = 6
      end
    end
    object pgeSAMI: TTabSheet
      Caption = 'SAMI (*.smi)'
      ImageIndex = 3
      TabVisible = False
      object lblSAMISubtitle: TLabel
        Left = 0
        Top = 88
        Width = 40
        Height = 13
        Caption = 'Subtitle:'
      end
      object lblSAMIBackground: TLabel
        Left = 112
        Top = 88
        Width = 60
        Height = 13
        Caption = 'Background:'
      end
      object lblSAMIAlign: TLabel
        Left = 232
        Top = 32
        Width = 27
        Height = 13
        Caption = 'Align:'
      end
      object pnlSAMISubtitleColor: TPanel
        Left = 0
        Top = 104
        Width = 105
        Height = 33
        Color = 253436
        TabOrder = 0
        OnClick = pnlSAMISubtitleColorClick
      end
      object pnlSAMIBackgroundColor: TPanel
        Left = 112
        Top = 104
        Width = 105
        Height = 33
        Color = clBlack
        TabOrder = 1
        OnClick = pnlSAMIBackgroundColorClick
      end
      object rdoSAMILeft: TRadioButton
        Left = 232
        Top = 48
        Width = 145
        Height = 17
        Caption = 'Left'
        TabOrder = 2
        OnClick = rdoSAMILeftClick
      end
      object rdoSAMICenter: TRadioButton
        Left = 232
        Top = 64
        Width = 145
        Height = 17
        Caption = 'Center'
        TabOrder = 3
        OnClick = rdoSAMICenterClick
      end
      object rdoSAMIRight: TRadioButton
        Left = 232
        Top = 80
        Width = 145
        Height = 17
        Caption = 'Right'
        TabOrder = 4
        OnClick = rdoSAMIRightClick
      end
      object pnlSAMISample: TPanel
        Left = 0
        Top = 0
        Width = 225
        Height = 81
        Caption = 'SAMPLE'
        Color = clBlack
        Font.Charset = ANSI_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object btnSAMISetFont: TButton
        Left = 232
        Top = 0
        Width = 105
        Height = 25
        Caption = 'Set font'
        TabOrder = 6
        OnClick = btnSAMISetFontClick
      end
    end
    object pgeSonicScenarist: TTabSheet
      Caption = 'Sonic Scenarist (*.sst)'
      ImageIndex = 4
      TabVisible = False
      object lblScenaristColor0: TLabel
        Left = 0
        Top = 64
        Width = 169
        Height = 13
        AutoSize = False
        Caption = 'Color 0 (background):'
      end
      object lblScenaristColor1: TLabel
        Left = 0
        Top = 96
        Width = 169
        Height = 13
        AutoSize = False
        BiDiMode = bdRightToLeft
        Caption = 'Color 1 (Font):'
        ParentBiDiMode = False
      end
      object lblScenaristColor2: TLabel
        Left = 0
        Top = 128
        Width = 169
        Height = 13
        AutoSize = False
        Caption = 'Color 2 (outline):'
      end
      object lblScenaristColor3: TLabel
        Left = 0
        Top = 160
        Width = 169
        Height = 13
        AutoSize = False
        Caption = 'Color 3 (antialiasing):'
      end
      object lblScenaristPaletteNumber: TLabel
        Left = 144
        Top = 40
        Width = 129
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Palette color n'#176':'
      end
      object lblScenaristContrast: TLabel
        Left = 272
        Top = 40
        Width = 97
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Contrast'
      end
      object lblScenaristFramerate: TLabel
        Left = 0
        Top = 0
        Width = 54
        Height = 13
        Caption = 'Framerate:'
      end
      object seScenaristContrast0: TSpinEdit
        Left = 288
        Top = 56
        Width = 65
        Height = 22
        MaxValue = 15
        MinValue = 0
        TabOrder = 6
        Value = 0
      end
      object seScenaristContrast1: TSpinEdit
        Left = 288
        Top = 88
        Width = 65
        Height = 22
        MaxValue = 15
        MinValue = 0
        TabOrder = 7
        Value = 0
      end
      object seScenaristContrast2: TSpinEdit
        Left = 288
        Top = 120
        Width = 65
        Height = 22
        MaxValue = 15
        MinValue = 0
        TabOrder = 8
        Value = 0
      end
      object seScenaristContrast3: TSpinEdit
        Left = 288
        Top = 152
        Width = 65
        Height = 22
        MaxValue = 15
        MinValue = 0
        TabOrder = 9
        Value = 0
      end
      object cmbScenaristFPS: TComboBox
        Left = 0
        Top = 16
        Width = 137
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = '25.000 PAL'
        OnChange = cmbScenaristFPSChange
        Items.Strings = (
          '25.000 PAL'
          '30.000 NTSC')
      end
      object seScenaristColor0: TSpinEdit
        Left = 176
        Top = 56
        Width = 65
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 2
        Value = 1
      end
      object seScenaristColor1: TSpinEdit
        Left = 176
        Top = 88
        Width = 65
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 3
        Value = 1
      end
      object seScenaristColor2: TSpinEdit
        Left = 176
        Top = 120
        Width = 65
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 4
        Value = 1
      end
      object seScenaristColor3: TSpinEdit
        Left = 176
        Top = 152
        Width = 65
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 5
        Value = 1
      end
      object chkScenaristDropFrame: TCheckBox
        Left = 144
        Top = 16
        Width = 217
        Height = 17
        Caption = 'Drop frame'
        TabOrder = 1
      end
    end
    object pgeSubStationAlpha: TTabSheet
      Caption = 'SubStation Alpha (*.ssa)'
      ImageIndex = 6
      TabVisible = False
      object edtSSATitle: TLabeledEdit
        Left = 88
        Top = 0
        Width = 281
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Title:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object edtSSAScript: TLabeledEdit
        Left = 88
        Top = 24
        Width = 281
        Height = 21
        EditLabel.Width = 31
        EditLabel.Height = 13
        EditLabel.Caption = 'Script:'
        LabelPosition = lpLeft
        TabOrder = 1
      end
      object pgeCtrlSSA: TPageControl
        Left = 0
        Top = 56
        Width = 369
        Height = 209
        ActivePage = pgeCosmetics
        Style = tsFlatButtons
        TabOrder = 2
        object pgeCosmetics: TTabSheet
          Caption = 'Cosmetics'
          object lblSSAColor: TLabel
            Left = 280
            Top = 0
            Width = 81
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'Color'
          end
          object lblSSAPrimary: TLabel
            Left = 176
            Top = 21
            Width = 97
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Primary:'
            Transparent = True
          end
          object lblSSASecondary: TLabel
            Left = 176
            Top = 53
            Width = 97
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Secondary:'
            Transparent = True
          end
          object lblSSATertiary: TLabel
            Left = 176
            Top = 85
            Width = 97
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Tertiary:'
            Transparent = True
          end
          object lblSSAShadow: TLabel
            Left = 176
            Top = 117
            Width = 97
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Shadow:'
            Transparent = True
          end
          object lblSSABorderStyle: TLabel
            Left = 0
            Top = 112
            Width = 62
            Height = 13
            Caption = 'Border style:'
          end
          object pnlSSASample: TPanel
            Left = 0
            Top = 41
            Width = 169
            Height = 56
            Caption = 'SAMPLE'
            Color = clBlack
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object btnSSASetFont: TButton
            Left = 0
            Top = 9
            Width = 113
            Height = 25
            Caption = 'Set font'
            TabOrder = 1
            OnClick = btnSSASetFontClick
          end
          object pnlSSAShadow: TPanel
            Left = 280
            Top = 113
            Width = 81
            Height = 25
            Color = clWhite
            TabOrder = 2
            OnClick = pnlSSAShadowClick
          end
          object pnlSSATertiary: TPanel
            Left = 280
            Top = 81
            Width = 81
            Height = 25
            Color = clWhite
            TabOrder = 3
            OnClick = pnlSSATertiaryClick
          end
          object pnlSSASecondary: TPanel
            Left = 280
            Top = 49
            Width = 81
            Height = 25
            Color = clWhite
            TabOrder = 4
            OnClick = pnlSSASecondaryClick
          end
          object pnlSSAPrimary: TPanel
            Left = 280
            Top = 16
            Width = 81
            Height = 25
            Color = clWhite
            TabOrder = 5
            OnClick = pnlSSAPrimaryClick
          end
          object cmbSSABorderStyle: TComboBox
            Left = 0
            Top = 128
            Width = 177
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 6
          end
        end
        object pgeOthers: TTabSheet
          Caption = 'Others'
          ImageIndex = 1
          object lblSSAEncoding: TLabel
            Left = 192
            Top = 77
            Width = 47
            Height = 13
            Caption = 'Encoding:'
          end
          object lblSSAShadowPos: TLabel
            Left = 0
            Top = 124
            Width = 105
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Shadow:'
          end
          object lblSSAOutline: TLabel
            Left = 0
            Top = 100
            Width = 105
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Outline:'
          end
          object lblSSARightMargin: TLabel
            Left = 0
            Top = 44
            Width = 105
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Right margin:'
          end
          object lblSSALeftMargin: TLabel
            Left = 0
            Top = 20
            Width = 105
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Left margin:'
          end
          object lblSSAVerticalMargin: TLabel
            Left = 0
            Top = 68
            Width = 105
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Vertical margin:'
          end
          object lblSSAAlignment: TLabel
            Left = 192
            Top = 0
            Width = 51
            Height = 13
            Caption = 'Alignment:'
          end
          object cmbSSAEncoding: TComboBox
            Left = 192
            Top = 93
            Width = 113
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 0
          end
          object seSSAShadow: TSpinEdit
            Left = 112
            Top = 120
            Width = 65
            Height = 22
            MaxValue = 4
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
          object seSSAOutline: TSpinEdit
            Left = 112
            Top = 96
            Width = 65
            Height = 22
            MaxValue = 4
            MinValue = 0
            TabOrder = 2
            Value = 0
          end
          object seSSARightMargin: TSpinEdit
            Left = 112
            Top = 40
            Width = 65
            Height = 22
            MaxValue = 1000
            MinValue = 0
            TabOrder = 3
            Value = 0
          end
          object seSSALeftMargin: TSpinEdit
            Left = 112
            Top = 16
            Width = 65
            Height = 22
            MaxValue = 1000
            MinValue = 0
            TabOrder = 4
            Value = 0
          end
          object seSSAVerticalMargin: TSpinEdit
            Left = 112
            Top = 64
            Width = 65
            Height = 22
            MaxValue = 1000
            MinValue = 0
            TabOrder = 5
            Value = 0
          end
          object cmbSSAAlignment: TComboBox
            Left = 192
            Top = 16
            Width = 113
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 6
          end
          object cmbSSASubTopMidTitle: TComboBox
            Left = 192
            Top = 40
            Width = 113
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 7
            Items.Strings = (
              'SubTitle'
              'TopTitle'
              'MidTitle')
          end
        end
      end
    end
    object pgeSubViewer: TTabSheet
      Caption = 'SubViewer (*.sub)'
      ImageIndex = 7
      TabVisible = False
      object lblSubViewer1Delay: TLabel
        Left = 50
        Top = 126
        Width = 31
        Height = 13
        Alignment = taRightJustify
        Caption = 'Delay:'
      end
      object edtSubViewer1Title: TLabeledEdit
        Left = 88
        Top = 0
        Width = 281
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Title:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object edtSubViewer1Author: TLabeledEdit
        Left = 88
        Top = 24
        Width = 281
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Author:'
        LabelPosition = lpLeft
        TabOrder = 1
      end
      object edtSubViewer1Source: TLabeledEdit
        Left = 88
        Top = 48
        Width = 281
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Source:'
        LabelPosition = lpLeft
        TabOrder = 2
      end
      object edtSubViewer1Program: TLabeledEdit
        Left = 88
        Top = 72
        Width = 281
        Height = 21
        EditLabel.Width = 44
        EditLabel.Height = 13
        EditLabel.Caption = 'Program:'
        LabelPosition = lpLeft
        TabOrder = 3
      end
      object edtSubViewer1Path: TLabeledEdit
        Left = 88
        Top = 96
        Width = 281
        Height = 21
        EditLabel.Width = 26
        EditLabel.Height = 13
        EditLabel.Caption = 'Path:'
        LabelPosition = lpLeft
        TabOrder = 4
      end
      object seSubViewer1Delay: TSpinEdit
        Left = 88
        Top = 123
        Width = 57
        Height = 22
        MaxValue = 65535
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
    end
    object pgeSubViewer2: TTabSheet
      Caption = 'SubViewer 2 (*.sub)'
      ImageIndex = 8
      TabVisible = False
      object lblSubViewer2Delay: TLabel
        Left = 0
        Top = 126
        Width = 81
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Delay:'
      end
      object lblSubViewer2CDTrack: TLabel
        Left = 152
        Top = 126
        Width = 105
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'CD-Track:'
      end
      object edtSubViewer2Title: TLabeledEdit
        Left = 88
        Top = 0
        Width = 281
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Title:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object edtSubViewer2Author: TLabeledEdit
        Left = 88
        Top = 24
        Width = 281
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Author:'
        LabelPosition = lpLeft
        TabOrder = 1
      end
      object edtSubViewer2Source: TLabeledEdit
        Left = 88
        Top = 48
        Width = 281
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Source:'
        LabelPosition = lpLeft
        TabOrder = 2
      end
      object edtSubViewer2Program: TLabeledEdit
        Left = 88
        Top = 72
        Width = 281
        Height = 21
        EditLabel.Width = 44
        EditLabel.Height = 13
        EditLabel.Caption = 'Program:'
        LabelPosition = lpLeft
        TabOrder = 3
      end
      object edtSubViewer2Path: TLabeledEdit
        Left = 88
        Top = 96
        Width = 281
        Height = 21
        EditLabel.Width = 26
        EditLabel.Height = 13
        EditLabel.Caption = 'Path:'
        LabelPosition = lpLeft
        TabOrder = 4
      end
      object seSubViewer2Delay: TSpinEdit
        Left = 88
        Top = 123
        Width = 57
        Height = 22
        MaxValue = 65535
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
      object seSubViewer2CDTrack: TSpinEdit
        Left = 264
        Top = 123
        Width = 57
        Height = 22
        MaxValue = 65535
        MinValue = 0
        TabOrder = 6
        Value = 0
      end
      object edtSubViewer2Comment: TLabeledEdit
        Left = 88
        Top = 152
        Width = 281
        Height = 21
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'Comment:'
        LabelPosition = lpLeft
        TabOrder = 7
      end
      object btnSubViewer2SetFont: TButton
        Left = 8
        Top = 192
        Width = 113
        Height = 25
        Caption = 'Set font'
        TabOrder = 8
        OnClick = btnSubViewer2SetFontClick
      end
      object pnlSubViewer2Sample: TPanel
        Left = 128
        Top = 192
        Width = 241
        Height = 57
        Caption = 'SAMPLE'
        Color = clBlack
        Font.Charset = ANSI_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 9
      end
    end
    object pgeTMPlayer: TTabSheet
      Caption = 'TMPlayer (*.txt)'
      ImageIndex = 9
      TabVisible = False
      object gbTMPlayerFormat: TGroupBox
        Left = 0
        Top = 0
        Width = 369
        Height = 65
        Caption = 'TMPlayer Format'
        TabOrder = 0
      end
      object gbTMPlayerMasFormat: TGroupBox
        Left = 0
        Top = 72
        Width = 369
        Height = 65
        Caption = 'TMPlayer+ Format'
        TabOrder = 1
      end
      object gbTMPlayerMultilineFormat: TGroupBox
        Left = 0
        Top = 144
        Width = 369
        Height = 65
        Caption = 'TMPlayer Multiline Format'
        TabOrder = 2
        object lblTMPlayerMultiline: TLabel
          Left = 34
          Top = 40
          Width = 64
          Height = 13
          Caption = 'hh:mm:ss,2='
          OnClick = lblTMPlayerMultilineClick
        end
      end
      object rdoTMPlayerFormat2: TRadioButton
        Left = 16
        Top = 40
        Width = 345
        Height = 17
        Caption = 'h:mm:ss:'
        TabOrder = 3
      end
      object rdoTMPlayerFormat1: TRadioButton
        Left = 16
        Top = 23
        Width = 345
        Height = 17
        Caption = 'hh:mm:ss:'
        TabOrder = 4
      end
      object rdoTMPlayerPlusFormat2: TRadioButton
        Left = 16
        Top = 112
        Width = 345
        Height = 17
        Caption = 'h:mm:ss='
        TabOrder = 5
      end
      object rdoTMPlayerPlusFormat1: TRadioButton
        Left = 16
        Top = 95
        Width = 345
        Height = 17
        Caption = 'hh:mm:ss='
        TabOrder = 6
      end
      object rdoTMPlayerMultiline: TRadioButton
        Left = 16
        Top = 167
        Width = 345
        Height = 17
        Caption = 'hh:mm:ss,1='
        Checked = True
        TabOrder = 7
        TabStop = True
      end
    end
  end
  object btnOk: TButton
    Left = 392
    Top = 328
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
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 480
    Top = 328
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object pnlHeading: TPanel
    Left = 184
    Top = 8
    Width = 377
    Height = 25
    BevelInner = bvLowered
    Caption = 'Heading'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
  end
  object clrDlg: TColorDialog
    Left = 8
    Top = 328
  end
  object fntDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 40
    Top = 328
  end
end
