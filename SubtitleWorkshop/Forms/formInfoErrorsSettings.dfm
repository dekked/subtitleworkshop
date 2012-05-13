object frmInfoErrorsSettings: TfrmInfoErrorsSettings
  Left = 192
  Top = 103
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmInfoErrorsSettings'
  ClientHeight = 456
  ClientWidth = 393
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
  object pgeCtrl: TPageControl
    Left = 8
    Top = 8
    Width = 377
    Height = 409
    ActivePage = pgeGeneral
    TabOrder = 0
    object pgeGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 3
      object lblOCRDefFile: TLabel
        Left = 8
        Top = 216
        Width = 90
        Height = 13
        Caption = 'OCR definition file:'
      end
      object bvlSep1: TBevel
        Left = 8
        Top = 32
        Width = 353
        Height = 3
        Shape = bsTopLine
      end
      object chkMarkErrorsInList: TCheckBox
        Left = 8
        Top = 40
        Width = 353
        Height = 17
        Caption = 'Mark errors in main form'#39's list'
        TabOrder = 0
      end
      object btnSetColor: TButton
        Left = 24
        Top = 64
        Width = 113
        Height = 25
        Caption = 'Set color...'
        TabOrder = 1
        OnClick = btnSetColorClick
      end
      object chkBold: TCheckBox
        Left = 24
        Top = 96
        Width = 337
        Height = 17
        Caption = 'Bold'
        TabOrder = 2
      end
      object chkItalic: TCheckBox
        Left = 24
        Top = 112
        Width = 337
        Height = 17
        Caption = 'Italic'
        TabOrder = 3
      end
      object chkUnderline: TCheckBox
        Left = 24
        Top = 128
        Width = 337
        Height = 17
        Caption = 'Underline'
        TabOrder = 4
      end
      object chkMarkOnLoad: TCheckBox
        Left = 8
        Top = 152
        Width = 353
        Height = 17
        Caption = 'Mark errors on load subtitle'
        TabOrder = 5
      end
      object chkFixOnLoad: TCheckBox
        Left = 8
        Top = 168
        Width = 353
        Height = 17
        Caption = 'Fix errors on load subtitle'
        TabOrder = 6
      end
      object btnEdit: TButton
        Left = 200
        Top = 232
        Width = 97
        Height = 25
        Caption = 'Edit'
        TabOrder = 7
        OnClick = btnEditClick
      end
      object chkFixOneUnitOverlap: TCheckBox
        Left = 8
        Top = 192
        Width = 353
        Height = 17
        Caption = 'Fix one unit overlap at load'
        TabOrder = 8
      end
      object cmbOCRFiles: TComboBox
        Left = 8
        Top = 232
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 9
      end
      object chkShowConfInMainForm: TCheckBox
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = 'Show confirmations in main form on fix'
        TabOrder = 10
      end
    end
    object pgeAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 4
      object lblMilliseconds: TLabel
        Left = 88
        Top = 128
        Width = 59
        Height = 13
        Caption = 'milliseconds.'
      end
      object lblMilliseconds2: TLabel
        Left = 280
        Top = 32
        Width = 59
        Height = 13
        Caption = 'milliseconds.'
      end
      object lblMilliseconds3: TLabel
        Left = 280
        Top = 80
        Width = 59
        Height = 13
        Caption = 'milliseconds.'
      end
      object lblCharacters: TLabel
        Left = 280
        Top = 128
        Width = 55
        Height = 13
        Caption = 'characters.'
      end
      object edtRepeatableChars: TLabeledEdit
        Left = 8
        Top = 24
        Width = 161
        Height = 21
        EditLabel.Width = 113
        EditLabel.Height = 13
        EditLabel.Caption = 'Repeatable characters:'
        TabOrder = 0
      end
      object edtProhibitedChars: TLabeledEdit
        Left = 8
        Top = 72
        Width = 161
        Height = 21
        EditLabel.Width = 106
        EditLabel.Height = 13
        EditLabel.Caption = 'Prohibited characters:'
        TabOrder = 1
      end
      object edtToleranceRepeatedSubs: TLabeledEdit
        Left = 8
        Top = 120
        Width = 57
        Height = 21
        EditLabel.Width = 158
        EditLabel.Height = 13
        EditLabel.Caption = 'Tolerance for repeated subtitles:'
        TabOrder = 2
        Text = '100'
      end
      object udToleranceRepeatedSubs: TUpDown
        Left = 65
        Top = 120
        Width = 16
        Height = 21
        Associate = edtToleranceRepeatedSubs
        Max = 700
        Position = 100
        TabOrder = 3
        Thousands = False
      end
      object edtSpaceAfterChars: TLabeledEdit
        Left = 8
        Top = 176
        Width = 153
        Height = 21
        EditLabel.Width = 114
        EditLabel.Height = 13
        EditLabel.Caption = 'Space after characters:'
        TabOrder = 4
      end
      object edtSpaceBeforeChars: TLabeledEdit
        Left = 8
        Top = 224
        Width = 153
        Height = 21
        EditLabel.Width = 122
        EditLabel.Height = 13
        EditLabel.Caption = 'Space before characters:'
        TabOrder = 5
      end
      object edtTooLongDuration: TLabeledEdit
        Left = 200
        Top = 24
        Width = 57
        Height = 21
        EditLabel.Width = 88
        EditLabel.Height = 13
        EditLabel.Caption = 'Too long duration:'
        TabOrder = 6
        Text = '6000'
      end
      object udTooLongDur: TUpDown
        Left = 257
        Top = 24
        Width = 16
        Height = 21
        Associate = edtTooLongDuration
        Min = 1000
        Max = 30000
        Position = 6000
        TabOrder = 7
        Thousands = False
      end
      object edtTooShortDuration: TLabeledEdit
        Left = 200
        Top = 72
        Width = 57
        Height = 21
        EditLabel.Width = 93
        EditLabel.Height = 13
        EditLabel.Caption = 'Too short duration:'
        TabOrder = 8
        Text = '700'
      end
      object udTooShortDur: TUpDown
        Left = 257
        Top = 72
        Width = 16
        Height = 21
        Associate = edtTooShortDuration
        Min = 1
        Max = 3000
        Position = 700
        TabOrder = 9
        Thousands = False
      end
      object edtTooLongLine: TLabeledEdit
        Left = 200
        Top = 120
        Width = 57
        Height = 21
        EditLabel.Width = 64
        EditLabel.Height = 13
        EditLabel.Caption = 'Too long line:'
        TabOrder = 10
        Text = '50'
      end
      object udTooLongLine: TUpDown
        Left = 257
        Top = 120
        Width = 15
        Height = 21
        Associate = edtTooLongLine
        Min = 20
        Max = 200
        Position = 50
        TabOrder = 11
        Thousands = False
      end
    end
    object pgeCheckFor: TTabSheet
      Caption = 'Check for'
      object bvlSep2: TBevel
        Left = 8
        Top = 48
        Width = 353
        Height = 9
        Shape = bsTopLine
      end
      object bvlSep3: TBevel
        Left = 8
        Top = 168
        Width = 353
        Height = 9
        Shape = bsTopLine
      end
      object chkCheckEmptySubtitles: TCheckBox
        Left = 8
        Top = 24
        Width = 353
        Height = 17
        Caption = 'Empty subtitles'
        TabOrder = 0
      end
      object chkCheckOverlapping: TCheckBox
        Left = 8
        Top = 56
        Width = 353
        Height = 17
        Caption = 'Overlapping subtitles'
        TabOrder = 1
      end
      object chkCheckBadValues: TCheckBox
        Left = 8
        Top = 72
        Width = 353
        Height = 17
        Caption = 'Bad values'
        TabOrder = 2
      end
      object chkCheckHearingImpaired: TCheckBox
        Left = 8
        Top = 176
        Width = 353
        Height = 17
        Caption = 'Hearing impaired subtitles'
        TabOrder = 3
      end
      object chkCheckTextBeforeColon: TCheckBox
        Left = 8
        Top = 192
        Width = 353
        Height = 17
        Caption = 'Text before colon (":")'
        TabOrder = 4
        OnClick = chkCheckTextBeforeColonClick
      end
      object chkCheckOnlyIfCapitalLetters: TCheckBox
        Left = 24
        Top = 208
        Width = 337
        Height = 17
        BiDiMode = bdLeftToRight
        Caption = 'Only if text is in capital letters'
        ParentBiDiMode = False
        TabOrder = 5
      end
      object chkCheckUnnecessaryDots: TCheckBox
        Left = 8
        Top = 224
        Width = 353
        Height = 17
        Caption = 'Unnecessary dots'
        TabOrder = 6
      end
      object chkCheckOverTwoLines: TCheckBox
        Left = 8
        Top = 144
        Width = 353
        Height = 17
        Caption = 'Subtitles over two lines'
        TabOrder = 7
      end
      object chkCheckProhibitedChars: TCheckBox
        Left = 8
        Top = 240
        Width = 353
        Height = 17
        Caption = 'Prohibited characters'
        TabOrder = 8
      end
      object chkCheckRepeatedChars: TCheckBox
        Left = 8
        Top = 256
        Width = 353
        Height = 17
        Caption = 'Repeated characters'
        TabOrder = 9
      end
      object chkCheckOCRErrors: TCheckBox
        Left = 8
        Top = 288
        Width = 353
        Height = 17
        Caption = 'OCR Errors'
        TabOrder = 11
      end
      object chkCheckRepeatedSubs: TCheckBox
        Left = 8
        Top = 272
        Width = 353
        Height = 17
        Caption = 'Repeated subtitles'
        TabOrder = 10
      end
      object chkCheckUnnecessarySpaces: TCheckBox
        Left = 8
        Top = 360
        Width = 353
        Height = 17
        Caption = 'Unnecessary spaces'
        TabOrder = 12
      end
      object chkCheckSpaceAfterCustomChars: TCheckBox
        Left = 8
        Top = 328
        Width = 353
        Height = 17
        Caption = 'Space after custom characters'
        TabOrder = 13
      end
      object chkCheckSpaceBeforeCustomChars: TCheckBox
        Left = 8
        Top = 344
        Width = 353
        Height = 17
        Caption = 'Space before custom characters'
        TabOrder = 14
      end
      object chkCheckTooLongDur: TCheckBox
        Left = 8
        Top = 96
        Width = 353
        Height = 17
        Caption = 'Too long durations'
        TabOrder = 15
      end
      object chkCheckTooShortDur: TCheckBox
        Left = 8
        Top = 112
        Width = 353
        Height = 17
        Caption = 'Too short durations'
        TabOrder = 16
      end
      object chkCheckTooLongLines: TCheckBox
        Left = 8
        Top = 128
        Width = 353
        Height = 17
        Caption = 'Too long lines'
        TabOrder = 17
      end
      object chkCheckLinesWithoutLetters: TCheckBox
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = 'Lines without letters'
        TabOrder = 18
      end
      object chkCheckOpnDlgInSubsWithOneLine: TCheckBox
        Left = 8
        Top = 312
        Width = 353
        Height = 17
        Caption = '"- " in subtitles with one line'
        TabOrder = 19
      end
    end
    object pgeFix: TTabSheet
      Caption = 'Fix'
      ImageIndex = 1
      object bvlSep4: TBevel
        Left = 8
        Top = 48
        Width = 353
        Height = 9
        Shape = bsTopLine
      end
      object bvlSep5: TBevel
        Left = 8
        Top = 120
        Width = 353
        Height = 9
        Shape = bsTopLine
      end
      object chkFixEmptySubtitles: TCheckBox
        Left = 8
        Top = 24
        Width = 353
        Height = 17
        Caption = 'Empty subtitles'
        TabOrder = 0
      end
      object chkFixOverlapping: TCheckBox
        Left = 8
        Top = 56
        Width = 353
        Height = 17
        Caption = 'Overlapping subtitles'
        TabOrder = 1
      end
      object chkFixBadValues: TCheckBox
        Left = 8
        Top = 72
        Width = 353
        Height = 17
        Caption = 'Bad values'
        TabOrder = 2
      end
      object chkFixHearingImpaired: TCheckBox
        Left = 8
        Top = 128
        Width = 353
        Height = 17
        Caption = 'Hearing impaired subtitles'
        TabOrder = 3
      end
      object chkFixTextBeforeColon: TCheckBox
        Left = 8
        Top = 144
        Width = 353
        Height = 17
        Caption = 'Text before colon (":")'
        TabOrder = 4
        OnClick = chkFixTextBeforeColonClick
      end
      object chkFixOnlyIfCapitalLetters: TCheckBox
        Left = 24
        Top = 160
        Width = 337
        Height = 17
        Caption = 'Only if text is in capital letters'
        TabOrder = 5
      end
      object chkFixUnnecessaryDots: TCheckBox
        Left = 8
        Top = 176
        Width = 353
        Height = 17
        Caption = 'Unnecessary dots'
        TabOrder = 6
      end
      object chkFixOverTwoLines: TCheckBox
        Left = 8
        Top = 96
        Width = 353
        Height = 17
        Caption = 'Subtitles over two lines'
        TabOrder = 7
      end
      object chkFixProhibitedChars: TCheckBox
        Left = 8
        Top = 192
        Width = 353
        Height = 17
        Caption = 'Prohibited characters'
        TabOrder = 8
      end
      object chkFixRepeatedChars: TCheckBox
        Left = 8
        Top = 208
        Width = 353
        Height = 17
        Caption = 'Repeated characters'
        TabOrder = 9
      end
      object chkFixRepeatedSubs: TCheckBox
        Left = 8
        Top = 224
        Width = 353
        Height = 17
        Caption = 'Repeated subtitles'
        TabOrder = 10
      end
      object chkFixOCRErrors: TCheckBox
        Left = 8
        Top = 240
        Width = 353
        Height = 17
        Caption = 'OCR Errors'
        TabOrder = 11
      end
      object chkFixUnnecessarySpaces: TCheckBox
        Left = 8
        Top = 312
        Width = 353
        Height = 17
        Caption = 'Unnecessary spaces'
        TabOrder = 12
      end
      object chkFixSpaceAfterCustomChars: TCheckBox
        Left = 8
        Top = 280
        Width = 353
        Height = 17
        Caption = 'Space after custom characters'
        TabOrder = 13
      end
      object chkFixSpaceBeforeCustomChars: TCheckBox
        Left = 8
        Top = 296
        Width = 353
        Height = 17
        Caption = 'Space before custom characters'
        TabOrder = 14
      end
      object chkFixLinesWithoutLetters: TCheckBox
        Left = 8
        Top = 8
        Width = 353
        Height = 17
        Caption = 'Lines without letters'
        TabOrder = 15
      end
      object chkFixOpnDlgInSubsWithOneLine: TCheckBox
        Left = 8
        Top = 264
        Width = 353
        Height = 17
        Caption = '"- " in subtitles with one line'
        TabOrder = 16
      end
    end
    object pgeUnnecessarySpaces: TTabSheet
      Caption = 'Unnecessary spaces'
      ImageIndex = 2
      object lblCheckFor: TLabel
        Left = 8
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Check for:'
      end
      object lblFix: TLabel
        Left = 8
        Top = 160
        Width = 18
        Height = 13
        Caption = 'Fix:'
      end
      object lstSpacesToCheck: TCheckListBox
        Left = 8
        Top = 24
        Width = 353
        Height = 113
        ItemHeight = 13
        Items.Strings = (
          'Enters and spaces at the beginning and end'
          'Spaces between enters (left and right)'
          'Double spaces and enters'
          'Spaces in front of punctuation marks'
          'Spaces after "?" and "?"'
          'Spaces before "?" and "!"'
          'Spaces between numbers')
        TabOrder = 0
      end
      object lstSpacesToFix: TCheckListBox
        Left = 8
        Top = 176
        Width = 353
        Height = 110
        ItemHeight = 13
        Items.Strings = (
          'Enters and spaces at the beginning and end'
          'Spaces between enters (left and right)'
          'Double spaces and enters'
          'Spaces in front of punctuation marks'
          'Spaces after "?" and "?"'
          'Spaces before "?" and "!"'
          'Spaces between numbers')
        TabOrder = 1
      end
    end
  end
  object btnOk: TButton
    Left = 216
    Top = 424
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
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 304
    Top = 424
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object dlgSetColor: TColorDialog
    Left = 40
    Top = 424
  end
  object dlgLoad: TOpenDialog
    Filter = 'OCR (*.ocr)|*.ocr'
    Left = 8
    Top = 424
  end
end
