object frmCustomFormats: TfrmCustomFormats
  Left = 197
  Top = 105
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'frmCustomFormats'
  ClientHeight = 448
  ClientWidth = 755
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
  object gbInformation: TGroupBox
    Left = 8
    Top = 8
    Width = 290
    Height = 393
    Caption = 'Information'
    TabOrder = 0
    object lblFPS: TLabel
      Left = 8
      Top = 162
      Width = 117
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FPS:'
    end
    object edtFormatName: TLabeledEdit
      Left = 128
      Top = 16
      Width = 153
      Height = 21
      EditLabel.Width = 31
      EditLabel.Height = 13
      EditLabel.Caption = 'Name:'
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object edtExtension: TLabeledEdit
      Left = 128
      Top = 40
      Width = 153
      Height = 21
      BiDiMode = bdLeftToRight
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.BiDiMode = bdLeftToRight
      EditLabel.Caption = 'Extension:'
      EditLabel.ParentBiDiMode = False
      LabelPosition = lpLeft
      ParentBiDiMode = False
      TabOrder = 1
      Text = '*.xxx'
    end
    object edtTimeStructure: TLabeledEdit
      Left = 128
      Top = 136
      Width = 97
      Height = 21
      EditLabel.Width = 49
      EditLabel.Height = 13
      EditLabel.Caption = 'Structure:'
      LabelPosition = lpLeft
      TabOrder = 5
      Text = 'hh:mm:ss,zzz'
    end
    object rdoTime: TRadioButton
      Left = 128
      Top = 96
      Width = 105
      Height = 17
      Caption = 'Time'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object rdoFrames: TRadioButton
      Left = 128
      Top = 112
      Width = 105
      Height = 17
      Caption = 'Frames'
      TabOrder = 4
    end
    object cmbFPS: TComboBox
      Left = 128
      Top = 160
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
    end
    object edtNewLineChar: TLabeledEdit
      Left = 128
      Top = 64
      Width = 153
      Height = 21
      BiDiMode = bdLeftToRight
      EditLabel.Width = 68
      EditLabel.Height = 13
      EditLabel.BiDiMode = bdLeftToRight
      EditLabel.Caption = 'New line char:'
      EditLabel.ParentBiDiMode = False
      LabelPosition = lpLeft
      ParentBiDiMode = False
      TabOrder = 2
      Text = '|'
    end
    object btnLoadProject: TButton
      Left = 16
      Top = 200
      Width = 121
      Height = 25
      Caption = 'Load project'
      TabOrder = 7
      OnClick = btnLoadProjectClick
    end
    object btnSaveProject: TButton
      Left = 144
      Top = 200
      Width = 129
      Height = 25
      Caption = 'Save project'
      TabOrder = 8
      OnClick = btnSaveProjectClick
    end
  end
  object chkRemember: TCheckBox
    Left = 8
    Top = 416
    Width = 293
    Height = 17
    Caption = 'Remember last custom format'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 656
    Top = 412
    Width = 89
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnSave: TButton
    Left = 536
    Top = 412
    Width = 113
    Height = 25
    Caption = '&Save!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object mmoCustomFormat: TMemo
    Left = 304
    Top = 8
    Width = 445
    Height = 393
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      '** Copyright '#169' 2002 URUSoft'
      '   Comments won'#39't be saved into the subtitle file'
      '   If you want to save these two characters into a file,'
      '   write {asterisk} instead (this will write only ONE asterisk)'
      
        '   Warning: you MUST read the help file before using this featur' +
        'e !*'
      
        'Subtitle Workshop - Custom format ** Placed once at the beginnin' +
        'g of the file !*'
      ''
      
        '{RepeatSub} ** This part repeats each time there is a new subtit' +
        'le !*'
      '** Here you should write the structure of only ONE subtitle !*'
      ''
      
        'Subtitle number: {SubCount}** This writes the actual subtitle nu' +
        'mber ONLY if it is between {RepeatSub} and {EndRepeat} !*'
      'Start time (or frames): {swStart}:{swFrameStart,10}'
      'End time (or frames): {swEnd}:{swFrameEnd,10}'
      'Subtitle text: {swText}'
      ''
      '{EndRepeat} ** Ends the repeating part !*'
      ''
      
        'End of Subtitle Workshop - Custom format ** Placed once at the e' +
        'nd of the file !*')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object dlgLoad: TOpenDialog
    Left = 16
    Top = 112
  end
  object dlgSave: TSaveDialog
    Left = 48
    Top = 112
  end
end
