object FormAddDialog: TFormAddDialog
  Left = 0
  Top = 0
  Caption = 'Add'
  ClientHeight = 160
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    234
    160)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 37
    Top = 127
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 118
    Top = 127
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PanelName: TPanel
    Left = 0
    Top = 0
    Width = 234
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    object LabelName: TLabel
      Left = 8
      Top = 8
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object ComboBoxName: TComboBox
      Left = 8
      Top = 22
      Width = 220
      Height = 21
      TabOrder = 0
    end
  end
  object PanelValue: TPanel
    Left = 0
    Top = 50
    Width = 234
    Height = 71
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 3
    object LabelValue: TLabel
      Left = 8
      Top = 6
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object RadioButtonString: TRadioButton
      Left = 8
      Top = 49
      Width = 49
      Height = 17
      Caption = 'String'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonStringClick
    end
    object RadioButtonNumber: TRadioButton
      Left = 63
      Top = 49
      Width = 58
      Height = 17
      Caption = 'Number'
      TabOrder = 1
      OnClick = RadioButtonNumberClick
    end
    object RadioButtonBoolean: TRadioButton
      Left = 127
      Top = 49
      Width = 58
      Height = 17
      Caption = 'Boolean'
      TabOrder = 2
      OnClick = RadioButtonBooleanClick
    end
    object RadioButtonNull: TRadioButton
      Left = 191
      Top = 49
      Width = 58
      Height = 17
      Caption = 'Null'
      TabOrder = 3
      OnClick = RadioButtonNullClick
    end
    object ComboBoxValue: TComboBox
      Left = 8
      Top = 22
      Width = 220
      Height = 21
      TabOrder = 4
    end
  end
end
