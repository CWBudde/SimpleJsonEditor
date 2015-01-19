object FormAddDialog: TFormAddDialog
  Left = 0
  Top = 0
  Caption = 'Add'
  ClientHeight = 95
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    339
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelName: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object LabelValue: TLabel
    Left = 177
    Top = 8
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object EditName: TEdit
    Left = 8
    Top = 24
    Width = 163
    Height = 21
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 96
    Top = 62
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 177
    Top = 62
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object EditValue: TEdit
    Left = 177
    Top = 24
    Width = 154
    Height = 21
    TabOrder = 3
  end
end
