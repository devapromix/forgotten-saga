object fNew: TfNew
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'fNew'
  ClientHeight = 379
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TBitBtn
    Left = 264
    Top = 346
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object cmdFill: TBitBtn
    Left = 416
    Top = 36
    Width = 43
    Height = 25
    Caption = 'Fill'
    TabOrder = 1
    OnClick = cmdFillClick
  end
  object cbxTiles: TComboBox
    Left = 208
    Top = 36
    Width = 185
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
end
