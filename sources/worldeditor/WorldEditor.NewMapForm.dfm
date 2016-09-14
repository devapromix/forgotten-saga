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
  object GroupBox1: TGroupBox
    Left = 56
    Top = 64
    Width = 265
    Height = 57
    Caption = 'Terrain'
    TabOrder = 1
    object btTerFill: TBitBtn
      Left = 208
      Top = 20
      Width = 43
      Height = 25
      Caption = 'Fill'
      TabOrder = 0
      OnClick = btTerFillClick
    end
    object cbTerrain: TComboBox
      Left = 17
      Top = 22
      Width = 185
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 56
    Top = 136
    Width = 265
    Height = 57
    Caption = 'Objects'
    TabOrder = 2
    object btObjFill: TBitBtn
      Left = 208
      Top = 20
      Width = 43
      Height = 25
      Caption = 'Fill'
      TabOrder = 0
      OnClick = btObjFillClick
    end
    object cbObjects: TComboBox
      Left = 9
      Top = 22
      Width = 185
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
  end
end
