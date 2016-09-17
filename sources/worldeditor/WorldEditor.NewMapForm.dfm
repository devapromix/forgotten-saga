object fNew: TfNew
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Tools'
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
    Left = 256
    Top = 329
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
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
    Left = 8
    Top = 71
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
      Left = 17
      Top = 22
      Width = 185
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
  end
  object btGen: TBitBtn
    Left = 392
    Top = 188
    Width = 153
    Height = 25
    Caption = 'Gen Cave'
    TabOrder = 3
    OnClick = btGenClick
  end
  object cbFloor: TComboBox
    Left = 392
    Top = 43
    Width = 185
    Height = 21
    Style = csDropDownList
    TabOrder = 4
  end
  object cbWall: TComboBox
    Left = 392
    Top = 8
    Width = 185
    Height = 21
    Style = csDropDownList
    TabOrder = 5
  end
  object edNum: TEdit
    Left = 400
    Top = 88
    Width = 25
    Height = 21
    TabOrder = 6
    Text = '7'
  end
  object UpDown1: TUpDown
    Left = 425
    Top = 88
    Width = 16
    Height = 21
    Associate = edNum
    Min = 1
    Max = 20
    Position = 7
    TabOrder = 7
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 134
    Width = 265
    Height = 79
    Caption = 'Terrain'
    TabOrder = 8
    object btAddTerSpot: TBitBtn
      Left = 208
      Top = 20
      Width = 43
      Height = 25
      Caption = 'Add'
      TabOrder = 0
      OnClick = btAddTerSpotClick
    end
    object cbTerSpot: TComboBox
      Left = 17
      Top = 22
      Width = 185
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object edTerSpotCount: TEdit
      Left = 88
      Top = 49
      Width = 25
      Height = 21
      TabOrder = 2
      Text = '10'
    end
    object UpDown2: TUpDown
      Left = 113
      Top = 49
      Width = 16
      Height = 21
      Associate = edTerSpotCount
      Min = 1
      Max = 50
      Position = 10
      TabOrder = 3
    end
    object edTerSpotSize: TEdit
      Left = 210
      Top = 51
      Width = 25
      Height = 21
      TabOrder = 4
      Text = '25'
    end
    object UpDown3: TUpDown
      Left = 235
      Top = 51
      Width = 16
      Height = 21
      Associate = edTerSpotSize
      Min = 10
      Position = 25
      TabOrder = 5
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 219
    Width = 265
    Height = 79
    Caption = 'Objects'
    TabOrder = 9
    object btAddObjSpot: TBitBtn
      Left = 208
      Top = 20
      Width = 43
      Height = 25
      Caption = 'Add'
      TabOrder = 0
      OnClick = btAddObjSpotClick
    end
    object cbObjSpot: TComboBox
      Left = 17
      Top = 22
      Width = 185
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object edObjSpotCount: TEdit
      Left = 88
      Top = 49
      Width = 25
      Height = 21
      TabOrder = 2
      Text = '10'
    end
    object UpDown4: TUpDown
      Left = 113
      Top = 49
      Width = 16
      Height = 21
      Associate = edObjSpotCount
      Min = 1
      Max = 50
      Position = 10
      TabOrder = 3
    end
    object edObjSpotSize: TEdit
      Left = 208
      Top = 51
      Width = 25
      Height = 21
      TabOrder = 4
      Text = '25'
    end
    object UpDown5: TUpDown
      Left = 233
      Top = 51
      Width = 16
      Height = 21
      Associate = edObjSpotSize
      Min = 10
      Position = 25
      TabOrder = 5
    end
  end
end
