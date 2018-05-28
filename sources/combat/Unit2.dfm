object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 447
  ClientWidth = 722
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 431
    ItemHeight = 13
    TabOrder = 0
  end
  object ListBox2: TListBox
    Left = 505
    Top = 8
    Width = 209
    Height = 431
    ItemHeight = 13
    TabOrder = 1
  end
  object Button4: TButton
    Left = 272
    Top = 414
    Width = 177
    Height = 25
    Caption = 'Quit'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button1: TButton
    Left = 272
    Top = 383
    Width = 177
    Height = 25
    Caption = 'Restart'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 272
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Attack'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 272
    Top = 288
    Width = 177
    Height = 25
    Caption = 'Run'
    TabOrder = 5
  end
  object Button5: TButton
    Left = 272
    Top = 336
    Width = 177
    Height = 25
    Caption = 'Auto'
    TabOrder = 6
  end
end
