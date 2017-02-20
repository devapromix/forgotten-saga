object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 403
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RichEdit1: TRichEdit
    Left = 160
    Top = 8
    Width = 337
    Height = 361
    Lines.Strings = (
      'RichEdit1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Attack'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 144
    Width = 145
    Height = 25
    Caption = 'Run'
    TabOrder = 2
    OnClick = Button2Click
  end
  object RichEdit2: TRichEdit
    Left = 504
    Top = 8
    Width = 177
    Height = 361
    Lines.Strings = (
      'RichEdit1')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ListBox1: TListBox
    Left = 8
    Top = 40
    Width = 145
    Height = 97
    ItemHeight = 13
    TabOrder = 4
  end
  object Button3: TButton
    Left = 8
    Top = 336
    Width = 145
    Height = 25
    Caption = 'Auto (100)'
    TabOrder = 5
    OnClick = Button3Click
  end
end
