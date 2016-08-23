unit Engine;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Graphics,
  Types;

const
  TK_A = ord('A');
  TK_B = ord('B');
  TK_C = ord('C');
  TK_D = ord('D');
  TK_E = ord('E');
  TK_F = ord('F');
  TK_G = ord('G');
  TK_H = ord('H');
  TK_I = ord('I');
  TK_J = ord('J');
  TK_K = ord('K');
  TK_L = ord('L');
  TK_M = ord('M');
  TK_N = ord('N');
  TK_O = ord('O');
  TK_P = ord('P');
  TK_Q = ord('Q');
  TK_R = ord('R');
  TK_S = ord('S');
  TK_T = ord('T');
  TK_U = ord('U');
  TK_V = ord('V');
  TK_W = ord('W');
  TK_X = ord('X');
  TK_Y = ord('Y');
  TK_Z = ord('Z');

  TK_1 = 49;
  TK_2 = 50;
  TK_3 = 51;
  TK_4 = 52;
  TK_5 = 53;
  TK_6 = 54;
  TK_7 = 55;
  TK_8 = 56;
  TK_9 = 57;
  TK_0 = 58;

  TK_F1 = 112;
  TK_F2 = 113;
  TK_F3 = 114;
  TK_F4 = 115;
  TK_F5 = 116;
  TK_F6 = 117;
  TK_F7 = 118;
  TK_F8 = 119;
  TK_F9 = 120;
  TK_F10 = 121;
  TK_F11 = 122;
  TK_F12 = 123;

  TK_ENTER = 13;
  TK_ESCAPE = 27;
  TK_SPACE = 32;

  TK_KP_0 = 96;
  TK_KP_1 = 97;
  TK_KP_2 = 98;
  TK_KP_3 = 99;
  TK_KP_4 = 100;
  TK_KP_5 = 101;
  TK_KP_6 = 102;
  TK_KP_7 = 103;
  TK_KP_8 = 104;
  TK_KP_9 = 105;

  TK_COMMA = 188;
  TK_PERIOD = 190;

  TK_RIGHT = 39;
  TK_LEFT = 37;
  TK_DOWN = 40;
  TK_UP = 38;

type
  TAlign = (aLeft, aCenter, aRight);

type
  TSize = record
    Width: Integer;
    Height: Integer;
  end;

type
  TEngine = class(TObject)
  private
    FSurface: TBitmap;
    FWindow: TSize;
    FChar: TSize;
  public
    procedure Clear;
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure KeyOut(X, Y: Integer; Caption: string; Key: string;
      Active: Boolean = True); overload;
    procedure KeyOut(X, Y: Integer; Caption: string; Key: string; Align: TAlign;
      Active: Boolean = True); overload;
    procedure CharOut(X, Y: Integer; Symbol: System.Char; Color: Integer);
    procedure TitleOut(Y: Integer; Text: string);
    procedure TextOut(X, Y: Integer; Text: string;
      Align: TAlign = aLeft); overload;
    procedure FontColor(Color: Integer);
    procedure FontBackColor(Color: Integer);
    function TextOut(aText: string; aRect: TRect; Align: TAlign = aLeft)
      : Integer; overload;
    function LightColor(Color: Integer; Percent: Byte): Integer;
    function DarkColor(Color: Integer; Percent: Byte): Integer;
    property Surface: TBitmap read FSurface write FSurface;
    property Window: TSize read FWindow write FWindow;
    property Char: TSize read FChar write FChar;
    function GetTextLength(Text: string): Integer;
    procedure Close;
  end;

const
  clClear = -1;

implementation

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, LazUTF8,
{$ENDIF}
  Forms, Classes, SysUtils, Common.Utils, Common.Variables;

constructor TEngine.Create(AWidth, AHeight: Integer);
begin
  Randomize;
  FWindow.Width := AWidth;
  FWindow.Height := AHeight;
  FSurface := Graphics.TBitmap.Create;
  FSurface.PixelFormat := pf16bit;
  FSurface.Canvas.Brush.Style := bsClear;
  FSurface.Canvas.Font.Name := 'Courier New';
  FSurface.Canvas.Font.Style := [fsBold];
  FSurface.Canvas.Font.Size := 10;
  FChar.Width := Surface.Canvas.TextWidth('W');
  FChar.Height := Surface.Canvas.TextHeight('W');
  FSurface.Width := FChar.Width * FWindow.Width;
  FSurface.Height := FChar.Height * FWindow.Height;
end;

destructor TEngine.Destroy;
begin
  FSurface.Free;
  inherited;
end;

procedure TEngine.FontColor(Color: Integer);
begin
  Surface.Canvas.Font.Color := Color;
end;

function TEngine.GetTextLength(Text: string): Integer;
begin
  Result := Length(Text);
end;

procedure TEngine.FontBackColor(Color: Integer);
begin
  case Color of
    clClear:
      Surface.Canvas.Brush.Style := bsClear;
  else
    Surface.Canvas.Brush.Color := Color;
  end;
end;

procedure TEngine.TitleOut(Y: Integer; Text: string);
begin
  FontColor(clTitle);
  TextOut(0, Y - 1, Text, aCenter);
  TextOut(0, Y, StringOfChar('=', Length(Text)), aCenter);
end;

procedure TEngine.CharOut(X, Y: Integer; Symbol: System.Char; Color: Integer);
begin
  FontColor(Color);
  TextOut(X, Y, Symbol);
end;

procedure TEngine.Clear;
begin
  Surface.Canvas.Brush.Color := clBlack;
  Surface.Canvas.FillRect(Rect(0, 0, Surface.Width, Surface.Height));
  FontBackColor(clClear);
end;

procedure TEngine.Close;
begin
  Application.Terminate;
end;

procedure TEngine.KeyOut(X, Y: Integer; Caption: string; Key: string;
  Active: Boolean = True);
var
  K: string;
begin
  K := '<' + Key + '>';
  if Active then
    FontColor(clHotKey)
  else
    FontColor(DarkColor(clHotKey, 60));
  TextOut(X, Y, K);
  FontColor(clButton);
  TextOut(X + Length(K) + 1, Y, Caption);
end;

procedure TEngine.KeyOut(X, Y: Integer; Caption: string; Key: string;
  Align: TAlign; Active: Boolean = True);
var
  S: string;
  L: Integer;
begin
  case Align of
    aLeft:
      KeyOut(X, Y, Caption, Key, Active);
    aCenter:
      begin
        S := '[' + Key + '] ' + Caption;
        L := ((((Char.Width * Window.Width) + (X * Char.Width)) div 2)) -
          ((Length(S) * Char.Width) div 2);
        KeyOut(L div Char.Width, Y, Caption, Key, Active);
      end;
    aRight:
      begin

      end;
  end;
end;

procedure TEngine.TextOut(X, Y: Integer; Text: string; Align: TAlign = aLeft);
begin
  case Align of
    aLeft:
      Surface.Canvas.TextOut(X * Char.Width, Y * Char.Height, Text);
    aCenter:
      Surface.Canvas.TextOut(((Window.Width div 2) - (Length(Text) div 2)) *
        Char.Width, Y * Char.Height, Text);
    aRight:
      Surface.Canvas.TextOut((Window.Width - Length(Text)) * Char.Width,
        Y * Char.Height, Text);
  end;
end;

function TEngine.TextOut(aText: string; aRect: TRect;
  Align: TAlign = aLeft): Integer;
var
  I, C, L: Word;
  SL: TStringList;
  S: string;

  procedure AddRow(Text: string);
  begin
    TextOut(aRect.Left div Char.Width, (L * Char.Height + aRect.Top)
      div Char.Height, Text, Align);
  end;

  function AddLine(astr, aword: string): Boolean;
  begin
    Result := Surface.Canvas.TextWidth(astr + aword) >= aRect.Right;
    if Result then
    begin
      AddRow(astr);
      Inc(L)
    end;
  end;

  procedure WordDivider;
  begin
    SL := TStringList.Create;
    StringReplace(aText, '  ', ' ', [rfReplaceAll]);
    SL.Delimiter := ' ';
    SL.DelimitedText := aText;
  end;

begin
  Result := 0;
  if (aText = '') then
    Exit;
  WordDivider;
  L := 0;
  S := '';
  C := SL.Count - 1;
  aRect.Top := aRect.Top * Char.Height;
  aRect.Left := aRect.Left * Char.Width;
  aRect.Right := aRect.Right * Char.Width;
  for I := 0 to C do
  begin
    if AddLine(S, SL[I]) then
      S := '';
    S := S + SL[I] + ' ';
    if (I = C) and (S <> '') then
    begin
      AddRow(S);
      Inc(L);
    end;
  end;
  Result := L;
  SL.Free;
end;

function TEngine.DarkColor(Color: Integer; Percent: Byte): Integer;
var
  R, G, B: Byte;
  C: Integer;
begin
  C := ColorToRGB(Color);
  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);
  R := R - MulDiv(R, Percent, 100);
  G := G - MulDiv(G, Percent, 100);
  B := B - MulDiv(B, Percent, 100);
  Result := RGB(R, G, B);
end;

function TEngine.LightColor(Color: Integer; Percent: Byte): Integer;
var
  R, G, B: Byte;
  C: Integer;
begin
  C := ColorToRGB(Color);
  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);
  R := R + MulDiv(255 - R, Percent, 100);
  G := G + MulDiv(255 - G, Percent, 100);
  B := B + MulDiv(255 - B, Percent, 100);
  Result := RGB(R, G, B);
end;

end.
