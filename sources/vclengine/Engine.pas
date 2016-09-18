unit Engine;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Graphics,
  Types;

{$REGION ' Keyboard scancodes '}

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

{$ENDREGION ' Keyboard scancodes '}
{$REGION ' TEngine '}

type
  TEngine = class(TObject)
  public const
    clClear = -1;
    kcBegin = '{';
    kcEnd = '}';
  public type
    TAlign = (aLeft, aCenter, aRight);
  public type
    TSize = record
      Width: Integer;
      Height: Integer;
    end;
  private
    FSurface: TBitmap;
    FWindow: TSize;
    FChar: TSize;
  public
    procedure Clear;
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Print(X, Y: Integer; S: string; A: TAlign = aLeft); overload;
    function Print(S: string; R: TRect; A: TAlign = aLeft): Integer; overload;
    procedure ForegroundColor(Color: Integer);
    procedure BackgroundColor(Color: Integer);
    function LightColor(Color: Integer; Percent: Byte): Integer;
    function DarkColor(Color: Integer; Percent: Byte): Integer;
    property Surface: TBitmap read FSurface write FSurface;
    property Window: TSize read FWindow write FWindow;
    property Char: TSize read FChar write FChar;
    function GetTextLength(Text: string): Integer;
    procedure Close;
  end;

{$ENDREGION ' TEngine '}

implementation

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, LazUTF8,
{$ENDIF}
  Forms, Classes, SysUtils;

{$REGION ' TEngine '}

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

procedure TEngine.ForegroundColor(Color: Integer);
begin
  Surface.Canvas.Font.Color := Color;
end;

function TEngine.GetTextLength(Text: string): Integer;
begin
  Result := Length(Text);
end;

procedure TEngine.BackgroundColor(Color: Integer);
begin
  case Color of
    clClear:
      Surface.Canvas.Brush.Style := bsClear;
  else
    Surface.Canvas.Brush.Color := Color;
  end;
end;

procedure TEngine.Clear;
begin
  Surface.Canvas.Brush.Color := clBlack;
  Surface.Canvas.FillRect(Rect(0, 0, Surface.Width, Surface.Height));
  BackgroundColor(clClear);
end;

procedure TEngine.Close;
begin
  Application.Terminate;
end;

procedure TEngine.Print(X, Y: Integer; S: string; A: TAlign = aLeft);
begin
  case A of
    aLeft:
      Surface.Canvas.TextOut(X * Char.Width, Y * Char.Height, S);
    aCenter:
      Surface.Canvas.TextOut(((Window.Width div 2) - (GetTextLength(S) div 2)) *
        Char.Width, Y * Char.Height, S);
    aRight:
      Surface.Canvas.TextOut((Window.Width - GetTextLength(S)) * Char.Width,
        Y * Char.Height, S);
  end;
end;

function TEngine.Print(S: string; R: TRect; A: TAlign = aLeft): Integer;
var
  I, C, L: Word;
  SL: TStringList;
  V: string;

  procedure AddRow(S: string);
  begin
    Print(R.Left div Char.Width, (L * Char.Height + R.Top)
      div Char.Height, S, A);
  end;

  function AddLine(astr, aword: string): Boolean;
  begin
    Result := Surface.Canvas.TextWidth(astr + aword) >= R.Right;
    if Result then
    begin
      AddRow(astr);
      Inc(L)
    end;
  end;

  procedure WordDivider;
  begin
    SL := TStringList.Create;
    StringReplace(S, '  ', ' ', [rfReplaceAll]);
    SL.Delimiter := ' ';
    SL.DelimitedText := S;
  end;

begin
  Result := 0;
  if (S = '') then
    Exit;
  WordDivider;
  L := 0;
  V := '';
  C := SL.Count - 1;
  R.Top := R.Top * Char.Height;
  R.Left := R.Left * Char.Width;
  R.Right := R.Right * Char.Width;
  for I := 0 to C do
  begin
    if AddLine(V, SL[I]) then
      V := '';
    V := V + SL[I] + ' ';
    if (I = C) and (V <> '') then
    begin
      AddRow(V);
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

{$ENDREGION ' TEngine '}

end.
