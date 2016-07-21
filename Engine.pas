unit Engine;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Types, Graphics;

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
    procedure KeyOut(X, Y: Integer; Caption: string; Key: string; Active: Boolean = True); overload;
    procedure KeyOut(X, Y: Integer; Caption: string; Key: string;
      Align: TAlign; Active: Boolean = True); overload;
    procedure CharOut(X, Y: Integer; Symbol: System.Char; Color: Integer);
    procedure TitleOut(Y: Integer; Text: string);
    procedure TextOut(X, Y: Integer; Text: string;
      Align: TAlign = aLeft); overload;
    procedure FontColor(Color: Integer);
    procedure FontBackColor(Color: Integer);
    procedure Border(Pos: TPoint; Color: Integer);
    function TextOut(aText: string; aRect: TRect; Align: TAlign = aLeft)
      : Integer; overload;
    function LightColor(Color: Integer; Percent: Byte): Integer;
    function DarkColor(Color: Integer; Percent: Byte): Integer;
    property Surface: TBitmap read FSurface write FSurface;
    property Window: TSize read FWindow write FWindow;
    property Char: TSize read FChar write FChar;
  end;

implementation

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, LazUTF8,
{$ENDIF}
  Classes, SysUtils, Common.Color;

constructor TEngine.Create(AWidth, AHeight: Integer);
begin
  Randomize;
  FSurface := Graphics.TBitmap.Create;
  FWindow.Width := AWidth;
  FWindow.Height := AHeight;
  FSurface.PixelFormat := pf16bit;   
  FSurface.Canvas.Brush.Style := bsClear;
  FSurface.Canvas.Font.Name := 'Courier New';
  FSurface.Canvas.Font.Style := [];
  FSurface.Canvas.Font.Size := 9;
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
  TextOut(0, Y, StringOfChar('=',
    {$IFDEF FPC}UTF8{$ENDIF}Length(Text)), aCenter);
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

procedure TEngine.KeyOut(X, Y: Integer; Caption: string; Key: string; Active: Boolean = True);
var
  K: string;
begin
  K := '[' + Key + ']';
  if Active then FontColor(clHotKey) else FontColor(DarkColor(clHotKey, 60));
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
        L := (((Surface.Width + (X * Char.Width)) div 2)) -
          (Surface.Canvas.TextWidth(S) div 2);
        KeyOut(L div Char.Width, Y, Caption, Key, Active);
      end;
    aRight:
      begin

      end;
  end;
end;

procedure TEngine.TextOut(X, Y: Integer; Text: string; Align: TAlign = aLeft);
var
  Width: Integer;
begin
  Width := Surface.Canvas.TextWidth(Text);
  case Align of
    aLeft:
      Surface.Canvas.TextOut(X * Char.Width, Y * Char.Height, Text);
    aCenter:
      Surface.Canvas.TextOut((((Surface.Width + (X * Char.Width)) div 2)) -
        (Width div 2), Y * Char.Height, Text);
    aRight:
      Surface.Canvas.TextOut(Surface.Width - Width, Y * Char.Height, Text);
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

procedure TEngine.Border(Pos: TPoint; Color: Integer);
begin
  Surface.Canvas.Pen.Color := Color;
  Surface.Canvas.Brush.Style := bsClear;
  Surface.Canvas.Rectangle(Pos.X * Char.Width - 1, Pos.Y * Char.Height - 1,
    Pos.X * Char.Width + Char.Width + 1, Pos.Y * Char.Height + Char.Height + 1);
end;

end.
