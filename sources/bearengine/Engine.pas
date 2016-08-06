unit Engine;

interface

uses
  Types;

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
    procedure Border(Pos: TPoint; Color: Integer);
    function TextOut(aText: string; aRect: TRect; Align: TAlign = aLeft)
      : Integer; overload;
    function LightColor(Color: Integer; Percent: Byte): Integer;
    function DarkColor(Color: Integer; Percent: Byte): Integer;
    property Window: TSize read FWindow write FWindow;
    property Char: TSize read FChar write FChar;
    function GetKey: Word;
  end;

implementation

uses
  BearLibTerminal,
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, LazUTF8,
{$ENDIF}
  Classes, SysUtils, Common.Color;

constructor TEngine.Create(AWidth, AHeight: Integer);
begin
  Randomize;
  FWindow.Width := AWidth;
  FWindow.Height := AHeight;
  terminal_set('font: resources\UbuntuMono-R.ttf, size=11;');
  terminal_set(Format('terminal.encoding=%s', ['windows-1251']));
  terminal_set(Format('window: size=%dx%d, icon=%s', [AWidth, AHeight,
    'ForgottenSaga.ico']));
  FChar.Width := terminal_state(TK_CELL_WIDTH);
  FChar.Height := terminal_state(TK_CELL_HEIGHT);
end;

destructor TEngine.Destroy;
begin
  inherited;
end;

procedure TEngine.FontColor(Color: Integer);
begin
{$IFNDEF USE_TERMINAL}
  // Surface.Canvas.Font.Color := Color;
{$ENDIF}
end;

function TEngine.GetKey: Word;
begin
  Result := terminal_read();
  if (Result = TK_RETURN) then
    Result := 13;
  if (Result = TK_ESCAPE) then
    Result := 27;
  if (Result = TK_SPACE) then
    Result := 32;
end;

procedure TEngine.FontBackColor(Color: Integer);
begin
{$IFNDEF USE_TERMINAL}
  case Color of
    clClear:
      // Surface.Canvas.Brush.Style := bsClear;
    else
      // Surface.Canvas.Brush.Color := Color;
  end;
{$ENDIF}
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
  terminal_clear();
end;

procedure TEngine.KeyOut(X, Y: Integer; Caption: string; Key: string;
  Active: Boolean = True);
var
  K: string;
begin
  K := '[' + Key + ']';
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
      terminal_print(X, Y, Text);
    aCenter:
      terminal_print((Window.Width div 2) - (Length(Text) div 2), Y, Text);
    aRight:
      terminal_print(Window.Width - Length(Text), Y, Text);
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
    // Result := Surface.Canvas.TextWidth(astr + aword) >= aRect.Right;
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
  Result := Color;
end;

function TEngine.LightColor(Color: Integer; Percent: Byte): Integer;
var
  R, G, B: Byte;
  C: Integer;
begin
  Result := Color;
end;

procedure TEngine.Border(Pos: TPoint; Color: Integer);
begin

end;

end.
