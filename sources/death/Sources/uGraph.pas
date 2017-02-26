unit uGraph;

interface

uses
  Graphics, SysUtils;

var
  BG: TBitmap;

type
  TAlign = (alLeft, alCenter, alRight, alTop, alMiddle, alBottom);

procedure TextOut(const AX, AY: Integer; const AKey: string; const AText: string); overload;
procedure TextOut(const AX, AY: Integer; const AText: string; AColor: Integer); overload;
procedure TextOut(const AY: Integer; const AText: string; AColor: Integer; AHorzAlign: TAlign = alLeft); overload;
procedure TitleOut(const S: string; AX: Integer = 0); overload;
procedure TitleOut(const S: string; AX, AY: Integer); overload;
procedure BarOut(const KeyID, StrID: string; IsNewBar: Boolean = False);

implementation

uses uConst, uColor;

var
  FKeyCursorPos: Byte = 0;

procedure TextOut(const AX, AY: Integer; const AKey: string; const AText: string); overload;
begin
  TextOut(AX     , AY, AKey, cLtYellow);
  TextOut(AX + 4, AY, AText, cRdYellow);
end;

procedure TextOut(const AX, AY: Integer; const AText: string; AColor: Integer);
begin
  BG.Canvas.Font.Color := AColor;
  BG.Canvas.TextOut(AX * CharWidth, AY * CharHeight, AText);
end;

procedure TextOut(const AY: Integer; const AText: string; AColor: Integer; AHorzAlign: TAlign = alLeft);
var
  L, AX: Integer;
begin
  AX := 0;
  case AHorzAlign of
    alLeft  : AX := 0;
    alCenter: begin
                L := Length(AText);
                AX := (ScreenWidth div 2) - (L div 2);
              end;
    alRight : begin
                L := Length(AText);
                AX := ScreenWidth - L;
              end;
  end;
  BG.Canvas.Font.Color := AColor;
  BG.Canvas.TextOut(AX * CharWidth, AY * CharHeight, AText);
end;

procedure TitleOut(const S: string; AX: Integer = 0); overload;
var
  I: Integer;
  L: string;
begin
  for I := 1 to Length(S) do L := L + '=';
  TextOut(AX, 0, S, cRdYellow);
  TextOut(AX, 1, L, cRdYellow);
end;

procedure TitleOut(const S: string; AX, AY: Integer); overload;
var
  I: Integer;
  L: string;
begin
  for I := 1 to Length(S) do L := L + '=';
  TextOut(AX, AY, S, cRdYellow);
  TextOut(AX, AY + 1, L, cRdYellow);
end;

procedure BarOut(const KeyID, StrID: string; IsNewBar: Boolean = False);
begin
  if IsNewBar then FKeyCursorPos := 0;
  TextOut(FKeyCursorPos, ScreenHeight - 1, KeyID, cLtYellow);
  Inc(FKeyCursorPos, Length(KeyID) + 1);
  TextOut(FKeyCursorPos, ScreenHeight - 1, AnsiLowerCase(StrID), cRdYellow);
  Inc(FKeyCursorPos, Length(StrID) + 1);
end;

initialization
  BG := TBitmap.Create;

finalization
  FreeAndNil(BG);

end.
