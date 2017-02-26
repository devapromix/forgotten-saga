unit uRace;

interface

procedure DrawRace();
procedure KeysRace(var Key: Word);

implementation

uses SysUtils, uLang, uDraw, uGraph, uConst, uPC, uTypes, uMain, uColor, uBox,
  uGame, uStringUtils;

procedure DrawRace();
var
  I, X, Y, P: Byte;   
  S, V, K: string;
begin
  TextOut((ScreenWidth div 2) - (Length(Lang.Lang(164)) div 2),
    12, Lang.Lang(164) + ':', cRdYellow);
  X := 5; Y := 14; P := 0;
  for I := 1 to RacesCount do
  begin
    if (PC.RaceID = I) then
    begin
      TextOut(X, Y, Chr(96 + I) + '.', cLtYellow);
      TextOut(X + 2, Y, RaceName(I), cLtYellow)
    end else begin
      TextOut(X, Y, Chr(96 + I) + '.', cYellow);
      TextOut(X + 2, Y, RaceName(I), cYellow);
    end;
    Inc(X, ColWidth);
    if (X > ScreenWidth) then
    begin
      X := 5;
      Inc(Y);
      if (Y > P) then P := Y;
      if (RacesCount div 3 <> RacesCount / 3) then Inc(P);
    end;
  end;
  //
  if (RaceName() = '') then
  begin
    S := PC.Name + '. ';
    TextOut((ScreenWidth div 2) - (Length(S) div 2), P + 1, S, cRdYellow);
  end else begin
    S := PC.Name + '. ' + RaceName() + '. ';
    if (PC.ClassID > 0) then S := S + ClassName() + '. ';
    TextOut((ScreenWidth div 2) - (Length(S) div 2), P + 1, S, cRdYellow);
    TextOut((ScreenWidth div 2) - (Length(S) div 2) + Length(PC.Name + '. '),
      P + 1, RaceName() + '. ', cLtYellow);
  end;
  DrawCharProp(P);
  // Описание расы
  if (RaceName() <> '') then
  begin
    Inc(P, 8);
    V := ''; S := Lang.Lang(PC.RaceID + 3250);
    repeat
      if (Length(S) > ScreenWidth - 10) then
      begin
        V := Copy(S, 1, ScreenWidth - 10);
        I := SU.LastPos(string(' '), V);
        V := Trim(Copy(S, I + 1, Length(S)));
        K := Copy(S, 1, I);
        Delete(S, 1, I);
      end;
      TextOut(5, P, K, cYellow);
      Inc(P);
    until (Length(S) <= ScreenWidth - 10);
  end;
  TextOut(5, P, V, cYellow);
  //
  BarOut('[esc]', Lang.Lang(163), true);
  BarOut('[a-' + Chr(96 + RacesCount) + ']', Lang.Lang(165), false);
  if (PC.RaceID = 0) then
    BarOut('[space, enter]', Lang.Lang(166), false)
  else begin
    BarOut('[space]', Lang.Lang(166), false);
    BarOut('[enter]', Lang.Lang(150), false);
  end;
end;

procedure KeysRace(var Key: Word);
var
  I: Byte;
begin
  case Key of
    27:
    begin
      fMain.DoTimer.Enabled := True;
      GameFrame := gfName;
      Draw();
    end;
    13:
    begin
      if (PC.RaceID <> 0) then
        GameFrame := gfClass
          else PC.RaceID := Random(RacesCount) + 1;
      Draw();
    end;
    32:
    begin
      repeat
        I := Random(RacesCount) + 1;
      until (PC.RaceID <> I);
      PC.RaceID := I;
      Draw();
    end;
    ord('A')..ord('A') + RacesCount - 1:
    begin
      PC.RaceID := Key - (ord('A') - 1);
      Draw();
    end;
  end;
end;

end.

