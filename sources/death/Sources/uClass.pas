unit uClass;

interface

procedure DrawClass();
procedure KeysClass(var Key: Word);

implementation

uses SysUtils, uLang, uDraw, uGraph, uConst, uPC, uTypes, uColor, uBox, uGame, uDungeon,
  uStringUtils;

procedure DrawClass();
var
  I, X, Y, P: Byte;
  S, V, K: string;
begin
  TextOut((ScreenWidth div 2) - (Length(Lang.Lang(167)) div 2),
    12, Lang.Lang(167) + ':', cRdYellow);
  X := 5; Y := 14; P := 0;
  for I := 1 to ClassesCount do
  begin
    if (PC.ClassID = I) then
    begin
      TextOut(X, Y, Chr(96 + I) + '.', cLtYellow);
      TextOut(X + 2, Y, ClassName(I), cLtYellow)
    end else begin
      TextOut(X, Y, Chr(96 + I) + '.', cYellow);
      TextOut(X + 2, Y, ClassName(I), cYellow);
    end;
    Inc(X, ColWidth);
    if (X > ScreenWidth) then
    begin
      X := 5;
      Inc(Y);
      if (Y > P) then P := Y;
      if (ClassesCount div 3 <> ClassesCount / 3) then Inc(P);
    end;
  end;
  //
  if (ClassName() = '') then
  begin
    S := PC.Name + '. ' + RaceName() + '. ';
    TextOut((ScreenWidth div 2) - (Length(S) div 2), P + 1, S, cRdYellow);
  end else begin
    S := PC.Name + '. ' + RaceName() + '. ' + ClassName() + '. ';
    TextOut((ScreenWidth div 2) - (Length(S) div 2), P + 1, S, cRdYellow);
    TextOut((ScreenWidth div 2) - (Length(S) div 2) +
      Length(PC.Name + '. ' + RaceName() + '. '),
        P + 1, ClassName() + '. ', cLtYellow);
  end;
  DrawCharProp(P);
  // Описание класса
  if (ClassName() <> '') then
  begin
    Inc(P, 8);
    V := ''; S := Lang.Lang(PC.ClassID + 3200);
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
  BarOut('[esc]', Lang.Lang(165), true);
  BarOut('[a-' + Chr(96 + ClassesCount) + ']', Lang.Lang(168), false);
  if (PC.ClassID = 0) then
    BarOut('[space, enter]', Lang.Lang(169), false)
  else begin
    BarOut('[space]', Lang.Lang(169), false);
    BarOut('[enter]', Lang.Lang(151), false);
  end;
end;

procedure MakeChar();
var
  I: Byte;
begin
  // Обнуляем все навыки
  for I := 1 to High(PC.Skills) do
  with PC.Skills[I] do
  begin
    Value := 0;
    Exp := 0;
  end;
  // Считаем атрибуты
  PC.Str := RaceProp(PC.RaceID).Str + ClassProp(PC.ClassID).Str;
  PC.Dex := RaceProp(PC.RaceID).Dex + ClassProp(PC.ClassID).Dex;
  PC.Sta := RaceProp(PC.RaceID).Sta + ClassProp(PC.ClassID).Sta;
  PC.Int := RaceProp(PC.RaceID).Int + ClassProp(PC.ClassID).Int;
  CalcPC();
  FillPC();
  // Навыки
  for I := Low(TSkills) to High(TSkills) do
  begin
    if (ClassProp(PC.ClassID).Skills[I].Value > 0) then
      PC.Skills[I].Value := ClassProp(PC.ClassID).Skills[I].Value
        else PC.Skills[I].Value := 0;
    PC.Skills[I].Exp := 0;    
  end;
  // Начинается игра
  GameFrameScreen := gfNone;
  GameFrameTemp := gfGame;
  GameFrame := gfGame;
  IsGame := True;
  //

  // Даем персонажу экипировку и др. предметы
  StartInv;

  // Генерируем пещеру
  Cave := TCave.Create;

end;

procedure KeysClass(var Key: Word);
var
  I: Byte;
begin
  case Key of
    27:
    begin
      GameFrame := gfRace;
      Draw();
    end;
    13:
    begin
      if (PC.ClassID <> 0) then
        MakeChar()
          else PC.ClassID := Random(ClassesCount) + 1;
      Draw();
    end;
    32:
    begin
      repeat
        I := Random(ClassesCount) + 1;
      until (PC.ClassID <> I);
      PC.ClassID := I;
      Draw();
    end;
    ord('A')..ord('A') + ClassesCount - 1:
    begin
      PC.ClassID := Key - (ord('A') - 1);
      Draw();
    end;
  end;
end;

end.

