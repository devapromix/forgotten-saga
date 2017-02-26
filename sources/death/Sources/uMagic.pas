unit uMagic;

interface

procedure Explosion(X, Y, D: Integer);
procedure Lightning(X, Y, D: Integer);
procedure FlyItem(X, Y, K: Integer);

implementation

uses Forms, uBattle, uItem, uMsg, uDungeon, uPC, uConst, uTypes, SysUtils, Math, uLang,
  uUtils, uMisc, uDraw, uBox;

procedure HitEnemy(X, Y, D: Integer);
var
  I: Integer;
begin
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  if (Creature > 0) then
  begin
    AnimEnemy := True;
    AnimEnemyPoint := SetPoint(X, Y);
    I := CreatureLife;
    Dec(I, D);
    if I < 0 then I := 0;
    CreatureLife := I;
    case Rand(1, 2) of
      1: AddMsg(Format(Lang.Lang( 90 + Rand(0, 4)), [D]));
      2: AddMsg(Format(Lang.Lang( 90 + Rand(5, 9)), [EnemyName(X, Y), D]));
    end;
    if (I = 0) then EnemyDead(X, Y);
  end;
end;

procedure Explosion(X, Y, D: Integer);
var
  A, B: Integer;
begin
  Cave.Dungeon[PC.Z].Cell[X][Y].Terrain := 0;
  Dec(X); Dec(Y);
  for A := 0 to 2 do for B := 0 to 2 do
    if Cave.Dungeon[PC.Z].Cell[X + A][Y + B].Terrain <> 1 then
    begin
      Cave.Dungeon[PC.Z].Cell[X + A][Y + B].Fire := 1;
      HitEnemy(X + A, Y + B, D);
      if (PC.X = X + A) and (PC.Y = Y + B) then
      begin
        AnimPC := True;
        D := (D * (100 - GetIntEquipArmor())) div 100;
        if (D < 5) then D := 5;
        Dec(PC.Life, D);
        AddMsg(Format(Lang.Lang(240 + Rand(0, 4)), [D]));
        if (PC.Life <= 0) then PC.Life := 0;
      end;
      case Cave.Dungeon[PC.Z].Cell[X + A][Y + B].Terrain of
        98: begin
              DropItemsFromChest(X + A, Y + B);
              AddMsg(Lang.Lang(255 + Rand(0, 3)) + ' ' + Lang.Lang(230 + Rand(0, 4)));
            end;
        99: DoBarrel(X + A, Y + B);
      end;
    end;
  Draw();
  BattleAnimations();
  Sleep(25);
  for A := 0 to 2 do
    for B := 0 to 2 do
      Cave.Dungeon[PC.Z].Cell[X + A][Y + B].Fire := 0;
  Draw();
end;

procedure Lightning(X, Y, D: Integer);

  procedure LineTo(X, Y, Z, D: Integer);
  var
    I, L, AX, AY, TX, TY, V, RX, RY: Integer;
    LR: Real;
  begin
    V := -1;
    TX := PC.X; TY := PC.Y;
    L := Math.Max(Abs(PC.X - X), Abs(PC.Y - Y));
    if (L > 10) then Exit;
    for I := 1 to L - 1 do
    begin
      LR := I / L;
      AX := PC.X + Round((X - PC.X) * LR);
      AY := PC.Y + Round((Y - PC.Y) * LR);
      if (Cave.Dungeon[PC.Z].Cell[AX][AY].Terrain = 1) then Break;
      RX := AX - TX;
      RY := AY - TY;
      if (Z > 0) then
      begin
        if ((RX > 0) and (RY > 0)) or ((RX < 0) and (RY < 0)) then V := 2;
        if ((RX > 0) and (RY < 0)) or ((RX < 0) and (RY > 0)) then V := 1;
        if (RY = 0) then V := 0;
        if (RX = 0) then V := 3;
      end else V := 0;
      with Cave.Dungeon[PC.Z].Cell[AX][AY] do
      begin
        Fire := Z + V;
        if (D > 0) then HitEnemy(AX, AY, Rand(1, Percent(D, PC.Skills[14].Value)));
        if (Terrain = 99) then DoBarrel(AX, AY);
      end;
      TX := AX; TY := AY;
    end;
  end;

begin
  LineTo(Cursor.X, Cursor.Y, 11, D);
  Draw();
  Sleep(25);
  LineTo(Cursor.X, Cursor.Y, 0, 0);
  Draw();
end;

procedure FlyItem(X, Y, K: Integer);
var
  I, L, AX, AY, TX, TY, V, RX, RY: Integer;
  LR: Real;
begin
  TX := PC.X; TY := PC.Y;
  L := Math.Max(Abs(PC.X - X), Abs(PC.Y - Y));
  if (L > 10) then Exit;
  for I := 1 to L - 1 do
  begin
    LR := I / L;
    AX := PC.X + Round((X - PC.X) * LR);
    AY := PC.Y + Round((Y - PC.Y) * LR);
    if (Cave.Dungeon[PC.Z].Cell[AX][AY].Terrain = 1) then Break;
    RX := AX - TX; RY := AY - TY;
    begin
      if ((RX > 0) and (RY > 0)) or ((RX < 0) and (RY < 0)) then V := 2;
      if ((RX > 0) and (RY < 0)) or ((RX < 0) and (RY > 0)) then V := 1;
      if (RY = 0) then V := 0;
      if (RX = 0) then V := 3;
    end;
    Cave.Dungeon[PC.Z].Cell[AX][AY].Fire := K + V;
    Cave.Dungeon[PC.Z].Cell[TX][TY].Fire := 0;
    Draw();
    Application.ProcessMessages;
    Sleep(1);
    TX := AX; TY := AY;
  end;
  Cave.Dungeon[PC.Z].Cell[AX][AY].Fire := 0;
  Draw();
end;

end.
