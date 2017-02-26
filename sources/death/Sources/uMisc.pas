unit uMisc;

interface

procedure DropItem(X, Y: Integer);
procedure DropItemsFromChest(X, Y: Integer);
procedure DoBarrel(X, Y: Integer);
procedure DoChest(X, Y: Integer);
procedure DoTrap(ID, X, Y: Integer);

implementation

uses uUtils, uMsg, uDungeon, uPC, uLang, uDraw, uMagic, uSkills;

procedure DropItem(X, Y: Integer);
var
  T: Integer;
begin
  with Cave.Dungeon[PC.Z] do
  begin
    // Gold
    if (Rand(1, 25) = 1) then
    begin
      T := (Rand(1, 9) * (PC.Z + 1)) + 1;
      if (Cell[X, Y].Boss > 1) then T := Rand(T, Cell[X, Y].Boss * T);
      DropList.AddItem(X, Y, 1, T);
    end;
    // Item
    if (Rand(1, 50) = 1) then
    begin
      T := Rand(2, 20);
      case T of
        18..19: DropList.AddItem(X, Y, T, Rand(10, 50));
            20: DropList.AddItem(X, Y, 160, Rand(3, 5));
           else DropList.AddItem(X, Y, T, 1);
      end;
    end;
    // Weapon or Armor
    if (Rand(1, 75) = 1) and (PC.Z > 0) then DropList.AddItem(X, Y, (((PC.Z div 3) * 10) + 10) + Rand(0, 9), 1);
    // Gem
    if (Rand(1, 100) = 1) and (PC.Z > 5) then DropList.AddItem(X, Y, Rand(150, 153), 1);
  end;
end;

procedure DropItemsFromChest(X, Y: Integer);
var
  I: Byte;
begin
  for I := 0 to 24 do DropItem(X, Y);
  Cave.Dungeon[PC.Z].Cell[X][Y].Terrain := 97;
end;

procedure DoChest(X, Y: Integer);
begin
  if (PC.Items[160] = 0) then
  begin
    AddMsg(Lang.Lang(Rand(0, 4) + 110));
    Exit;
  end;
  if (Rand(0, 100) <= PC.Skills[12].Value * 2) then
  begin
    SkillUp(12, 15);
    Inc(PC.Scores, 10);
    DropItemsFromChest(X, Y);
    AddMsg(Lang.Lang(Rand(7, 9) + 100));
  end else begin
    Dec(PC.Items[160]);
    AddMsg(Lang.Lang(Rand(5, 7) + 110));
  end;
  Draw();
end;

procedure DoBarrel(X, Y: Integer);
var
  I: Byte;
begin
  case Rand(1, 2) of
    1: // Explosion //
    begin
      AddMsg(Lang.Lang(245 + Rand(0, 4)));
      Explosion(X, Y, Rand(15, 25));
    end;
    2: // Drop Item //
    begin
      Cave.Dungeon[PC.Z].Cell[X][Y].Terrain := 0;
      AddMsg(Lang.Lang(235 + Rand(0, 4)));
      for I := 1 to 3 do DropItem(X, Y);
    end;
  end; // case
  Inc(PC.Scores);
end;

procedure DoTrap(ID, X, Y: Integer);
begin
  case ID of
    2..5:
    begin
      AddMsg(Lang.Lang(Rand(0, 9) + 1500));
    end;
    6:
    begin
      AddMsg(Lang.Lang(Rand(0, 9) + 1500));
      Explosion(X, Y, (PC.Z * 5) + Rand(25, 75) + 5);
    end;
  end;
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  begin
    Terrain := 90;
    Inc(PC.Scores, 10);
    Trap := True;
  end;
end;

end.
