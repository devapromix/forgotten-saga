unit uCellItems;

interface

procedure DrawCellItems();
procedure KeysCellItems(var Key: Word);

implementation

uses SysUtils, uGraph, uColor, uConst, uDraw, uLang, uPC, uDropList,
  uDungeon, uStringUtils, uBox, uMsg, uGame, uTypes, uInv, uItem, uUtils;

var
  P: Integer;

procedure AutoEquip(ItemID: Word);
begin
  case GetItemType(ItemID) of
    // Оружие
    0..6:
    if (PC.EquipItems[1] = 0) then
    begin
      ItemSelInvID := ItemID;
      Equip();
    end;
    7:
    if (PC.EquipItems[2] = 0) then
    begin
      ItemSelInvID := ItemID;
      Equip();
    end;
    8:
    if (PC.EquipItems[3] = 0) then
    begin
      ItemSelInvID := ItemID;
      Equip();
    end;
    9:
    if (PC.EquipItems[4] = 0) then
    begin
      ItemSelInvID := ItemID;
      Equip();
    end;
  end;
end;

procedure AddDItem(const S: string; const Count: Integer);
var
  C: string;
begin
  if (Count <= 0) then Exit;
  C := ''; if (Count > 1) then C := ' (' + IntToStr(Count) + ')';
  TextOut(0, P, Chr(P + 95) + '. ' + S + C, cLtGray);
  Inc(P);
end;

procedure DrawItems();
var
  S: string;
  I, F: Integer;
  E: TExplodeResult;
begin
  with Cave.Dungeon[PC.Z].DropList do
  begin
    S := GetCellItems(PC.X, PC.Y);
    if (S = '') then Exit;
    E := SU.Explode(String(','), S);
    F := Length(E);
    if F > 26 then F := 26;
    for I := 0 to F - 1 do
    begin
      AddDItem(Lang.Lang((StrToInt(SU.Key('-', E[I]))) + 700),
        StrToInt(SU.Value('-', E[I], '1')));
    end;
  end;
end;

procedure DrawCellItems();
var
  C, K: Word;
begin
  C := Cave.Dungeon[PC.Z].DropList.CellItemsCount(PC.X, PC.Y);
  if (C = 1) then
  begin
    K := ord('A');
    KeysCellItems(K);
    Draw();
    Exit;
  end;
  P := 2;
  TitleOut(Lang.Lang(203));
  DrawItems();
  BarOut('[esc]', Lang.Lang(40), true);
  if (C > 0) then
  begin
    BarOut('[a-' + Chr(96 + C) + ']', Lang.Lang(152), false);
    BarOut('[space]', Lang.Lang(159));
  end;
end;

procedure KeysCellItems(var Key: Word);
var
  C, I, J, D: Integer;
  K: Word;
  S: string;
  E: TExplodeResult;
begin
  case Key of
    27:
    begin
      GameFrame := gfGame;
      ShowFloorObjects(PC.X, PC.Y, True);
      Draw();
    end;
    32:
    with Cave.Dungeon[PC.Z].DropList do
    begin
      C := CellItemsCount(PC.X, PC.Y);
      for I := 0 to C do
      begin
        K := ord('A');
        KeysCellItems(K);
      end;
    end;
    ord('A')..ord('Z'):
    with Cave.Dungeon[PC.Z].DropList do
    begin
      C := CellItemsCount(PC.X, PC.Y);
      I := Key - (ord('A') - 1);
      if (I <= C) then
      begin
        if not IsEmptyCell(PC.X, PC.Y) then
        begin
          D := 1;
          S := GetCellItems(PC.X, PC.Y);
          E := SU.Explode(String(','), S);
          if (Pos('-', E[I - 1]) > 0) then
          begin
            D := StrToInt(Copy(E[I - 1], Pos('-', E[I - 1]) + 1, Length(E[I - 1])));
            E[I - 1] := Copy(E[I - 1], 1, Pos('-', E[I - 1]) - 1);
          end;
          // ID предмета
          S := '';
          J := StrToInt(E[I - 1]);
          if (D > 1) then S := ' (' + IntToStr(D) + ')';
          Inc(PC.Items[J], D);
          // Scores
          case J of
            1: Inc(PC.Scores, D);
          end;
          // Нет места в инвентаре
          if (InvCount > 26) then
          begin
            Dec(PC.Items[J], D);
            AddMsg(Lang.Lang(19));
            GameFrame := gfGame;
            ShowFloorObjects(PC.X, PC.Y, True);
            Draw();
            Exit;
          end;
          AddMsg(Format(Lang.Lang(2090 + Rand(0, 9)), [Lang.Lang(J + 700) + S]));
          ClearCellItem(PC.X, PC.Y, I - 1);
          // Автонадевание предмета
          AutoEquip(J);
          //
          GameFrame := gfGame;
          ShowFloorObjects(PC.X, PC.Y, True);
          Draw();
        end;
      end;
    end;
  end;
end;

end.

