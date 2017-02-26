unit uBattle;

interface

uses uColor;

procedure DrawFlyItem(Damage: Integer = 0);
procedure Battle(const X, Y: Integer);
procedure DistBattle(const X, Y: Integer);
procedure EnemyDead(X, Y: Integer);
function EnemyName(const X, Y: Integer): string;
procedure EnemyRound(const X, Y: Integer);
procedure BattleAnimations(AColor: Integer = cLtRed);

implementation

uses SysUtils, uTypes, uStringUtils, uUtils, uLang, uDungeon, uPC, uMsg, uKillList,
  uConst, uDraw, uItem, uBox, uMain, uSkills, uGame, uSpells, uMisc,
  uMagic, uQuest, uEnemy;

function EnemyName(const X, Y: Integer): string;
var
  E: TExplodeResult;
begin
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  begin
    Result := Lang.Lang(Creature + 900);
    E := SU.Explode(string(','), Result);
    if (High(E) >= Boss) then Result := Trim(E[Boss]);
  end;
end;

procedure EnemyDead(X, Y: Integer);
var
  I, J, Exp: Integer;
  B: Boolean;
begin
//  if (PC.Life > 0) then
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  begin
    AddMsg(EnemyName(X, Y) + ' ' + Lang.Lang(40 + Rand(3, 9)));
    // Счетчик
    KillList.Kill(EnemyName(X, Y));
    // Квест qtKill
    for I := 0 to Quests.Count - 1 do
      if (Quests.Quest[I].State = qsQuest)
        and (Quests.Quest[I].QType = qtKill)
          and (Quests.Quest[I].ObjectID = Creature)
            and (Quests.Quest[I].ObjectCount > Quests.Quest[I].Progress)
              and (Boss = 0) then
                Quests.Quest[I].Progress := Quests.Quest[I].Progress + 1;
    //
    Cave.Dungeon[PC.Z].DropList.AddItem(X, Y, 100 + Creature);
    if (Creature > PC.TitleID) then PC.TitleID := Creature;
    // Доб. опыт и ген. вещи
    Exp := 0;
    for J := 0 to Boss + 5 do
    begin
      Exp := Exp + Creature;
      DropItem(X, Y);
    end;
    // Главный враг на 25-м уровне
    if (Boss = 4) then
    begin
      Exp := 500;
      IsVictory := True;
      Cave.Dungeon[PC.Z].DropList.AddItem(X, Y, 1, Rand(1, 9) * 25 + Rand(200, 250));
      Cave.Dungeon[PC.Z].DropList.AddItem(X, Y, Rand(150, 153), 1);
      Cave.Dungeon[PC.Z].DropList.AddItem(X, Y, Rand(0, 9) + 90, 1);
    end;
    // Доб. опыт
    B := AddExp(Exp);
    AddMsg(Format(Lang.Lang(70 + Rand(0, 4)), [Exp]));
    // Пов. уровень
    if B then
    begin
      AddMsg(Lang.Lang(75 + Rand(0, 4)));
      Inc(PC.SkillPoint, 5);
    end;
    // Стир. врага с карты
    with Enemy do for I := 0 to Count - 1 do
    if (EnemyList[I].X > 0) and (EnemyList[I].Y > 0)
      and (EnemyList[I].X = X) and (EnemyList[I].Y = Y) then
    begin
      EnemyList[I].X := 0;
      EnemyList[I].Y := 0;
    end; 
    CreatureLife := 0;
    Creature := 0;
    Boss := 0;
    // Ярость без навыка "Неистовство"
    Inc(PC.Fury);
    // Навык "Неистовство"
    if (Rand(5, 100) < PC.Skills[17].Value) then
    begin
      SkillUp(17, Rand(3, 5));
      Inc(PC.Fury, Rand(1, 2));
    end;
    if (PC.Fury > PC.MaxFury) then PC.Fury := PC.MaxFury;
    J := 0; if (PC.Skills[17].Value > 5) then J := Round(PC.Skills[17].Value div 3);
    PC.FuryCounter := PC.MaxFury + J;
    //
    Inc(PC.V);
  end;
end;

procedure BattleAnimations(AColor: Integer = cLtRed);
begin
  if AnimPC then DrawPC(AColor);
  if AnimEnemy then DrawEnemy(AnimEnemyPoint.X, AnimEnemyPoint.Y, True);
  if AnimPC or AnimEnemy then
  begin
    AnimPC := False;
    AnimEnemy := False;
    Refresh();
    Sleep(49);
  end;
end;

procedure DrawFlyItem(Damage: Integer = 0);
begin
  if SMode then
  case GetItemType(PC.EquipItems[1]) of
    // Магия
    0: if (SpellTypeIP(PC.ActSpell) = 0) then
         case ActSpellType of
           // Холод
           1: FlyItem(Cursor.X, Cursor.Y, 27);
           // Молния
           2: Lightning(Cursor.X, Cursor.Y, Damage);
           // Земля
           3: FlyItem(Cursor.X, Cursor.Y, 31);
           // Огонь
           4: FlyItem(Cursor.X, Cursor.Y, 35);
         end;
    // Оружие
    2..4:
       FlyItem(Cursor.X, Cursor.Y, 23);
    // Стрела
    5: FlyItem(Cursor.X, Cursor.Y, 15);
    // Болт
    6: FlyItem(Cursor.X, Cursor.Y, 19);
  end;
end;

procedure PCRound(const X, Y: Integer);
var
  I, J, G, Damage, Exp: Integer;
  B: Boolean;
  IP: TItemPropertiesResult;
begin
  if (PC.Life > 0) then
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  begin
    // Промах
    if (Rand(0, 100) > GetAccuracyPC(Creature)) then
    begin
      AddMsg('=== Промах ===');
      Exit;
    end;
    // Damage
    IP := GetEquipDamage();
    //
    AnimEnemy := True;
    AnimEnemyPoint := SetPoint(X, Y);
    // Урон врагу
    G := 0; // Крит. урон
    if (Rand(0, 100) <= PC.Skills[GetItemType(PC.EquipItems[1]) + 1].Value) then
    case GetItemType(PC.EquipItems[1]) of
      0:       // Интеллект
        if (PC.ActSpell > 0) and (SpellTypeIP(PC.ActSpell) = 0) and SMode then G := PC.Int;
      1, 2, 4: // Сила
        G := PC.Str;
      3, 5, 6: // Ловкость
        G := PC.Dex;
    end;
    Damage := Rand(G + IP.MinDamage, G + IP.MaxDamage);
    if (Damage < 1) then Damage := 1;
    // Урон заклинанием
    if (GetItemType(PC.EquipItems[1]) = 0) and (PC.ActSpell > 0) and SMode then
    begin
      if (SpellTypeIP(PC.ActSpell) = 0)
        and (Rand(1, 100) <= PC.Skills[ActSpellType + 12].Value + 50) then
      begin
        SkillUp(ActSpellType + 12);
        Damage := Damage + ActSpellDamage;
        AddMsg(Format(Lang.Lang(140 + Rand(0, 4)), [Lang.Lang(PC.ActSpell + 3000)]));
      end else
      begin
        AnimEnemy := False;
        AddMsg(Lang.Lang(277));
        Exit;
      end;
    end;
    // Fly
    DrawFlyItem(Damage);
    //
    if (G = 0) then
    case Rand(1, 2) of
      1: AddMsg(Format(Lang.Lang( 90 + Rand(0, 4)), [Damage]));
      2: AddMsg(Format(Lang.Lang( 90 + Rand(5, 9)), [EnemyName(X, Y), Damage]));
    end else
    case Rand(1, 2) of
      1: AddMsg(Format(Lang.Lang(190 + Rand(0, 4)), [Damage]));
      2: AddMsg(Format(Lang.Lang(190 + Rand(5, 9)), [EnemyName(X, Y), Damage]));
    end;
    I := CreatureLife; Dec(I, Damage); if (I < 0) then I := 0; CreatureLife := I;
    // Blood
    if (G > 0) then
      Cave.Dungeon[PC.Z].Cell[X + Rand(1, 3) - 2][Y + Rand(1, 3) - 2].Blood := Rand(1, 9);
    // Kill Enemy
    if (I = 0) then EnemyDead(X, Y);
    // Skills
    SkillUp(19);
    if (PC.EquipItems[1] > 0) then SkillUp(GetItemType(PC.EquipItems[1]) + 1, 1);
  end;
end;

procedure EnemyRound(const X, Y: Integer);
var
  Armor, Damage: Integer;
begin
  if (PC.Life > 0) then
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  begin
    //
    if (CreatureLife > 0) then
    // Защита
    begin
      Armor := GetIntEquipArmor();
      // Блок
      if (Armor > 0) and (Rand(0, 100) <= (PC.Skills[8].Value + 10)) then
      begin
        AddMsg(Format(Lang.Lang(180 + Rand(0, 9)), [EnemyName(X, Y)]));
        // Пов. навык "Защита"
        SkillUp(8, 5);
      // Навык "Уклонение"
      end else if (Rand(0, 100) < PC.Skills[10].Value) then
      begin
        AddMsg(Format(Lang.Lang(1680 + Rand(0, 9)), [EnemyName(X, Y)]));
        SkillUp(10, Rand(2, 3));
      // Промах
      end else if (Rand(0, 100) <= (Armor + 10)) then
      begin
        AddMsg(Format(Lang.Lang(80 + Rand(0, 9)), [EnemyName(X, Y)]));
      // Урон
      end else begin
        // Вр. повреждения
        Damage := ((Creature + Boss) * (100 - Armor)) div 100;
        if (Damage < 1) then Damage := 1;
        case Rand(1, 2) of
          1: AddMsg(Format(Lang.Lang(60 + Rand(0, 4)), [Damage]));
          2: AddMsg(Format(Lang.Lang(60 + Rand(5, 9)), [EnemyName(X, Y), Damage]));
        end;
        Dec(PC.Life, Damage);
        AnimPC := True;
        if (PC.Life <= 0) then
        begin
          LastEnemyName := Trim(EnemyName(X, Y));
          DeadPC();
        end;
      end;
    end;
  end;
end;

procedure DistBattle(const X, Y: Integer);
var
  AX, AY: Integer;
begin
  if SMode and (GetDist(PC.X, PC.Y, X, Y) <= 10) and VizCell(X, Y) then
  begin
    // Heal
    if (GetItemType(PC.EquipItems[1]) = 0) and (SpellTypeIP(PC.ActSpell) = 1) then
    begin
      AddMsg(Lang.Lang(272));
      Exit;
    end;
    // Barrel
    if (Cave.Dungeon[PC.Z].Cell[Cursor.X][Cursor.Y].Terrain = 99) then
    begin
      if (SpellTypeIP(PC.ActSpell) = 0) and (GetItemType(PC.EquipItems[1]) = 0) then
        AddMsg(Format(Lang.Lang(140 + Rand(0, 4)), [Lang.Lang(PC.ActSpell + 3000)]));
      DrawFlyItem();
      DoBarrel(Cursor.X, Cursor.Y);
    end else
    // Атака PC
      PCRound(X, Y);
    MoveEnemies();
    // Посох и заклинания
    if (GetItemType(PC.EquipItems[1]) = 0) then Dec(PC.Mana, ActSpellManaCost);
    // Топор, копье, молот
    if (GetItemType(PC.EquipItems[1]) >= 2) and (GetItemType(PC.EquipItems[1]) <= 4) then
    begin
      Cave.Dungeon[PC.Z].DropList.AddItem(X, Y, PC.EquipItems[1], 1);
      Dec(PC.Items[PC.EquipItems[1]]);
      if (PC.Items[PC.EquipItems[1]] = 0) then PC.EquipItems[1] := 0;
    end;
    // Лук
    if (GetItemType(PC.EquipItems[1]) = 5) then Dec(PC.Items[18]);
    // Арбалет
    if (GetItemType(PC.EquipItems[1]) = 6) then Dec(PC.Items[19]);
{    // Ответ от ближайших врагов
    for AX := PC.X - 1 to PC.X + 1 do
      for AY := PC.Y - 1 to PC.Y + 1 do
        if (Cave.Dungeon[PC.Z].Cell[AX][AY].Creature > 0) then
          EnemyRound(AX, AY); }
    BattleAnimations();
  end else InfoMsg := Lang.Lang(130);
end;

procedure Battle(const X, Y: Integer);
var
  T: Byte;
begin
  T := GetItemType(PC.EquipItems[1]);
  if (T <> 5) and (T <> 6) then PCRound(X, Y);
  BattleAnimations();
end;

end.
