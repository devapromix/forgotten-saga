unit uGame;

interface

procedure DrawRP();
procedure DrawGame();
procedure DrawCharProp(P: Byte);
procedure KeysGame(var Key: Word); overload;
procedure KeysGame(var Key: Char); overload;
procedure DrawDamageAndArmorLine(X, Y, Z, AColor1, AColor2: Integer);
procedure ShowFloorObjects(X, Y: Integer; V: Boolean; F: Boolean = False);
procedure DrawEnemy(X, Y: Integer; F: Boolean = False);
function VizCell(X, Y: Integer): Boolean;
procedure MovePlayer(DX, DY: Integer );
procedure MoveEnemies();

implementation

uses SysUtils, Math, uPC, uDraw, uConst, uMsg, uDungeon, uGraph, uColor, uLang,
  uKillList, uBox, uUtils, uDropList, uStringUtils, uBattle, uItem, uTypes, uInit,
  uSkills, uSpells, uCreature, uNPC, uMisc, uMagic, uMain, uJournal, uCellItems,
  uQuest, uEnemy;

procedure ShowFloorObjects(X, Y: Integer; V: Boolean; F: Boolean = False);
var
  I: Integer;
  S, H: string;
  D: TExplodeResult;
  K: Word;
begin
  if not DebugMode then
    if (not VizCell(X, Y) or not (GetDist(PC.X, PC.Y, X, Y) <= 10)) then Exit;
  if V then
  with Cave.Dungeon[PC.Z] do
  begin
    // NPC
    if F then
    with Cell[X][Y] do
    begin
      if (PC.Z = -1) then
      begin
        S := ''; H := '';
        I := GetNPCID(X, Y);
        if TMode then H := ' ' + Lang.Lang(171);
        if (I < 0) then Exit;
        if DebugMode then S := ' [' + IntToStr(I) + ']';
        InfoMsg := Lang.Lang(I + 2100) + H + S;
        Exit;
      end;
    end;
    //
    if Cell[X][Y].UpStairs then
    begin
      if (PC.Z = 0) then
        InfoMsg := Lang.Lang(103)
          else InfoMsg := Lang.Lang(101) + '.';
    end else
    if Cell[X][Y].DownStairs then
    begin
      if (PC.Z = -1) then
        InfoMsg := Lang.Lang(104)
          else InfoMsg := Lang.Lang(105) + '.';
    end else
    with DropList do
    begin
      if not IsEmptyCell(X, Y) then
      begin
        // Подн. золото
        if AutoPickUpGold then
        with DropList do
        begin
          S := GetCellItems(X, Y);
          D := SU.Explode(String(','), S);
          for I := 0 to High(D) do
            if (Pos('1-', D[I]) > 0) then
            begin
              K := I + ord('A');
              KeysCellItems(K);
            end;
        end;
        I := DropList.GetCellFirstItemID(X, Y);
        if (CellItemsCount(X, Y) > 1)
          then InfoMsg := Lang.Lang(30) + ' (' + Lang.Lang(I + 700) + ').'
            else InfoMsg := Lang.Lang(58) + ' ' + Lang.Lang(I + 700) + '.';
      end;
    end;
  end;
  // Look mode
  if F then
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  begin
    // PC
    if (X = PC.X) and (Y = PC.Y) then
    begin
      S := '';
      if SMode and (SpellTypeIP(PC.ActSpell) = 1) then
      begin
        S := Lang.Lang(276);
        InfoMsg := Trim(PC.Name + ' [' + IntToStr(PC.Life) + '/' + IntToStr(PC.MaxLife) + '] ' + S);
      end;
    end;
    // Creature
    if (Creature > 0) then
    begin
      S := ''; H := '';
      if SMode then
      case GetItemType(PC.EquipItems[1]) of
        0    : if (SpellTypeIP(PC.ActSpell) = 0) then S := Lang.Lang(131);
        2..4 : S := Lang.Lang(173);
          else S := Lang.Lang(131);
      end;
      if DebugMode then
        H := ' [' + IntToStr(CreatureLife)
          + '/' + IntToStr(GetMaxCreatureLife(Creature, Boss)) + ']';
      InfoMsg := Lang.Lang(900 + Creature);
      D := SU.Explode(string(','), InfoMsg);
      I := Boss; if (High(D) < Boss) then I := High(D);
      InfoMsg := Trim(D[I]) + H + ' ' + S;
    end;
  end;
end;

procedure ModeShowFloorObjects();
begin
  if LMode or TMode then ShowFloorObjects(Cursor.X, Cursor.Y, True, True);
  if SMode then ShowFloorObjects(Cursor.X, Cursor.Y, False, True);
end;

//
procedure DrawCharProp(P: Byte);

procedure DisplayCharProp();
var
  S: string;
  I, Z, X: Integer;
  E: TExplodeResult;
begin
  X := 5;
  TextOut(X, P + 3, Lang.Lang(286) + ':', cRdYellow);
  if (PC.ClassID = 0) then TextOut(20, P + 3, IntToStr(RaceProp(PC.RaceID).Str), cRdYellow)
    else TextOut(20, P + 3, IntToStr(RaceProp(PC.RaceID).Str) + ' + ' + IntToStr(ClassProp(PC.ClassID).Str), cRdYellow);
  TextOut(X, P + 4, Lang.Lang(287) + ':', cRdYellow);
  if (PC.ClassID = 0) then TextOut(20, P + 4, IntToStr(RaceProp(PC.RaceID).Dex), cRdYellow)
    else TextOut(20, P + 4, IntToStr(RaceProp(PC.RaceID).Dex) + ' + ' + IntToStr(ClassProp(PC.ClassID).Dex), cRdYellow);
  TextOut(X, P + 5, Lang.Lang(288) + ':', cRdYellow);
  if (PC.ClassID = 0) then TextOut(20, P + 5, IntToStr(RaceProp(PC.RaceID).Sta), cRdYellow)
    else TextOut(20, P + 5, IntToStr(RaceProp(PC.RaceID).Sta) + ' + ' + IntToStr(ClassProp(PC.ClassID).Sta), cRdYellow);
  TextOut(X, P + 6, Lang.Lang(289) + ':', cRdYellow);
  if (PC.ClassID = 0) then TextOut(20, P + 6, IntToStr(RaceProp(PC.RaceID).Int), cRdYellow)
    else TextOut(20, P + 6, IntToStr(RaceProp(PC.RaceID).Int) + ' + ' + IntToStr(ClassProp(PC.ClassID).Int), cRdYellow);
  //
  Inc(X, ColWidth);
  TextOut(X,      P + 4, Lang.Lang(23) + ':', cRdGreen);
  if (PC.ClassID = 0) then TextOut(31 + 15, P + 4, IntToStr(RaceProp(PC.RaceID).Sta * 5), cRdGreen)
    else TextOut(31 + 15, P + 4, IntToStr((RaceProp(PC.RaceID).Sta + ClassProp(PC.ClassID).Sta) * 5), cRdGreen);
  //
  TextOut(X,      P + 5, Lang.Lang(24) + ':', cRdBlue);
  if (PC.ClassID = 0) then TextOut(31 + 15, P + 5, IntToStr(RaceProp(PC.RaceID).Int * 3), cRdBlue)
    else TextOut(31 + 15, P + 5, IntToStr((RaceProp(PC.RaceID).Int + ClassProp(PC.ClassID).Int) * 3), cRdBlue);
  //
  TextOut(X,      P + 6, Lang.Lang(14) + ':', cRdRed);
  if (PC.ClassID = 0) then TextOut(31 + 15, P + 6, IntToStr(Percent(56, 50 - RaceProp(PC.RaceID).Int)), cRdRed)
    else TextOut(31 + 15, P + 6, IntToStr(Percent(56, 50 - (RaceProp(PC.RaceID).Int + ClassProp(PC.ClassID).Int))), cRdRed);
  
  Inc(X, ColWidth);
  if (PC.ClassID > 0) then
  begin
    // Skils
    Z := 1;
    for I := Low(TSkills) to High(TSkills) do
      if (ClassProp(PC.ClassID).Skills[I].Value >= 10) then
      begin
        TextOut(X, P + 2 + Z, Lang.Lang(1600 + I - 1)
          + ', ' + IntToStr(ClassProp(PC.ClassID).Skills[I].Value), cRdYellow);
        Inc(Z);
      end;
    // Items
    Z := 1;
    Inc(X, ColWidth);
    S := Trim(GetClassEq());
    if (S <> '') then
    begin
      E := SU.Explode(string(','), S);
      for I := 0 to High(E) do
      begin
        TextOut(X, P + 2 + Z, Trim(E[I]), cRdYellow);
        Inc(Z);
      end;
    end;
  end;
end;

begin
  if (PC.RaceID > 0) or (PC.ClassID > 0) then DisplayCharProp();
end;

// Right panel
procedure DrawRP();
var
  C, I, L, P, U, M: Integer;
  S, H, V: string;
begin
  M := ScreenWidth div 5;
  V := ''; for I := 1 to M do V := V + ' ';
  P := ScreenWidth - PCPanelWidth;
  U := 0;
  if (Length(Lang.Lang(14)) > U) then U := Length(Lang.Lang(14));
  if (Length(Lang.Lang(15)) > U) then U := Length(Lang.Lang(15));
  if (Length(Lang.Lang(23)) > U) then U := Length(Lang.Lang(23));
  if (Length(Lang.Lang(24)) > U) then U := Length(Lang.Lang(24));
  U := U + P + 2;
  //
  C := cRdGreen;
  L := Round(PC.Life * M / PC.MaxLife);
  S := ''; for I := L - 1 downto 0 do S := S + ' ';
  if (S = '') and (PC.Life > 0) then S := ' ';
  TextOut(P, 1, Lang.Lang(23) + ':', C);
  TextOut(U, 1, IntToStr(PC.Life) + '/' + IntToStr(PC.MaxLife), C);
  if (PC.Life > 0) then
  begin
    BG.Canvas.Brush.Color := cDkGray;
    TextOut(ScreenWidth - M, 1, V, cDkGray);
    BG.Canvas.Brush.Color := C;
    TextOut(ScreenWidth - M, 1, S, C);
    BG.Canvas.Brush.Color := cBlack;
  end;
  //
  C := cRdBlue;
  L := Round(PC.Mana * M / PC.MaxMana);
  S := ''; for I := L - 1 downto 0 do S := S + ' ';
  if (S = '') and (PC.Mana > 0) then S := ' ';
  TextOut(P, 2, Lang.Lang(24) + ':', C);
  TextOut(U, 2, IntToStr(PC.Mana) + '/' + IntToStr(PC.MaxMana), C);
  if (PC.Life > 0) then
  begin
    BG.Canvas.Brush.Color := cDkGray;
    TextOut(ScreenWidth - M, 2, V, cDkGray);
    BG.Canvas.Brush.Color := C;
    TextOut(ScreenWidth - M, 2, S, C);
    BG.Canvas.Brush.Color := cBlack;
  end;
  //
  C := cRdYellow;
  L := Round(PC.Exp * M / PC.MaxExp);
  S := ''; for I := L - 1 downto 0 do S := S + ' ';
  if (S = '') and (PC.Exp > 0) then S := ' ';
  H := ''; if DebugMode then H := ' (' + IntToStr(PC.SkillPoint) + ')';
  TextOut(P, 3, Lang.Lang(15) + ':', C);
  TextOut(U, 3, IntToStr(PC.Exp) + '/' + IntToStr(PC.MaxExp) + H, C);
  if (PC.Life > 0) then
  begin
    BG.Canvas.Brush.Color := cDkGray;
    TextOut(ScreenWidth - M, 3, V, cDkGray);
    BG.Canvas.Brush.Color := C;
    TextOut(ScreenWidth - M, 3, S, C);
    BG.Canvas.Brush.Color := cBlack;
  end;
  //
  C := cRdRed;
  L := Round(PC.Fury * M / PC.MaxFury);
  S := ''; for I := L - 1 downto 0 do S := S + ' ';
  if (S = '') and (PC.Fury > 0) then S := ' ';
  H := ''; if DebugMode then H := ' (' + IntToStr(PC.FuryCounter) + ')';
  TextOut(P, 4, Lang.Lang(14) + ':', C);
  TextOut(U, 4, IntToStr(PC.Fury) + '/' + IntToStr(PC.MaxFury) + H, C);
  if (PC.Life > 0) then
  begin
    BG.Canvas.Brush.Color := cDkGray;
    TextOut(ScreenWidth - M, 4, V, cDkGray);
    BG.Canvas.Brush.Color := C;
    TextOut(ScreenWidth - M, 4, S, C);
    BG.Canvas.Brush.Color := cBlack;
  end;
end;

function GetCritDamage: Word;
begin
  Result := 0;
  case GetItemType(PC.EquipItems[1]) of
    0:       // Интеллект
      Result := PC.Int;
    1, 2, 4: // Сила
      Result := PC.Str;
    3, 5, 6: // Ловкость
      Result := PC.Dex;
  end;
end;

procedure DrawDamageAndArmorLine(X, Y, Z, AColor1, AColor2: Integer);
var
  IP: TItemPropertiesResult;
  C, I, V, H: Integer;
  S, U: string;
begin
  // Защита
  if (Z = 0) then
  begin
    Z := Length(Lang.Lang(20)) + 2;
    V := Length(Lang.Lang(18)) + 2;
  end else V := Z;
  TextOut(X, Y + 1, Lang.Lang(18) + ':', AColor1);
  TextOut(X + V, Y + 1, IntToStr(GetIntEquipArmor()), AColor2);
  // Урон
  IP := GetEquipDamage();
  TextOut(X, Y, Lang.Lang(20) + ':', AColor1);
  // Атака
  if (PC.ActSpell > 0) and (GetItemType(PC.EquipItems[1]) = 0)
    and (SpellTypeIP(PC.ActSpell) = 0) then
  begin
    C := SpellColor(PC.ActSpell);
    H := ActSpellDamage;
  end else
  begin
    C := AColor2;
    H := 0;
  end;
  //
  S := ''; U := '';
  I := PC.Skills[GetItemType(PC.EquipItems[1]) + 1].Value;
  if (I > 0) then
    S := ', ' + IntToStr(IP.MinDamage + H + GetCritDamage) + '-'
      + IntToStr(IP.MaxDamage + H + GetCritDamage){ + ' (' + IntToStr(I) + '%)'};
  if (PC.ActSpell = 0) and (GetItemType(PC.EquipItems[1]) = 0) then S := '';
  U := IntToStr(IP.MinDamage + H) + '-' + IntToStr(IP.MaxDamage + H) + S;
  if (PC.ActSpell > 0) and (SpellTypeIP(PC.ActSpell) = 0)
    and (GetItemType(PC.EquipItems[1]) = 0) then
  begin
    S := IntToStr(IP.MinDamage) + '-' + IntToStr(IP.MaxDamage) + ', ';
    TextOut(X + Z, Y, S, AColor2);
    Z := Z + Length(S);
  end;
  if (SpellTypeIP(PC.ActSpell) = 1) and (GetItemType(PC.EquipItems[1]) = 0) then
  begin
    U := IntToStr(IP.MinDamage) + '-' + IntToStr(IP.MaxDamage);
    C := AColor2;
  end;
  TextOut(X + Z, Y, U, C);
end;

procedure DrawPanel();
var
  I, L, P: Integer;
  S: string;
  IP: TItemPropertiesResult;
  M: Byte;
begin
  DrawRP();
  P := ScreenWidth - PCPanelWidth;
  // Weapon damage
  IP := GetEquipDamage();
  // Act. magic spell or arrows or bolts
  S := '';
  M := GetItemType(PC.EquipItems[1]);
  case M of
    // Show arrows or bolts
    5, 6:
       begin
         S := ' (' + IntToStr(PC.Items[M + 13]) + ')';
         if (PC.Items[M + 13] > 0) then
           TextOut(5, Lang.Lang(M + 713) + S, cRdYellow, alRight)
             else TextOut(5, Lang.Lang(M + 713) + S, cRdGray, alRight);
       end;
    // Show act. spell   
    else begin
         case SpellTypeIP(PC.ActSpell) of
            1 : S := ' [' + IntToStr(ActSpellDamage) + ']';
           else S := ' [' + IntToStr(ActSpellDamage + IP.MinDamage)
                  + '-' + IntToStr(ActSpellDamage + IP.MaxDamage) + ']';
         end;
         if (PC.ActSpell > 0) and (SpellTypeIP(PC.ActSpell) = 1) then
         begin
           if (ActSpellManaCost <= PC.Mana) then
             TextOut(5, Lang.Lang(PC.ActSpell + 3000) + S, SpellColor(PC.ActSpell), alRight)
               else TextOut(5, Lang.Lang(PC.ActSpell + 3000) + S, cRdGray, alRight);
         end else
           if (PC.ActSpell > 0) and (ActSpellManaCost > 0) then
             if (M = 0) and (ActSpellManaCost <= PC.Mana) then
               TextOut(5, Lang.Lang(PC.ActSpell + 3000) + S, SpellColor(PC.ActSpell), alRight)
                 else TextOut(5, Lang.Lang(PC.ActSpell + 3000) + S, cRdGray, alRight);
       end;
  end;
  L := 1;
  for I := 1 to 4 do
  if (PC.EquipItems[I] > 0) then
  begin
    TextOut(P, 5 + L, Lang.Lang(PC.EquipItems[I] + 700), cGray);
    Inc(L);
  end;
  // Урон и защита
  DrawDamageAndArmorLine(P, 11, 0, cGray, cGray);
  // Голод
  S := ''; if DebugMode then S := ' (' + IntToStr(PC.Hunger) + ')';
  if (PC.Life > 0) then
  case PC.Hunger of
      0 .. 100 : TextOut(19, Lang.Lang(25) + S, cLtRed, alRight);
    101 .. 250 : TextOut(19, Lang.Lang(26) + S, cLtYellow, alRight);
    251 .. 500 : TextOut(19, Lang.Lang(27) + S, cLtGreen, alRight);
    else if DebugMode then TextOut(19, IntToStr(PC.Hunger), cWhiteGre, alRight);
  end;
end;

function VizCell(X, Y: Integer): Boolean;
begin
  Result := Cave.Dungeon[PC.Z].Cell[X][Y].Visible;
end;

procedure LineTo(X, Y: Integer);
var
  I, L, AX, AY: Integer;
  LR: Real;
begin
  L := Math.Max(Abs(PC.X - X), Abs(PC.Y - Y)) + 1;
  for I := 1 to L do
  begin            
    LR := I / L;
    AX := PC.X + Trunc((X - PC.X) * LR);
    AY := PC.Y + Trunc((Y - PC.Y) * LR);
    Cave.Dungeon[PC.Z].Cell[AX][AY].Visible := True;
    if (Cave.Dungeon[PC.Z].Cell[AX][AY].Terrain = 1) then Break;
  end;
end;

var W: Integer;

procedure DrawEnemy(X, Y: Integer; F: Boolean = False);
var
  T: TTileInfo;
  I, C, HY: Integer;
begin
  with Cave.Dungeon[PC.Z].Cell[X][Y] do
  if (Creature > 0) then
  begin
    if (Boss > 0) then
    begin
      T := SetTileInfo(Chr(Creature + 64), Creature * 10000);
      if (Boss = 4) then T := SetTileInfo('@', cWhiteGre);
    end else T := SetTileInfo(Chr(Creature + 96), Creature * 20000);
    if F then C := cLtRed else C := T.Color;
    TextOut(X, Y + 1, T.Char, DarkColor(C, W * 8));
    // Lifebar
    with BG.Canvas do begin
      Pen.Width := 3;
      HY := ((Y + 1) * CharHeight) + 1;
      Pen.Color := DarkColor(cGray, W * 8);
      MoveTo(X * CharWidth + 1, HY);
      LineTo(((X + 1) * CharWidth - 1) - 2, HY);
      Pen.Color := DarkColor(cRed, W * 8);
      MoveTo(X * CharWidth + 1, HY);
      I := Round(CreatureLife * (CharWidth - 1) / GetMaxCreatureLife(Creature, Boss)) - 3;
      if (I < 0) then I := 0;
      LineTo(X * CharWidth + 1 + I, HY);
    end;
  end;
end;

procedure Heal();
begin
  if (PC.Life < PC.MaxLife) then
  begin
    if (PC.Mana >= ActSpellManaCost) then
    begin
      AddMsg(Format(Lang.Lang(140 + Rand(0, 4)), [Lang.Lang(PC.ActSpell + 3000)]));
      Dec(PC.Mana, ActSpellManaCost);
      if (Rand(1, 100) <= PC.Skills[15].Value + 50) then
      begin
        AddMsg(Format(Lang.Lang(273), [ActSpellDamage]));
        SkillUp(15);
        Inc(PC.Life, ActSpellDamage);
        if (PC.Life > PC.MaxLife) then PC.Life := PC.MaxLife;
      end else AddMsg(Lang.Lang(277));
    end else InfoMsg := Lang.Lang(138);
  end else AddMsg(Lang.Lang(274));
  SMode := not SMode;
  ModeShowFloorObjects();
end;

procedure Shoot();
var
  T: Integer;
begin
  LLPos := 0;
  if LMode then LMode := False;
  if TMode then TMode := False;
  // Выходим, если у нас нет соотв. оружия или нет зарядов или закл.
  T := GetItemType(PC.EquipItems[1]);
  // Лечение без оружия
  if (T = -1) and (Cursor.X = PC.X) and (Cursor.Y = PC.Y)
    and (PC.ActSpell > 0) and (SpellTypeIP(PC.ActSpell) = 1) then
  begin
    Heal();
    MoveEnemies();
    Exit;
  end;
  // Если не посох, топор, копье, молот или стрелковое оружие
  case T of
    -1:
    begin
      InfoMsg := Lang.Lang(118);
      Exit;
    end;
    1, 7..9:
    begin
      InfoMsg := Lang.Lang(119);
      Exit;
    end;
  end;
  // Посохи и заклинания
  if (T = 0) then
  begin
    // Нет заклинания
    if (PC.ActSpell = 0) then
    begin
      InfoMsg := Lang.Lang(134);
      Exit;
    end;
    // Недостаточно маны
    if (PC.Mana < ActSpellManaCost) then
    begin
      InfoMsg := Lang.Lang(138);
      Exit;
    end;
  end;
  // Луки и арбалеты
  if (T = 5) and (PC.Items[18] = 0) then begin InfoMsg := Lang.Lang(28); Exit; end;
  if (T = 6) and (PC.Items[19] = 0) then begin InfoMsg := Lang.Lang(29); Exit; end;
  // Исцеление магией
  if SMode and (Cursor.X = PC.X) and (Cursor.Y = PC.Y) and (SpellTypeIP(PC.ActSpell) = 1) then
  begin
    Heal();
    MoveEnemies();
    Exit;
  end;
  // Атака дист.
  if SMode and ((Cave.Dungeon[PC.Z].Cell[Cursor.X][Cursor.Y].Creature > 0)
    or (Cave.Dungeon[PC.Z].Cell[Cursor.X][Cursor.Y].Terrain = 99)) then
      begin
        DistBattle(Cursor.X, Cursor.Y);
      end;
  SMode := not SMode;
  NoSpell := False;
  if SMode then
  case SpellTypeIP(PC.ActSpell) of
     1 : InfoMsg := Lang.Lang(275);
    else InfoMsg := Lang.Lang(132);
  end;
  ModeShowFloorObjects();
end;

procedure Look();
begin
  if SMode then SMode := False;
  if TMode then TMode := False;
  LMode := not LMode;
  if LMode then InfoMsg := Lang.Lang(133);
  ModeShowFloorObjects();
end;

procedure Talk();
begin
  if SMode then SMode := False;
  if LMode then LMode := False;
  if TMode and (GetNPCID(Cursor.X, Cursor.Y) >= 0) then
    TalkNPC(Cursor.X, Cursor.Y);
  TMode := not TMode;
  if TMode then InfoMsg := Lang.Lang(172);
  ModeShowFloorObjects();
end;

procedure DrawGame();
var
  X, Y, I, J, C, C1, C2, C3, C4, C5, C6: Integer;
  E: TPoint;
  S: string;
  Min, Max: TPoint;   
  T: TTileInfo;
  B: Boolean;
begin
  Min.X := PC.X - 10; if Min.X < 0 then Min.X := 0;
  Max.X := PC.X + 10; if Max.X > DungeonWidth - 1 then Max.X := DungeonWidth - 1;
  Min.Y := PC.Y - 10; if Min.Y < 0 then Min.Y := 0;
  Max.Y := PC.Y + 10; if Max.Y > DungeonHeight - 1 then Max.Y := DungeonHeight - 1;
  for Y := Min.Y to Max.Y do for X := Min.X to Max.X do
    Cave.Dungeon[PC.Z].Cell[X][Y].Visible := False;
  for I := Min.X to Max.X do LineTo(I, Min.Y);
  for I := Min.Y to Max.Y do LineTo(Max.X, I);
  for I := Max.X downto Min.X do LineTo(I, Max.Y);
  for I := Max.Y downto Min.Y do LineTo(Min.X, I);
  Cave.Dungeon[PC.Z].Cell[PC.X][PC.Y].Visible := True;
  //
  for Y := 0 to DungeonHeight - 1 do
    for X := 0 to DungeonWidth - 1 do

{
  // Test
  PC.X := 10;
  PC.Y := 10;
  for Y := PC.Y - 10 to PC.Y + 10 do for X := PC.X - 10 to PC.X + 10 do

}

  begin
    B := False;
    W := GetDist(PC.X, PC.Y, X, Y);
    if DebugMode then W := 0;
    if (not DebugMode and (W <= 10) and VizCell(X, Y)) or DebugMode then B := True;
    if B then begin
      //
      if (SMode or LMode or TMode) and (Cursor.X = X) and (Cursor.Y = Y) then
      begin
        with Cave.Dungeon[PC.Z].Cell[X][Y] do begin
          if (Creature > 0) or (Terrain = 99)
            then BG.Canvas.Brush.Color := cLtRed
              else BG.Canvas.Brush.Color := cRdYellow;
        end;
      end else BG.Canvas.Brush.Color := cBlack;
      // Цветовые схемы уровней
      case PC.Z of
        3:  // Каменоломни
        begin
          C1 := cDkGray;
          C2 := cRdGray;
          C3 := cGray;
          C4 := cRdGray;
          C5 := cDkGray;
          C6 := cLtGray;
        end;
        4:  // Изумрудные пещеры
        begin
          C1 := cDkGreen;
          C2 := cRdGreen;
          C3 := cDkGreen;
          C4 := cRdGreen;
          C5 := cDkTeal;
          C6 := cRdTeal;
        end;
        5:  // Родники
        begin
          C1 := cDkBlue;
          C2 := cRdBlue;
          C3 := cBlue;
          C4 := cLtNavy;
          C5 := cDkBlue;
          C6 := cDkBlue;
        end;
        22, 24: // Кровавый Колодец, Грот Крови и Обитель Хаоса
        begin
          C1 := cRed;
          C2 := cDkBrown;
          C3 := cDkRed;
          C4 := cDkYellow;
          C5 := cDkRed;
          C6 := cRdRed;
        end; // Обычные уровни
        else begin
          C1 := cDkBlue;
          C2 := cRdGray;
          C3 := cDkGreen;
          C4 := cDkGray;
          C5 := cDkBrown;
          C6 := cDkTeal;
        end;
      end;
      // Blood
      with Cave.Dungeon[PC.Z].Cell[X][Y] do
      if (Blood > 0) then
      begin
        C1 := Blood * 100;
        C2 := Blood * 100;
        C3 := Blood * 100;
        C4 := Blood * 100;
        C5 := Blood * 100;
        C6 := Blood * 100;
      end;
      //
      with Cave.Dungeon[PC.Z].Cell[X][Y] do
      begin
        case Terrain of
           0: begin
                // Цвет пола
                case PC.Z of
                   1: C := DarkColor(cDkTeal, 75);
                   2: C := DarkColor(cDkBrown, 60);
                   3: C := DarkColor(cDkGray, 50);
                   4: C := DarkColor(cDkGreen, 50);
                   5: C := DarkColor(cDkNavy, 10);
                  13: C := DarkColor(cDkBlue, 50);
                  14: C := DarkColor(cDkGray, 60);
                  22: C := DarkColor(cDkRed, 30);
                  23: C := DarkColor(cDkRed, 40);
                  24: C := DarkColor(cDkRed, 50);
                  else C := cDkGray;
                end;
                if (Blood > 0) then C := Blood * 100;
                T := SetTileInfo('.', C);
              end;
           1: begin
                // Цвет стен
                case PC.Z of
                   1: C := DarkColor(cDkTeal, 0);
                   2: C := DarkColor(cDkBrown, 0);
                   3: C := DarkColor(cDkGray, 0);
                   4: C := DarkColor(cDkGreen, 0);
                   5: C := DarkColor(cBlue, 0);
                   6: C := DarkColor(cDkBlue, 0);
                  13: C := DarkColor(cRdBlue, 80);
                  14: C := DarkColor(cRdGray, 40);
                  15: C := DarkColor(cRdGray, 0);
                  19: C := DarkColor(cLtNavy, 50);
                  22: C := DarkColor(cRed, 0);
                  23: C := DarkColor(cDkRed, 0);
                  24: C := DarkColor(cDkBrown, 0);
                  else C := cDkGray;
                end;
                if (Blood > 0) then C := Blood * 100;
                T := SetTileInfo('#', C);
              end;
           2: T := SetTileInfo('+', cDkBrown);
           3: T := SetTileInfo('-', cDkGray);
           4: T := SetTileInfo('"', C1);
           5: T := SetTileInfo('"', C2);
           6: T := SetTileInfo('"', C3);
           7: T := SetTileInfo('"', C4);
           8: T := SetTileInfo('"', C5);
           9: T := SetTileInfo('"', C6);
          10: T := SetTileInfo('T', cGreen);
          11: if DebugMode then T := SetTileInfo('.', cLtGray)
                else T := SetTileInfo('.', cDkGray);
          // Traps
          90: T := SetTileInfo('''', cDkGray);
          91..96:
              begin
                C := 0;
                if Trap then
                case Terrain of
                   91: C := cLtGray;
                   96: C := cLtRed;
                  else C := cLtBlue;
                end else if DebugMode then C := cDkGray;
                case Terrain of
                   91: T := SetTileInfo('O', C);
                   96: T := SetTileInfo('^', C);
                  else T := SetTileInfo('''', C);
                end;
              end;  
          // Chests and Barrels
          97: T := SetTileInfo('_', cDkGray);
          98: T := SetTileInfo('=', cGray);
          99: T := SetTileInfo('O', cBrown);
        end;
        if UpStairs then T := SetTileInfo('<', cLtGray);
        if DownStairs then
        begin
          if (PC.Z = -1) then
            T := SetTileInfo('*', cBrown)
              else  T := SetTileInfo('>', cLtGray);
        end;
        if (PC.Z >= 0) and (Creature > 0) then
        begin
          DrawEnemy(X, Y);
          Continue;
        end;
        // Маг. эффекты
        if (Fire > 0) then begin
          J := PC.EquipItems[1];
          case Fire of
          // Огонь
           1: T := SetTileInfo('*', cLtRed);
          // Молния
          11: T := SetTileInfo('-', cLtPurple);
          12: T := SetTileInfo('/', cLtPurple);
          13: T := SetTileInfo('\', cLtPurple);
          14: T := SetTileInfo('|', cLtPurple);
          // Стрелы
          15: T := SetTileInfo('-', cWhiteYel);
          16: T := SetTileInfo('/', cWhiteYel);
          17: T := SetTileInfo('\', cWhiteYel);
          18: T := SetTileInfo('|', cWhiteYel);
          // Болты
          19: T := SetTileInfo('-', cWhiteGre);
          20: T := SetTileInfo('/', cWhiteGre);
          21: T := SetTileInfo('\', cWhiteGre);
          22: T := SetTileInfo('|', cWhiteGre);
          // Копья
          23: T := SetTileInfo('-', J * 5000);
          24: T := SetTileInfo('/', J * 5000);
          25: T := SetTileInfo('\', J * 5000);
          26: T := SetTileInfo('|', J * 5000);
          // Холод
          27..30:
              T := SetTileInfo('.', cLtBlue);
          // Земля
          31..34:
              T := SetTileInfo('#', cLtBrown);
          // Огонь
          35..38:
              T := SetTileInfo('*', cLtRed);
          end;
        end;
        // NPC
        if (PC.Z = -1) then
        for I := 0 to High(NPCPos) do
        if (X = NPCPos[I].X) and (Y = NPCPos[I].Y) then
        begin
          case I of
            0: C := cRdBlue;
            1: C := cWhiteGre;
            2: C := cPurple;
            3: C := cRdGreen;
            4: C := cRdRed;
            5: C := cRdPurple;
          else C := cLtYellow;
          end;
          T := SetTileInfo('@', C);
        end;
      end;
{      if (PC.Z = -1) and (T.Char = '@')
        and ((Cave.Dungeon[PC.Z].Cell[X][Y - 1].Terrain = 0)
          or (Cave.Dungeon[PC.Z].Cell[X][Y - 1].Terrain = 11)) then
          begin
            TextOut(X, Y, '?', DarkColor(cRdYellow, W * 8));
          end;  }
      TextOut(X, Y + 1, T.Char, DarkColor(T.Color, W * 8));
    end;
  end;
  //
  with Cave.Dungeon[PC.Z].DropList do
  for I := 0 to Count - 1 do
  begin
    B := False;
    E := GetCoords(I);
    W := GetDist(PC.X, PC.Y, E.X, E.Y);
    if DebugMode then W := 0;
    if (not DebugMode and (W <= 10) and VizCell(E.X, E.Y)) or DebugMode then B := True;
    if B then begin
      // Cursor shoot, spell, look
      if (SMode or LMode or TMode) and (Cursor.X = E.X) and (Cursor.Y = E.Y) then
        BG.Canvas.Brush.Color := cRdYellow
          else BG.Canvas.Brush.Color := cBlack;
      J := GetCellFirstItemID(E.X, E.Y);
      case J of
          1: T := SetTileInfo('$', cLtYellow);
          // Растения
          2: T := SetTileInfo(':', cWhiteGre);
          3: T := SetTileInfo(':', cDkGreen);
          4: T := SetTileInfo(':', cRdGreen);
          5: T := SetTileInfo(':', cWhiteYel);
          6: T := SetTileInfo(':', cLtGreen);
          7: T := SetTileInfo(':', cWhite);
          8: T := SetTileInfo(':', cLtRed);
          9: T := SetTileInfo(':', cRdGray);
          // Напитки
         10: T := SetTileInfo('!', cLtRed);
         11: T := SetTileInfo('!', cRdRed);
         12: T := SetTileInfo('!', cRdYellow);
         13: T := SetTileInfo('!', cLtBlue);
         14: T := SetTileInfo('!', cRdBlue);
         15: T := SetTileInfo('!', cBlue);
         16: T := SetTileInfo('!', cPurple);
         17: T := SetTileInfo('!', cLtPurple);
         // Стрелы и болты
         18: T := SetTileInfo(';', cWhiteYel);
         19: T := SetTileInfo(';', cWhiteGre);
         // Экипировка
         20..99:
             begin
               S := IntToStr(J);
               case S[2] of
                 '0': T := SetTileInfo(')', J * 5000); // Посохи
                 '1': T := SetTileInfo('/', J * 5000); // Мечи
                 '2': T := SetTileInfo('(', J * 5000); // Секиры
                 '3': T := SetTileInfo('|', J * 5000); // Копья
                 '4': T := SetTileInfo('\', J * 5000); // Молоты
                 '5': T := SetTileInfo('{', J * 5000); // Луки
                 '6': T := SetTileInfo('}', J * 5000); // Арбалеты
                 '7': T := SetTileInfo('&', J * 5000); // Щиты
                 '8': T := SetTileInfo('^', J * 5000); // Шлемы
                 '9': T := SetTileInfo(']', J * 5000); // Туники
               end;
             end;
        // Трофеи
        101..126:
             begin
               with Cave.Dungeon[PC.Z].Cell[E.X][E.Y] do
                 if (Blood > 0) then C := Blood * 100 else C := cDkGray;
               T := SetTileInfo('%', C);
             end;
        //
        127: T := SetTileInfo('!', cWhiteGre);
        128: T := SetTileInfo('!', cWhiteGre);
        129: T := SetTileInfo('!', cWhiteGre);
        //
        150: T := SetTileInfo(',', cLtRed);
        151: T := SetTileInfo(',', cLtGreen);
        152: T := SetTileInfo(',', cLtYellow);
        153: T := SetTileInfo(',', cLtBlue);
        // Отмычки
        160: T := SetTileInfo(',', cWhiteYel);
        // Свитки
        161..184:
             T := SetTileInfo('~', SpellColor(J - 160));
        //
        else T := SetTileInfo('#', cWhite);
      end;
      with Cave.Dungeon[PC.Z].Cell[E.X][E.Y] do
      if (Creature = 0) and (Terrain < 98) then
        TextOut(E.X, E.Y + 1, T.Char, DarkColor(T.Color, W * 8));
    end;
  end;
  //
  DrawPC();
  DrawPanel();
  DrawMsg(True, 18, 20);
end;

procedure Fill();
begin
  if (PC.Hunger <= 0) then Exit;
  if (PC.Life <= 0) then
  begin
    DeadPC();
    Exit;
  end;
  // Восстановление
  if (PC.Life < PC.MaxLife) and (PC.LifeRes > 0) then Inc(PC.Life);
  if (PC.LifeRes > 0) then Dec(PC.LifeRes);
  if (PC.Mana < PC.MaxMana) and (PC.ManaRes > 0) then Inc(PC.Mana);
  if (PC.ManaRes > 0) then Dec(PC.ManaRes);

  // Пополнение жизни
  if (PC.Life < PC.MaxLife) and (Round(PC.T / 65) = PC.T / 65) then
  begin
    Inc(PC.Life, PC.Sta div 7);
    AddMsg(Format(Lang.Lang(270), [PC.Sta div 7]));
    if (PC.Life > PC.MaxLife) then PC.Life := PC.MaxLife;
  end;

  // Пополнение маны
  if (PC.Mana < PC.MaxMana) and (Round(PC.T / 25) = PC.T / 25) then
  begin
    Inc(PC.Mana, PC.Int div 3);
    AddMsg(Format(Lang.Lang(271), [PC.Int div 3]));
    if (PC.Mana > PC.MaxMana) then PC.Mana := PC.MaxMana;
  end;
end;

procedure Check();
begin
  // Hunger
  if (PC.Hunger <= 0) then Dec(PC.Life);
  // Trap
  with Cave.Dungeon[PC.Z].Cell[PC.X][PC.Y] do
  case Terrain of
    91: begin
          if not TrapPit then
          begin
            AddMsg(Lang.Lang(1510 + Rand(0, 4)));
            TrapPit := True;
          end;
        end;
    92..96:
        DoTrap(Terrain - 90, PC.X, PC.Y);
  end;
  //
  if (PC.Life <= 0) then
  begin
    DeadPC();
    Exit;
  end;
end;

function MyCallback(X, Y: Integer): Boolean;
begin
  case Cave.Dungeon[PC.Z].Cell[X][Y].Terrain of
    1, 10, 91..99: Result := False;
    else Result := True;
  end;
end;

type
  TGetXYVal = function(X, Y: integer): boolean;stdcall;
function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;
  external 'BeaRLibPF.dll';

procedure MoveEnemies();
var
  Z, NX, NY: Integer;
begin
  if (PC.Z >= 0) then
  begin
    for Z := 0 to Enemy.Count - 1 do
      if (Enemy.EnemyList[Z].X >= PC.X - 11) and (Enemy.EnemyList[Z].X <= PC.X + 11)
        and (Enemy.EnemyList[Z].Y >= PC.Y - 11) and (Enemy.EnemyList[Z].Y <= PC.Y + 11) then
        begin
          if not DoAStar(DungeonWidth, DungeonHeight, Enemy.EnemyList[Z].X, Enemy.EnemyList[Z].Y,
            PC.X, PC.Y, @MyCallback, NX, NY) then Exit;
          if (NX = PC.X) and (NY = PC.Y) then
          begin
            if (PC.Life > 0) and (Cave.Dungeon[PC.Z].
              Cell[Enemy.EnemyList[Z].X, Enemy.EnemyList[Z].Y].CreatureLife > 0) then
                EnemyRound(Enemy.EnemyList[Z].X, Enemy.EnemyList[Z].Y);
          end else Enemy.Move(Z, Enemy.EnemyList[Z].X, Enemy.EnemyList[Z].Y, NX, NY);
        end;
    BattleAnimations();
  end;
end;

procedure MovePlayer(DX, DY: Integer );
var
  X, Y, NX, NY: Integer;
begin
  if SMode or LMode or TMode then
  begin
    NX := Cursor.X + DX;
    NY := Cursor.Y + DY;
    if NX < 0 then NX := 0;
    if NY < 0 then NY := 0;
    if NX > DungeonWidth - 1 then NX := DungeonWidth - 1;
    if NY > DungeonHeight - 1 then NY := DungeonHeight - 1;
    Cursor.X := NX;
    Cursor.Y := NY;
    ModeShowFloorObjects();
    // Trap detect
    for Y := Cursor.Y - 3 to Cursor.Y + 3 do
      for X := Cursor.X - 3 to Cursor.X + 3 do
        if (X > 3) and (X < DungeonWidth - 3 - 1) and (Y > 3) and (Y < DungeonHeight - 3 - 1) then
          with Cave.Dungeon[PC.Z].Cell[X][Y] do
            if not Trap and Visible and (Terrain >= 91) and (Terrain <= 96) then
              if (Rand(0, 200) <= PC.Skills[18].Value) then
              begin
                AddMsg('Trap detected!');
                SkillUp(18, Rand(5, 10));
                Inc(PC.Scores, 5);
                Trap := True;
              end;
    //
    Draw();
    Exit;
  end;
  // Do
  NX := PC.X + DX;
  NY := PC.Y + DY;
  if NX < 0 then NX := 0;
  if NY < 0 then NY := 0;
  if NX > DungeonWidth - 1 then NX := DungeonWidth - 1;
  if NY > DungeonHeight - 1 then NY := DungeonHeight - 1;
  with Cave.Dungeon[PC.Z].Cell[NX][NY] do
  begin
    // Battle
    if (Creature > 0) and (CreatureLife > 0) then
    begin
      Battle(NX, NY);
      MoveEnemies();
      if (PC.Life > 0) then Fill();
      Inc(PC.T);
      Draw();
      LLPos := 0;
      Exit;
    end;
    //
    case Terrain of
      // Stop
      1, 10:
      begin
        MoveEnemies();
        Draw();
        Inc(PC.T);
        Draw();
        LLPos := 0;
        Exit;
      end;
      // Open door
      2:
      begin
        Terrain := 3;
        AddMsg('Open door.');
        MoveEnemies();
        Inc(PC.T);
        Draw();
        LLPos := 0;
        Exit;
      end;
      // Chest
      98:
      begin
        DoChest(NX, NY);
        if (PC.Life <= 0) then DeadPC();
        MoveEnemies();
        Inc(PC.T);
        Draw();
        LLPos := 0;
        Exit;
      end;
      // Barrel
      99:
      begin
        DoBarrel(NX, NY);
        if (PC.Life <= 0) then DeadPC();
        MoveEnemies();
        Inc(PC.T);
        Draw();
        LLPos := 0;
        Exit;
      end;
    end;
    // Move
    if (PC.Z = -1) then MoveNPCs();
    if (PC.Z >= 0) and (PC.Hunger > 0) then
    begin
      if DebugMode then Dec(PC.Hunger, Rand(0, 1))
        else Dec(PC.Hunger);
    end;
    if (PC.FuryCounter > 0) then Dec(PC.FuryCounter);
    if (PC.FuryCounter <= 0) then PC.Fury := 0;
    Inc(PC.T);
    if (Cave.Dungeon[PC.Z].Cell[PC.X][PC.Y].Terrain <> 91) or (Rand(1, 24) = 1) then
    begin
      PC.X := NX;
      PC.Y := NY;
      Cursor.X := NX;
      Cursor.Y := NY;
      TrapPit := False;
    end;
  end;
  //
  MoveEnemies();
  Check();
  ShowFloorObjects(PC.X, PC.Y, True);
  LLPos := 0;
  Fill();
  Draw();
end;

procedure KeysGame(var Key: Word);
begin
  if (PC.Life <= 0) then
  begin
    GameFrame := gfDefeat;
    Draw();
    Exit;
  end;
  case Key of
    37: MovePlayer(-1,  0);
    38: MovePlayer( 0, -1);
    39: MovePlayer( 1,  0);
    40: MovePlayer( 0,  1);
    27: // Menu
    begin
      if LMode or SMode or TMode then
      begin
        LMode := False;
        SMode := False;
        TMode := False;
        Cursor.X := PC.X;
        Cursor.Y := PC.Y;
        Draw();
        Exit;
      end;
      GameFrame := gfMenu;
      Draw();
    end;
    ord('S'): // Shoot/Spell
    begin
      Shoot();
      Draw();
    end;
    ord('L'): // Look
    begin
      Look();
      Draw();
    end;
    ord('T'): // Talk
    begin
      Talk();
      Draw();
    end;
    ord('P'): // Character
    begin
      GameFrame := gfChar;
      Draw();
    end;
    ord('I'): // Inventory
    begin
      GameFrame := gfInv;
      Draw();
    end;
    ord('M'): // Messages
    begin
      GameFrame := gfMsg;
      Draw();
    end;
    ord('B'): // Magic book
    begin
      if PC.IsBook then GameFrame := gfBook else InfoMsg := Lang.Lang(250);
      Draw();
    end;
    ord('A'): // Alchemy
    begin
      if PC.IsAlchemy then GameFrame := gfAlchemy else InfoMsg := Lang.Lang(251);
      Draw();
    end;
    ord('C'): // Craft
    begin
      if PC.IsCraft then GameFrame := gfCraft else InfoMsg := Lang.Lang(252);
      Draw();
    end;
    ord('K'): // Kill list
    begin
      GameFrame := gfKills;
      Draw();
    end;
    ord('G'): // Pickup item
    begin
      if (Cave.Dungeon[PC.Z].DropList.CellItemsCount(PC.X, PC.Y) > 0) then GameFrame := gfCellItems;
      Draw();
    end;
    ord('Q'): // Quests
    begin
      GameFrame := gfJournal;
      Draw();
    end;
    ord('O'): // Test
    begin

    end;
    ord('V'): // test victory
    begin
      GameFrameScreen := gfVictory;
      GameFrame := gfVictory;
      Draw();
    end;
  end;
end;

procedure KeysGame(var Key: Char);
var
  I: Byte;
begin
  with Cave.Dungeon[PC.Z].Cell[PC.X][PC.Y] do
  case Key of
    '<': begin  // Up
           if UpStairs then
           begin
             Dec(PC.Z);
             AddMsg(Format(Lang.Lang(102), [Lang.Lang(2000 + PC.Z)]));
             ShowFloorObjects(PC.X, PC.Y, True);
             Enemy.Find;
             Draw();
           end;
         end;
    '>': begin  // Down
           if DownStairs then
           begin
             Inc(PC.Z);
             AddMsg(Format(Lang.Lang(106), [Lang.Lang(2000 + PC.Z)]));
             ShowFloorObjects(PC.X, PC.Y, True);
             Enemy.Find;
             Draw();
             // Квест qtLoc
             for I := 0 to Quests.Count - 1 do
               if (Quests.Quest[I].State = qsQuest)
                 and (Quests.Quest[I].QType = qtLoc)
                   and (Quests.Quest[I].ObjectCount = PC.Z) then
                     Quests.Quest[I].Progress := Quests.Quest[I].Progress + 1;
           end;
         end;
    '?': if DebugMode then
         begin // Help
           GameFrameTemp := gfGame;
           GameFrame := gfHelp;
           Draw();
         end;
    '-': if DebugMode then
         begin
           if (PC.Z > DungeonMin) then
             Dec(PC.Z);
           Draw();
         end;
    '+': if DebugMode then
         begin
           if (PC.Z < DungeonsCount - 1) then
             Inc(PC.Z);
           Draw();
         end;
  end;
end;

end.
