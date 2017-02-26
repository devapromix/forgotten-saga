unit uItem;

interface
      
uses uTypes;

procedure DrawItem();
procedure Equip();
procedure KeysItem(var Key: Word);
function GetItemProperties(const ID: Integer): TItemPropertiesResult;
function GetEquipDamage(): TItemPropertiesResult;
function GetEquipArmor(): TItemPropertiesResult;
function GetIntEquipArmor(): Integer;
function GetItemType(const ID: Integer): Integer;
function GetItemLevel(const ID: Integer): Integer;
function GetItemStr(const ID: Integer): Integer;
function GetItemDex(const ID: Integer): Integer;
function GetItemInt(const ID: Integer): Integer;

implementation

uses SysUtils, uDraw, uConst, uColor, uGraph, uLang, uBox, uPC, uDungeon,
  uGame, uMsg, uStringUtils, uUtils, uSkills, uSpells;

procedure DrawItem();
var
  A: TItemPropertiesResult;
  I, C: Integer;
  S: string;
begin
  TitleOut(Lang.Lang(ItemSelInvID + 700));
  case ItemSelInvID of
    1..19:
      begin
        TextOut(0, 2, Lang.Lang(ItemSelInvID + 300), cLtGray);
      end;
    20..99:
      begin
        TextOut(0, 2, Lang.Lang(ItemSelInvID + 300), cLtGray);
        case ItemSelInvID of
          // Одноручное оружие
          21..24, 31..34, 41..44, 51..54, 61..64:
          TextOut(0, 4, Lang.Lang(260), cLtGray);
          // Стрелковое оружие
          25, 26, 35, 36, 45, 46, 55, 56, 65, 66, 75, 76, 85, 86, 95, 96:
          TextOut(0, 4, Lang.Lang(261), cLtGray);
          // Щит
          27, 37, 47, 57, 67, 77, 87, 97:
          TextOut(0, 4, Lang.Lang(262), cLtGray);
          // Шлем
          28, 38, 48, 58, 68, 78, 88, 98:
          TextOut(0, 4, Lang.Lang(263), cLtGray);
          // Броня
          29, 39, 49, 59, 69, 79, 89, 99:
          TextOut(0, 4, Lang.Lang(264), cLtGray);
          // Двуручное оружие
          else TextOut(0, 4, Lang.Lang(265), cLtGray);
        end;

        // Skill
        I := GetItemType(ItemSelInvID);
        if I > 7 then I := 7;
        if DebugMode then
          TextOut(5, 'Item type: ' + IntToStr(I), cGray, alRight);
        TextOut(0, 5, Lang.Lang(21) + ': ' + Lang.Lang(1600 + I)
          + ', ' + IntToStr(PC.Skills[I + 1].Value), cLtGray);
        BG.Canvas.Brush.Color := cDkGray;
        TextOut(25, 5, '                              ', cDkGray);
        C := Round(PC.Skills[I + 1].Exp * 30 / 100);
        S := ''; for I := C - 1 downto 0 do S := S + ' ';
        BG.Canvas.Brush.Color := cRdYellow;
        TextOut(25, 5, S, cRdYellow);
        BG.Canvas.Brush.Color := cBlack;

        // Item prop
        A := GetItemProperties(ItemSelInvID);
        case GetItemType(ItemSelInvID) of
          0..6: TextOut(0, 6, Lang.Lang(16) + ': ' + IntToStr(A.MinDamage) + '-' + IntToStr(A.MaxDamage), cLtGray);
          7..9: TextOut(0, 6, Lang.Lang(17) + ': ' + IntToStr(A.Armor), cLtGray);
        end;
        
        // Требования
        I := 9; C := cLtGray;
        begin
          TextOut(0, 8, Lang.Lang(280) + ':', cLtGray);
          C := cLtGray;
          if (PC.Level < GetItemLevel(ItemSelInvID)) then C := cRed;
          TextOut(0, I, Lang.Lang(284) + ': ' + IntToStr(GetItemLevel(ItemSelInvID)), C);
          Inc(I);
          C := cLtGray;
          if (GetItemStr(ItemSelInvID) > 0) then
          begin
            if (PC.Str < GetItemStr(ItemSelInvID)) then C := cRed;
            TextOut(0, I, Lang.Lang(286) + ': ' + IntToStr(GetItemStr(ItemSelInvID)), C);
            Inc(I);
          end;
          C := cLtGray;
          if (GetItemDex(ItemSelInvID) > 0) then
          begin
            if (PC.Dex < GetItemDex(ItemSelInvID)) then C := cRed;
            TextOut(0, I, Lang.Lang(287) + ': ' + IntToStr(GetItemDex(ItemSelInvID)), C);
            Inc(I);
          end;
          C := cLtGray;
          if (GetItemInt(ItemSelInvID) > 0) then
          begin
            if (PC.Int < GetItemInt(ItemSelInvID)) then C := cRed;
            TextOut(0, I, Lang.Lang(289) + ': ' + IntToStr(GetItemInt(ItemSelInvID)), C);
            Inc(I);
          end;
        end;
        //
      end;
    100..160:
      begin
        TextOut(0, 2, Lang.Lang(ItemSelInvID + 300), cLtGray);
      end;
    // Scrolls 
    161..184:
      begin
        I := ItemSelInvID - 160;
        S := ''; if DebugMode then S := ' ID: ' + IntToStr(ItemSelInvID);
        TextOut(0, 2, Lang.Lang(ItemSelInvID + 300) + S, cLtGray);
        TextOut(0, 4, Format(Lang.Lang(279), [SpellManaCost(I), TeachSpellManaCost(I), PC.Mana]), cLtGray);
      end;
    else
      begin
        S := ''; if DebugMode then S := ' ID: ' + IntToStr(ItemSelInvID);
        TextOut(0, 2, Lang.Lang(59) + S, cLtGray);
      end;
  end;
  if (ItemSelInvCount > 1) then
    TextOut(0, 3, Lang.Lang(39) + ' ' + IntToStr(ItemSelInvCount), cLtGray);
  BarOut('[esc]', Lang.Lang(36), true);
  if (ItemSelInvID > 1)
         then BarOut('[d]', Lang.Lang(42));
  case ItemSelInvID of
    20 .. 99: BarOut('[e]', Lang.Lang(38));
    10 .. 17: BarOut('[q]', Lang.Lang(33));
    2  ..  9: BarOut('[r]', Lang.Lang(32));
    161..184: BarOut('[t]', Lang.Lang(170));
  end;
  DrawMsg(True, 18, 19);
end;

procedure Drink();
begin
  if (PC.Items[ItemSelInvID] > 0) then
  begin
    Dec(PC.Items[ItemSelInvID]);
    case ItemSelInvID of
      10..12:
      begin
        case ItemSelInvID of
          10: PC.LifeRes := PC.LifeRes + 100;
          11: PC.LifeRes := PC.LifeRes + 150;
          12: PC.LifeRes := PC.LifeRes + 200;
        end;
      end;
      13..15:
      begin
        case ItemSelInvID of
          13: PC.ManaRes := PC.ManaRes + 100;
          14: PC.ManaRes := PC.ManaRes + 150;
          15: PC.ManaRes := PC.ManaRes + 200;
        end;
      end;
      16: 
      begin
        PC.LifeRes := PC.LifeRes + 250;
        PC.ManaRes := PC.ManaRes + 250;
      end;
      17:
      begin
        PC.Life := PC.Life + 100;
        if (PC.Life > PC.MaxLife) then PC.Life := PC.MaxLife;
        PC.Mana := PC.Mana + 100;
        if (PC.Mana > PC.MaxMana) then PC.Mana := PC.MaxMana;
      end;
    end;
    AddMsg(Format(Lang.Lang(120 + Rand(0, 4)), [Lang.Lang(700 + ItemSelInvID)]));
    // Повышаем навык алхимии
    SkillUp(9, 10);
  end;
end;

procedure Eat();
begin
  if (PC.Items[ItemSelInvID] > 0) then
  begin
    Dec(PC.Items[ItemSelInvID]);
    case ItemSelInvID of
      2: Inc(PC.Hunger, 250);
      3: Inc(PC.Hunger, 200);
      4: Inc(PC.Hunger, 175);
      5: Inc(PC.Hunger,  50);
      6: Inc(PC.Hunger,  75);
      7: Inc(PC.Hunger, 100);
      8: Inc(PC.Hunger, 125);
      9: Inc(PC.Hunger, 150);
    end;
    AddMsg(Format(Lang.Lang(125 + Rand(0, 4)), [Lang.Lang(700 + ItemSelInvID)]));
    if (PC.Hunger > 1000) then PC.Hunger := 1000;
    // Повышаем навык алхимии
    SkillUp(9, 5);
  end;
end;  

procedure Drop();
var
  I: Integer;
begin
  PC.Items[ItemSelInvID] := 0;
  if (ItemSelInvID > 19) then
  for I := 1 to ItemSelInvCount do
  begin
    AddMsg(Lang.Lang(56) + ' ' + Lang.Lang(ItemSelInvID + 700) + '.');
    Cave.Dungeon[PC.Z].DropList.AddItem(PC.X, PC.Y, ItemSelInvID, 1);
  end else begin
    AddMsg(Lang.Lang(56) + ' ' + Lang.Lang(ItemSelInvID + 700) + ' (' + IntToStr(ItemSelInvCount) + ').');
    Cave.Dungeon[PC.Z].DropList.AddItem(PC.X, PC.Y, ItemSelInvID, ItemSelInvCount);
  end;
end;

function GetItemProperties(const ID: Integer): TItemPropertiesResult;
var
  S: string;
begin
  Result.Armor := 0;
  Result.MinDamage := 0;
  Result.MaxDamage := 0;
  if (ID < 20) or (ID > 99) then Exit;
  S := Lang.Lang(ID + 700);
  Delete(S, 1, Pos('[', S));
  Delete(S, Length(S), Length(S));
  if Pos('-', S) > 0 then begin
    // Это оружие
    Result.Armor := 0;
    Result.MinDamage := StrToInt(SU.Before('-', S));
    Result.MaxDamage := StrToInt(SU.After('-', S));
  end else begin
    // Это броня, щит или шлем
    Result.Armor := StrToInt(S);
    Result.MinDamage := 0;
    Result.MaxDamage := 0;
  end;
end; 

function GetEquipDamage(): TItemPropertiesResult;
begin
  Result.Armor := 0;
  Result.MinDamage := 1;
  Result.MaxDamage := 1;
  if (PC.EquipItems[1] > 0) then Result := GetItemProperties(PC.EquipItems[1]);
  if (Result.MinDamage < 1) then Result.MinDamage := 1;
  if (Result.MaxDamage < 1) then Result.MaxDamage := 1;
  if (PC.Fury > 0) then Result.MaxDamage := Result.MaxDamage + PC.Fury;
end;

function GetEquipArmor(): TItemPropertiesResult;
var
  I: Integer;
  IP: TItemPropertiesResult;
begin
  Result.Armor := 0;
  Result.MinDamage := 0;
  Result.MaxDamage := 0;
  for I := 2 to 4 do
    if (PC.EquipItems[I] > 0) then
    begin
      IP := GetItemProperties(PC.EquipItems[I]);
      Result.Armor := Result.Armor + IP.Armor;
    end;
end;

function GetIntEquipArmor(): Integer;
var
  IP: TItemPropertiesResult;
begin
  IP := GetEquipArmor();
  Result := IP.Armor;
end;

procedure Equip();
var
  I: Integer;
begin
  // Снимаем
  for I := 1 to 4 do
  if (PC.EquipItems[I] = ItemSelInvID) then
  begin
    PC.EquipItems[I] := 0;
    AddMsg(Format(Lang.Lang(10), [Lang.Lang(ItemSelInvID + 700)]));
    Exit;
  end;
  // Проверяем
  if (PC.Level < GetItemLevel(ItemSelInvID))
    or ((GetItemStr(ItemSelInvID) > 0) and (PC.Str < GetItemStr(ItemSelInvID)))
    or ((GetItemDex(ItemSelInvID) > 0) and (PC.Dex < GetItemDex(ItemSelInvID)))
    or ((GetItemInt(ItemSelInvID) > 0) and (PC.Int < GetItemInt(ItemSelInvID)))
  then begin
    case GetItemType(ItemSelInvID) of
      0..7: AddMsg(Lang.Lang(158));
       else AddMsg(Lang.Lang(157));
    end;
    Exit;
  end;
  // Надеваем
  case GetItemType(ItemSelInvID) of
      // Од. оружие
          1..4 : if (ItemSelInvID < 70) then
                 begin
                   PC.EquipItems[1] := ItemSelInvID;
                   AddMsg(Format(Lang.Lang(8), [Lang.Lang(ItemSelInvID + 700)]));
                 end else
      // Дв. оружие
                 begin
                   PC.EquipItems[1] := ItemSelInvID;
                   PC.EquipItems[2] := 0;
                   AddMsg(Format(Lang.Lang(8), [Lang.Lang(ItemSelInvID + 700)]));
                 end;
      // Посох, лук и арбалет
       0, 5, 6 :
                 begin
                   PC.EquipItems[1] := ItemSelInvID;
                   PC.EquipItems[2] := 0;
                   AddMsg(Format(Lang.Lang(8), [Lang.Lang(ItemSelInvID + 700)]));
                 end;
      // Щит
             7 : begin
                     // Дв. оружие
                   if ((PC.EquipItems[1] >= 70)
                     // Посох
                     or ((GetItemType(PC.EquipItems[1]) = 0)
                     // Ст. оружие
                     or (GetItemType(PC.EquipItems[1]) >= 5))) then
                       PC.EquipItems[1] := 0;
                   PC.EquipItems[2] := ItemSelInvID;
                   AddMsg(Format(Lang.Lang(8), [Lang.Lang(ItemSelInvID + 700)]));
                 end;
      // Шлем
             8 : begin
                   PC.EquipItems[3] := ItemSelInvID;
                   AddMsg(Format(Lang.Lang(9), [Lang.Lang(ItemSelInvID + 700)]));
                 end;
      // Броня
             9 : begin
                   PC.EquipItems[4] := ItemSelInvID;
                   AddMsg(Format(Lang.Lang(9), [Lang.Lang(ItemSelInvID + 700)]));
                 end;  
  end;
end;

function GetItemType(const ID: Integer): Integer;
begin
  Result := -1;
  if (ID >= 20) and (ID <= 99) then
    Result := StrToInt(IntToStr(ID)[2]);
end;

function GetItemLevel(const ID: Integer): Integer;
begin
  Result := 0;
  if (ID >= 20) and (ID <= 99) then
    Result := StrToInt(IntToStr(ID)[1]) - 1;
end;

function GetItemStr(const ID: Integer): Integer;
begin
  Result := 0;
  case GetItemType(ID) of
    1, 2, 4, 7..9: Result := (GetItemLevel(ID) * 10) + 5;
  end;
end;

function GetItemDex(const ID: Integer): Integer;
begin
  Result := 0;
  case GetItemType(ID) of
    3, 5, 6: Result := (GetItemLevel(ID) * 10) + 5;
  end;
end;

function GetItemInt(const ID: Integer): Integer;
begin
  Result := 0;
  case GetItemType(ID) of
    0: Result := (GetItemLevel(ID) * 10) + 5;
  end;
end;

procedure KeysItem(var Key: Word);

  procedure Quit();
  begin
    GameFrame := gfGame;
    ShowFloorObjects(PC.X, PC.Y, True);
    Draw();
  end;
  
begin
  case Key of
    27:
    begin
      GameFrame := gfInv;
      Draw();
    end;
    ord('E'):
    if ((ItemSelInvID >= 20) and (ItemSelInvID <= 99)) then
    begin
      Equip();
      GameFrame := gfInv;
      Draw();
    end;
    ord('D'):
    if (ItemSelInvID > 1) then
    begin
      Drop();
      Quit();
    end;
    ord('T'):
    if (ItemSelInvID >= 161) and (ItemSelInvID <= 184) and (PC.Items[ItemSelInvID] > 0) then
    begin
      TeachSpell();
      Quit();
    end;
    ord('R'):
    if (ItemSelInvID >= 2) and (ItemSelInvID <= 9) and (PC.Items[ItemSelInvID] > 0) then
    begin
      Eat();
      Quit();
    end;
    ord('Q'):
    if (ItemSelInvID >= 10) and (ItemSelInvID <= 17) and (PC.Items[ItemSelInvID] > 0) then
    begin
      Drink();
      Quit();
    end;
  end;
end;

end.

