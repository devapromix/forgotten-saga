unit uPC;

interface

uses uConst, uColor, uTypes;

type
  TPC = record
    Name: string;
    TitleID: Integer;
    RaceID: Integer;
    ClassID: Integer;
    X: Integer;
    Y: Integer;
    Z: Integer;
    T: Integer;
    V: Integer;
    IsBook: Boolean;
    IsCraft: Boolean;
    IsAlchemy: Boolean;
    Scores: Integer;
    Hunger: Integer;
    ActSpell: Integer;
    MagicType: Integer;
    Str, Dex, Sta, Int: Integer;
    EV, AC: Integer;
    Armor: Integer;
    Level, Exp, MaxExp, SkillPoint: Integer;
    Life: Integer;
    MaxLife: Integer;
    Mana: Integer;
    MaxMana: Integer;
    Fury: Integer;
    MaxFury: Integer;
    FuryCounter: Integer;
    LifeRes, ManaRes: Integer;
    Spells: array[1..24] of Integer;
    Items: array[1..ItemsCount] of Integer;
    EquipItems: array[1..4] of Integer;
    CraftIngItems: array[1..4] of Integer;
    Skills: TSkills;
  end;

procedure CalcPC();
procedure DrawPC(C: Integer = cDkYellow);
procedure DeadPC();
procedure FillPC();
procedure ClearPC();
function GetAccuracyPC(const ALevel: Integer): Integer;
function GetClassEq(): string;
function GenPCName(): string;
function RaceName(I: Byte = 0): string;
function ClassName(I: Byte = 0): string;
function AddExp(const I: Integer): Boolean;
function RaceProp(I: Byte = 0): THeroProp;
function ClassProp(I: Byte = 0): THeroProp;
procedure AddItemAndEquip(const ItemID: Word; ItemSlotID: Byte = 1);
procedure StartInv;

var
  PC: TPC;

implementation

uses SysUtils, uGraph, uLang, uUtils, uSpells, uStringUtils, uMsg, uDraw;

var
  LastPrefID: Integer = 0;
  LastNameID: Integer = 0;
  LastSuffID: Integer = 0;

procedure CalcPC();
begin
  with PC do begin
    MaxLife := Sta * 5;
    MaxMana := Int * 3;
    MaxFury := Percent(56, 50 - Int);
  end;
end;

procedure DrawPC(C: Integer = cDkYellow);
begin
  if (PC.Life <= 0) then C := cDkGray;
  begin
    if (SMode or LMode or TMode) and (Cursor.X = PC.X) and (Cursor.Y = PC.Y) then
      BG.Canvas.Brush.Color := cRdYellow else BG.Canvas.Brush.Color := cBlack;
    BG.Canvas.Font.Color := C;
    BG.Canvas.TextOut(PC.X * CharWidth, (PC.Y + 1) * CharHeight, '@');
    BG.Canvas.Brush.Color := cBlack;
  end;
end;

procedure DeadPC();
begin
  AddMsg(PC.Name + ' ' + Lang.Lang(40 + Rand(3, 9)));
  PC.Hunger := 0;
  PC.Life := 0;
  Draw();
end;

procedure FillPC();
begin
  with PC do begin
    Life := MaxLife;
    Mana := MaxMana;
  end;
end;

// Точность в %
function GetAccuracyPC(const ALevel: Integer): Integer;
begin
  Result := (PC.Level + PC.Dex + PC.Skills[19].Value + 100) * 100 div (ALevel * 10 + 100);
  if (Result < 0) then Result := 0;
  if (Result > 100) then Result := 100;
end;

function GenPCName(): string;
var
  CurrentPrefID: Integer;
  CurrentNameID: Integer;
  CurrentSuffID: Integer;
begin
  repeat
    CurrentPrefID := Random(7);
    if (CurrentPrefID <> LastPrefID) then
    begin
      LastPrefID := CurrentPrefID;
      Break;
    end;
  until False;
  //
  repeat
    CurrentNameID := Random(7);
    if (CurrentNameID <> LastNameID) then
    begin
      LastNameID := CurrentNameID;
      Break;
    end;
  until False;
  //
  repeat
    CurrentSuffID := Random(7);
    if (CurrentSuffID <> LastSuffID) then
    begin
      LastSuffID := CurrentSuffID;
      Break;
    end;
  until False;
  //
  case LastPrefID of
     0 : Result := 'Кар';
     1 : Result := 'Дос';
     2 : Result := 'Мет';
     3 : Result := 'Экт';
     4 : Result := 'Огр';
     5 : Result := 'Авг';
    else Result := 'Лит';
  end;
  //
  if (Random(9) > 2) then
  case LastNameID of
     0 : Result := Result + 'ав';
     1 : Result := Result + 'оз';
     2 : Result := Result + 'ур';
     3 : Result := Result + 'ад';
     4 : Result := Result + 'ин';
     5 : Result := Result + 'ил';
    else Result := Result + 'ос';
  end;
  //
  case LastSuffID of
     0 : Result := Result + 'ис';
     1 : Result := Result + 'ил';
     2 : Result := Result + 'ог';
     3 : Result := Result + 'он';
     4 : Result := Result + 'ом';
     5 : Result := Result + 'орн';
    else Result := Result + 'ер';
  end;
end;

function RaceName(I: Byte = 0): string;
begin
  if (I = 0) then I := PC.RaceID;
  Result := Lang.Lang(I + 3100);
end;

function ClassName(I: Byte = 0): string;
begin
  if (I = 0) then I := PC.ClassID;
  Result := Lang.Lang(I + 3150);
end;

function RaceProp(I: Byte = 0): THeroProp;
begin
  if (I = 0) then I := PC.RaceID;
  case I of
    1:
    begin
      Result.Str := 15;
      Result.Dex := 10;
      Result.Sta := 15;
      Result.Int := 10;
    end;
    2:
    begin
      Result.Str := 10;
      Result.Dex := 20;
      Result.Sta := 10;
      Result.Int := 10;
    end;
    3:
    begin
      Result.Str := 10;
      Result.Dex := 10;
      Result.Sta := 10;
      Result.Int := 20;
    end;
    4:
    begin
      Result.Str := 15;
      Result.Dex := 15;
      Result.Sta := 15;
      Result.Int := 5;
    end;
    5:
    begin
      Result.Str := 20;
      Result.Dex := 10;
      Result.Sta := 15;
      Result.Int := 5;
    end;
    6:
    begin
      Result.Str := 25;
      Result.Dex := 5;
      Result.Sta := 15;
      Result.Int := 5;
    end;
    7:
    begin
      Result.Str := 5;
      Result.Dex := 10;
      Result.Sta := 10;
      Result.Int := 25;
    end;
    8:
    begin
      Result.Str := 15;
      Result.Dex := 20;
      Result.Sta := 5;
      Result.Int := 10;
    end;
    9:
    begin
      Result.Str := 20;
      Result.Dex := 5;
      Result.Sta := 20;
      Result.Int := 5;
    end;
    10:
    begin
      Result.Str := 5;
      Result.Dex := 5;
      Result.Sta := 5;
      Result.Int := 5;
    end;
    11:
    begin
      Result.Str := 5;
      Result.Dex := 5;
      Result.Sta := 5;
      Result.Int := 5;
    end;
    12:
    begin
      Result.Str := 5;
      Result.Dex := 5;
      Result.Sta := 5;
      Result.Int := 5;
    end;
    else
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 0;
      Result.Int := 0;
    end;
  end;

end;

function ClassProp(I: Byte = 0): THeroProp;
var
  J: Byte;
begin
  for J := Low(TSkills) to High(TSkills) do
  begin
    Result.Skills[J].Value := 0;
    Result.Skills[J].Exp := 0;
  end;    
  if (I = 0) then I := PC.RaceID;
  case I of
    1:
    begin
      Result.Str := 10;
      Result.Dex := 0;
      Result.Sta := 5;
      Result.Int := 0;
      Result.Skills[ 3].Value := 10;
      Result.Skills[ 8].Value := 10;
      Result.Skills[17].Value := 10;
    end;
    2:
    begin
      Result.Str := 5;
      Result.Dex := 5;
      Result.Sta := 5;
      Result.Int := 0;
      Result.Skills[ 2].Value := 10;
      Result.Skills[ 8].Value := 10;
      Result.Skills[10].Value := 5;
    end;
    3:
    begin
      Result.Str := 0;
      Result.Dex := 10;
      Result.Sta := 5;
      Result.Int := 0;
      Result.Skills[ 6].Value := 15;
      Result.Skills[10].Value := 15;
      Result.Skills[11].Value := 10;
    end;
    4:
    begin
      Result.Str := 0;
      Result.Dex := 15;
      Result.Sta := 0;
      Result.Int := 0;
      Result.Skills[ 7].Value := 15;
      Result.Skills[10].Value := 15;
      Result.Skills[11].Value := 10;
    end;
    5:
    begin
      Result.Str := 5;
      Result.Dex := 10;
      Result.Sta := 0;
      Result.Int := 0;
      Result.Skills[ 4].Value := 10;
      Result.Skills[ 8].Value := 10;
      Result.Skills[10].Value := 10;
    end;
    6: // Рыцарь
    begin
      Result.Str := 5;
      Result.Dex := 0;
      Result.Sta := 5;
      Result.Int := 5;
      Result.Skills[ 5].Value := 10;
      Result.Skills[ 8].Value := 10;
      Result.Skills[ 9].Value := 10;
      Result.Skills[10].Value := 5;
      Result.Skills[12].Value := 5;
      Result.Skills[15].Value := 10;
    end;
    7:
    begin
      Result.Str := 0;
      Result.Dex := 10;
      Result.Sta := 5;
      Result.Int := 0;
      Result.Skills[10].Value := 20;
      Result.Skills[12].Value := 15;
      Result.Skills[18].Value := 15;
    end;
    8:
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 5;
      Result.Int := 10;
    end;
    9:
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 0;
      Result.Int := 15;
      Result.Skills[ 1].Value := 10;
      Result.Skills[ 9].Value := 20;
      Result.Skills[13].Value := 25;
    end;
    10:
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 0;
      Result.Int := 15;
      Result.Skills[ 1].Value := 10;
      Result.Skills[ 9].Value := 20;
      Result.Skills[14].Value := 25;
    end;
    11: // Друид
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 5;
      Result.Int := 10;
      Result.Skills[ 1].Value := 15;
      Result.Skills[ 9].Value := 20;
      Result.Skills[15].Value := 20;
    end;
    12:
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 0;
      Result.Int := 15;
      Result.Skills[ 1].Value := 10;
      Result.Skills[ 9].Value := 20;
      Result.Skills[16].Value := 25;
    end;
    else
    begin
      Result.Str := 0;
      Result.Dex := 0;
      Result.Sta := 0;
      Result.Int := 0;
    end;
  end;

end;

function AddExp(const I: Integer): Boolean;
begin
  Result := False;
  Inc(PC.Exp, I);
  Inc(PC.Scores, I);
  if (PC.Exp >= PC.MaxExp) then
  begin
    PC.Exp := PC.Exp - PC.MaxExp;
    Inc(PC.Level);
    Inc(PC.Scores, PC.Level * 100);
    Inc(PC.MaxExp, PC.Level * ExpMod);
    Result := True;
  end;
end;

procedure AddItem(const ItemID: Word; ACount: Word = 1);
begin
  PC.Items[ItemID] := ACount;
end;


procedure AddItemAndEquip(const ItemID: Word; ItemSlotID: Byte = 1);
begin
  AddItem(ItemID);
  PC.EquipItems[ItemSlotID] := ItemID;
end;

function GetClassEq(): string;
var
  S: string;
  I: Byte;
  E, X: TExplodeResult;
begin
  S := '';
  case PC.ClassID of
     1 : S := '722,727';          // Варвар
     2 : S := '721,727';          // Воин
     3 : S := '725,718,219';      // Стрелок
     4 : S := '726,719,219';      // Рейнжер
     5 : S := '723,727';          // Страж
     6 : S := '724,727,200,208,873';  // Рыцарь
     7 : S := '';          //
     8 : S := '';          //
     9 : S := '720,200,208,861';      // Маг Воды
    10 : S := '720,200,208,867';      // Маг Воздуха
    11 : S := '720,200,208,873';      // Маг Земли
    12 : S := '720,200,208,879';      // Маг Огня
  end;
  Result := '';
  E := SU.Explode(string(','), S);
  for I := 0 to High(E) do if (Trim(E[I]) <> '') then
  begin
    S := Lang.Lang(Trim(E[I]));
    X := SU.Explode(string('['), S);
    S := Trim(X[0]);
    if (E[I] = '718') or (E[I] = '719') then S := S + ' (200)';
    Result := Result + ',' + S;
  end;
  if (Length(Result) > 0) and (Result[1] = ',') then Delete(Result, 1, 1);
end;

procedure StartInv;
begin
  with PC do
  case ClassID of
    1: // Варвар
    begin
      IsAlchemy := False;
      IsBook := False;
      MagicType := 0;
      AddItemAndEquip(22, 1);
      AddItemAndEquip(27, 2);
    end;
    2: // Воин
    begin
      IsBook := False;
      MagicType := 0;
      AddItemAndEquip(21, 1);
      AddItemAndEquip(27, 2);
    end;
    3: // Стрелок
    begin
      IsCraft := True;
      IsBook := False;
      MagicType := 0;
      AddItemAndEquip(25);
      AddItem(18, 200);
    end;
    4: // Рейнжер
    begin
      IsCraft := True;
      IsBook := False;
      MagicType := 0;
      AddItemAndEquip(26);
      AddItem(19, 200);
    end;
    5: // Страж
    begin
      IsBook := False;
      MagicType := 0;
      AddItemAndEquip(23, 1);
      AddItemAndEquip(27, 2);
    end;
    6: // Паладин
    begin
      IsBook := True;
      MagicType := 0;
      AddItemAndEquip(24, 1);
      AddItemAndEquip(27, 2);
      Spells[1] := 13;
      ActSpell := 13;
    end;
    7: // Боевой Маг
    begin
      IsBook := True;
      MagicType := 0;
      AddItemAndEquip(20);
    end;
    8: // Колдун
    begin
      IsBook := True;
      MagicType := 0;
      AddItemAndEquip(20);
    end;
    9: // Маг Воды
    begin
      IsBook := True;
      MagicType := 1;
      AddItemAndEquip(20);
      Spells[1] := 1;
      ActSpell := 1;
    end;
    10: // Маг Воздуха
    begin
      IsBook := True;
      MagicType := 2;
      AddItemAndEquip(20);
      Spells[1] := 7;
      ActSpell := 7;
    end;
    11: // Маг Земли
    begin
      IsBook := True;
      MagicType := 3;
      AddItemAndEquip(20);
      Spells[1] := 13;
      ActSpell := 13;
    end;
    12: // Маг Огоня
    begin
      IsBook := True;
      MagicType := 4;
      AddItemAndEquip(20);
      Spells[1] := 19;
      ActSpell := 19;
    end;
  end;
end;

procedure ClearPC();
var
  I: Integer;
begin
  with PC do
  begin
    IsBook := False;
    IsCraft := False;
    IsAlchemy := True;
    Name := '';
    RaceID := 0;
    ClassID := 0;
    TitleID := 0;
    MagicType := 0;
    ActSpell := 0;
    EV := 0;
    AC := 0;
    X := -1;
    Y := -1;
    Z := -1;
    T := 0;
    V := 0;
    Armor := 0;
    Exp := 0;
    Level := 1;
    MaxExp := ExpMod;
    Str := 0;
    Dex := 0;
    Sta := 0;
    Int := 0;
    SkillPoint := 0;
    Scores := 0;
    LifeRes := 0;
    ManaRes := 0;
    Fury := 0;
    MaxFury := 20;
    FuryCounter := 0;
    Hunger := 1000;

    //=========================//

//    Items[1] := 1;
//    for I := 2 to 9 do Items[I] := 25;
//    for I := 100 to 110 do Items[I] := 25;
    Items[1] := 49;
{    EquipItems[1] := 90;
    EquipItems[2] := 97;
    EquipItems[3] := 98;
    EquipItems[4] := 99;
}

  {  EquipItems[1] := 20;
    Items[20] := 1;

    EquipItems[4] := 69;
    Items[69] := 1;

    // Лук и арбалет
    Items[25] := 1;
    Items[26] := 1;
    Items[18] := 5;
    Items[19] := 5;

    // Заклинания
    for I := 1 to 24 do
    begin
      A := Rand(1, 24);
      if not IsSpell(A) then PC.Spells[I] := A;
    end;
    }
  end;
end;



initialization
  Randomize;
  ClearPC();

end.


