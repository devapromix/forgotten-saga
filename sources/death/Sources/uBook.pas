unit uBook;

interface

procedure DrawBook();
procedure KeysBook(var Key: Word);

implementation

uses SysUtils, uDraw, uConst, uTypes, uGraph, uLang, uPC, uGame, uColor, uItem,
  uSpells, uBox, uMsg;

var
  P, U: Integer;

procedure DrawSpells();
const
  V = '                   ';
var
  K: TItemPropertiesResult;
  D, MI, M, C, L, I, J, H: Integer;
  S: string;

  procedure AddBookSpell(ASpell: string; AColor: Integer);
  var
    S: string;
  begin
    S := ''; if DebugMode then S := ' ID: ' + IntToStr(PC.Spells[I]);
    TextOut(0, P, Trim(Chr(U + 95) + '.' + ASpell + S), AColor);
    Inc(P); Inc(U);
  end;

  procedure AddTitle(C: Integer);
  var
    I: Byte;
    F: string;
    S: string;
  begin
    if (H > 1) then Inc(P);
    S := ''; if DebugMode then S := S + IntToStr(H) + '.';
    S := S + Lang.Lang(5 - H + 3025);
    TextOut(0, P, S, C);
    Inc(P); F := '';
    for I := 0 to Length(S) - 1 do F := F + '-';
    TextOut(0, P, F, C);
    Inc(P); Inc(H);
  end;

begin
  // Sort spells
  with PC do
  for L := 1 to High(Spells) - 1 do
  begin
    D := 0;
    mi:=0;
    for m := L  to High(Spells) do
      if Spells[m]>mi then
         begin
         mi:=Spells[m];
         D := M;
         end;
    mi:=Spells[l];
    Spells[l]:=Spells[D];
    Spells[D]:=mi;
  end;
  //
  P := 2;
  U := 2;
  S := Lang.Lang(137);
  if (PC.ActSpell > 0) then
  begin
    // Урон оружия
    K := GetEquipDamage();
    S := Lang.Lang(PC.ActSpell + 3000);
    case SpellTypeIP(PC.ActSpell) of
      1:
      begin
        S := S + ' [' + IntToStr(ActSpellDamage) + ']';
      end;
      else
      begin
        if (GetItemType(PC.EquipItems[1]) = 0) then
          S := S + ' [' + IntToStr(ActSpellDamage + K.MinDamage) + '-'
            + IntToStr(ActSpellDamage + K.MaxDamage) + ']'
              else S := S + ' [' + IntToStr(ActSpellDamage) + ']';
      end;
    end;
  end;
  //
  if (GetItemType(PC.EquipItems[1]) = 0) and (PC.ActSpell > 0) and (ActSpellManaCost <= PC.Mana) then
    C := ActSpellColor else C := cRdGray;
  TextOut(ScreenWidth div 2, 2, S, C);
  if DebugMode then TextOut(2, 'ID: ' + IntToStr(PC.ActSpell), cDkGray, alRight);
  if (PC.ActSpell > 0) then
  begin
    case SpellTypeIP(PC.ActSpell) of
      1:
      TextOut(ScreenWidth div 2, 4, Lang.Lang(23) + ': +' + IntToStr(ActSpellDamage), C);
      else
      if (GetItemType(PC.EquipItems[1]) = 0) then
        TextOut(ScreenWidth div 2, 4, Lang.Lang(20) + ': ' + IntToStr(ActSpellDamage + K.MinDamage)
          + '-' + IntToStr(ActSpellDamage + K.MaxDamage), C)
            else TextOut(ScreenWidth div 2, 4, Lang.Lang(20) + ': ' + IntToStr(ActSpellDamage), C);
    end;
    TextOut(ScreenWidth div 2, 5, Lang.Lang(24) + ': -' + IntToStr(ActSpellManaCost), C);
    TextOut(ScreenWidth div 2, 6, Lang.Lang(SpellType(PC.ActSpell) + 3025), C);
  end;
  S := IntToStr(PC.Mana) + '/' + IntToStr(PC.MaxMana);
  TextOut(4, S, cRdBlue, alRight);
  //
  L := Round(PC.Mana * 15 / PC.MaxMana);
  S := ''; for I := L - 1 downto 0 do S := S + ' ';
  if (S = '') and (PC.Mana > 0) then S := ' ';
  BG.Canvas.Brush.Color := cDkGray;
  TextOut(ScreenWidth - 15, 5, V, cDkGray);
  BG.Canvas.Brush.Color := cRdBlue;
  TextOut(ScreenWidth - 15, 5, S, cRdBlue);
  BG.Canvas.Brush.Color := cBlack;
  //
  H := 0;
  for I := 1 to High(PC.Spells) do if (PC.Spells[I] > 0) then
  begin
    case PC.Spells[I] of
      1 .. 6: if (H = 4) then AddTitle(SpellColor(PC.Spells[I]));
      7 ..12: begin
                if (H < 3) then H := 3; if (H = 3) then AddTitle(SpellColor(PC.Spells[I]));
              end;
      13..18: begin
                if (H < 2) then H := 2; if (H = 2) then AddTitle(SpellColor(PC.Spells[I]));
              end;
      19..24: begin
                if (H < 1) then H := 1; if (H = 1) then AddTitle(SpellColor(PC.Spells[I]));
              end;
    end;
    if (PC.Spells[I] = PC.ActSpell) then AddBookSpell(Lang.Lang(PC.Spells[I] + 3000)
      + ' [' + IntToStr(SpellDamage(PC.Spells[I])) + ']', cRdYellow)
      else AddBookSpell(Lang.Lang(PC.Spells[I] + 3000)
        + ' [' + IntToStr(SpellDamage(PC.Spells[I])) + ']', SpellColor(PC.Spells[I]));
  end;
end;

procedure DrawBook();
var
  I, C: Byte;
begin
  TitleOut(Lang.Lang(208));
  TitleOut(Lang.Lang(136), ScreenWidth div 2);
  DrawSpells();
  BarOut('[esc]', Lang.Lang(40), true);
  if (PC.Life > 0) then
  begin
    C := 0;
    for I := 1 to High(PC.Spells) do if (PC.Spells[I] > 0) then Inc(C);
    if (C > 0) then
      if (C = 1) then
        BarOut('[a]', Lang.Lang(153), false)
          else BarOut('[a-' + Chr(96 + C) + ']', Lang.Lang(153), false);
  TitleOut(Lang.Lang(202), ScreenWidth div 2, 18);
  DrawMsg(True, 18, 19);  end;
end;

procedure KeysBook(var Key: Word);
var
  C, I: Integer;
begin
  case Key of
    27:
    begin
      if (PC.Life <= 0) then begin
        GameFrame := gfDefeat;
      end else begin
        GameFrame := gfGame;
        ShowFloorObjects(PC.X, PC.Y, True);
      end;
      Draw();
    end;
    ord('A')..ord('X'):
    if (PC.Life > 0) then
    begin
      C := 0;
      for I := 1 to High(PC.Spells) do if (PC.Spells[I] > 0) then Inc(C);
      I := Key - (ord('A') - 1);
      if (I <= C) then
      begin
        I := PC.Spells[I];
        if (I > 0) and (PC.ActSpell <> I) then PC.ActSpell := I else PC.ActSpell := 0;
        Draw();
      end;
    end;
  end;
end;

end.

