unit uChar;

interface

procedure DrawChar();
procedure KeysChar(var Key: Word);

implementation

uses SysUtils, uDraw, uConst, uGraph, uColor, uPC, uLang, uGame, uItem,
  uTypes, uSkills;

var
  LW: Byte;

procedure AddLine(const APos: Integer; const SDef, SPar: string; AColor: Integer = cDkYellow); overload;
begin
  TextOut(0, APos, SDef, cLtYellow);
  TextOut(LW, APos, SPar, AColor);
end;

procedure AddLine(const APos: Integer; const SDef: string; DPar: Integer); overload;
begin
  TextOut(0, APos, SDef, cLtYellow);
  TextOut(LW, APos, IntToStr(DPar), cDkYellow);
end;

procedure AddLine(const APos: Integer; const AKey, SDef, SPar: string; AColor: Integer = cDkYellow); overload;
begin
  TextOut(0, APos, '[' + AKey + ']', cLtYellow);
  TextOut(4, APos, SDef, cLtYellow);
  TextOut(LW, APos, SPar, AColor);
end;

procedure DrawChar();
var
  IP: TItemPropertiesResult;
  S: string;
begin
  LW := ScreenWidth div 5;
  TitleOut(PC.Name);
  // Раса
  AddLine( 2, Lang.Lang(281) + ':', RaceName());
  // Класс
  AddLine( 3, Lang.Lang(282) + ':', ClassName());
  // Титул
  AddLine( 4, Lang.Lang(283) + ':', Lang.Lang(1200 + PC.TitleID));
  // Уровень
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and DebugMode then
    AddLine( 6, 'z', Lang.Lang(284) + ':', IntToStr(PC.Level))
      else AddLine( 6, Lang.Lang(284) + ':', IntToStr(PC.Level));
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and DebugMode then
    AddLine( 7, 'x', Lang.Lang(285) + ':', IntToStr(PC.SkillPoint))
      else AddLine( 7, Lang.Lang(285) + ':', IntToStr(PC.SkillPoint));

  // Атрибуты
  S := '';
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and ((PC.SkillPoint > 0) or DebugMode) then
    AddLine( 9, 'a', Lang.Lang(286) + ':', IntToStr(PC.Str))
      else AddLine( 9, Lang.Lang(286) + ':', IntToStr(PC.Str));
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and ((PC.SkillPoint > 0) or DebugMode) then
    AddLine(10, 'b', Lang.Lang(287) + ':', IntToStr(PC.Dex))
      else AddLine(10, Lang.Lang(287) + ':', IntToStr(PC.Dex));
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and ((PC.SkillPoint > 0) or DebugMode) then
    AddLine(11, 'c', Lang.Lang(288) + ':', IntToStr(PC.Sta))
      else AddLine(11, Lang.Lang(288) + ':', IntToStr(PC.Sta));
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and ((PC.SkillPoint > 0) or DebugMode) then
    AddLine(12, 'd', Lang.Lang(289) + ':', IntToStr(PC.Int))
      else AddLine(12, Lang.Lang(289) + ':', IntToStr(PC.Int));

  // Точность
  AddLine(15, Lang.Lang(1618) + ':', IntToStr(GetAccuracyPC(PC.Level)) + '%');
  //
  IP := GetEquipDamage();
  //
  AddLine(28, Lang.Lang(11) + ':', Format(Lang.Lang(13), [PC.Sta div 7, 65]));
  AddLine(29, Lang.Lang(12) + ':', Format(Lang.Lang(13), [PC.Int div 3, 25]));
  // Урон и броня
  DrawDamageAndArmorLine(0, 31, LW, cLtYellow, cDkYellow);
  // Доп. статистика
  AddLine(34, Lang.Lang(296) + ':', Lang.Lang(PC.Z + 2000));
  AddLine(35, Lang.Lang(297) + ':', PC.Scores);
  AddLine(36, Lang.Lang(298) + ':', PC.V);
  AddLine(37, Lang.Lang(299) + ':', PC.T);
  //
  if (ShowCharAllSkills = 1) then DrawRP();
  // Умения
  DrawSkills();
  //
  BarOut('[esc]', Lang.Lang(40), true);
  BarOut('[space]', Lang.Lang(36));
  if (PC.Life > 0) and (GameFrameScreen = gfNone)
    and ((PC.SkillPoint > 0) or DebugMode) then
    BarOut('[a-d]', Lang.Lang(41));
  if DebugMode then
  begin
    BarOut('[z]', '+' + Lang.Lang(284));
    BarOut('[x]', '+' + Lang.Lang(285));
  end;
  if (ShowCharAllSkills = 1) then BarOut('[s]', AnsiLowerCase(Lang.Lang(57)))
    else BarOut('[s]', AnsiLowerCase(Lang.Lang(22)));
end;

procedure KeysChar(var Key: Word);
begin
  case Key of
    ord('A')..ord('D'):
    if (PC.Life > 0) and (GameFrameScreen = gfNone)
      and ((PC.SkillPoint > 0) or DebugMode) then
    begin
      case Key of
        ord('A'):
        begin
          Inc(PC.Str);
          if not DebugMode then Dec(PC.SkillPoint);
        end;
        ord('B'):
        begin
          Inc(PC.Dex);
          if not DebugMode then Dec(PC.SkillPoint);
        end;
        ord('C'):
        begin
          Inc(PC.Sta);
          if not DebugMode then Dec(PC.SkillPoint);
        end;
        ord('D'):
        begin
          Inc(PC.Int);
          if not DebugMode then Dec(PC.SkillPoint);
        end;
      end;
      CalcPC();
      Draw();
    end;
    ord('S'):
    begin
      Dec(ShowCharAllSkills);
      if (ShowCharAllSkills = 0) then ShowCharAllSkills := 2;
      Draw();
    end;
    ord('Z'):
      if DebugMode then
      begin
        PC.Exp := PC.MaxExp;
        AddExp(1);
        Draw();
      end;
    ord('X'):
      if DebugMode then
      begin
        Inc(PC.SkillPoint);
        Draw();
      end;
    32:
    begin
      GameFrame := gfInv;
      Draw();
    end;
    27:
    begin
      if (GameFrameScreen = gfVictory) then
      begin
        GameFrame := gfVictory;
        Draw();
        Exit;
      end;
      if (PC.Life <= 0) then
      begin
        GameFrame := gfDefeat;
        Draw();
      end else
      begin
        ShowFloorObjects(PC.X, PC.Y, True);
        GameFrame := gfGame;
        Draw();
      end;
    end;
  end;
end;

end.

