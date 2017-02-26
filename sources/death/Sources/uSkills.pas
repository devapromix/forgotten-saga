unit uSkills;

interface

const SkillMax = 50;

procedure DrawSkills();
procedure SkillUp(Skill: Byte; Count: Byte = 1);

implementation

uses SysUtils, uTypes, uGraph, uLang, uColor, uPC, uMsg, uBox, uConst;

procedure DrawSkills();
var
  C, D, I, J, P, T, N, U, Z, F, VP: Integer;
  S, V: string;
  K: Byte;
begin
  if (ShowCharAllSkills = 0) then Exit;
  VP := ScreenWidth div 4;
  V := ''; for I := 1 to VP do V := V + ' ';
  if (ShowCharAllSkills = 1) then T := 8 else T := 2;
  if (ShowCharAllSkills = 1) then S := Lang.Lang(22) else S := Lang.Lang(57);
  I := 0; Z := 0;
  TitleOut(S, VP * 2, T - 2);
  for K := 1 to High(PC.Skills) do
  begin
    Inc(I);
    N := PC.Skills[I].Value;
    if (N = 0) and (ShowCharAllSkills = 1) then Continue else Inc(Z);
    if Odd(Z) then P := VP * 2 else P := VP * 3;
    case N of
      0 .. 9: U := VP - 2;
      10..99: U := VP - 3;
         else U := VP - 4;
    end;
    case N of
      0    : F := cRdRed;
      50   : F := cRdYellow;   
        else F := cRdGreen;
    end;
    TextOut(P, T, Lang.Lang(1600 + I - 1), F);
    TextOut(P + U, T, IntToStr(N), F);
    BG.Canvas.Brush.Color := cDkGray;
    TextOut(P, T + 1, V, cDkGray);
    C := Round(PC.Skills[I].Exp * VP / 100);
    S := ''; for J := C - 1 downto 0 do S := S + ' ';
    BG.Canvas.Brush.Color := cRdYellow;
    TextOut(P, T + 1, S, cRdYellow);   
    BG.Canvas.Brush.Color := cBlack;
    // Line
    D := (T + 1) * CharHeight;
    BG.Canvas.Pen.Width := 1;
    BG.Canvas.Pen.Color := cBlack;
    BG.Canvas.MoveTo(VP * 3 * CharWidth - 1, D);
    BG.Canvas.LineTo(VP * 3 * CharWidth - 1, D + CharHeight);
    //
    if (P = VP * 3) then Inc(T, 2);
  end;
end;

procedure SkillUp(Skill: Byte; Count: Byte = 1);
begin
  if (PC.Skills[Skill].Value < SkillMax) then
  begin
    Inc(PC.Skills[Skill].Exp, Count);
    if (PC.Skills[Skill].Exp >= 100) then
    begin
      Inc(PC.Skills[Skill].Value);
      Inc(PC.Scores, PC.Skills[Skill].Exp); 
      PC.Skills[Skill].Exp := 0;
      Inc(PC.SkillPoint);
      if (PC.Skills[Skill].Value = 1) then
        AddMsg(Format(Lang.Lang(1599), [Lang.Lang(1600 + Skill - 1)]))
          else AddMsg(Format(Lang.Lang(100), [Lang.Lang(1600 + Skill - 1),
            PC.Skills[Skill].Value]));
      if (PC.Skills[Skill].Value > SkillMax) then PC.Skills[Skill].Value := SkillMax;
    end;
  end;
end;

end.
