unit uRecords;

interface

procedure DrawRecords();
procedure KeysRecords(var Key: Word);

implementation

uses SysUtils, uLang, uGraph, uColor, uScores, uPC, uConst, uTypes, uDraw;

procedure DrawRecords();
var
  C, I: Integer;
  S: string;
begin
  TitleOut(Lang.Lang(50));
  for I := 0 to Scores.MaxCount do
  begin
    if (I >= Scores.MaxCount) or (I >= Scores.Count) then Break;
    if (I < 9) then S := ' ' else S := '';
    S := S + IntToStr(I + 1) + '.';
    if (Scores.Line = I) and (GameFrameTemp = gfNone) then C := cRdYellow
      else C := cRdGreen;
    TextOut(0, I + 3, S + ' ' + Scores.GetName(I), C);
    TextOut(21, I + 3, Scores.GetLevel(I), C);

    TextOut(ScreenWidth - 10, I + 3, Scores.GetScore(I), C);
  end;
  if (Scores.Count > 0) then
  begin
    TextOut(21, 2, Lang.Lang(284), cYellow);

    TextOut(ScreenWidth - 10, 2, Lang.Lang(297), cYellow);
  end;
  BarOut('[esc]', Lang.Lang(40), True);
end;

procedure KeysRecords(var Key: Word);
begin
  if Key = 27 then
    if (GameFrameTemp = gfNone) then Halt
      else begin
        GameFrame := GameFrameTemp;
        Draw();
      end;
end;

end.
