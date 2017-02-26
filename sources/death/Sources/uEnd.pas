unit uEnd;

interface

procedure DrawEnd();
procedure KeysEnd(var Key: Word);

implementation

uses SysUtils, uGraph, uLang, uConst, uColor, uPC, uMsg, uUtils, uTypes,
  uDraw, uScores, uSkills;

procedure DrawEnd();
begin
  case GameFrame of
    gfVictory:
    begin
      TitleOut(Lang.Lang(209));
      GameFrameScreen := gfVictory;
    end;
    gfDefeat:
    begin
      TitleOut(Lang.Lang(206));
      GameFrameScreen := gfDefeat;
    end;
  end;
  BarOut('[esc]', Lang.Lang(40), true);
  if DebugMode and (PC.Life <= 0) then
    BarOut('[enter]', Lang.Lang(1660));
  if (GameFrame = gfDefeat) then
    BarOut('[g]', AnsiLowerCase(Lang.Lang(296)));
  BarOut('[i]', Lang.Lang(36));
  BarOut('[p]', Lang.Lang(37));
  BarOut('[m]', Lang.Lang(35));
  BarOut('[k]', Lang.Lang(34));
end;

procedure KeysEnd(var Key: Word);
begin
  case Key of
    13:
    if DebugMode and (PC.Life <= 0) then
    begin
      FillPC();
      PC.Hunger := 1000;
      GameFrameScreen := gfNone;
      GameFrame := gfGame;
      Draw();
    end;
    27:
    begin
      Scores.Add(PC.Scores, PC.Level, PC.Name);
      GameFrame := gfRecords;
      GameFrameTemp := gfNone;
      Draw();
    end;
    ord('I'):
    begin
      GameFrame := gfInv;
      Draw();
    end;
    ord('P'):
    begin
      GameFrame := gfChar;
      Draw();
    end;
    ord('G'):
    if (GameFrame = gfDefeat) then
    begin
      GameFrame := gfGame;
      Draw();
    end;
    ord('M'):
    begin
      GameFrame := gfMsg;
      Draw();
    end;
    ord('K'):
    begin
      GameFrame := gfKills;
      Draw();
    end;
  end;
end;

end.
