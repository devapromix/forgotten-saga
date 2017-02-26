unit uKills;

interface

procedure DrawKills();
procedure KeysKills(var Key: Word);

implementation

uses uPC, uTypes, uConst, uGame, uDraw, uKillList, uGraph, uLang, uColor;

var
  X, Y, W: Integer;

procedure AddLine(EnemyName, KillCount: string);
begin
  TextOut(X, Y, EnemyName + ', ' + KillCount, cRdGreen);
  Inc(X, W + 1);
  if (X > (W + 1) * 2) then
  begin
    Inc(Y);
    X := 0;
  end;
end;

procedure DrawKills();
var
  I: Integer;
begin
  X := 0;
  Y := 2;
  W := ScreenWidth div 3;
  TitleOut(Lang.Lang(205));
  with KillList do for I := 0 to Count - 1 do AddLine(NameFromIndex(I), CountFromIndex(I));
  BarOut('[esc]', Lang.Lang(40), true);
end;

procedure KeysKills(var Key: Word);
begin
  case Key of
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
