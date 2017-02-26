unit uJournal;

interface

procedure DrawJournal();
procedure KeysJournal(var Key: Word);

implementation

uses SysUtils, uPC, uTypes, uConst, uGame, uDraw, uGraph, uLang, uColor, uQuest;

var
  P: Byte;

procedure DrawJournal();
var
  I: Byte;
  S: string;
  C: Integer;
begin
  P := 3;
  TitleOut(Lang.Lang(211));
  TextOut( 4, 2, Lang.Lang(2190), cRed);
  TextOut(20, 2, Lang.Lang(2191), cRed);
  TextOut(30, 2, Lang.Lang(2192), cRed);
  S := Lang.Lang(2193);
  TextOut(ScreenWidth - Length(S), 2, S, cRed);
  //
  for I := 0 to Quests.Count - 1 do
  if (Quests.Quest[I].State <> qsNone) then
  begin
    C := cLtGray;
    if (Quests.Quest[I].State = qsDone) then C := cGray;
    if (P <= 9) then S := ' ' + IntToStr(P - 2) else S := IntToStr(P - 2);
    TextOut(0, P, S + '.', C);
    TextOut(4, P, Quests.Quest[I].Name, C);
    TextOut(20, P, Quests.Quest[I].NPC, C);
    case Quests.Quest[I].QType of
      qtKill:
        TextOut(30, P, Format(Quests.Quest[I].Description,
          [Quests.Quest[I].ObjectCount, 'муравьиных львов']), C);
      qtItem:
        TextOut(30, P, Format(Quests.Quest[I].Description,
          [Quests.Quest[I].ObjectCount, 'предметов']), C);
      qtLoc:
        TextOut(30, P, Format(Quests.Quest[I].Description,
          [Lang.Lang(Quests.Quest[I].ObjectCount + 2000)]), C);

    end;
    if (Quests.Quest[I].State = qsQuest) then
    begin
      case Quests.Quest[I].QType of
        qtKill:
        begin
          S := Lang.Lang(2182) + ' ' + IntToStr(Quests.Quest[I].Progress) + '/' +
            IntToStr(Quests.Quest[I].ObjectCount);
          if (Quests.Quest[I].Progress = Quests.Quest[I].ObjectCount) then
            S := Lang.Lang(2194);
        end;
        qtItem:
        begin
          S := Lang.Lang(2181) + ' ' + IntToStr(PC.Items[Quests.Quest[I].ObjectID]) +
            '/' + IntToStr(Quests.Quest[I].ObjectCount);
          if (PC.Items[Quests.Quest[I].ObjectID] >= Quests.Quest[I].ObjectCount) then
            S := Lang.Lang(2194);
        end;
        qtLoc:
        begin
          S := Lang.Lang(2180);
          if (Quests.Quest[I].Progress > 0) then
            S := Lang.Lang(2194);
        end;
      end;
      TextOut(ScreenWidth - Length(S), P, S, cLtYellow);
    end else
    begin
      S := Lang.Lang(2195);
      TextOut(ScreenWidth - Length(S), P, S, C);
    end;
    Inc(P);
  end;
  BarOut('[esc]', Lang.Lang(40), true);
end;

procedure KeysJournal(var Key: Word);
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
