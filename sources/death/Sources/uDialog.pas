unit uDialog;

interface

procedure DrawDialog();
procedure KeysDialog(var Key: Word);

implementation

uses SysUtils, uBox, uColor, uGraph, uLang, uGame, uPC, uConst, uTypes, uDraw, uNPC,
  uStringUtils, uQuest, uMsg;

var
  cmdNext: Boolean = False;
  cmdQuest: Boolean = False;
  P, W, ID: Byte;

procedure Quest(ID: Byte);
begin
  case Quests.Quest[ID].State of
           qsNone:
           begin
//             Quests.Quest[ID].NPC := 'Джаред';
             Quests.Quest[ID].State := qsQuest;
             //Quests.Quest[1].Difficulty := Rand(0, 2);
             //Quests.Quest[1].ObjectCount := Quests.Quest[1].ObjectCount
             //  + Quests.Quest[1].Difficulty;
             AddMsg(Format(Lang.Lang(2198), [Quests.Quest[ID].Name]));
           end;
  end;
end;

procedure AddDialogMsg(S: string); overload;
var
  L, V: Integer;
  H: string;
begin
  S := S + ' ';
  L := Length(S);
  if (L > W) then
  begin
    repeat
      L := Length(S);
      H := Copy(S, 1, W);
      V := SU.LastPos(' ', H);
      H := Copy(S, 1, V);
      TextOut(P, H, cGreen, alCenter);
      Delete(S, 1, V);
      Inc(P);
    until (L = 0) or (P > 23);
  end else TextOut(P, S, cGreen, alCenter);
end;

procedure AddDialogMsg(LID: Integer); overload;
begin
  AddDialogMsg(Lang.Lang(LID));
end;

procedure DrawDialog();
var
  S: string;
  X, Y: Byte;
  I: Integer;
begin
  ID := GetNPCID(Cursor.X, Cursor.Y);
  if DebugMode then S := ' [' + IntToStr(ID) + ', ' + IntToStr(NPCVid[ID]) + ']'
    else S := '';
  //
  DrawGame;
  for Y := 15 to 24 do
    for X := 10 to ScreenWidth - 10 do
     TextOut(X, Y, ' ', cBlack);
  TextOut(15, Lang.Lang(2100 + ID) + S, cRdYellow, alCenter);
  P := 16;
  W := ScreenWidth - 24;
  //
  case ID of
    0: // Охотник Джаред
    case NPCVid[ID] of
      0:
      begin
        AddDialogMsg(2220);
        NPCVid[0] := 3; // Test
      end;
      1:
      begin
        AddDialogMsg(2221);
        cmdNext := True;
      end;
      2:
      begin
        AddDialogMsg(Format(Lang.Lang(2222), ['муравьиных львов']));
        cmdQuest := True;
      end;
      3:
      begin
        // Квест "Охота"
        Quests.Check(0);
        if (Quests.Quest[0].State = qsQuest) then AddDialogMsg(2223)
          else
            begin
              AddDialogMsg(2224);
              // Многие уже знают, что выполнен квест охотника
              if (NPCVid[4] = 2) then NPCVid[4] := 3;
            end;
      end;
      4:
      begin
        AddDialogMsg(2225);
      end;
    end;

    4: // Старейшина Тарум
    case NPCVid[ID] of
      0:
      begin
        AddDialogMsg(Format(Lang.Lang(2201), [PC.Name]));
        cmdNext := True;
      end;
      1:
      begin
        AddDialogMsg(2202);
        cmdQuest := True;
      end;
      2:
      begin
        // Квест "Провал"
        Quests.Check(3);
        if (Quests.Quest[3].State = qsQuest) then AddDialogMsg(2203)
          else
            begin
              // Поговорить с охотником
              S := Lang.Lang(2204);
              if (NPCVid[0] <= 1) then S := S + ' ' + Lang.Lang(2205);
              AddDialogMsg(S);
              // Охотник уже знает, что выполнен квест старейшины
              if (NPCVid[0] = 0) then NPCVid[0] := 1;
            end;
      end;
      3:
      begin
        AddDialogMsg(2206);
      end;
    end;

  end;
  S := '[esc] ' +  Lang.Lang(40) + '         ';
  if cmdNext then S := S + ' [enter] ' + Lang.Lang(150);
  if cmdQuest then S := S + ' [enter] ' + Lang.Lang(174);
  TextOut(24, S, cLtYellow, alCenter);
end;

procedure KeysDialog(var Key: Word);

procedure Quit();
begin
  ShowFloorObjects(PC.X, PC.Y, True);
  GameFrame := gfGame;
  cmdNext := False;
  cmdQuest := False;
  Draw();
end;

begin
  if cmdNext and (Key = 13) then
  begin
    cmdNext := False;
    Inc(NPCVid[ID]);
    Draw;
    Exit;
  end;
  if cmdQuest and (Key = 13) then
  begin
    case ID of
      // Квесты охотника Джареда
      0:
      case NPCVid[ID] of
        2: Quest(0);
      end;
      // Квесты старейшины Тарума
      4:
      case NPCVid[ID] of
        1: Quest(3);
      end;
    end;
    cmdQuest := False;
    Inc(NPCVid[ID]);
    Quit();
    Exit;
  end;
  case Key of
    27: Quit();
  end;
end;

end.
