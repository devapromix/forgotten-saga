unit uNPC;

interface

uses uTypes;

const
  NPCCount = 7;

var
  NPCPos: array [0..NPCCount - 1] of TPoint;
  NPCVid: array [0..NPCCount - 1] of Byte;

procedure MoveNPCs();
function GetNPCID(X, Y: Integer): ShortInt;
procedure TalkNPC(X, Y: Integer);

implementation

uses uUtils, uConst, uPC, uDungeon, uQuest, uMsg, uDraw;

procedure TalkNPC(X, Y: Integer);
begin
  case GetNPCID(X, Y) of
    0:
    begin
      GameFrame := gfDialog;
    end;
    4:
    begin
      GameFrame := gfDialog;
    end;

  end;
end;

procedure MoveNPCs();
var
  I, X, Y: ShortInt;
begin
  for I := 0 to High(NPCPos) do
  begin
    X := 0;
    Y := 0;
    case Rand(0, 3) of
      0: if ((NPCPos[I].X) < DungeonWidth - 1) then X := 1;
      1: if ((NPCPos[I].X) > 0) then X := -1;
      2: if ((NPCPos[I].Y) < DungeonHeight - 1) then Y := 1;
      3: if ((NPCPos[I].Y) > 0) then Y := -1;
    end;
    with Cave.Dungeon[PC.Z].Cell[NPCPos[I].X + X, NPCPos[I].Y + Y] do
    if (Terrain <> 11) {and (Terrain <> 0)} then
    begin
      if (Terrain = 2) then Terrain := 3 else Continue;

    end;
    Inc(NPCPos[I].X, X);
    Inc(NPCPos[I].Y, Y);
  end;
end;

function GetNPCID(X, Y: Integer): ShortInt;
var
  I: Byte;
begin
  Result := -1;
  if (PC.Z = -1) then
    for I := 0 to High(NPCPos) do
    begin
      if (X = NPCPos[I].X) and (Y = NPCPos[I].Y) then
      begin
        Result := I;
        Break;
      end;
    end;
end;

var
  I: Byte;

initialization
  for I := 0 to High(NPCVid) do NPCVid[I] := 0;
  
end.
