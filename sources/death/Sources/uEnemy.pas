unit uEnemy;

interface

uses uTypes;

type
  TEnemyList = array of TPoint;

  TEnemy = class(TObject)
  private
    FCount: Integer;
  public
    EnemyList: TEnemyList;
    constructor Create;
    destructor Destroy; override;
    procedure Move(ID, FromX, FromY, ToX, ToY: Integer);
    procedure Process;
    procedure Clear;
    function Count: Integer;
    procedure Find;
  end;

var
  Enemy: TEnemy;

implementation

uses uConst, uDungeon, uPC, uBattle, uUtils;

{ TEnemy }

procedure TEnemy.Clear;
begin
  FCount := 0;
  SetLength(EnemyList, 0);
end;

function TEnemy.Count: Integer;
begin
  Result := FCount;
end;

constructor TEnemy.Create;
begin
  Self.Clear;
end;

destructor TEnemy.Destroy;
begin
  Self.Clear;
  inherited;
end;

procedure TEnemy.Find;
var
  X, Y, Z: Integer;
begin
  Self.Clear;
  for Y := 0 to DungeonHeight - 1 do
    for X := 0 to DungeonWidth - 1 do
      if (Cave.Dungeon[PC.Z].Cell[X][Y].Creature > 0) then
      begin
        Inc(FCount);
        Z := Length(EnemyList) + 1;
        SetLength(EnemyList, Z);
        EnemyList[Z - 1].X := X;
        EnemyList[Z - 1].Y := Y;
      end;
end;

procedure TEnemy.Move(ID, FromX, FromY, ToX, ToY: Integer);
var
  X, Y: Integer;
begin
  try with Cave.Dungeon[PC.Z] do
  if (Cell[FromX][FromY].Creature > 0) and (Cell[ToX][ToY].Terrain <> 1) then
  begin
    if (Cell[ToX][ToY].Creature = 0) then
    begin
      Cell[ToX][ToY].Boss := Cell[FromX][FromY].Boss;
      Cell[FromX][FromY].Boss := 0;
      Cell[ToX][ToY].Creature := Cell[FromX][FromY].Creature;
      Cell[FromX][FromY].Creature := 0;
      Cell[ToX][ToY].CreatureLife := Cell[FromX][FromY].CreatureLife;
      Cell[FromX][FromY].CreatureLife := 0;
      EnemyList[ID].X := ToX;
      EnemyList[ID].Y := ToY;
    end;
  end except end;
end;

procedure TEnemy.Process;
begin

end;

initialization
  Enemy := TEnemy.Create;

finalization
  Enemy.Free;
  
end.
