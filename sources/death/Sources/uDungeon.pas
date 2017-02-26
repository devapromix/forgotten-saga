unit uDungeon;

interface

uses Windows, uConst, uDropList;

type
  TCell = class(TObject)
  private
    FVisible: Boolean;
    FTerrain: Integer;
    FUpStairs: Boolean;
    FDownStairs: Boolean;
    FCreature: Integer;
    FCreatureLife: Integer;
    FBoss: Integer;
    FFire: Integer;
    FBlood: Integer;
    FTrap: Boolean;
    procedure SetTerrain(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetDownStairs(const Value: Boolean);
    procedure SetUpStairs(const Value: Boolean);
    procedure SetCreature(const Value: Integer);
    procedure SetCreatureLife(const Value: Integer);
    procedure SetBoss(const Value: Integer);
    procedure SetFire(const Value: Integer);
    procedure SetBlood(const Value: Integer);
    procedure SetTrap(const Value: Boolean);
  public
    property Terrain: Integer read FTerrain write SetTerrain;
    property Fire: Integer read FFire write SetFire;
    property Trap: Boolean read FTrap write SetTrap;
    property Creature: Integer read FCreature write SetCreature;
    property CreatureLife: Integer read FCreatureLife write SetCreatureLife;
    property Visible: Boolean read FVisible write SetVisible;
    property UpStairs: Boolean read FUpStairs write SetUpStairs;
    property DownStairs: Boolean read FDownStairs write SetDownStairs;
    property Boss: Integer read FBoss write SetBoss;
    property Blood: Integer read FBlood write SetBlood;
    constructor Create;
    destructor Destroy; override;
  end;

  TDungeon = class(TObject)
  private
    procedure Clear;
    procedure Generate(const DungeonID: Integer);
  public
    DropList: TDropList;
    Cell: array [0..DungeonWidth - 1, 0..DungeonHeight - 1] of TCell;
    constructor Create(const DungeonID: Integer);
    destructor Destroy; override;
  end;

  TCave = class(TObject)
  private
    FWidth: Integer;
    FHeight: Integer;
    FCount: Integer;
  public
    Dungeon: array [DungeonMin..DungeonsCount - 1] of TDungeon;
    function Done(const X, Y: Integer; ID: Integer): Boolean;
    procedure AddStairs(const ID: Integer);
    procedure AddObjects(const ID: Integer);
    procedure AddPlants(const ID: Integer);
    property Count: Integer read FCount;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Cave: TCave;

implementation

uses Math, uMain, SysUtils, uUtils, uPC, uBox, uCreature, uNPC, uSpells;

var
  FLastDungeonStairsPos: TPoint;
  NPC_ID: Byte = 0;

{ TDungeon }

procedure TDungeon.Clear;
var
  X, Y: Integer;
begin
  for Y := 0 to DungeonHeight - 1 do
    for X := 0 to DungeonWidth - 1 do
      Cell[X, Y].Create;
  DropList.Clear();
end;

constructor TDungeon.Create(const DungeonID: Integer);
var
  X, Y: Integer;
begin
  for Y := 0 to DungeonHeight - 1 do
    for X := 0 to DungeonWidth - 1 do
      Cell[X, Y] := TCell.Create;
  DropList := TDropList.Create;
  Clear;
  Generate(DungeonID);
end;

destructor TDungeon.Destroy;
var
  X, Y: Integer;
begin
  for Y := 0 to DungeonHeight - 1 do
    for X := 0 to DungeonWidth - 1 do
      FreeAndNil(Cell[X, Y]);
  FreeAndNil(DropList);
  inherited;
end;

procedure TDungeon.Generate(const DungeonID: Integer);
var
  X1, Y1, X2, Y2, X, Y, Z: Integer;

function CountNear(X, Y, Tile: Integer): Integer;
var
  Res: Integer;
begin
  res:=0;
  begin
  if Cell[x-1,y].Terrain=tile then res:=res+1;
  if Cell[x+1,y].Terrain=tile then res:=res+1;
  if Cell[x,y-1].Terrain=tile then res:=res+1;
  if Cell[x,y+1].Terrain=tile then res:=res+1;
  if Cell[x-1,y-1].Terrain=tile then res:=res+1;
  if Cell[x-1,y+1].Terrain=tile then res:=res+1;
  if Cell[x+1,y+1].Terrain=tile then res:=res+1;
  if Cell[x+1,y+1].Terrain=tile then res:=res+1;
  end;
  Result:=res;
end;

function FreeSpace(x1,y1,x2,y2:integer):boolean;
 var i,j:integer;
begin
 dec(x1);
 inc(x2);
 dec(y1);
 inc(y2);
 freespace:=true;
 for i:=x1 to x2 do
  for j:=y1 to y2 do
   if ((Cell[i,j].Terrain <> 0)) then
   begin
     freespace:=false;
     Exit;
   end;
end;

procedure CatacombsCreate(X1, Y1, X2, Y2: Integer);
var
  C, I, SX, SY, X, Y: Integer;
  V: array of TPoint;

  function AddRoom(AX, AY, SX, SY: Integer): Boolean;
  var
    X, Y: Integer;
  begin
    for Y := AY - SY to AY + SY do
      for X := AX - SX to AX + SX do
        Cell[X, Y].FTerrain := 0;
  end;

begin
  C := 50;
  SetLength(V, C);
  for I := 0 to C - 1 do
  begin
    SX := 3;
    SY := 3;
    V[I].X := Rand(X1 + SX, X2 - SX);
    V[I].Y := Rand(Y1 + SY, Y2 - SY);
    AddRoom(V[I].X, V[I].Y, SX, SY);
  end;
end;

procedure AntNestCreate(x1,y1,x2,y2:integer);
var
  KX, KY, K, DX, DY: Real;
  C, X, Y, I, PY, PX: Integer;
begin
  X := X2 - X1 + 1;
  Y := Y2 - Y1 + 1;
  Cell[X div 2, Y div 2].FTerrain := 0;
  for I := 0 to (X * Y div 3) do
  begin
   try
   k := (Random(360) + 1) * 3.1419532 / 180;
   kx := (X/2) + (Y/2)*sin(k);
   ky := (Y/2) + (Y/2)*cos(k);
   dx := 1;
   dy := 1;
   while ((abs(dx)<10) and (abs(dy)<10)) do
   begin
     dx := Random(100)+1;
     dy := Random(100)+1;
   end;
   dx := dx - 60;
   dy := dy - 60;
   dx := dx / 30;
   dy := dy / 30;
   C := 0;
   while (True) do
   begin
    if (C + 1 > 10000) then Break;
    C := C + 1;
    kx := kx + dx;
    ky := ky + dy;
    px := round(kx);
    py := round(ky);
   if (px<0) then
      begin
       px := X;
       kx := px;
      end;
    if (px>X) then
      begin
       px := 1;
       kx := px;
      end;
    if (py<0) then
      begin
       py := Y;
       ky := py;
      end;
    if (py>Y) then
      begin
       py := 1;
       ky := py;  
      end;         
      if (px=0) then px := random(x)+1;
      if (py=0) then py := random(y)+1;
    if ((px>1)  and  (Cell[px-1,py].FTerrain = 0)) or
       ((py>1)  and  (Cell[px,py-1].FTerrain = 0)) or
       ((px<X)  and  (Cell[px+1,py].FTerrain = 0)) or
       ((py<Y)  and  (Cell[px,py+1].FTerrain = 0)) then
     begin
      Cell[PX, PY].FTerrain := 0;
      Break;
     end;
   end;
   except end;
  end; 
end;

procedure CavesCreate(X1, Y1, X2, Y2: Integer);
var
  X, Y, I, J, Res: Integer;
  Density: Real;
begin
  Density := 0.8;//PC.Z * 0.1;
  X := X2 - X1;
  Y := Y2 - Y1;
  for i:=1 to round(X*Y*density) do
    Cell[random(X)+1,random(Y)+1].Terrain:=0;
  for i:=1 to X do
  for j:=1 to Y do
   begin
    if (i<=1) or (j<=1) or (i>=X-1) or (j>=Y-1) then
       begin
         Cell[i,j].Terrain:=0;
         Continue;
       end;
   res:=countnear(i,j,1);
     if (Cell[i,j].Terrain=1) then
       begin
         if res<4 then
            Cell[i,j].Terrain:=0;
       end
     else
        begin
         if res>4 then
          Cell[i,j].Terrain:=0;
       end
   end;

  for res:=1 to 10 do
    for i:=2 to X-1 do
      for j:=2 to Y-1 do
        if (CountNear(I, J, 0) < 3)
          or (CountNear(I, J, 1) >= 7) then
            Cell[I, J].Terrain := 0;
end;

procedure PartDraw(X1, Y1: Integer);
var
  N, S, E, W, I, J, K, F: Integer;
begin
  if (DungeonID = -1) then F := Rand(5, 10) else F := Rand(4, 9);
  I := X1;
  J := Y1;
  for K := 1 to 20 do
  try
    N := Random(6);
    E := Random(6);
    S := Random(6);
    W := Random(6);
    if (N = 1) and (I > 0) then
    begin
      I := I - 1;
      if (Cell[I, J].Terrain <> 0) then Exit;
      Cell[I, J].Terrain := F;
    end;
    if (S = 1) and (I < DungeonWidth - 1) then
    begin
      I := I + 1;
      if (Cell[I, J].Terrain <> 0) then Exit;
      Cell[I, J].Terrain := F;
    end;
    if (E = 1) and (J < DungeonHeight - 1) then
    begin
      J := J + 1;
      if (Cell[I, J].Terrain <> 0) then Exit;
      Cell[I, J].Terrain := F;
    end;
    if (W = 1) and (J > 0) then
    begin
      J := J - 1;
      if (Cell[I, J].Terrain <> 0) then Exit;
      Cell[I, J].Terrain := F;
    end;
  except end;
end;

procedure AddNPCHouse(X1, Y1: Integer);
var
  X, Y: Integer;
begin
  X2 := X1 + Rand(4, 8);
  Y2 := Y1 + Rand(4, 8);
  begin
    for X := Pred(X1) to Succ(X2) do
      for Y := Pred(Y1) to Succ(Y2) do
        Cell[X, Y].Terrain := 11;
    for X := X1 to X2 do
      for Y := Y1 to Y2 do
        if (X = X1) or (X = X2) or (Y = Y1) or (Y = Y2)
          then Cell[X, Y].Terrain := 1;
    X := Rand(X1 + 2, X2 - 2);
    Y := Rand(Y1 + 2, Y2 - 2);
    case Rand(0, 3) of
      0: Cell[X, Y1].Terrain := 2;
      1: Cell[X, Y2].Terrain := 2;
      2: Cell[X1, Y].Terrain := 2;
      3: Cell[X2, Y].Terrain := 2;
    end;
    //
    NPCPos[NPC_ID].X := Succ(X1);
    NPCPos[NPC_ID].Y := Succ(Y1);
    Inc(NPC_ID);
    //
    case Rand(1, 3) of
      1: if ((X2 - X1) >= 6) then
         begin
           X := X1 + ((X2 - X1) div 2);
           if (Cell[X, Y1].Terrain = 2) or (Cell[X, Y2].Terrain = 2) then Exit;
           for Y := Y1 to Y2 do Cell[X, Y].Terrain := 1;
           Y := Rand(Y1 + 2, Y2 - 2);
           Cell[X, Y].Terrain := 2;
         end;
      2: if ((Y2 - Y1) >= 6) then
         begin
           Y := Y1 + ((Y2 - Y1) div 2);
           if (Cell[X1, Y].Terrain = 2) or (Cell[X2, Y].Terrain = 2) then Exit;
           for X := X1 to X2 do Cell[X, Y].Terrain := 1;
           X := Rand(X1 + 2, X2 - 2);
           Cell[X, Y].Terrain := 2;
         end;
    end;
  end;
end;

var
  W1, W2: Byte;
  K: Integer;

begin
  // Village
  if (DungeonID = -1) then
  begin
    for X := 0 to DungeonWidth - 1 do
      for Y := 0 to DungeonHeight - 1 do
        Cell[X, Y].Terrain := 0;
    K := 0;
    repeat
      X := Math.RandomRange(2, DungeonWidth - 11);
      Y := Math.RandomRange(2, DungeonWidth - 11);
      if FreeSpace(X, Y, X + 8, Y + 8) then
      begin
        AddNPCHouse(X, Y);
        Inc(K);
      end;
    until (K = 12);

    {
    //
    W1 := Rand(0, 20);
    W2 := Rand(0, 20);
    for X := Succ(9 + W1) to Pred(21 + W1) do
      Cell[X, 6].Terrain := 11;
    for X := Succ(9 + W2) to Pred(21 + W2) do
      Cell[X, 29].Terrain := 11;
    for X := Succ(5) to Pred(25) do Cell[X, 18].Terrain := 11;
    X := Rand(Pred(16), Succ(16));
    for Y := 7 to 28 do Cell[X, Y].Terrain := 11;
    Exit;
    //
    AddNPCHouse(Rand(Pred(9), Succ(9)), Rand(Pred(3), Succ(3)));
    AddNPCHouse(Rand(Pred(21), Succ(21)), Rand(Pred(3), Succ(3)));

    AddNPCHouse(Rand(Pred(3), Succ(3)), Rand(Pred(15), Succ(15)));
    AddNPCHouse(Rand(Pred(27), Succ(27)), Rand(Pred(15), Succ(15)));
    AddNPCHouse(Rand(Pred(15), Succ(15)), Rand(Pred(15), Succ(15)));

    AddNPCHouse(Rand(Pred(9), Succ(9)), Rand(Pred(27), Succ(27)));
    AddNPCHouse(Rand(Pred(21), Succ(21)), Rand(Pred(27), Succ(27)));
    //  }
    for Z := 0 to 199 do PartDraw(Rand(0, DungeonWidth - 1), Rand(0, DungeonHeight - 1));
    Exit;
  end;

  // Dungeons
  for X := 1 to DungeonWidth - 2 do
    for Y := 1 to DungeonHeight - 2 do
      Cell[X, Y].Terrain := 1;

  Z := 4 - (DungeonID div 6);

  CavesCreate(1, 1, DungeonWidth - 1, DungeonHeight - 1);
{  case DungeonID of
    0 .. 5, 14, 24: CatacombsCreate(1, 1, DungeonWidth - 2, DungeonHeight - 2);
    6 ..13, 15..23: AntNestCreate(1, 1, DungeonWidth - 2, DungeonHeight - 2);
  end;
  if (DungeonID > 9) then AntNestCreate(9, 9, DungeonWidth - 8, DungeonHeight - 8);
}
  for X := 1 to DungeonWidth - 2 do
    for Y := 1 to DungeonHeight - 2 do
      if (CountNear(X, Y, 1) <= Z) then
        Cell[X, Y].Terrain := 0;

  if (DungeonID <= 2) then
  for Z := 0 to 9 do
  begin
    X1 := Rand(4, DungeonWidth - 10);
    Y1 := Rand(4, DungeonHeight - 10);
    X2 := X1 + Rand(4, 8);
    Y2 := Y1 + Rand(4, 8);
    if FreeSpace(X1, Y1, X2, Y2) then
    begin
      for X := X1 to X2 do
        for Y := Y1 to Y2 do
          if (X = X1) or (X = X2) or (Y = Y1) or (Y = Y2)
            then Cell[X, Y].Terrain := 1
            else Cell[X, Y].Terrain := 0;
      X := Rand(X1 + 2, X2 - 2);
      Y := Rand(Y1 + 2, Y2 - 2);
      case Rand(0, 3) of
        0: Cell[X, Y1].Terrain := 2;
        1: Cell[X, Y2].Terrain := 2;
        2: Cell[X1, Y].Terrain := 2;
        3: Cell[X2, Y].Terrain := 2;
      end;
    end;
  end;
  //
  for Z := 0 to 99 do
    PartDraw(Rand(4, DungeonWidth - 5), Rand(4, DungeonHeight - 5));
    
  {for X := 0 to DungeonWidth - 1 do
  begin
    Cell[X][0].Terrain := 1;
    Cell[X][DungeonWidth - 1].Terrain := 1;
  end;
  for Y := 0 to DungeonHeight - 1 do
  begin
    Cell[0][Y].Terrain := 1;
    Cell[DungeonHeight - 1][Y].Terrain := 1;
  end; }
end;

{ TCell }

constructor TCell.Create;
begin
  Boss := 0;
  Fire := 0;
  Trap := False;
  Blood := 0;
  Terrain := 1;
  Creature := 0;
  CreatureLife := 0;
  DownStairs := False;
  UpStairs := False;
  Visible := False;
end;

destructor TCell.Destroy;
begin

  inherited;
end;

procedure TCell.SetBlood(const Value: Integer);
begin
  FBlood := Value;
end;

procedure TCell.SetBoss(const Value: Integer);
begin
  FBoss := Value;
end;

procedure TCell.SetCreature(const Value: Integer);
begin
  FCreature := Value;
end;

procedure TCell.SetCreatureLife(const Value: Integer);
begin
  FCreatureLife := Value;
end;

procedure TCell.SetDownStairs(const Value: Boolean);
begin
  FDownStairs := Value;
end;

procedure TCell.SetFire(const Value: Integer);
begin
  if (Value < 0) or (Value > 100) then Exit;
  FFire := Value;
end;

procedure TCell.SetTerrain(const Value: Integer);
begin
  FTerrain := Value;
end;

procedure TCell.SetTrap(const Value: Boolean);
begin
  FTrap := Value;
end;

procedure TCell.SetUpStairs(const Value: Boolean);
begin
  FUpStairs := Value;
end;

procedure TCell.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TCave }

procedure TCave.AddObjects(const ID: Integer);
var
  C, F, I, J, K, X, Y, Z: Integer;
  B: Boolean;
begin
  // Creatures
  if (ID >= 0) then
  for I := 0 to DungeonCreatures - 1 do
  begin
    repeat
      X := Rand(1, DungeonWidth - 2);
      Y := Rand(1, DungeonHeight - 2);
      Z := Rand(ID - 2, ID + 2);
      if Z < 1 then Z := 1;
      if Z > 26 then Z := 26;
    until Done(X, Y, ID);
    with Dungeon[ID].Cell[X][Y] do
    begin
      Creature := Z;
      if (Rand(1, 30 - ID) = 1) then Boss := Rand(1, 3);
      CreatureLife := GetMaxCreatureLife(Creature, Boss);
    end;
  end;

  // The BOSS!!!
  if (ID >= 0) then
  begin
    if (ID = DungeonsCount - 1) then
    begin
      repeat
        X := Rand(1, DungeonWidth - 2);
        Y := Rand(1, DungeonHeight - 2);
      until Done(X, Y, ID);
      with Dungeon[ID].Cell[X][Y] do
      begin
        Boss := 4;
        Creature := 26;
        CreatureLife := GetMaxCreatureLife(Creature, Boss);
      end;
    end;
  end;

  // Gold
  if (ID >= 0) then
  for I := 0 to DungeonGold - 1 do
  begin
    repeat
      X := Rand(1, DungeonWidth - 2);
      Y := Rand(1, DungeonHeight - 2);
      Z := Rand(1, 9) * (ID + 1);
      if Z < 2 then Z := 2;
    until Done(X, Y, ID);
    Dungeon[ID].DropList.AddItem(X, Y, 1, Z);
  end;

  // Plants
  AddPlants(ID);

  case ID of
    8, 22..24: C := 3;
    else C := 1;
  end;

  // Item
  for F := 1 to C do
  if (ID >= 0) then
  with Dungeon[ID] do
  for I := 1 to 5 do
  begin
    repeat
      X := Rand(1, DungeonWidth - 2);
      Y := Rand(1, DungeonHeight - 2);
    until Done(X, Y, ID);
    begin
      Z := Rand(10, 19);
      case Z of
         18 : DropList.AddItem(X, Y, Z, Rand(10, 50));
         19 : DropList.AddItem(X, Y, Z, Rand(10, 50));
         else DropList.AddItem(X, Y, Z, 1);
      end;
    end;
  end;

  // Scrolls
  for F := 1 to C do
  if (ID >= 0) then
  with Dungeon[ID] do
  begin
    if (Rand(1, 2) = 1) then
    begin
      repeat
        X := Rand(1, DungeonWidth - 2);
        Y := Rand(1, DungeonHeight - 2);
      until Done(X, Y, ID);
      Z := ID div 3;
      if (Z < 1) then Z := 1;
      if (Z > 6) then Z := 6;
      Z := GetSpellID(Rand(1, 4), Z);
      DropList.AddItem(X, Y, Z + 160, 1);
    end;                               
  end;

  // Gem
  for F := 1 to C do
  if (ID >= 0) then
  with Dungeon[ID] do
  begin
    if ((ID > 9) and (Rand(0, 19) = 1)) then
    begin            
      repeat
        X := Rand(1, DungeonWidth - 2);
        Y := Rand(1, DungeonHeight - 2);
      until Done(X, Y, ID);
      DropList.AddItem(X, Y, Rand(150, 153), 1);
    end;
  end;

  // Weapon or Armor
  for F := 1 to C do
  if (ID >= 0) and (ID < 20) then
  with Dungeon[ID] do
  begin
    repeat
      X := Rand(1, DungeonWidth - 2);
      Y := Rand(1, DungeonHeight - 2);
      Z := (((ID div 3) * 10) + 20) + Rand(0, 9);
      if Z < 20 then Z := 20;
      if Z > 99 then Z := 99;
    until Done(X, Y, ID);
    DropList.AddItem(X, Y, Z, 1);
  end;

  // Traps
  if (ID >= 0) then
  with Dungeon[ID] do
  for I := 0 to ID + Rand(9, 14) do
  begin
    repeat
      X := Rand(4, DungeonWidth - 5);
      Y := Rand(4, DungeonHeight - 5);
    until Done(X, Y, ID);
    Cell[X][Y].Terrain := Rand(91, 96);
  end;

  // Barrels and chests
  for F := 1 to C do
  if (ID >= 0) then
  with Dungeon[ID] do
  for I := 0 to Rand(2, 6) do
  begin
    B := True;
    repeat
      X := Rand(4, DungeonWidth - 5);
      Y := Rand(4, DungeonHeight - 5);
    until Done(X, Y, ID);
    for K := X - 1 to X + 1 do
      for J := Y - 1 to Y + 1 do
        if Done(K, J, ID) then
          if (Rand(0, 2) <= 1) then
            if B and (Rand(0, 3) = 0) then begin B := False; Dungeon[ID].Cell[K][J].Terrain := 98; end
              else Dungeon[ID].Cell[K][J].Terrain := 99;
  end;

  // PC start pos
  if (ID = -1) then
  begin
    repeat
      PC.X := Rand(1, DungeonWidth - 2);
      PC.Y := Rand(1, DungeonHeight - 2);
    until Done(PC.X, PC.Y, ID)
      and (Dungeon[ID].DropList.IsEmptyCell(PC.X, PC.Y));
  end;
  Cursor.X := PC.X;
  Cursor.Y := PC.Y;
end;

procedure TCave.AddPlants(const ID: Integer);
var
  C, I, X, Y: Integer;
begin
  if (ID >= 0) then
  with Dungeon[ID] do
  for I := 0 to 9 do
  begin
    repeat
      X := Rand(1, DungeonWidth - 2);
      Y := Rand(1, DungeonHeight - 2);
    until Done(X, Y, ID);
    case Rand(0, 9) of
      1..6: C := 1;
      7..9: C := 2;
       else C := 3;
    end;
    if ((Cell[X][Y].Terrain >= 4) and (Cell[X][Y].Terrain <= 9)) then
      DropList.AddItem(X, Y, Rand(2, 9), C);
  end;
end;

procedure TCave.AddStairs(const ID: Integer);
var
  UpStairsPos, DownStairsPos: TPoint;
  C: Integer;
begin
  if (ID > -1) then
  with Dungeon[ID] do
  begin
    UpStairsPos.X := FLastDungeonStairsPos.X;
    UpStairsPos.Y := FLastDungeonStairsPos.Y;
    with Cell[UpStairsPos.X, UpStairsPos.Y] do
    begin
      FTerrain := 0;
      FUpStairs := True;
    end;
  end;
  if (ID < DungeonsCount - 1) then
  with Dungeon[ID] do
  begin
    C := 0;
    repeat
      DownStairsPos.X := Rand(1, DungeonWidth - 2);
      DownStairsPos.Y := Rand(1, DungeonHeight - 2);
      Inc(C);
      if (C > 999) then Exit;
    until (FLastDungeonStairsPos.X <> DownStairsPos.X)
      and (FLastDungeonStairsPos.Y <> DownStairsPos.Y)
      and Done(DownStairsPos.X, DownStairsPos.Y, ID)
      and Done(DownStairsPos.X, DownStairsPos.Y, ID + 1);
    with Cell[DownStairsPos.X, DownStairsPos.Y] do
    begin
      FTerrain := 0;
      FDownStairs := True;
      FLastDungeonStairsPos.X := DownStairsPos.X;
      FLastDungeonStairsPos.Y := DownStairsPos.Y;
    end;
  end;
end;

constructor TCave.Create;
var
  I: ShortInt;
begin
  FWidth := DungeonWidth;
  FHeight := DungeonHeight;
  FCount := DungeonsCount;
  for I := -1 to DungeonsCount - 1 do Dungeon[I] := TDungeon.Create(I);
  FLastDungeonStairsPos.X := 0;
  FLastDungeonStairsPos.Y := 0;
  for I := -1 to DungeonsCount - 1 do AddStairs(I);
  for I := 0 to DungeonsCount - 1 do AddStairs(I);
  for I := -1 to DungeonsCount - 1 do AddObjects(I);
end;

destructor TCave.Destroy;
var
  I: ShortInt;
begin
  for I := -1 to DungeonsCount - 1 do FreeAndNil(Dungeon[I]);
  inherited;
end;

function TCave.Done(const X, Y: Integer; ID: Integer): Boolean;
begin
  Result := (Dungeon[ID].Cell[X][Y].FCreature = 0)
    and ((Dungeon[ID].Cell[X][Y].FTerrain = 0)
    or ((Dungeon[ID].Cell[X][Y].FTerrain >= 4)
    and (Dungeon[ID].Cell[X][Y].FTerrain <= 9)))
    and (Dungeon[ID].Cell[X][Y].FUpStairs = False)
    and (Dungeon[ID].Cell[X][Y].FDownStairs = False)
end;

initialization
  FLastDungeonStairsPos.X := 0;
  FLastDungeonStairsPos.Y := 0;

end.
