unit Common.Map;

interface

uses Engine, ForgottenSaga.Creature;

// Размер карты
const
  MapWidth = 80;
  MapHeight = 40;

const
  Layers = 2;
  TileDarkPercent = 40;

const
  lrTerrain = 0;
  lrObjects = 1;

type
  TTileEnum = (tNone, tRes1, tRes2, tRes3, tRes4, tRes5, tDirt, tGrass, tStone,
    tSand, tRock, tWater, tDeepWater, tLava, tRes6, tRes7, tRes8, tRes9, tRes10,
    tWillowTree, tOakTree, tAshTree, tYewTree, tBirchTree, tAspenTree,
    tMapleTree, tWalnutTree, tPineTree, tCedarTree, tSpruceTree, tRes11, tRes12,
    tRes13, tRes14, tRes15, tStDn, tStUp, tRes16, tRes17, tRes18, tRes19,
    tRes20, tStoneFloor, tStoneWall, tRes21, tRes22, tRes23, tRes24,
    tRes25, tRoad);

const
  TileStr: array [TTileEnum] of string = ('NONE', 'RES1', 'RES2', 'RES3',
    'RES4', 'RES5', 'DIRT', 'GRASS', 'STONE', 'SAND', 'ROCK', 'WATER',
    'DEEP_WATER', 'LAVA', 'RES6', 'RES7', 'RES8', 'RES9', 'RES10',
    'WILLOW_TREE', 'OAK_TREE', 'ASH_TREE', 'YEW_TREE', 'BIRCH_TREE',
    'ASPEN_TREE', 'MAPLE_TREE', 'WALNUT_TREE', 'PINE_TREE', 'CEDAR_TREE',
    'SPRUCE_TREE', 'RES11', 'RES12', 'RES13', 'RES14', 'RES15', 'STAIRS_DOWN',
    'STAIRS_UP', 'RES16', 'RES17', 'RES18', 'RES19', 'RES20', 'STONE_FLOOR',
    'STONE_WALL', 'RES21', 'RES22', 'RES23', 'RES24', 'RES25', 'ROAD');

type
  TTileProp = record
    Name: string;
    Symbol: System.Char;
    Passable: Boolean;
    Color: Integer;
  end;

type
  TTiles = class(TObject)
  private
    FTiles: array [TTileEnum] of TTileProp;
    function Add(Name: string; Char: System.Char; Passable: Boolean;
      Color: Integer): TTileProp;
  public
    function GetTile(Tile: TTileEnum): TTileProp;
    procedure LoadFromFile(FileName: string);
  end;

type
  TLayer = array [0 .. MapHeight - 1, 0 .. MapWidth - 1, 0 .. Layers - 1]
    of TTileEnum;

type
  TCustomMap = class(TEntity)
  private
    FHeight: Integer;
    FWidth: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create(Width, Height: Integer);
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

type
  TDir = (drLeft, drUp, drRight, drDown, drTop, drBottom);

type
  TMap = class(TCustomMap)
  private
    FMap: TLayer;
  public
    FileName: string;
    Map: array [TDir] of Integer;
    procedure Clear;
    procedure ClearLayer(LayerID: Byte);
    procedure FillLayer(LayerID: Byte; Tile: TTileEnum);
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    function CellInMap(X, Y: Integer): Boolean;
    function GetTopTileChar(X, Y: Integer): Char;
    procedure SetTile(X, Y, Z: Integer; Tile: TTileEnum);
    function GetTile(X, Y, Z: Integer): TTileEnum;
    function HasTile(Tile: TTileEnum; X, Y: Integer;
      Z: Integer = lrTerrain): Boolean;
    procedure Render;
    procedure Gen;
  end;

implementation

uses Classes, SysUtils, ForgottenSaga.Game;

const
  Offset = 40;

  { TTiles }

function TTiles.Add(Name: string; Char: Char; Passable: Boolean; Color: Integer)
  : TTileProp;
begin
  Result.Name := Name;
  Result.Symbol := Char;
  Result.Passable := Passable;
  Result.Color := Color;
end;

function TTiles.GetTile(Tile: TTileEnum): TTileProp;
begin
  Result := FTiles[Tile];
end;

procedure TTiles.LoadFromFile(FileName: string);
var
  I: TTileEnum;
  S: string;
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try
    for I := Low(TTileEnum) to High(TTileEnum) do
    begin
      S := TileStr[I];
      if F.SectionExists(S) then
      begin
        FTiles[I].Name := F.ReadString(S, 'Name', '');
        FTiles[I].Symbol := F.ReadString(S, 'Symbol', '?')[1];
        FTiles[I].Passable := F.ReadBool(S, 'Passable', False);
        FTiles[I].Color := F.ReadColor(S, 'Color', '100,100,100');
      end;
    end;
  finally
    F.Free;
  end;
end;

{ TCustomMap }

constructor TCustomMap.Create(Width, Height: Integer);
begin
  FHeight := Height;
  FWidth := Width;
end;

function TCustomMap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCustomMap.GetWidth: Integer;
begin
  Result := FWidth;
end;

{ TMap }

function TMap.CellInMap(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height);
end;

constructor TMap.Create;
begin
  inherited Create(MapWidth, MapHeight);
  Clear;
end;

destructor TMap.Destroy;
begin
  inherited;
end;

procedure TMap.LoadFromFile(AFileName: string);
var
  X, Y, Z, I: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  L.LoadFromFile(AFileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  for Z := 0 to Layers - 1 do
  begin
    I := L.IndexOf(Format('[%d]', [Z])) + 1;
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        FMap[Y][X][Z] := TTileEnum(Ord(L[Y + I][X + 1]) - Offset);
  end;
  L.Free;
end;

procedure TMap.SaveToFile(AFileName: string);
var
  X, Y, Z: Integer;
  L: TStringList;
  S: string;
begin
  L := TStringList.Create;
  L.Append(Format('; %s', [ExtractFileName(AFileName)]));
  for Z := 0 to Layers - 1 do
  begin
    L.Append(Format('[%d]', [Z]));
    for Y := 0 to Height - 1 do
    begin
      S := '';
      for X := 0 to Width - 1 do
        S := S + Chr(Ord(FMap[Y][X][Z]) + Offset);
      L.Append(S);
    end;
  end;
  L.SaveToFile(AFileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  L.Free;
end;

procedure TMap.Render;
var
  X, Y, Z: Integer;
  TerTile, ObjTile: TTileProp;
begin
  for Z := 0 to Layers - 1 do
    for Y := 0 to Self.Height - 1 do
      for X := 0 to Self.Width - 1 do
        case Z of
          lrTerrain:
            begin
              TerTile := Saga.Tiles.GetTile(FMap[Y][X][Z]);
              Saga.Engine.FontBackColor(Saga.Engine.DarkColor(TerTile.Color, TileDarkPercent));
              Saga.Engine.CharOut(X, Y, TerTile.Symbol, TerTile.Color);
            end;
          lrObjects:
            if not HasTile(tNone, X, Y, Z) then
            begin
              TerTile := Saga.Tiles.GetTile(FMap[Y][X][lrTerrain]);
              ObjTile := Saga.Tiles.GetTile(FMap[Y][X][Z]);
              Saga.Engine.FontBackColor(Saga.Engine.DarkColor(TerTile.Color, TileDarkPercent));
              Saga.Engine.CharOut(X, Y, ObjTile.Symbol, ObjTile.Color);
            end;
        end;
end;

procedure TMap.FillLayer(LayerID: Byte; Tile: TTileEnum);
var
  X, Y: Integer;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      FMap[Y][X][LayerID] := Tile;
end;

procedure TMap.ClearLayer(LayerID: Byte);
begin
  FillLayer(LayerID, tNone);
end;

procedure TMap.Clear;
var
  I: Integer;
begin
  for I := 0 to Layers - 1 do
    ClearLayer(I);
end;

function TMap.HasTile(Tile: TTileEnum; X, Y: Integer;
  Z: Integer = lrTerrain): Boolean;
begin
  Result := FMap[Y][X][Z] = Tile
end;

procedure TMap.SetTile(X, Y, Z: Integer; Tile: TTileEnum);
begin
  FMap[Y][X][Z] := Tile;
end;

function TMap.GetTile(X, Y, Z: Integer): TTileEnum;
begin
  Result := FMap[Y][X][Z];
end;

function TMap.GetTopTileChar(X, Y: Integer): Char;
var
  N, Z: Integer;
begin
  Result := ' ';
  for Z := Layers - 1 downto 0 do
    if (GetTile(X, Y, Z) <> tNone) then
    begin
      Result := Saga.Tiles.GetTile(GetTile(X, Y, Z)).Symbol;
      N := Saga.World.CurrentItems.Has(X, Y);
      if (N > -1) then
        Result := Saga.World.CurrentItems.Get(N).Symbol;
      N := Saga.World.CurrentCreatures.Has(X, Y);
      if (N > -1) then
        Result := Saga.World.CurrentCreatures.Get(N).Symbol;
      if (Saga.Player.Has(X, Y)) then
        Result := '@';
      Exit;
    end;
end;

procedure TMap.Gen;
begin
  // GenTestMap();
  // GenOrcVillage();
  // GenDarkCave();
end;

end.
