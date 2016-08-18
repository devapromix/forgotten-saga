unit Common.Map;

interface

uses Common.Map.Tiles, ForgottenSaga.Creature;

type
  TLayer = array [0 .. MapHeight - 1, 0 .. MapWidth - 1,
    0 .. Layers - 1] of TTile;

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
  TMap = class(TCustomMap)
  private
    FMap: TLayer;
  public
    FileName: string;
    Map: array [TDir] of Integer;
    procedure Clear;
    procedure ClearLayer(LayerID: Byte);
    procedure FillLayer(LayerID: Byte; Tile: TTile);
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    function CellInMap(X, Y: Integer): Boolean;
    function GetTopTileChar(X, Y: Integer): Char;
    procedure SetTile(X, Y, Z: Integer; Tile: TTile);
    function GetTile(X, Y, Z: Integer): TTile;
    function HasTile(Tile: TTile; X, Y: Integer;
      Z: Integer = lrTerrain): Boolean;
    procedure Render;
    procedure Gen;
  end;

implementation

uses Engine, Classes, SysUtils, Math, Common.Utils, Common.Map.Generator,
  Common.Color, ForgottenSaga.Game;

const
  Offset = 40;

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
  L.LoadFromFile(AFileName);
  for Z := 0 to Layers - 1 do
  begin
    I := L.IndexOf(Format('[%d]', [Z])) + 1;
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        FMap[Y][X][Z] := TTile(Ord(L[Y + I][X + 1]) - Offset);
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
  L.SaveToFile(AFileName);
  L.Free;
end;

procedure TMap.Render;
var
  X, Y, Z: Integer;
begin
  for Z := 0 to Layers - 1 do
    for Y := 0 to Self.Height - 1 do
      for X := 0 to Self.Width - 1 do
        case Z of
          lrTerrain:
            Saga.Tiles.Render(Saga.Engine, X, Y, Z, FMap[Y][X][Z]);
          lrObjects:
            if not HasTile(tNone, X, Y, Z) then
              Saga.Tiles.Render(Saga.Engine, X, Y, Z, FMap[Y][X][Z]);
        end;
end;

procedure TMap.FillLayer(LayerID: Byte; Tile: TTile);
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

function TMap.HasTile(Tile: TTile; X, Y: Integer;
  Z: Integer = lrTerrain): Boolean;
begin
  Result := FMap[Y][X][Z] = Tile
end;

procedure TMap.SetTile(X, Y, Z: Integer; Tile: TTile);
begin
  FMap[Y][X][Z] := Tile;
end;

function TMap.GetTile(X, Y, Z: Integer): TTile;
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
