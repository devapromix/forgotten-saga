unit Common.Map.Tiles;

interface

uses Engine;

// Размер карты
const
  MapWidth = 80;
  MapHeight = 40;

const  
  Layers = 2;

const
  lrTerrain = 0;
  lrObjects = 1;

type
  TDir = (drLeft, drUp, drRight, drDown, drTop, drBottom);

type
  TTile = (tNone, tDirt, tGrass, tGrass2, tSmallTree, tTree, tStDn, tStUp,
    tStone, tStoneWall, tStoneWall2, tStone2, tStone3);

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
    FTiles: array [TTile] of TTileProp;
    function Add(Name: string; Char: System.Char; Passable: Boolean;
      Color: Integer): TTileProp;
  public
    constructor Create;
    function GetTile(Tile: TTile): TTileProp;
    procedure Render(Engine: TEngine; X, Y, Z: Integer; Tile: TTile);
  end;

implementation

uses Common.Color, ForgottenSaga.Game;

{ TTiles }

function TTiles.Add(Name: string; Char: Char; Passable: Boolean; Color: Integer)
  : TTileProp;
begin
  Result.Name := Name;
  Result.Symbol := Char;
  Result.Passable := Passable;
  Result.Color := Color;
end;

function TTiles.GetTile(Tile: TTile): TTileProp;
begin
  Result := FTiles[Tile];
end;

constructor TTiles.Create;
begin
  FTiles[tNone] := Add('None', ' ', False, cBlack);
  FTiles[tDirt] := Add('Dirt', '.', True, clDirt);
  FTiles[tGrass] := Add('Grass', '"', True, clGrass);
  FTiles[tGrass2] := Add('Grass', '"', True, clPlant);
  FTiles[tSmallTree] := Add('Tree', 't', False, clLtPlant);
  FTiles[tTree] := Add('Tree', 'T', False, clDkPlant);
  FTiles[tStDn] := Add('Entrance to dungeon', '>', True, clTileEnt);
  FTiles[tStUp] := Add('Out of the dungeon', '<', True, clTileEnt);
  FTiles[tStoneWall] := Add('Stone wall', '#', False, clStone2);
  FTiles[tStoneWall2] := Add('Stone wall', '#', False, clStone4);
  FTiles[tStone] := Add('Stone floor', '.', True, clStone);
  FTiles[tStone2] := Add('Stone floor', '.', True, clStone3);
  FTiles[tStone3] := Add('Stone floor', '.', True, cDkGray);
end;

procedure TTiles.Render(Engine: TEngine; X, Y, Z: Integer; Tile: TTile);
begin
  Engine.FontBackColor(Engine.DarkColor(FTiles[Tile].Color, 95));
  Engine.CharOut(X, Y, FTiles[Tile].Symbol, FTiles[Tile].Color);
end;

end.
