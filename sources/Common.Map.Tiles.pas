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
    procedure LoadFromFile(FileName: string);
    procedure Render(Engine: TEngine; X, Y, Z: Integer; Tile: TTile);
  end;

implementation

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

procedure TTiles.LoadFromFile(FileName: string);
begin

end;

constructor TTiles.Create;
begin
  FTiles[tNone] := Add('None', ' ', False, 0);
  FTiles[tDirt] := Add('Dirt', '.', True, $00FFFFFF);
  FTiles[tGrass] := Add('Grass', '"', True, $00FFFFFF);
  FTiles[tGrass2] := Add('Grass', '"', True, $00FFFFFF);
  FTiles[tSmallTree] := Add('Tree', 't', False, $00FFFFFF);
  FTiles[tTree] := Add('Tree', 'T', False, $00FFFFFF);
  FTiles[tStDn] := Add('Entrance to dungeon', '>', True, $00FFFFFF);
  FTiles[tStUp] := Add('Out of the dungeon', '<', True, $00FFFFFF);
  FTiles[tStoneWall] := Add('Stone wall', '#', False, $00FFFFFF);
  FTiles[tStoneWall2] := Add('Stone wall', '#', False, $00FFFFFF);
  FTiles[tStone] := Add('Stone floor', '.', True, $00FFFFFF);
  FTiles[tStone2] := Add('Stone floor', '.', True, $00FFFFFF);
  FTiles[tStone3] := Add('Stone floor', '.', True, $00FFFFFF);
end;

procedure TTiles.Render(Engine: TEngine; X, Y, Z: Integer; Tile: TTile);
begin
  Engine.FontBackColor(Engine.DarkColor(FTiles[Tile].Color, 90));
  Engine.CharOut(X, Y, FTiles[Tile].Symbol, FTiles[Tile].Color);
end;

end.
