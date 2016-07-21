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
  FTiles[tNone] := Add('Пустота', ' ', False, cBlack);
  FTiles[tDirt] := Add('Грязь', '.', True, clDirt);
  FTiles[tGrass] := Add('Трава', '"', True, clGrass);
  FTiles[tGrass2] := Add('Трава (Остролист)', '"', True, clPlant);
  FTiles[tSmallTree] := Add('Tree', 't', False, clLtPlant);
  FTiles[tTree] := Add('Tree', 'T', False, clDkPlant);
  FTiles[tStDn] := Add('Вход в подземелье', '>', True, clTileEnt);
  FTiles[tStUp] := Add('Выход из подземелья', '<', True, clTileEnt);
  FTiles[tStone] := Add('Каменный пол', '.', True, clStone);
  FTiles[tStoneWall] := Add('Каменная стена', '#', False, clStone2);
  FTiles[tStone2] := Add('Каменный пол', '.', True, clStone3);
  FTiles[tStoneWall2] := Add('Каменная стена', '#', False, clStone4);
  FTiles[tStone3] := Add('Каменный пол', '.', True, cDkGray);
end;

procedure TTiles.Render(Engine: TEngine; X, Y, Z: Integer; Tile: TTile);
begin
  Engine.FontBackColor(Engine.DarkColor(FTiles[Tile].Color, 95));
  Engine.CharOut(X, Y, FTiles[Tile].Symbol, FTiles[Tile].Color);
end;

end.
