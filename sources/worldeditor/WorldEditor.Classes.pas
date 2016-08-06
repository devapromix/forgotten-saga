unit WorldEditor.Classes;

interface

uses Windows, Graphics, Types, Controls, ForgottenSaga.Game, Common.Map, Common.Map.Tiles;

type
  TEditor = class(TSaga)
  private
    FMap: TMap;
    FPos: TPoint;
    FToolBarHeight: Integer;
    FTiles: TTiles;
    FTile: TTile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RenderTerrain;
    procedure RenderObjects;
    procedure RenderItems;
    procedure RenderCreatures;
    procedure MouseMove(X, Y: Integer);
    procedure MouseDown(Button: TMouseButton;X, Y: Integer);
    procedure KeyDown(var Key: Word);
    property Pos: TPoint read FPos write FPos;
    property ToolBarHeight: Integer read FToolBarHeight write FToolBarHeight;
    property Map: TMap read FMap write FMap;
    property Tiles: TTiles read FTiles write FTiles;
    property Tile: TTile read FTile write FTile;
  end;

var
  Editor: TEditor;

implementation

uses Engine, Common.Utils;

{ TEditor }

constructor TEditor.Create;
begin
  inherited Create(MapWidth, MapHeight);
  Map := TMap.Create;
  Tiles := TTiles.Create;
  Map.LoadFromFile(GetPath('resources') + 'orc_camp.map');
end;

destructor TEditor.Destroy;
begin
  Tiles.Free;
  Map.Free;
  inherited;
end;

procedure TEditor.KeyDown(var Key: Word);
begin

end;

procedure TEditor.MouseDown(Button: TMouseButton; X, Y: Integer);
begin
  case Button of
    mbLeft:
      Map.SetTile(Pos.X, Pos.Y, lrTerrain, Self.Tile);
    mbRight:
      Map.SetTile(Pos.X, Pos.Y, lrTerrain, tNone);
  end;
end;

procedure TEditor.MouseMove(X, Y: Integer);
begin
  Pos := Point(X div Engine.Char.Width, (Y - ToolBarHeight) div Engine.Char.Height);
  if (GetKeyState(VK_LBUTTON) < 0) and (Map.CellInMap(Pos.X, Pos.Y)) then
    Map.SetTile(Pos.X, Pos.Y, lrTerrain, Self.Tile);
end;

procedure TEditor.RenderCreatures;
begin

end;

procedure TEditor.RenderItems;
begin

end;

procedure TEditor.RenderObjects;
var
  X, Y: Integer;
begin
  for Y := 0 to Map.Height - 1 do
    for X := 0 to Map.Width - 1 do
      if not Map.HasTile(tNone, X, Y, lrObjects) then
        Tiles.Render(Engine, X, Y, lrObjects, Map.GetTile(X, Y, lrObjects));
end;

procedure TEditor.RenderTerrain;
var
  X, Y: Integer;
begin
  for Y := 0 to Map.Height - 1 do
    for X := 0 to Map.Width - 1 do
      Tiles.Render(Engine, X, Y, lrTerrain, Map.GetTile(X, Y, lrTerrain));
end;

end.
