unit WorldEditor.Classes;

interface

uses Windows, Graphics, Types, Controls, ForgottenSaga.Classes,
  ForgottenSaga.Entities;

{$REGION ' TEditor '}

function __(S: string): string;

type
  TEditor = class(TSaga)
  private
    FCurrentMap: TMap;
    FPos: TPoint;
    FToolBarHeight: Integer;
    FTile: TTiles.TTileEnum;
    FCurrentMapFile: string;
    FModified: Boolean;
    FCreatures: TCreatures;
    FItems: TItems;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RenderTerrain;
    procedure RenderObjects;
    procedure RenderItems;
    procedure RenderCreatures;
    procedure MouseMove(Layer: TMap.TLayerEnum; X, Y: Integer);
    procedure MouseDown(Layer: TMap.TLayerEnum; Button: TMouseButton; X, Y: Integer);
    procedure KeyDown(var Key: Word);
    property CurrentMapFile: string read FCurrentMapFile write FCurrentMapFile;
    property Pos: TPoint read FPos write FPos;
    property ToolBarHeight: Integer read FToolBarHeight write FToolBarHeight;
    property CurrentMap: TMap read FCurrentMap write FCurrentMap;
    property Tile: TTiles.TTileEnum read FTile write FTile;
    property Modified: Boolean read FModified write FModified;
    property Creatures: TCreatures read FCreatures write FCreatures;
    property Items: TItems read FItems write FItems;
  end;

var
  Editor: TEditor;

{$ENDREGION ' TEditor '}

implementation

uses Engine;

{$REGION ' TEditor '}

function __(S: string): string;
begin
  Result := '?';
  if (S = '') then
    Exit;
  Result := Editor.Lg.Get(S);
end;

constructor TEditor.Create;
begin
  inherited Create(TMap.Size.Width, TMap.Size.Height);
  CurrentMap := TMap.Create;
  CurrentMapFile := '';
  Creatures := TCreatures.Create;
  Items := TItems.Create;
  Modified := False;
end;

destructor TEditor.Destroy;
begin
  Items.Free;
  Creatures.Free;
  CurrentMap.Free;
  inherited;
end;

procedure TEditor.KeyDown(var Key: Word);
begin

end;

procedure TEditor.MouseDown(Layer: TMap.TLayerEnum; Button: TMouseButton; X, Y: Integer);
begin
  case Button of
    mbLeft:
      begin
        case Layer of
          lrTerrain, lrObjects:
            CurrentMap.SetTile(Pos.X, Pos.Y, Layer, Self.Tile);
        end;
        Modified := True;
      end;
    mbRight:
      begin
        case Layer of
          lrTerrain, lrObjects:
            CurrentMap.SetTile(Pos.X, Pos.Y, Layer, tNone);
        end;
        Modified := True;
      end;
  end;
end;

procedure TEditor.MouseMove(Layer: TMap.TLayerEnum; X, Y: Integer);
begin
  Pos := Point(X div Engine.Char.Width, (Y - ToolBarHeight)
    div Engine.Char.Height);
  if (GetKeyState(VK_LBUTTON) < 0) and (CurrentMap.CellInMap(Pos.X, Pos.Y)) then
    CurrentMap.SetTile(Pos.X, Pos.Y, Layer, Self.Tile);
end;

procedure TEditor.RenderCreatures;
var
  I: Integer;
  E: TCreature;
begin
  for I := 0 to Creatures.Count - 1 do
  begin
    E := Creatures.Entity[I];
    Editor.UI.DrawChar(E.Pos.X, E.Pos.Y, E.Symbol, E.Color, E.BackColor);
  end;
end;

procedure TEditor.RenderItems;
var
  I: Integer;
  E: TItem;
begin
  for I := 0 to Items.Count - 1 do
  begin
    E := Items.Entity[I];
    Editor.UI.DrawChar(E.Pos.X, E.Pos.Y, E.Symbol, E.Color, E.BackColor);
  end;
end;

procedure TEditor.RenderObjects;
var
  X, Y: Integer;
  TerTile, ObjTile: TTiles.TTileProp;
begin
  for Y := 0 to CurrentMap.Height - 1 do
    for X := 0 to CurrentMap.Width - 1 do
      if not CurrentMap.HasTile(tNone, X, Y, lrObjects) then
      begin
        TerTile := Editor.Tiles.GetTile(CurrentMap.GetTile(X, Y, lrTerrain));
        ObjTile := Editor.Tiles.GetTile(CurrentMap.GetTile(X, Y, lrObjects));
        Editor.UI.DrawChar(X, Y, ObjTile.Symbol, ObjTile.Color,
          Editor.Engine.DarkColor(TerTile.Color, TTiles.TileDarkPercent));
      end;
end;

procedure TEditor.RenderTerrain;
var
  X, Y: Integer;
  TerTile: TTiles.TTileProp;
begin
  for Y := 0 to CurrentMap.Height - 1 do
    for X := 0 to CurrentMap.Width - 1 do
    begin
      TerTile := Editor.Tiles.GetTile(CurrentMap.GetTile(X, Y, lrTerrain));
      Editor.UI.DrawChar(X, Y, TerTile.Symbol, TerTile.Color,
        Editor.Engine.DarkColor(TerTile.Color, TTiles.TileDarkPercent));
    end;
end;

{$ENDREGION ' TEditor '}

end.
