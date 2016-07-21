unit Common.Map.Generator;

interface

procedure GenOrcVillage();
procedure GenDarkCave();
procedure GenTestMap();

implementation

uses Math, ForgottenSaga.Game, Common.Map.Tiles;

procedure Cave(Tile: TTile);
var
  I: Integer;
  kx, ky, k, dx, dy: real;
  x, y, py, px: Integer;
  counter: Integer;
begin
  x := MapWidth; // + 20;
  y := MapHeight; // + 20;
  Saga.World.CurrentMap.SetTile(x div 2, y div 2, lrTerrain, Tile);
  for I := 0 to (x * y div 5) do
  begin
    try
      k := (random(360) + 1) * 3.1419532 / 180;
      kx := (x / 3) + (y / 2) * sin(k);
      ky := (y / 3) + (y / 2) * cos(k);
      dx := 1;
      dy := 1;
      while ((abs(dx) < 10) and (abs(dy) < 10)) do
      begin
        dx := random(100) + 1;
        dy := random(100) + 1;
      end;
      dx := dx - 40;
      dy := dy - 30;
      dx := dx / 50;
      dy := dy / 30;
      counter := 0;
      while (true) do
      begin
        if counter + 1 > 10000 then
          break;
        counter := counter + 1;
        kx := kx + dx;
        ky := ky + dy;
        px := round(kx);
        py := round(ky);
        if (px < 0) then
        begin
          px := x;
          kx := px;
        end;
        if (px > x) then
        begin
          px := 1;
          kx := px;
        end;
        if (py < 0) then
        begin
          py := y;
          ky := py;
        end;
        if (py > y) then
        begin
          py := 1;
          ky := py;
        end;
        if (px = 0) then
          px := random(x) + 1;
        if (py = 0) then
          py := random(y) + 1;
        if ((px > 1) and (Saga.World.CurrentMap.HasTile(Tile ,px - 1, py))) or
          ((py > 1) and (Saga.World.CurrentMap.HasTile(Tile ,px, py - 1))) or
          ((px < x) and (Saga.World.CurrentMap.HasTile(Tile ,px + 1, py))) or
          ((py < y) and (Saga.World.CurrentMap.HasTile(Tile ,px, py + 1))) then
          if (px <> 0) and (px <> MapWidth - 1) and (py <> 0) and
            (py <> MapHeight - 1) then
          begin
            Saga.World.CurrentMap.SetTile(px, py, lrTerrain, Tile);
            break;
          end;
      end;
    except
    end;
  end;
end;

procedure AddSpot(Tile: TTile);
var
  I, x, y, k, L: Integer;
begin
  k := Math.RandomRange(0, Saga.World.CurrentMap.Width);
  L := Math.RandomRange(0, Saga.World.CurrentMap.Height);
  x := k;
  y := L;
  for I := 1 to 25 do
  begin
    x := x + Math.RandomRange(0, 3) - 1;
    y := y + Math.RandomRange(0, 3) - 1;
    if Saga.World.CurrentMap.CellInMap(x, y) then
      Saga.World.CurrentMap.SetTile(x, y, lrTerrain, Tile);
  end;
end;

procedure AddTile(x, y: Integer; Tile: TTile); overload;
begin
  Saga.World.CurrentMap.SetTile(x, y, lrTerrain, Tile);
end;

procedure AddTile(Tile: TTile); overload;
var
  x, y: Integer;
begin
  x := Math.RandomRange(0, Saga.World.CurrentMap.Width);
  y := Math.RandomRange(0, Saga.World.CurrentMap.Height);
  AddTile(x, y, Tile);
end;

function GetFloor: Word;
begin
  Result := Math.RandomRange(ord(tDirt), ord(tGrass));
end;

procedure GenOrcVillage();
var
  I: Integer;
begin
  Saga.World.CurrentMap.FillLayer(lrTerrain, tDirt);
  for I := 1 to 90 do
    AddSpot(tStone);
  for I := 1 to 50 do
    AddSpot(tGrass);
  for I := 1 to 20 do
    AddSpot(tGrass2);
  // for I := 1 to 5 do AddSpot(tlTree);
  for I := 1 to 5 do
    AddSpot(tSmallTree);
  for I := 1 to 15 do
    AddSpot(tStoneWall);
  for I := 1 to 15 do
    AddSpot(tStoneWall2);
  // AddTile(56, 14, tlStDn);
  // AddTile({56, 14,}33, 27, tlStUp);
  AddTile(34, 27, tStDn);
end;

procedure GenDarkCave();
begin
  Saga.World.CurrentMap.FillLayer(lrTerrain, tStoneWall2);
  Cave(tStone3);
  Cave(tStone3);
  Cave(tStone3);
  // AddTile(56, 14, tStUp);
  // AddTile(33, 27, tStUp);
  AddTile(34, 27, tStUp);
end;

procedure GenTestMap();
begin
  Saga.World.CurrentMap.FillLayer(lrTerrain, tDirt);
end;

end.
