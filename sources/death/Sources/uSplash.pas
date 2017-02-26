unit uSplash;

interface

procedure DrawSplash();
procedure KeysSplash(var Key: Word);

implementation

uses uDraw, uConst, uScreen, uTypes;

procedure DrawSplash();
begin
  LoadFrame('logo');
  DrawFrame();
end;

procedure KeysSplash(var Key: Word);
begin
  GameFrame := gfMenu;
  Draw();
end;

end.

