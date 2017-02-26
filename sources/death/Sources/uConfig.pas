unit uConfig;

interface

procedure DrawConfig();
procedure KeysConfig(var Key: Word);

implementation

uses uInit, uLang, uDraw, uGraph, uConst, uTypes, uColor;

var Y: Byte = 5;

procedure DrawConfigLine(LangID: Integer; Flag: Boolean);
var
  S: string;
begin
  TextOut( 5,  Y, '[' + Chr(Y - 3 + 95) + ']', Lang.Lang(LangID));
  if Flag then
    TextOut(ScreenWidth - 9,  Y, '[X]', cLtYellow)
      else
        TextOut(ScreenWidth - 9,  Y, '[ ]', cLtYellow);
  Inc(Y);
end;

procedure DrawConfig();
begin
  Y := 5;
  TitleOut(Lang.Lang(1650), 5, 3);
  DrawConfigLine(1652, AutoPickUpGold);
  //
  BarOut('[esc]', Lang.Lang(40), true);
  BarOut('[a-' + Chr(Y - 4 + 95) + ']', Lang.Lang(1651), false);
end;

procedure KeysConfig(var Key: Word);
begin
  case Key of
    27:
    begin
      GameFrame := GameFrameTemp;
      Draw();
    end;
    ord('A'):
    begin
      AutoPickUpGold := not AutoPickUpGold;
      Draw();
    end;
  end;
end;

end.
