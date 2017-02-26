unit uHelpKeys;

interface

procedure DrawHelpKeys();
procedure KeysHelpKeys(var Key: Word);

implementation

uses uConst, uDraw, uGraph, uLang, uColor, uTypes;

procedure DrawHelpKeys();
var
  I: Integer;
begin
  I := ScreenWidth div 2;
  TitleOut(Lang.Lang(217), 5, 3);
  //
  TextOut( 5,  5, '[a]', Lang.Lang(200));
  TextOut( 5,  6, '[b]', Lang.Lang(208));
  TextOut( 5,  7, '[c]', Lang.Lang(225));
  TextOut( 5,  8, '[g]', Lang.Lang(203));
  TextOut( 5,  9, '[i]', Lang.Lang(201));
  //
  TextOut( I,  5, '[k]', Lang.Lang(205));
  TextOut( I,  6, '[l]', Lang.Lang(216));
  TextOut( I,  7, '[m]', Lang.Lang(202));
  TextOut( I,  8, '[p]', Lang.Lang(214));
  TextOut( I,  9, '[q]', Lang.Lang(211));
  TextOut( I, 10, '[s]', Lang.Lang(139));
  TextOut( I, 11, '[t]', Lang.Lang(215));
  //
  TextOut( I, 18, '[?]', Lang.Lang(54));
  TextOut( I, 19, '[<]', Lang.Lang(101));
  TextOut( I, 20, '[>]', Lang.Lang(105));
  //
  BarOut('[esc]', Lang.Lang(40), true);
end;

procedure KeysHelpKeys(var Key: Word);
begin
  case Key of
    27:
    begin
      GameFrame := gfHelp;
      Draw();
    end;
  end;
end;

end.

