unit uHelp;

interface

procedure DrawHelp();
procedure KeysHelp(var Key: Word);

implementation

uses uConst, uDraw, uGraph, uLang, uColor, uTypes;

procedure DrawHelp();
begin
  TitleOut(Lang.Lang(54), 5, 3);
  TextOut( 5,  5, '[a]', Lang.Lang(218));
  TextOut( 5,  6, '[b]', Lang.Lang(217));
  BarOut('[esc]', Lang.Lang(40), true);
  BarOut('[a-b]', Lang.Lang(156), false);
end;

procedure KeysHelp(var Key: Word);
begin
  case Key of
    27:
    begin
      GameFrame := GameFrameTemp;
      Draw();
    end;
    ord('A'): // About
    begin
      GameFrame := gfHelpAbout;
      Draw();
    end;
    ord('B'): // Keys
    begin
      GameFrame := gfHelpKeys;
      Draw();
    end;
  end;
end;

end.

