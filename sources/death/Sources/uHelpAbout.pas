unit uHelpAbout;

interface

procedure DrawHelpAbout();
procedure KeysHelpAbout(var Key: Word);

implementation

uses uConst, uDraw, uGraph, uLang, uColor, uTypes;

procedure DrawHelpAbout();
begin
  TitleOut(Lang.Lang(218), 5, 3);
  //
  
  //
  BarOut('[esc]', Lang.Lang(40), true);
end;

procedure KeysHelpAbout(var Key: Word);
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

