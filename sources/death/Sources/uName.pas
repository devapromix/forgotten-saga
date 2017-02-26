unit uName;

interface

procedure DrawName();
procedure KeysName(var Key: Word); overload;
procedure KeysName(var Key: Char); overload;

implementation

uses uDraw, uGraph, uConst, uColor, uPC, uLang, uTypes, uMain;

procedure DrawName();
begin
  TextOut((ScreenWidth div 2) - (Length(Lang.Lang(160)) div 2),
    12, Lang.Lang(160) + ':', cRdYellow);
  BarOut('[esc]', Lang.Lang(161), true);
  if (PCName = '') then
    BarOut('[space, enter]', Lang.Lang(162), false)
  else begin
    BarOut('[space]', Lang.Lang(162), false);
    BarOut('[enter]', Lang.Lang(150), false);
  end;
  if ShowCursor then
  begin
    TextOut(32, 14, PCName, cLtYellow);
    TextOut(32 + Length(PCName), 14, CursorChar, cLtRed)
  end else TextOut(32, 14, PCName, cLtYellow);
  Refresh();
end;

procedure AddChar(C: string);
begin
  if (Length(PCName) < 16) then
  begin
    PCName := PCName + C;
    DrawName();
  end;
end;

procedure KeysName(var Key: Char);
const
  C1 = 'abcdefghijklmnopqrstuvwxyz';
  C2 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  C3 = 'àáâãäåºæçè³¿éêëìíîïðñòóôõö÷øùþÿüúý¸';
  C4 = 'ÀÁÂÃÄÅªÆÇÈ²¯ÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÞßÜÚÝ¨';
  C5 = '-';
  Chars = C1 + C2 + C3 + C4 + C5;
begin
  if (Pos(Key, Chars) > 0) then AddChar(Key);
end;

procedure KeysName(var Key: Word);
begin
  case Key of
    8:
    begin
      if (Length(PCName) > 0) then
      begin
        Delete(PCName, Length(PCName), 1);
        DrawName();
      end;
    end;
    13:
    begin
      if (PCName <> '') then
      begin
        fMain.DoTimer.Enabled := False;
        PC.Name := PCName;
        GameFrame := gfRace;
      end else PCName := GenPCName();
      Draw();
    end;
    32:
    begin          
      PCName := GenPCName();
      DrawName();
    end;
    27:
    begin
      fMain.DoTimer.Enabled := False;
      GameFrame := gfMenu;
      Draw();
    end;
  end;
end;

end.


