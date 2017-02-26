unit uMenu;

interface

uses
  Classes, SysUtils, Graphics;

procedure DrawMenu();
procedure KeysMenu(var Key: Word); overload;
procedure KeysMenu(var Key: Char); overload;

implementation

uses uMain, uGraph, uColor, uConst, uDraw, uScreen, uLang, uUtils, uTypes;

const MenuItemsCount = 6;

procedure DrawMenu();
var
  I, J, K: Integer;
  S: string;
begin
  LoadFrame('logo');
  DrawFrame();
  K := 0;
  for I := 0 to MenuItemsCount do
  begin
    J := Length(Lang.Lang(I + 50));
    if (J > K) then K := J;
  end;
  K := ScreenWidth - K - (ScreenWidth div 10);
  for I := 0 to MenuItemsCount - 1 do
  begin
    case I of
      0: if IsGame then
           S := Lang.Lang(51) else
             S := Lang.Lang(52);
      1: S := Lang.Lang(53);
      2: S := Lang.Lang(54);
      3: S := Lang.Lang(50);
      4: S := Lang.Lang(1650);
      5: S := Lang.Lang(55);
    end;
    if (I = MenuPos) then
    begin
      TextOut(K - 2, I + 25, '@', cRdRed);
      TextOut(K, I + 25, S, cLtYellow);
    end else TextOut(K, I + 25, S, cDkYellow);
  end;
  with BG.Canvas do
  begin
    Font.Color := cLtGray;
    I := Font.Size;
    J := I - 4;
    if J > 8 then J := 8;
    Font.Size := J;
    TextOut(K * CharWidth, (ScreenHeight - 1) * CharHeight, DeathVersion);
    Font.Size := I;
  end;
  
end;

procedure KeysMenu(var Key: Word);
begin
  case Key of
    27: if IsGame then
        begin
          GameFrame := gfGame;
          Draw();
        end;
    13:
    begin
      case MenuPos of
        0:
        begin
          GameFrameTemp := gfMenu;
          if IsGame then
          begin
            GameFrame := gfGame;
            GameFrameTemp := gfGame;
          end else
          begin
            fMain.DoTimer.Enabled := True;
            GameFrame := gfName;
            PCName := '';
          end;
          Draw();
        end;
        2:
        begin
          GameFrame := gfHelp;
          GameFrameTemp := gfMenu;
          Draw();
        end;
        3:
        begin
          GameFrame := gfRecords;
          GameFrameTemp := gfMenu;
          Draw();
        end;
        4:
        begin
          GameFrame := gfConfig;
          GameFrameTemp := gfMenu;
          Draw();
        end;
        5: Halt;
      end;
    end;
    38:
    begin
      if MenuPos = 0 then MenuPos := MenuItemsCount - 1 else Dec(MenuPos);
      Draw();
    end;
    40:
    begin
      if MenuPos >= MenuItemsCount - 1 then MenuPos := 0 else Inc(MenuPos);
      Draw();
    end;
  end;
end;

procedure KeysMenu(var Key: Char);
begin
  case Key of
    '?': begin // Help
           GameFrame := gfHelp;
           GameFrameTemp := gfMenu;
           Draw();
         end;
  end;
end;

end.


