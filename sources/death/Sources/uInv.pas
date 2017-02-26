unit uInv;

interface

uses Classes;

procedure DrawInv();
procedure KeysInv(var Key: Word); overload;
procedure KeysInv(var Key: Word; Shift: TShiftState); overload;
function InvCount: Word;
function GetSelItemID(const Index: Integer): Integer;
function GetSelItemCount(const Index: Integer): Integer;

implementation

uses SysUtils, uGraph, uColor, uConst, uDraw, uLang, uPC, uBox, uGame,
  uTypes, uMsg;

var
  P, U, Y: Integer;

function GetSelItemID(const Index: Integer): Integer;
var
  I, C: Integer;
begin
  C := 0;
  Result := 0;
  for I := 1 to ItemsCount do
  if (PC.Items[I] > 0) then
  begin
    Inc(C);
    if (Index = C) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function GetSelItemCount(const Index: Integer): Integer;
var
  I, C: Integer;
begin
  C := 0;
  Result := 0;
  for I := 1 to ItemsCount do
  if (PC.Items[I] > 0) then
  begin
    Inc(C);
    if (Index = C) then
    begin
      Result := PC.Items[I];
      Exit;
    end;
  end;
end;

procedure AddInvItem(const S: string; const ID, Count: Integer);
var
  C, M, D, F: string;
  E, AColor: Integer;
begin
  if (Count <= 0) then Exit;
  E := 0;
  C := ''; if (Count > 1) then C := ' (' + IntToStr(Count) + ')';
  if ((ID >= 20) and (ID <= 99)) then
  begin
    M := IntToStr(ID);
    case M[2] of
      '0'..'6' : if PC.EquipItems[1] = ID then E := 1;
           '7' : if PC.EquipItems[2] = ID then E := 2;
           '8' : if PC.EquipItems[3] = ID then E := 3;
           '9' : if PC.EquipItems[4] = ID then E := 4;
    end;
  end;
//  F := Trim(Copy(S, 1, Pos('[', S) - 1));
  F := Trim(S);
  D := ''; if DebugMode then D := ' ID: ' + IntToStr(ID);
  if (E = 0) then
  begin
    AColor := cLtGray;
    TextOut(0, Y, Trim(Chr(P + 95) + '.' + F + C + D), AColor);
    Inc(P);
    Inc(Y);
  end else begin
    AColor := cLtGray; 
    case E of
      1: begin
           TextOut(ScreenWidth div 2, U, 'Правая рука:', cRed);
           Inc(U);
         end;
      2: begin
           TextOut(ScreenWidth div 2, U, 'Левая рука:', cRed);
           Inc(U);
         end;
      3: begin
           TextOut(ScreenWidth div 2, U, 'Голова:', cRed);
           Inc(U);
         end;
      4: begin
           TextOut(ScreenWidth div 2, U, 'Торс:', cRed);
           Inc(U);
         end;
    end;
    TextOut(ScreenWidth div 2, U, Trim(Chr(P + 95) + '.' + F + C), AColor);
    Inc(U);
    Inc(P);
  end;
end;

procedure DrawItems();
var
  I: Integer;
begin
  for I := 1 to ItemsCount do if (PC.Items[I] > 0) then
    AddInvItem(Lang.Lang(I + 700), I, PC.Items[I]);
end;

procedure DrawAddItems();
var
  I, P, F: Byte;
begin
  P := 1;
  //
  if PC.IsBook then Inc(P);
  if PC.IsCraft then Inc(P);
  if PC.IsAlchemy then Inc(P);
  I := 18 - P;
  if (P > 1) then
  begin
    TitleOut(Lang.Lang(210), ScreenWidth div 2, I);
    //
    if PC.IsCraft then
    begin
      F := 0;
      if (PC.Life > 0) and (GameFrameScreen = gfNone) then
      begin
        F := 9;
        TextOut(ScreenWidth div 2, I + P, '[ctrl+c]', cLtYellow);
      end;
      TextOut(ScreenWidth div 2 + F, I + P, Lang.Lang(219), cRdYellow);
      Dec(P);
    end;
    //
    if PC.IsBook then
    begin
      F := 0;
      if (PC.Life > 0) and (GameFrameScreen = gfNone) then
      begin
        F := 9;
        TextOut(ScreenWidth div 2, I + P, '[ctrl+b]', cLtYellow);
      end;
      TextOut(ScreenWidth div 2 + F, I + P, Lang.Lang(208), cRdYellow);
      Dec(P);
    end;
    //
    if PC.IsAlchemy then
    begin
      F := 0;
      if (PC.Life > 0) and (GameFrameScreen = gfNone) then
      begin
        F := 9;
        TextOut(ScreenWidth div 2, I + P, '[ctrl+a]', cLtYellow);
      end;
      TextOut(ScreenWidth div 2 + F, I + P, Lang.Lang(200), cRdYellow);
      Dec(P);
    end;
  end;
end;

procedure DrawInv();
begin
  P := 2; U := 2; Y := 2;
  TitleOut(Lang.Lang(201));
  TitleOut(Lang.Lang(204), ScreenWidth div 2);
  DrawItems();
  DrawAddItems();
  BarOut('[esc]', Lang.Lang(40), true);
  if (PC.Life > 0) and (GameFrameScreen = gfNone) then
    if (InvCount > 0) then
      if (InvCount = 1) then
        BarOut('[a]', Lang.Lang(152), false)
          else BarOut('[a-' + Chr(96 + InvCount) + ']', Lang.Lang(152), false);
  BarOut('[space]', Lang.Lang(37));
  DrawMsg(True, 18, 19);
end;

procedure KeysInv(var Key: Word);
var
  I, C: Integer;
begin
  case Key of
    32:
    begin
      GameFrame := gfChar;
      Draw();
    end;
    27:
    begin
      if (GameFrameScreen = gfVictory) then
      begin
        GameFrame := gfVictory;
        Draw();
        Exit;
      end;
      if (PC.Life <= 0) then
      begin
        GameFrame := gfDefeat;
        Draw();
      end else
      begin
        ShowFloorObjects(PC.X, PC.Y, True);
        GameFrame := gfGame;
        Draw();
      end;
    end;
    ord('A')..ord('Z'):
    if (PC.Life > 0) and (GameFrameScreen = gfNone) then begin
      C := 0;
      for I := 1 to ItemsCount do if (PC.Items[I] > 0) then Inc(C);
      I := Key - (ord('A') - 1);
      if (I <= C) then
      begin
        ItemSelInvCount := GetSelItemCount(I);
        ItemSelInvID := GetSelItemID(I);
        GameFrame := gfItem;
        Draw();
      end;
    end;
  end;
end;

procedure KeysInv(var Key: Word; Shift: TShiftState);
begin
  if (GameFrameScreen = gfVictory) or (GameFrameScreen = gfDefeat) then Exit;
  if (ssCtrl in Shift) then
  case Key of
    ord('A'):
    if PC.IsAlchemy then
    begin
      GameFrame := gfAlchemy;
      Draw();
    end;
    ord('B'):
    if PC.IsBook then
    begin
      GameFrame := gfBook;
      Draw();
    end;
    ord('C'):
    if PC.IsCraft then
    begin
      GameFrame := gfCraft;
      Draw();
    end;
  end;
end;

function InvCount: Word;
var
  I: Byte;
begin
  Result := 0;
  for I := 1 to ItemsCount do if PC.Items[I] > 0 then Inc(Result);
end;

end.

