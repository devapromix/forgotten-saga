unit uMsg;

interface

uses uTypes;

var
  EvMsg: array [0..3] of string;
  InfoMsg: string = '';
  C: Byte = 0;

procedure DrawMsg(IsShortList: Boolean; Height, FSize: Integer);
procedure KeysMsg(var Key: Word);
procedure AddMsg(S: string);

implementation

uses Classes, SysUtils, uColor, uPC, uConst, uGraph, uGame, uDraw, uLang,
  uStringUtils;

var
  ML, SL: TStringList;
  U: Byte = 0;

procedure DrawMsg(IsShortList: Boolean; Height, FSize: Integer);
var
  I, C, P, L, AColor: Integer;
  S: string;
begin
  if IsShortList then
  begin
    if (PC.Life > 0) then
    with BG.Canvas do begin
      Font.Color := cRdYellow;
      S := Lang.Lang(PC.Z + 2000);
      if DebugMode then S := S + ' (' + IntToStr(PC.X) + ':' + IntToStr(PC.Y) + ':' + IntToStr(PC.Z) + ')';
      if (FSize >= 20) then TextOut((ScreenWidth - Length(S)) * CharWidth, 0, S);
      if InfoMsg <> '' then
      begin
        TextOut(0, 0, InfoMsg);
        InfoMsg := '';
      end;
    end;
    L := ScreenWidth div 2;
    P := 0;
    C := ML.Count;
    if (C > 0) then
    begin
      if (C > FSize) then P := C - FSize;
      for I := P to C - 1 do
      begin
        Inc(Height);
        if ((C - LLPos) > I) then AColor := cLtGray else AColor := cRdYellow;
        TextOut(L, Height + 1, ML[I], AColor);
      end;
    end;               
  end else begin
    TitleOut(Lang.Lang(202));
    BarOut('[esc]', Lang.Lang(40), true);
    FSize := ScreenHeight - 4;
    L := 0;
    P := 0;
    C := SL.Count;
    if (C > 0) then
    begin
      if (C > FSize) then P := C - FSize;
      for I := P to C - 1 do
      begin
        Inc(Height);
        TextOut(L, Height + 1, SL[I], cLtGray);
      end;
    end;
  end;
end;

procedure KeysMsg(var Key: Word);
begin
  case Key of
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
  end;
end;

procedure AddMsg(S: string);
var
  L, J: Integer;
  K, F: string;
label l1;
begin
  S := Trim(S);
  F := S;
  if (S <> '') then
  begin
    if (SL.Count > 0) and (LLPos <> 0) then
    begin
      K := Trim(SL[SL.Count - 1]);
      if (Length(K + ' ' + F) <= ScreenWidth) then
      begin
        SL[SL.Count - 1] := K + ' ' + F;
        goto l1;
      end;
    end;
    L := Length(F);
    if (L > ScreenWidth) then
    begin
      J := SU.LastPos(' ', Copy(F, 1, ScreenWidth));
      SL.Append(Copy(F, 1, J));
      Delete(F, 1, J);
    end;
    SL.Append(F);
    //
    l1:
    if (ML.Count > 0) and (LLPos <> 0) then
    begin
      K := Trim(ML[ML.Count - 1]);
      if (Length(K + ' ' + S) <= ScreenWidth div 2) then
      begin
        ML[ML.Count - 1] := K + ' ' + S;
        Exit;
      end;
    end;
    L := Length(S);
    if (L > ScreenWidth div 2) then
    begin
      J := SU.LastPos(' ', Copy(S, 1, ScreenWidth div 2));
      ML.Append(Copy(S, 1, J));
      Delete(S, 1, J);
      Inc(LLPos);
    end;
    ML.Append(S);
    Inc(LLPos);
  end;
end;

initialization
  ML := TStringList.Create;
  SL := TStringList.Create;

finalization
  FreeAndNil(ML);
  FreeAndNil(SL);

end.

