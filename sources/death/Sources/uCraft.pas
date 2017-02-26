unit uCraft;

interface

procedure DrawCraft();
procedure KeysCraft(var Key: Word);

implementation

uses SysUtils, Classes, uGraph, uDraw, uLang, uConst, uTypes, uGame, uPC, uColor,
  uBox, uInv, uStringUtils, uSkills, uMsg, uUtils;

var
  P, U, Y, A: Integer;
  ResID: Integer = 0;
  R1, R2: TStringList;

procedure AddRecipes();
begin
  // Alchemy //
  // Эликсир из трав
  R1.Append('0,2,3,4=10');
  // Грибной шнапс
  R1.Append('0,7,8,9=13');

  // Craft //
  // Обычные стрелы
  R2.Append('0,0,100,102=18');
  // Обычные болты
  R2.Append('0,0,100,101=19');
end;

procedure AddCraftItem(const S: string; const ID, Count: Integer);
var
  C, D, F: string;
  E, AColor: Integer;
begin
  if (Count <= 0) then Exit;
  E := 0; C := ''; if (Count > 1) then C := ' (' + IntToStr(Count) + ')';
  if (PC.CraftIngItems[1] > 0) and (PC.CraftIngItems[1] = ID) then E := 1;
  if (PC.CraftIngItems[2] > 0) and (PC.CraftIngItems[2] = ID) then E := 2;
  if (PC.CraftIngItems[3] > 0) and (PC.CraftIngItems[3] = ID) then E := 3;
  if (PC.CraftIngItems[4] > 0) and (PC.CraftIngItems[4] = ID) then E := 4;
  F := Trim(S); D := ''; if DebugMode then D := ' ID: ' + IntToStr(ID);
  if (E = 0) then
  begin
    AColor := cGray;
    case GameFrame of
      gfAlchemy:
      case ID of
        2..9:
        AColor := cLtGray;
      end;
      gfCraft:
      case ID of
        100..127:
        AColor := cLtGray;
      end;
    end;
    if (ID = ResID) then AColor := cLtYellow;
    TextOut(0, Y, Trim(Chr(P + 95) + '.' + F + C + D), AColor);
    Inc(P);
    Inc(Y);
  end else begin
    AColor := cLtGray;
    TextOut(ScreenWidth div 2, U, Trim(Chr(P + 95) + '.' + F + C + D), AColor);
    Inc(U);
    Inc(P);
  end;
  Inc(A);
end;

procedure DrawCraftItems();
var
  I: Integer;
begin
  for I := 1 to ItemsCount do if (PC.Items[I] > 0) then
    AddCraftItem(Lang.Lang(I + 700), I, PC.Items[I]);
end;

procedure DrawCraft();
const
  V = '                   ';
var
  D, S, M: string;
  C, W, I, J, T: Integer;
begin
  W := ScreenWidth div 2;
  P := 2; U := 2; Y := 2; A := 0;
  TitleOut(Lang.Lang(201));
  case GameFrame of
    gfAlchemy : TitleOut(Lang.Lang(200), W);
    gfCraft   : TitleOut(Lang.Lang(219), W);
  end;
  DrawCraftItems();
  TitleOut(Lang.Lang(213), W, 7);
  if (ResID > 0) then
  begin
    S := Lang.Lang(ResID + 300);
    D := ''; if DebugMode then D := ' ID: ' + IntToStr(ResID);
    TextOut(W, 9, Lang.Lang(ResID + 700) + ' (' + IntToStr(PC.Items[ResID]) + ') ' + D, cLtYellow);
    if (Length(S) <= W) then TextOut(W, 10, S, cGray)
    else begin
      M := Copy(S, 1, W);
      for I := Length(M) downto 1 do if (M[I] = #32) then Break;
      TextOut(W, 10, Copy(S, 1, I), cGray);
      TextOut(W, 11, Copy(S, I + 1, Length(S)), cGray);
    end;
  end;
  //
  case GameFrame of
    gfAlchemy:
    begin
      TextOut(W, 14, Lang.Lang(21) + ': ' + Lang.Lang(1608) + ', ' + IntToStr(PC.Skills[9].Value), cLtGray);
      BG.Canvas.Brush.Color := cDkGray;
      TextOut(W + 20, 14, V, cDkGray);
      C := Round(PC.Skills[9].Exp * 19 / 100);
      S := ''; for J := C - 1 downto 0 do S := S + ' ';
      BG.Canvas.Brush.Color := cRdYellow;
      TextOut(W + 20, 14, S, cRdYellow);
      BG.Canvas.Brush.Color := cBlack;
    end;
    gfCraft:
    begin
      TextOut(W, 14, Lang.Lang(21) + ': ' + Lang.Lang(1610) + ', ' + IntToStr(PC.Skills[11].Value), cLtGray);
      BG.Canvas.Brush.Color := cDkGray;
      TextOut(W + 20, 14, V, cDkGray);
      C := Round(PC.Skills[11].Exp * 19 / 100);
      S := ''; for J := C - 1 downto 0 do S := S + ' ';
      BG.Canvas.Brush.Color := cRdYellow;
      TextOut(W + 20, 14, S, cRdYellow);
      BG.Canvas.Brush.Color := cBlack;
    end;
  end;
  //
  case GameFrame of
    gfCraft:
      TextOut(W, 16, Format(Lang.Lang(226), [PC.Skills[11].Value, PC.Skills[11].Value * 2]), cLtGray);
    gfAlchemy:
      TextOut(W, 16, Format(Lang.Lang(227), [PC.Skills[9].Value + 50]) + '%.', cLtGray);
  end;
  //
  BarOut('[esc]', Lang.Lang(40), true);
  if (PC.Life > 0) then
    if (A > 0) then
      if (A = 1) then
        BarOut('[a]', Lang.Lang(152), false)
          else BarOut('[a-' + Chr(96 + A) + ']', Lang.Lang(152), false);
  if ((PC.CraftIngItems[1] > 0) or (PC.CraftIngItems[2] > 0) or (PC.CraftIngItems[3] > 0)
    or (PC.CraftIngItems[4] > 0)) and (PC.Life > 0) then
      BarOut('[space]', Lang.Lang(155), false);
  if (ResID > 0) and (PC.Life > 0) then
    BarOut('[enter]', Lang.Lang(154), false);
  TitleOut(Lang.Lang(202), W, 18);
  DrawMsg(True, 18, 19);
end;

procedure DoCraft(I1, I2, I3, I4: Integer);
var
  I, L, m, mi, d: Integer;
  RR, FF: TExplodeResult;
  W: Array4;
begin
  ResID := 0; if (I1 = 0) and (I2 = 0) and (I3 = 0) and (I4 = 0) then Exit;
  // Sort
  W[0] := I1; W[1] := I2; W[2] := I3; W[3] := I4;
  Sort(W);
  I1 := W[0]; I2 := W[1]; I3 := W[2]; I4 := W[3];
  // Рецепты
  case GameFrame of
    gfAlchemy:
    for I := 0 to R1.Count - 1 do
    begin
      RR := SU.Explode(string('='), R1[I]);
      FF := SU.Explode(string(','), RR[0]);
      if (StrToInt(FF[0]) = I1)
        and (StrToInt(FF[1]) = I2)
          and (StrToInt(FF[2]) = I3)
            and (StrToInt(FF[3]) = I4) then
            begin
              ResID := StrToInt(RR[1]);
              Exit;
            end;
    end;
    gfCraft:
    for I := 0 to R2.Count - 1 do
    begin
      RR := SU.Explode(string('='), R2[I]);
      FF := SU.Explode(string(','), RR[0]);
      if (StrToInt(FF[0]) = I1)
        and (StrToInt(FF[1]) = I2)
          and (StrToInt(FF[2]) = I3)
            and (StrToInt(FF[3]) = I4) then
            begin
              ResID := StrToInt(RR[1]);
              Exit;
            end;
    end;
  end;
end;

procedure KeysCraft(var Key: Word);
var
 C, I: Integer;
begin
  case Key of
    27:
    begin
      for I := 1 to 4 do PC.CraftIngItems[I] := 0;
      ResID := 0;
      GameFrame := gfGame;
      ShowFloorObjects(PC.X, PC.Y, True);
      Draw();
    end;
    13:
    if (PC.Life > 0) and (ResID > 0) then
    begin
      case GameFrame of
        gfCraft:
        begin
          case ResID of
            // Стрелы и болты
            18..19:
            begin
              I := PC.Skills[11].Value;
              C := Rand(I, I * 2);
              Inc(PC.Items[ResID], C);
              AddMsg(Format(Lang.Lang(ResID + 235), [C, I * 2]));
            end;
            // Другое
            else Inc(PC.Items[ResID]);
          end;
          SkillUp(11, 15);
        end;
        gfAlchemy:
        begin
          if (Rand(0, 100) <= PC.Skills[9].Value + 50) then
          begin
            Inc(PC.Items[ResID]);
            AddMsg(Format(Lang.Lang(259), [Lang.Lang(ResID + 700)]));
            SkillUp(9, 15);
          end else AddMsg(Lang.Lang(228));
        end;
      end;
      //
      for I := 1 to 4 do
        if (PC.CraftIngItems[I] > 0) then
        begin
          Dec(PC.Items[PC.CraftIngItems[I]]);
          if (PC.Items[PC.CraftIngItems[I]] <= 0) then
            PC.CraftIngItems[I] := 0;
        end;           
      DoCraft(
        PC.CraftIngItems[1],
        PC.CraftIngItems[2],
        PC.CraftIngItems[3],
        PC.CraftIngItems[4]);
      Draw();
    end;
    32:
    if ((PC.CraftIngItems[1] > 0) or (PC.CraftIngItems[2] > 0) or (PC.CraftIngItems[3] > 0)
      or (PC.CraftIngItems[4] > 0)) and (PC.Life > 0) then
      begin
        for I := 1 to 4 do PC.CraftIngItems[I] := 0;
        ResID := 0;
        Draw();
      end;
    ord('A')..ord('Z'):
    if (PC.Life > 0) then
    begin
      I := Key - (ord('A') - 1);
      if (I <= A) then
      begin
        A := GetSelItemID(I);
        C := GetSelItemCount(I);
        if (C > 0) then
          for I := 1 to 4 do
          if (PC.CraftIngItems[I] = A) then
          begin
            PC.CraftIngItems[I] := 0;
            DoCraft(
              PC.CraftIngItems[1],
              PC.CraftIngItems[2],
              PC.CraftIngItems[3],
              PC.CraftIngItems[4]);
            Draw();
            Exit;
          end;
        //
        if (C > 0) then
          for I := 1 to 4 do
            if (PC.CraftIngItems[I] = 0) then
            begin
              PC.CraftIngItems[I] := A;
              Break;
            end;
        DoCraft(
          PC.CraftIngItems[1],
          PC.CraftIngItems[2],
          PC.CraftIngItems[3],
          PC.CraftIngItems[4]);
        Draw();
      end;
    end;
  end;
end;

initialization
  R1 := TStringList.Create;
  R2 := TStringList.Create;
  AddRecipes();

finalization
  FreeAndNil(R1);
  FreeAndNil(R2);

end.
