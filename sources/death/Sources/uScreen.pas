unit uScreen;

interface

uses
  Classes, SysUtils, StrUtils;

procedure DrawFrame();
procedure LoadFrame(const FileName: string);

implementation

uses uMain, uColor, uPC, uConst, uGraph;

const cDefaultColor = cLtGray;

function CharColor(CharString: string): Integer;
begin
  Result := cDefaultColor;
  if (CharString = 'Y') then Result := cLtYellow;
  if (CharString = 'y') then Result := cRdYellow;
  if (CharString = '>') then Result := cDefaultColor;
end;

var
  F: TStringList;

procedure DrawFrame();
var
  X, Y, Z, K: Integer;
  C: string;
begin
  BG.Canvas.Font.Color := cDefaultColor;
  for Y := 0 to F.Count - 1 do
  begin
    Z := Length(F[Y]); K := 1;
    for X := 0 to Z do
    begin
      C := Copy(F[Y], X + K, 1);
      if (C = '^') then
      begin
        Inc(K); C := Copy(F[Y], X + K, 1);
        BG.Canvas.Font.Color := CharColor(C);
        Inc(K); C := Copy(F[Y], X + K, 1);
      end;
      BG.Canvas.TextOut(X * CharWidth, Y * CharHeight, C);
    end;
  end;
end;

procedure LoadFrame(const FileName: string);
//var
  //S: string;
begin
  try
    //Zip[1].OpenArchive;
    //Zip[1].ExtractToString(FileName, S);
    //F.Text := S;
    //Zip[1].CloseArchive;
  except
    //ErrorInZip(FileName);
  end;
end;

initialization
  F := TStringList.Create;

finalization
  F.Free;

end.


