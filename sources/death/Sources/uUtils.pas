unit uUtils;

interface

uses uTypes;

function Rand(A, B: Integer): Integer;
function GetDist(x1, y1, x2, y2: single): word;
function Percent(N, P: Integer): Integer;
procedure Sort(var M: Array4);

implementation

uses Windows, SysUtils;

function Rand(A, B: Integer): Integer;
begin
  Result := Round(Random(B - A + 1) + A);
end;

function GetDist(x1, y1, x2, y2: single): word;
begin
  result := round(sqrt(sqr(x2 - x1) + sqr(y2 - y1)));
end;

function Percent(N, P: Integer): Integer;
begin
  Result := N * P div 100
end;

procedure Sort(var M: Array4);

procedure Min(I: Integer; var NMin: Integer);
var
  J: Integer;
begin
  NMin := I;
  for J := I + 1 to 3 do
  if M[J] < M[NMin] then NMin := J;
end;

var
  I, NMin, Buf: Integer;
begin
  for I := 0 to 2 do
  begin
    Min(I, NMin);
    Buf := M[I];
    M[I] := M[NMin];
    M[NMin] := Buf;
  end;
end;

end.
