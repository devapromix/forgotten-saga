unit uDropList;

interface

uses Classes, SysUtils, uTypes;

type
  TDropList = class(TObject)
  private
    FCoords: TStringList;
    FDropItems: TStringList;
  public
    procedure Clear();
    procedure ClearCellItems(const X, Y: Word);
    procedure ClearCellItem(const X, Y, P: Word);
    procedure AddItem(const X, Y: Word; const ItemID: Integer; const ACount: Integer = 1);
    function GetCellItems(const X, Y: Word): string; overload;
    function GetCellItems(const I: Integer): string; overload;
    function GetCoords(const I: Integer): TPoint;
    function IsEmptyCell(const X, Y: Word): Boolean;
    function GetCellFirstItemID(const X, Y: Word): Integer;
    function Count(): Integer;
    function CellItemsCount(const X, Y: Word): Integer;
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses uStringUtils, uBox;

{ TDropList }

procedure TDropList.Clear;
begin
  FCoords.Clear;
  FDropItems.Clear;
end;

function TDropList.Count: Integer;
begin
  Result := FCoords.Count;
end;

constructor TDropList.Create;
begin
  FCoords := TStringList.Create;
  FDropItems := TStringList.Create;
end;

destructor TDropList.Destroy;
begin
  FreeAndNil(FCoords);
  FreeAndNil(FDropItems);
  inherited;
end;

procedure TDropList.AddItem(const X, Y: Word; const ItemID: Integer; const ACount: Integer = 1);
var
  I: Integer;
  Index, C: string;
begin
  C := '';
  Index := Format('%d:%d', [X, Y]);
  I := FCoords.IndexOf(Index);
  if (ACount > 1) then C := '-' + IntToStr(ACount);
  if (I < 0) then
  begin
    FCoords.Append(Index);
    FDropItems.Append(IntToStr(ItemID) + C);
  end else FDropItems[I] := FDropItems[I] + ',' + IntToStr(ItemID) + C;
end;

function TDropList.GetCellItems(const X, Y: Word): string;
var
  I: Integer;
  Index: string;
begin
  Result := '';
  Index := Format('%d:%d', [X, Y]);
  I := FCoords.IndexOf(Index);
  if (I >= 0) then Result := Trim(FDropItems[I]);
end;      

function TDropList.GetCoords(const I: Integer): TPoint;
begin
  Result.X := StrToInt(SU.Key(':', FCoords[I]));
  Result.Y := StrToInt(SU.Value(':', FCoords[I], ''));
end;

function TDropList.GetCellItems(const I: Integer): string;
begin
  Result := Trim(FDropItems[I]);
end;

function TDropList.IsEmptyCell(const X, Y: Word): Boolean;
var
  I: Integer;
begin
  Result := True;
  I := FCoords.IndexOf(Format('%d:%d', [X, Y]));
  if (I >= 0) then Result := False;
end;

procedure TDropList.ClearCellItem(const X, Y, P: Word);
var
  I, J: Integer;
  E: TExplodeResult;
  L, R: string;
begin
  I := FCoords.IndexOf(Format('%d:%d', [X, Y]));
  if (I >= 0) then
  begin
    R := '';
    E := SU.Explode(string(','), FDropItems[I]);
    for J := 0 to High(E) do
    begin
      if (J < High(E)) then L := ',' else L := '';
      if J <> P then R := R + E[J] + L;
    end;
    if (R <> '') and (R[Length(R)] = ',') then R[Length(R)] := ' ';
    FDropItems[I] := Trim(R);
    if (R = '') then
    begin
      FCoords.Delete(I);
      FDropItems.Delete(I);
    end;
  end;
end;

procedure TDropList.ClearCellItems(const X, Y: Word);
var
  I: Integer;
begin
  I := FCoords.IndexOf(Format('%d:%d', [X, Y]));
  if (I >= 0) then
  begin
    FCoords.Delete(I);
    FDropItems.Delete(I);
  end;
end;

function TDropList.GetCellFirstItemID(const X, Y: Word): Integer;
var
  I, C: Integer;
  E: TExplodeResult;
begin
  Result := 0;
  I := FCoords.IndexOf(Format('%d:%d', [X, Y]));
  C := Self.CellItemsCount(X, Y) - 1;
  if (I >= 0) then
  begin
    E := SU.Explode(string(','), FDropItems[I]);
    if (Pos('-', E[C]) > 0) then E[C] := Copy(E[C], 1, Pos('-', E[C]) - 1);
    Result := StrToInt(E[C]);
  end;
end;

function TDropList.CellItemsCount(const X, Y: Word): Integer;
var
  I: Integer;
  E: TExplodeResult;
begin
  Result := 0;
  I := FCoords.IndexOf(Format('%d:%d', [X, Y]));
  if (I >= 0) then
  begin
    E := SU.Explode(string(','), FDropItems[I]);
    Result := Length(E);
  end;
end;

end.
