unit ForgottenSaga.Inv;

interface

uses Classes;

type
  TInvByte = 1 .. 26;

type
  TRecInv = record
    ID: string;
    Stack, Doll: Boolean;
    Count, Weight, Tough: Integer;
  end;

type
  TCustomInventory = class(TObject)
  private
    FItem: array [TInvByte] of TRecInv;
    function IndexOf(ID: string): Integer;
  public
    MaxCount: Integer;
    MaxWeight: Integer;
    constructor Create(AMaxCount, AMaxWeight: Integer);
    destructor Destroy; override;
    procedure Clear(AMaxCount, AMaxWeight: Integer);
    procedure Empty(I: TInvByte);
    function Add(ID: string; ACount, AWeight, ATough: Integer;
      AStack: Boolean): Boolean;
    function Del(ID: string; ACount: Integer): Boolean; overload;
    function Del(I, ACount: Integer): Boolean; overload;
    function Count: Integer;
    function Weight: Integer;
    function GetID(I: Integer): string;
    function GetCount(I: Integer): Integer;
    function GetTough(I: Integer): Integer;
    function GetDoll(I: Integer): Boolean;
    function GetStack(I: Integer): Boolean;
    function GetWeight(I: Integer): Integer;
    function GetItemWeight(I: Integer): Integer;
  end;

  TInventory = class(TCustomInventory)
  private
    FList: TStringList;
  public
    function ToText(I: TInvByte): string;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    function Equip(I: Integer): Boolean;
    function UnEquip(I: Integer): Boolean;
    constructor Create(AMaxCount, AMaxWeight: Integer);
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Common.Utils;

constructor TCustomInventory.Create(AMaxCount, AMaxWeight: Integer);
begin
  Clear(AMaxCount, AMaxWeight);
end;

destructor TCustomInventory.Destroy;
begin

end;

function TCustomInventory.GetID(I: Integer): string;
begin
  Result := FItem[I].ID;
end;

function TCustomInventory.GetCount(I: Integer): Integer;
begin
  Result := FItem[I].Count;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.GetWeight(I: Integer): Integer;
begin
  Result := FItem[I].Weight * FItem[I].Count;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.GetItemWeight(I: Integer): Integer;
begin
  Result := FItem[I].Weight;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.GetTough(I: Integer): Integer;
begin
  Result := FItem[I].Tough;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.GetDoll(I: Integer): Boolean;
begin
  Result := FItem[I].Doll;
end;

function TCustomInventory.GetStack(I: Integer): Boolean;
begin
  Result := FItem[I].Stack;
end;

procedure TCustomInventory.Clear(AMaxCount, AMaxWeight: Integer);
var
  I: TInvByte;
begin
  MaxCount := AMaxCount;
  MaxWeight := AMaxWeight;
  for I := Low(TInvByte) to High(TInvByte) do
    Empty(I);
end;

procedure TCustomInventory.Empty(I: TInvByte);
begin
  FItem[I].ID := '';
  FItem[I].Count := 0;
  FItem[I].Weight := 0;
  FItem[I].Tough := 0;
  FItem[I].Stack := False;
  FItem[I].Doll := False;
end;

function TCustomInventory.IndexOf(ID: string): Integer;
var
  I: TInvByte;
begin
  Result := 0;
  for I := Low(TInvByte) to High(TInvByte) do
    if (FItem[I].ID = ID) then
    begin
      Result := I;
      Break;
    end;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.Count: Integer;
var
  I: TInvByte;
  FCount: Integer;
begin
  FCount := 0;
  for I := Low(TInvByte) to High(TInvByte) do
    if (FItem[I].ID <> '') then
      FCount := FCount + 1;
  Result := FCount;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.Weight: Integer;
var
  I: TInvByte;
  FWeight: Integer;
begin
  FWeight := 0;
  for I := Low(TInvByte) to High(TInvByte) do
    if (FItem[I].ID <> '') then
      FWeight := FWeight + (FItem[I].Weight * FItem[I].Count);
  Result := FWeight;
  if (Result < 0) then
    Result := 0;
end;

function TCustomInventory.Add(ID: string; ACount, AWeight, ATough: Integer;
  AStack: Boolean): Boolean;
var
  I: TInvByte;
begin
  Result := False;
  if (ACount <= 0) then
    Exit;
  I := IndexOf(ID);
  if (I = 0) or not AStack then
  begin
    I := IndexOf('');
    if (I = 0) then
      Exit;
  end;
  if (Self.Weight >= MaxWeight) then
    Exit;
  if (FItem[I].Count = 0) and (Self.Count >= MaxCount) then
    Exit;
  FItem[I].ID := ID;
  FItem[I].Weight := AWeight;
  FItem[I].Stack := AStack;
  FItem[I].Tough := ATough;
  FItem[I].Count := FItem[I].Count + ACount;
  Result := True;
end;

function TCustomInventory.Del(I, ACount: Integer): Boolean;
var
  J: TInvByte;
begin
  Result := False;
  if (FItem[I].Count = 0) or (FItem[I].Count < ACount) then
    Exit;

  FItem[I].Count := FItem[I].Count - ACount;
  if (FItem[I].Count = 0) then
    Empty(I);
  Result := True;

  for J := I to Count do
  begin
    if (FItem[J].ID = '') then
    begin
      FItem[J] := FItem[J + 1];
      Empty(J + 1);
    end;
  end;
end;

function TCustomInventory.Del(ID: string; ACount: Integer): Boolean;
var
  I: TInvByte;
begin
  Result := False;
  I := IndexOf(ID);
  if (I = 0) then
    Exit;
  Del(I, ACount);
end;

{ TInventory }

constructor TInventory.Create(AMaxCount, AMaxWeight: Integer);
begin
  inherited Create(AMaxCount, AMaxWeight);
  FList := TStringList.Create;
end;

destructor TInventory.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TInventory.LoadFromFile(FileName: string);
var
  P, I: Integer;
  E: TStringList;
begin
  P := 1;
  Clear(MaxCount, MaxWeight);
  FList.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  for I := 0 to FList.Count - 1 do
  begin
    E := ExplodeString('/', FList[I]);
    if (Trim(E[0]) <> '') then
    begin
      FItem[P].ID := E[0];
      FItem[P].Count := StrToInt(E[1]);
      FItem[P].Weight := StrToInt(E[2]);
      FItem[P].Tough := StrToInt(E[3]);
      FItem[P].Stack := (FItem[P].Count > 1);
      FItem[P].Doll := StrToBool(E[4]);
    end;
    Inc(P);
  end;
end;

procedure TInventory.SaveToFile(FileName: string);
var
  I: TInvByte;
begin
  FList.Clear;
  for I := Low(TInvByte) to High(TInvByte) do
    with FItem[I] do
      FList.Append(Format('%s/%d/%d/%d/%s', [ID, Count, Weight, Tough,
        BoolToStr(Doll)]));
  FList.SaveToFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
end;

function TInventory.ToText(I: TInvByte): string;
begin
  with FItem[I] do
    Result := Format('%s/%d/%d/%d/%s', [ID, Count, Weight, Tough,
      BoolToStr(Doll)])
end;

function TInventory.Equip(I: Integer): Boolean;
begin
  Result := False;
  if not FItem[I].Stack and (FItem[I].Count = 1) then
  begin
    FItem[I].Doll := True;
    Result := True;
  end;
end;

function TInventory.UnEquip(I: Integer): Boolean;
begin
  Result := False;
  if FItem[I].Doll and not FItem[I].Stack and (FItem[I].Count = 1) then
  begin
    FItem[I].Doll := False;
    Result := True;
  end;
end;

end.
