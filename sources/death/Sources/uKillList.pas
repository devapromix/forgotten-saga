unit uKillList;

interface

uses Classes;
       
type
  TKillList = class(TObject)
  private
    FKillList: TStringList;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    function Count(): Integer;
    procedure Kill(const AName: string);
    function NameFromIndex(const Index: Integer): string;
    function CountFromIndex(const Index: Integer): string;
    class procedure AI();
  end;

var
  KillList: TKillList;

implementation

uses SysUtils, uConst;

{ TKillList }

procedure TKillList.Kill(const AName: string);
var
  S: string;
  I: Integer;
begin
  S := FKillList.Values[Trim(AName)];
  if (Trim(S) = '') then I := 0 else I := StrToInt(S);
  FKillList.Values[Trim(AName)] := IntToStr(I + 1);
end;

procedure TKillList.Clear;
begin
  FKillList.Clear;
end;

function TKillList.Count: Integer;
begin
  Result := FKillList.Count;
end;

function TKillList.CountFromIndex(const Index: Integer): string;
begin
  Result := FKillList.ValueFromIndex[Index];
end;

constructor TKillList.Create;
begin
  FKillList := TStringList.Create;
end;

destructor TKillList.Destroy;
begin
  FreeAndNil(FKillList);
  inherited;
end;

function TKillList.NameFromIndex(const Index: Integer): string;
begin
  Result := FKillList.Names[Index];
end;

class procedure TKillList.AI;
begin

end;

initialization
  KillList := TKillList.Create;

finalization
  FreeAndNil(KillList);

end.
