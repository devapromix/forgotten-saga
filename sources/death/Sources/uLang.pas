unit uLang;

interface

uses Classes, SysUtils;

type
  TLang = class(TObject)
  private
    FID: TStringList;
    FValue: TStringList;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Load(const FileName: string);
    function Lang(const ID: Integer): string; overload;
    function Lang(const ID: string): string; overload;
    procedure Clear();
  end;

var
  Lang: TLang;

implementation

uses uConst;

{ TLang }

procedure TLang.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

constructor TLang.Create;
begin
  FID := TStringList.Create;
  FValue := TStringList.Create;
end;

destructor TLang.Destroy;
begin
  FreeAndNil(FID);
  FreeAndNil(FValue);
  inherited;
end;

function TLang.Lang(const ID: string): string;
var
  I: Integer;
begin
  I := FID.IndexOf(Trim(ID));
  if (I < 0) then Result := '' else Result := FValue[I];
end;

function TLang.Lang(const ID: Integer): string;
begin
  Result := Lang(IntToStr(ID));
end; 

procedure TLang.Load(const FileName: string);
var
  A: TStringList;
  I, J: Integer;
  S: string;
begin
  A := TStringList.Create;
  try
    Self.Clear;
    A.LoadFromFile(FileName);
    for I := 0 to A.Count - 1 do
    begin
      S := Trim(A[I]);
      J := Pos('=', S);
      Self.FID.Append(Trim(Copy(S, 1, J - 1)));
      Self.FValue.Append(Trim(Copy(S, J + 1, Length(S))));
    end;
  finally
    FreeAndNil(A);
  end;
end;

initialization
  Lang := TLang.Create;

finalization
  FreeAndNil(Lang);

end.
