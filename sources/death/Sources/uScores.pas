unit uScores;

interface

uses Classes;

type
  TScores = class(TObject)
  private
    FScores: TStringList;
    FLevels: TStringList;
    FNames: TStringList;
    FFileName: string;
    FMaxCount: Integer;
    FLine: Integer;
    procedure SetFileName(const Value: string);
    procedure SetMaxCount(const Value: Integer);
    procedure SetLine(const Value: Integer);
  public
    property FileName: string read FFileName write SetFileName;
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property Line: Integer read FLine write SetLine;
    procedure Clear;
    function Count: Integer;
    procedure Add(const AScore: Integer; const ALevel: Integer; const AName: string);
    function GetScore(const I: Integer): string;
    function GetLevel(const I: Integer): string;
    function GetName(const I: Integer): string;
    procedure Save;
    procedure Load;
    constructor Create(const AFileName: string; const AMaxCount: Integer);
    destructor Destroy; override;
  end;

var
  Scores: TScores;

implementation

uses SysUtils, uUtils, uTypes, uStringUtils, uBox, uConst;

{ TScores }

procedure TScores.Add(const AScore: Integer; const ALevel: Integer; const AName: string);
var
  I: Integer;
  B: Boolean;
begin
  B := False;
  for I := 0 to FScores.Count - 1 do
  begin
    if (AScore >= StrToInt(FScores[I])) then
    begin
      Line := I;
      FScores.Insert(Line, IntToStr(AScore));
      FLevels.Insert(Line, IntToStr(ALevel));
      FNames.Insert(Line, AName);
      B := True;
      Save;
      Exit;
    end;
  end;
  if not B then
  begin
    FScores.Append(IntToStr(AScore));
    FLevels.Append(IntToStr(ALevel));
    FNames.Append(AName);
    Line := FScores.Count - 1;
    Save;
    Exit;
  end;
end;

procedure TScores.Clear;
begin
  FScores.Clear;
  FLevels.Clear;
  FNames .Clear;
end;

function TScores.Count: Integer;
begin
  Result := FScores.Count;
end;

constructor TScores.Create(const AFileName: string; const AMaxCount: Integer);
begin
  FScores  := TStringList.Create;
  FLevels  := TStringList.Create;
  FNames   := TStringList.Create;
  FileName := AFileName;
  MaxCount := AMaxCount;
  Line     := 0;
end;

destructor TScores.Destroy;
begin
  FreeAndNil(FScores);
  FreeAndNil(FLevels);
  FreeAndNil(FNames);
  inherited;
end;

function TScores.GetLevel(const I: Integer): string;
begin
  Result := FLevels[I];
end;

function TScores.GetName(const I: Integer): string;
begin
  Result := FNames[I];
end;

function TScores.GetScore(const I: Integer): string;
begin
  Result := FScores[I];
end;

procedure TScores.Load;
var
  I, C: Integer;
  A: TStringList;
  R: TExplodeResult;
  S: string;
begin
  A := TStringList.Create;
  try
    try
      Clear;
      //Zip[2].Password := Zip2ArchivePassword;
      //Zip[2].OpenArchive;
      //A.Text := Zip[2].ExtractToText(FileName);
      //Zip[2].OpenArchive;
      C := A.Count - 1;
      if C > MaxCount - 1 then C := MaxCount - 1;
      for I := 0 to C do
      begin
        R := SU.Explode(String(','), A[I]);
        FScores.Append(Trim(R[0]));
        FLevels.Append(Trim(R[1]));
        FNames.Append(Trim(R[2]));
      end;
    except

    end;
  finally
    FreeAndNil(A);
  end;
end;

procedure TScores.Save;
var
  I, C: Integer;
  A: TStringList;
begin     
  A := TStringList.Create;
  try
    C := Count - 1;
      if C > MaxCount - 1 then C := MaxCount - 1;
    for I := 0 to C do A.Append(GetScore(I) + ',' + GetLevel(I) + ',' + GetName(I));
    //Zip[2].Password := Zip2ArchivePassword;
    //Zip[2].OpenArchive;
    //Zip[2].AddFromString(FileName, A.Text);
    //Zip[2].OpenArchive;
  finally
    FreeAndNil(A);
  end;
end;

procedure TScores.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TScores.SetLine(const Value: Integer);
begin
  FLine := Value;
end;

procedure TScores.SetMaxCount(const Value: Integer);
begin
  FMaxCount := Value;
end;

initialization
  Scores := TScores.Create('scores', 35);

finalization
  FreeAndNil(Scores);

end.
