unit ForgottenSaga.Script;

interface

uses Classes;

type
  TVars = class(TObject)
  private
    FID: TStringList;
    FValue: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    procedure Empty(const AVar: string);
    function Has(const AVar: string): Boolean;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    function GetStr(const AVar: string): string;
    procedure SetStr(const AVar, AValue: string);
    function GetInt(const AVar: string): Integer;
    procedure SetInt(const AVar: string; const AValue: Integer);
  end;

type
  TScript = class(TObject)
  private
    FIsNext: Boolean;
    FIsIf: Boolean;
    FList: TStringList;
    FCloseTag: string;
    FVars: TVars;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    function GetSource(const ID: string): string;
    procedure Exec(const ID: string);
    procedure Next(const ID: string);
    procedure Run(const Code: string);
    procedure RunCode(const Code: string);
    property CloseTag: string read FCloseTag write FCloseTag;
    property Vars: TVars read FVars write FVars;
  end;

implementation

uses SysUtils, Engine, ForgottenSaga.Game, ForgottenSaga.Scenes, Common.Utils,
  ForgottenSaga.Creature;

constructor TScript.Create;
begin
  CloseTag := 'close';
  FList := TStringList.Create;
  FVars := TVars.Create;
  FIsNext := False;
  FIsIf := False;
end;

destructor TScript.Destroy;
begin
  FVars.Free;
  FList.Free;
end;

procedure TScript.Clear;
begin
  FVars.Clear;
  FList.Clear;
end;

procedure TScript.LoadFromFile(const FileName: string);
var
  I: Integer;
  S: string;
begin
  FList.LoadFromFile(FileName, TEncoding.UTF8);
  for I := FList.Count - 1 downto 0 do
  begin
    S := Trim(FList[I]);
    if (S = '') or (S[1] = ';') then
    begin
      FList.Delete(I);
      Continue;
    end;
    if (Pos(';', S) > 0) then
      Delete(S, Pos(';', S), Saga.Engine.GetTextLength(S));
    FList[I] := S;
  end;
  // ShowMessage(SL.Text);
end;

function TScript.GetSource(const ID: string): string;
var
  I: Integer;
  Flag: Boolean;
begin
  Flag := False;
  Result := '';
  for I := 0 to FList.Count - 1 do
  begin
    if (FList[I] = ':' + ID) then
    begin
      Flag := True;
      Continue;
    end;
    if Flag then
    begin
      if (FList[I] = 'end') then
        Break;
      Result := Result + FList[I] + #13#10;
    end;
  end;
  // ShowMessage(Result);
end;

procedure TScript.Next(const ID: string);
var
  Link, Code: string;
  P: Integer;
begin
  Code := '';
  Link := Trim(ID);
  P := Pos('(', Link);
  if (P > 0) then
  begin
    Code := Trim(Copy(Link, P + 1, Saga.Engine.GetTextLength(Link) - P - 1));
    Link := Trim(Copy(Link, 1, P - 1));
  end;
  if (Code <> '') then
    Self.RunCode(Code);
  if (Link = CloseTag) then
  begin
    Saga.Stages.SetStage(stGame);
    Exit;
  end;
  FIsNext := False;
  Exec(Link);
end;

procedure TScript.Exec(const ID: string);
var
  L: TStringList;
  I: Integer;
begin
  L := TStringList.Create;
  L.Text := Self.GetSource(ID);
  for I := 0 to L.Count - 1 do
    if FIsNext then
      Continue
    else
      Self.Run(L[I]);
  L.Free;
end;

procedure TScript.RunCode(const Code: string);
var
  I: Integer;
  SL: TStringList;
begin
  SL := ExplodeString('&', Code);
  for I := 0 to SL.Count - 1 do
    if FIsNext then
      Continue
    else
      Self.Run(Trim(SL[I]));
end;

procedure TScript.Run(const Code: string);
var
  S, L: string;
  I, J, K, E: Integer;
  SL: TStringList;

  function GetLastCode(Tag: string; Code: string): string;
  begin
    Result := Trim(Copy(Code, Saga.Engine.GetTextLength(Tag) + 2,
      Saga.Engine.GetTextLength(Code)));
  end;

  procedure SetNext(Flag: Boolean);
  begin
    Exec(GetLastCode('goto', Code));
    FIsNext := Flag;
  end;

  function IsTag(Tag: string; ACode: string = ''): Boolean;
  var
    R: string;
  begin
    ACode := Trim(ACode);
    if (ACode = '') then
      R := Copy(Code, 1, Saga.Engine.GetTextLength(Tag))
    else
      R := ACode;
    Result := R = Tag;
  end;

  function GetIf(K: System.Char; S: string): Boolean;
  var
    N: string;
    V, A: Integer;
  begin
    Result := False;
    N := AnsiLowerCase(Trim(Copy(S, 1, Pos(K, S) - 1)));
    Val(Trim(Copy(S, Pos(K, S) + 1, Saga.Engine.GetTextLength(S))), V, A);
    if (Vars.Has(N)) then
      A := Vars.GetInt(N)
    else
      A := 0;
    case K of
      '=':
        Result := not(A = V);
      '>':
        Result := not(A > V);
      '<':
        Result := not(A < V);
    end;
  end;

begin
  if IsTag('endif') then
    FIsIf := False;

  if FIsIf then
    Exit;

  if IsTag('pln') then
    Saga.Log[lgDialog].Add(GetLastCode('pln', Code));

  if IsTag('log') then
    Saga.Log[lgGame].Add(GetLastCode('log', Code));

  if IsTag('box') then
    Box(GetLastCode('box', Code));

  if IsTag('btn') then
  begin
    S := GetLastCode('btn', Code);
    L := Trim(Copy(S, 1, Pos(',', S) - 1));
    Delete(S, 1, Pos(',', S));
    S := Trim(S);
    TStageDialog(Saga.Stages.GetStage(stDialog)).LinkList.Append(S, L);
  end;

  if IsTag('qlog') then
  begin
    S := GetLastCode('qlog', Code);
    L := Trim(Copy(S, 1, Pos(':', S) - 1));
    Delete(S, 1, Pos(':', S));
    Val(L, I, E);
    if IsTag('begin', S) then
    begin
      Saga.Log[lgGame].Add(__('The new quest is added to the log.'));
      Exit;
    end;
    if IsTag(CloseTag, S) then
    begin
      Saga.Log[lgGame].Add(__('You have completed the quest.'));
      Saga.Quest.Add(I - 1, __('I have completed this quest.'));
      Exit;
    end;
    Saga.Quest.Add(I - 1, S);
  end;

  if (Pos('=', Code) > 0) and not IsTag('if') and not IsTag('btn') then
  begin
    S := AnsiLowerCase(Trim(Copy(Code, 1, Pos('=', Code) - 1)));
    L := Trim(Copy(Code, Pos('=', Code) + 1, Saga.Engine.GetTextLength(Code)));
    Vars.SetStr(S, L);
    // Box(S + '=>' + L);
  end;

  if IsTag('if') then
  begin
    S := Trim(Copy(Code, 4, Pos('then', Code) - 4));
    if (Pos('=', S) > 0) then
      FIsIf := GetIf('=', S)
    else if (Pos('>', S) > 0) then
      FIsIf := GetIf('>', S)
    else if (Pos('<', S) > 0) then
      FIsIf := GetIf('<', S);
  end;

  // Карта:Объект:Диалог
  if IsTag('dialog') then
  begin
    S := GetLastCode('dialog', Code);
    SL := ExplodeString(':', Code);
    I := StrToIntDef(SL[0], 0);
    J := StrToIntDef(SL[1], 0);
    K := StrToIntDef(SL[2], 0);
    Saga.World.GetMapCreatures(I).Get(J).Dialog := K;
  end;

  if IsTag('heal') then
  begin
    Saga.Player.Atr[atLife].SetToMax;
  end;

  if IsTag('exp') then
  begin
    S := GetLastCode('exp', Code);
    Saga.Player.AddExp(StrToInt(S));
  end;

  if IsTag('goto') then
    SetNext(True);

  if IsTag('proc') then
    SetNext(False);

  if IsTag('exit') then
    FIsNext := True;

  if IsTag(CloseTag) then
    Saga.Stages.SetStage(stGame);
end;

{ TVars }

procedure TVars.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

function TVars.Count: Integer;
begin
  Result := FID.Count;
end;

constructor TVars.Create;
begin
  FID := TStringList.Create;
  FValue := TStringList.Create;
end;

destructor TVars.Destroy;
begin
  FID.Free;
  FValue.Free;
  inherited;
end;

procedure TVars.Empty(const AVar: string);
var
  I: Integer;
begin
  I := FID.IndexOf(AVar);
  if (I < 0) then
    Exit;
  FID.Delete(I);
  FValue.Delete(I);
end;

function TVars.GetStr(const AVar: string): string;
var
  I: Integer;
begin
  I := FID.IndexOf(AVar);
  if I < 0 then
    Result := ''
  else
    Result := FValue[I];
end;

procedure TVars.SetStr(const AVar, AValue: string);
var
  I: Integer;
begin
  I := FID.IndexOf(AVar);
  if (I < 0) then
  begin
    FID.Append(AVar);
    FValue.Append(AValue);
  end
  else
    FValue[I] := AValue;
end;

function TVars.GetInt(const AVar: string): Integer;
var
  S: string;
  E: Integer;
begin
  S := Trim(GetStr(AVar));
  if S = '' then
    Result := 0
  else
    Val(S, Result, E);
end;

procedure TVars.SetInt(const AVar: string; const AValue: Integer);
begin
  SetStr(AVar, Format('%d', [AValue]));
end;

function TVars.Has(const AVar: string): Boolean;
begin
  Result := FID.IndexOf(AVar) > -1;
end;

procedure TVars.LoadFromFile(const FileName: string);
var
  A: TStringList;
  I, J: Integer;
  S: string;
begin
  A := TStringList.Create;
  try
    Self.Clear;
    A.LoadFromFile(FileName, TEncoding.UTF8);
    for I := 0 to A.Count - 1 do
    begin
      S := Trim(A[I]);
      J := Pos(',', S);
      Self.FID.Append(Trim(Copy(S, 1, J - 1)));
      Self.FValue.Append(Trim(Copy(S, J + 1, Saga.Engine.GetTextLength(S))));
    end;
  finally
    A.Free;
  end;
end;

procedure TVars.SaveToFile(const FileName: string);
var
  I: Integer;
  S: TStringList;
begin
  S := TStringList.Create;
  for I := 0 to FID.Count - 1 do
    S.Append(FID[I] + ',' + FValue[I]);
  S.SaveToFile(FileName, TEncoding.UTF8);
  S.Free;
end;

end.
