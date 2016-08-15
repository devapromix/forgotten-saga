unit ForgottenSaga.Creature;

interface

uses Types, Classes, IniFiles;

type
  TBaseEntity = class(TObject)
  private
    FPos: TPoint;
  public
    procedure SetPosition(const APos: TPoint); overload;
    procedure SetPosition(const X, Y: Integer); overload;
    property Pos: TPoint read FPos;
    function Has(const APos: TPoint): Boolean; overload;
    function Has(const X, Y: Integer): Boolean; overload;
  end;

type
  TEntity = class(TBaseEntity)
  private
    FName: string;
  public
    constructor Create();
    destructor Destroy; override;
    property Name: string read FName write FName;
  end;

type
  TAdvEntity = class(TEntity)
  private
    FColor: Integer;
    FSymbol: Char;
    FLevel: Byte;
    FActive: Boolean;
  public
    constructor Create();
    destructor Destroy; override;
    property Color: Integer read FColor write FColor;
    property Symbol: Char read FSymbol write FSymbol;
    property Level: Byte read FLevel write FLevel;
    property Active: Boolean read FActive write FActive;
  end;

type
  TBar = class(TObject)
  private
    FCur: Integer;
    FMax: Integer;
    FAdv: Integer;
    function GetCur: Integer;
    function GetMax: Integer;
    procedure SetAdv(const Value: Integer);
    function GetAdv: Integer;
    procedure SetMax(Value: Integer);
    procedure SetCur(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function IsMin: Boolean;
    function IsMax: Boolean;
    procedure Dec(Value: Integer = 1);
    procedure Inc(Value: Integer = 1);
    property Cur: Integer read GetCur write SetCur;
    property Max: Integer read GetMax write SetMax;
    property Adv: Integer read GetAdv write SetAdv;
    procedure Add(Values: string);
    function ToString: string;
    procedure SetToMin;
    procedure SetToMax;
  end;

type
  TAtrEnum = (atExp, atLife, atMana, atAdr);

const
  AtrStr: array [TAtrEnum] of string = ('Exp', 'Life', 'Mana', 'Adrenalin');

type
  TCustomCreature = class(TAdvEntity)
  private
    FAtr: array [TAtrEnum] of TBar;
    FSkillPoints: Byte;
    FStatPoints: Byte;
    procedure SetSkillPoints(const Value: Byte);
    procedure SetStatPoints(const Value: Byte);
  protected
    function GetAtr(I: TAtrEnum): TBar;
    procedure SetAtr(I: TAtrEnum; const Value: TBar);
  public
    constructor Create(MaxLife, MaxMana: Integer);
    destructor Destroy; override;
    property Atr[I: TAtrEnum]: TBar read GetAtr write SetAtr;
    property StatPoints: Byte read FStatPoints write SetStatPoints;
    property SkillPoints: Byte read FSkillPoints write SetSkillPoints;
    procedure AddAtr(Atrib: TAtrEnum; Max: Integer; IsToMax: Boolean);
    procedure Fill;
  end;

type
  TForce = (fcAlly, fcEnemy);

type
  TCreature = class(TCustomCreature)
  private
    FForce: TForce;
    FFileName: string;
    FDialog: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Force: TForce read FForce write FForce;
    property Dialog: Integer read FDialog write FDialog;
    property FileName: string read FFileName write FFileName;
    procedure Move(AX, AY: ShortInt);
    procedure Render;
  end;

type
  TCategory = (ctNone, ctStaff, ctSpear, ctAxe, ctSword);

const
  CatStr: array [TCategory] of string = ('', 'staff', 'spear', 'axe', 'sword');

type
  TMaterial = (mtNone, mtWood, mtBone, mtStone, mtMetal);

const
  MatStr: array [TMaterial] of string = ('', 'wood', 'bone', 'stone', 'metal');

type
  TItem = class(TAdvEntity)
  private
    FCount: Integer;
    FCategory: TCategory;
    FMaterial: TMaterial;
    FDurability: TBar;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
    procedure Assign(Value: TItem);
    procedure Calc;
    property Count: Integer read FCount write FCount;
    property Category: TCategory read FCategory write FCategory;
    property Material: TMaterial read FMaterial write FMaterial;
    property Durability: TBar read FDurability write FDurability;
  end;

type
  TAdvIniFile = class(TIniFile)
  public
    function ReadCategory(Section, Ident: string; DefaultValue: TCategory)
      : TCategory;
    procedure WriteCategory(Section, Ident: string; Value: TCategory);
    function ReadMaterial(Section, Ident: string; DefaultValue: TMaterial)
      : TMaterial;
    procedure WriteMaterial(Section, Ident: string; Value: TMaterial);
  end;

type
  TLook = class(TBaseEntity)
  private
    FEnabled: Boolean;
  public
    procedure Render;
    procedure Move(AX, AY: Integer);
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

type
  TInvByte = 1 .. 26;

type
  TInventor = class(TObject)
  private
    FItem: array [TInvByte] of TItem;
    function GetItem(I: TInvByte): TItem;
    procedure SetItem(I: TInvByte; const Value: TItem);
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Item[I: TInvByte]: TItem read GetItem write SetItem;
    function AddItem(Value: TItem): Boolean;
    procedure Clear(I: TInvByte); overload;
    procedure Clear; overload;
  end;

type
  TPlayer = class(TCreature)
  private
    FMap: Byte;
    FRace: Byte;
    FLook: TLook;
    FInventory: TInventor;
    FScore: Word;
    FPrevName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Score: Word read FScore write FScore;
    property Race: Byte read FRace write FRace;
    property Map: Byte read FMap write FMap;
    function GetRaceName: string;
    function GetFullName: string;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure Move(AX, AY: ShortInt);
    property Look: TLook read FLook write FLook;
    property Inventory: TInventor read FInventory write FInventory;
    procedure GenName;
    procedure Clear;
    procedure Defeat;
    procedure Pickup;
    procedure Victory;
    procedure AddExp(A: Word);
  end;

type
  TCreatures = class(TObject)
  private
    FCreature: array of TCreature;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ID: Integer);
    function Get(N: Integer): TCreature;
    function Has(X, Y: Integer): Integer;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    function Count: Integer;
    procedure Render;
    procedure Clear;
  end;

type
  TItems = class(TObject)
  private
    FItem: array of TItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Symbol: Char; Color, Level: Integer; Name: string;
      Material: TMaterial; Category: TCategory; Durability: Word;
      Amount: Word = 1);
    procedure Del(const Index: Integer);
    function Get(N: Integer): TItem;
    function Has(X, Y: Integer): Integer;
    function GetIndex(N, X, Y: Integer): Integer;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    function Count: Integer; overload;
    function Count(X, Y: Integer): Integer; overload;
    function GetItemPropStr(Item: TItem): string;
    procedure Pickup(I: Integer);
    procedure Render;
    procedure Clear;
  end;

const
  BarFmt = '%d/%d';
  InfFmt = '%s %s';
  KeyFmt = '<%s> %s';

implementation

uses SysUtils, Math, Engine, ForgottenSaga.Game, Common.Map.Tiles, Common.Color,
  Common.Utils, ForgottenSaga.Scenes, Common.Variables;

const
  RaceNameDiv: array [TRaceEnum] of string = ('', '-', ' ');
  cAdr = 75;
  cExp = 15;

  { TBaseEntity }

function TBaseEntity.Has(const APos: TPoint): Boolean;
begin
  Result := (FPos.X = APos.X) and (FPos.Y = APos.Y)
end;

function TBaseEntity.Has(const X, Y: Integer): Boolean;
begin
  Result := (FPos.X = X) and (FPos.Y = Y)
end;

procedure TBaseEntity.SetPosition(const X, Y: Integer);
begin
  FPos := Point(X, Y)
end;

procedure TBaseEntity.SetPosition(const APos: TPoint);
begin
  FPos := APos
end;

{ TEntity }

constructor TEntity.Create;
begin
  inherited Create;
  Name := '';
end;

destructor TEntity.Destroy;
begin

  inherited;
end;

{ TAdvEntity }

constructor TAdvEntity.Create;
begin
  inherited Create;
  Active := True;
  Color := $00FFFFFF;
  Symbol := '?';
  Level := 1;
end;

destructor TAdvEntity.Destroy;
begin

  inherited;
end;

{ TBar }

procedure TBar.Add(Values: string);
var
  SL: TStringList;
begin
  SL := ExplodeString('/', Values);
  FCur := StrToIntDef(Trim(SL[0]), 0);
  FMax := StrToIntDef(Trim(SL[1]), 0);
end;

constructor TBar.Create;
begin
  Adv := 0;
end;

procedure TBar.Dec(Value: Integer);
begin
  if ((Cur > 0) and (Value > 0)) then
    SetCur(GetCur - Value);
  if (Cur < 0) then
    Cur := 0;
end;

destructor TBar.Destroy;
begin

  inherited;
end;

function TBar.GetAdv: Integer;
begin
  Result := FAdv;
end;

function TBar.GetCur: Integer;
begin
  Result := FCur;
end;

function TBar.GetMax: Integer;
begin
  Result := FMax;
end;

procedure TBar.Inc(Value: Integer);
begin
  if ((Cur < Max) and (Value > 0)) then
    SetCur(GetCur + Value);
  if (Cur > Max) then
    Cur := Max;
end;

function TBar.IsMax: Boolean;
begin
  Result := (Cur >= Max);
end;

function TBar.IsMin: Boolean;
begin
  Result := (Cur = 0);
end;

procedure TBar.SetAdv(const Value: Integer);
begin
  FAdv := Value;
end;

procedure TBar.SetCur(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  if (Value > Max) then
    Value := Max;
  FCur := Value;
end;

procedure TBar.SetMax(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  FMax := Value + Adv;
  if (Cur >= Max) then
    SetToMax;
end;

procedure TBar.SetToMax;
begin
  Cur := Max;
end;

procedure TBar.SetToMin;
begin
  Cur := 0;
end;

function TBar.ToString: string;
begin
  Result := Format(BarFmt, [Cur, Max]);
end;

{ TCustomCreature }

constructor TCustomCreature.Create(MaxLife, MaxMana: Integer);
var
  I: TAtrEnum;
begin
  inherited Create;
  StatPoints := 0;
  SkillPoints := 0;
  for I := Low(TAtrEnum) to High(TAtrEnum) do
  begin
    FAtr[I] := TBar.Create;
    case I of
      atExp:
        AddAtr(I, cExp, False);
      atAdr:
        AddAtr(I, cAdr, False);
      atLife:
        AddAtr(I, MaxLife, True);
      atMana:
        AddAtr(I, MaxMana, True);
    end;
  end;
end;

destructor TCustomCreature.Destroy;
var
  I: TAtrEnum;
begin
  for I := Low(TAtrEnum) to High(TAtrEnum) do
    FAtr[I].Free;
  inherited;
end;

procedure TCustomCreature.AddAtr(Atrib: TAtrEnum; Max: Integer;
  IsToMax: Boolean);
begin
  Atr[Atrib].Max := Max;
  if IsToMax then
    Atr[Atrib].SetToMax
  else
    Atr[Atrib].SetToMin;
end;

procedure TCustomCreature.Fill;
begin
  Atr[atLife].SetToMax;
  Atr[atMana].SetToMax;
end;

procedure TCustomCreature.SetSkillPoints(const Value: Byte);
begin
  FSkillPoints := Value;
end;

procedure TCustomCreature.SetStatPoints(const Value: Byte);
begin
  FStatPoints := Value;
end;

function TCustomCreature.GetAtr(I: TAtrEnum): TBar;
begin
  Result := FAtr[I];
end;

procedure TCustomCreature.SetAtr(I: TAtrEnum; const Value: TBar);
begin
  FAtr[I] := Value;
end;

{ TCreature }

constructor TCreature.Create;
begin
  inherited Create(100, 100);
  Color := clCreature;
  Symbol := '?';
  Force := fcEnemy;
  Dialog := 0;
  FileName := '';
end;

destructor TCreature.Destroy;
begin

  inherited;
end;

procedure TCreature.Move(AX, AY: ShortInt);
begin
  if (Saga.World.CurrentMap.CellInMap(Pos.X + AX, Pos.Y + AY)) and
    (Saga.Tiles.GetTile(Saga.World.CurrentMap.GetTile(Pos.X + AX, Pos.Y + AY,
    lrTerrain)).Passable) then
    SetPosition(Pos.X + AX, Pos.Y + AY);
end;

procedure TCreature.Render;
begin
  case Force of
    fcEnemy:
      Saga.Engine.FontBackColor($000022);
    fcAlly:
      Saga.Engine.FontBackColor($002200);
  end;
  Saga.Engine.CharOut(Pos.X, Pos.Y, Symbol, Color);
end;

{ TCreatures }

procedure TCreatures.Add(const ID: Integer);
begin

end;

procedure TCreatures.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FCreature[I].Free;
  SetLength(FCreature, 0);
end;

function TCreatures.Count: Integer;
begin
  Result := Length(FCreature);
end;

constructor TCreatures.Create;
begin

end;

destructor TCreatures.Destroy;
begin
  Self.Clear;
  inherited;
end;

function TCreatures.Get(N: Integer): TCreature;
begin
  Result := FCreature[N];
end;

function TCreatures.Has(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if (FCreature[I].Has(X, Y)) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TCreatures.LoadFromFile(FileName: string);
var
  I, L: Integer;
  F: TIniFile;
  B: Boolean;
  S: string;
begin
  Self.Clear;
  F := TIniFile.Create(FileName);
  try
    for I := 0 to 99 do
    begin
      S := Format('%d', [I]);
      if (F.SectionExists(S)) then
      begin
        L := Count;
        SetLength(FCreature, L + 1);
        FCreature[L] := TCreature.Create;
        FCreature[L].Name := F.ReadString(S, 'Name', '');
        FCreature[L].SetPosition(Point(F.ReadInteger(S, 'X', 0),
          F.ReadInteger(S, 'Y', 0)));
        FCreature[I].Atr[atLife].Add(F.ReadString(S, 'Life',
          Format(BarFmt, [100, 100])));
        FCreature[L].Symbol := F.ReadString(S, 'Symbol', '?')[1];
        FCreature[L].Color := F.ReadInteger(S, 'Color', clCreature);
        FCreature[L].Dialog := F.ReadInteger(S, 'Dialog', 0);
        FCreature[L].Level := F.ReadInteger(S, 'Level', 1);
        FCreature[L].FileName := F.ReadString(S, 'File', '');
        B := F.ReadBool(S, 'NPC', False);
        if B then
          FCreature[L].Force := fcAlly
        else
          FCreature[L].Force := fcEnemy;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCreatures.Render;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FCreature[I].Render;
  Saga.Player.Render;
  Saga.Player.Look.Render;
end;

procedure TCreatures.SaveToFile(FileName: string);
var
  I: Integer;
  S: string;
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try
    for I := 0 to Count - 1 do
    begin
      S := Format('%d', [I]);
      F.WriteString(S, 'Name', FCreature[I].Name);
      F.WriteInteger(S, 'X', FCreature[I].Pos.X);
      F.WriteInteger(S, 'Y', FCreature[I].Pos.Y);
      F.WriteString(S, 'Life', Format(BarFmt, [FCreature[I].Atr[atLife].Cur,
        FCreature[I].Atr[atLife].Max]));
      F.WriteInteger(S, 'Level', FCreature[I].Level);
      F.WriteString(S, 'Symbol', FCreature[I].Symbol);
      F.WriteString(S, 'File', FCreature[I].FileName);
      F.WriteInteger(S, 'Color', FCreature[I].Color);
      F.WriteInteger(S, 'Dialog', FCreature[I].Dialog);
      F.WriteBool(S, 'NPC', FCreature[I].Force = fcAlly);
    end;
  finally
    F.Free;
  end;
end;

{ TPlayer }

procedure TPlayer.AddExp(A: Word);
begin
  if (Atr[atExp].Cur + A >= Atr[atExp].Max) then
  begin
    Atr[atExp].SetCur(Atr[atExp].Cur + A - Atr[atExp].Max);
    Score := Score + (Level * 10);
    Level := Level + 1;
    Atr[atExp].SetMax(Atr[atExp].Max + Percent(Atr[atExp].Max, 20));
  end
  else
    Atr[atExp].Inc(A);
end;

procedure TPlayer.Clear;
var
  R: TRaceEnum;
begin
  R := TRaceEnum(Race);
  Color := Saga.Race[R].Color;
  SetPosition(Saga.Race[R].Pos);
  Map := Saga.Race[R].Map;
  Score := 0;
  Level := 1;
  AddAtr(atExp, cExp, False);
  AddAtr(atAdr, cAdr, False);
  AddAtr(atLife, Saga.Race[R].Life, True);
  AddAtr(atMana, Saga.Race[R].Mana, True);
  Look.Enabled := False;
  Inventory.Clear();
  FPrevName := '';
end;

constructor TPlayer.Create;
begin
  inherited Create;
  FPrevName := '';
  Look := TLook.Create;
  Inventory := TInventor.Create();
  Look.Enabled := False;
  SetPosition(40, 20);
  Color := clHotKey;
  Symbol := '@';
  Force := fcAlly;
  Race := 0;
  Map := 0;
end;

procedure TPlayer.Defeat;
begin
  Saga.Recs.Save;
  Saga.Stages.SetStage(stDefeat);
end;

procedure TPlayer.Victory;
begin
  Saga.Recs.Save;
  Saga.Stages.SetStage(stVictory);
end;

destructor TPlayer.Destroy;
begin
  Inventory.Free;
  Look.Free;
  inherited;
end;

procedure TPlayer.GenName;
var
  SL: TStringList;
  I, M: Byte;
  R: string;
begin
  Name := '';
  M := Math.RandomRange(1, 3);
  if (Race = 2) then
    M := 1;
  R := RaceName[TRaceEnum(Race)];
  SL := ExplodeString('|', R);
  for I := 0 to M do
  begin
    Name := Name + RandStr(',', __(SL[I]));
    if (I < M) then
      Name := Name + RaceNameDiv[TRaceEnum(Race)];
  end;
  if (FPrevName = Name) then
    GenName;
  FPrevName := Name;
end;

function TPlayer.GetFullName: string;
begin
  Result := Format(__(InfFmt + ' %d level'), [Name, GetRaceName, Level]);
end;

function TPlayer.GetRaceName: string;
begin
  Result := Saga.Race[TRaceEnum(Race)].Name;
end;

procedure TPlayer.LoadFromFile(FileName: string);
var
  S: string;
  F: TIniFile;
  X, Y: Integer;
  I: TInvByte;
begin
  F := TIniFile.Create(FileName);
  try
    // Character
    S := 'Player';
    if (F.SectionExists(S)) then
    begin
      Name := F.ReadString(S, 'Name', '');
      X := F.ReadInteger(S, 'X', 0);
      Y := F.ReadInteger(S, 'Y', 0);
      SetPosition(X, Y);
      Race := F.ReadInteger(S, 'Race', 0);
      Map := F.ReadInteger(S, 'Map', 0);
      Level := F.ReadInteger(S, 'Level', 1);
      Atr[atExp].Add(F.ReadString(S, 'Exp', Format(BarFmt, [0, cExp])));
      Atr[atAdr].Add(F.ReadString(S, 'Adr', Format(BarFmt, [0, cAdr])));
      Atr[atLife].Add(F.ReadString(S, 'Life', Format(BarFmt, [100, 100])));
      Atr[atMana].Add(F.ReadString(S, 'Mana', Format(BarFmt, [100, 100])));
      Score := F.ReadInteger(S, 'Score', 0);
    end;
    // Inventory
    Inventory.Clear();
    for I := Low(TInvByte) to High(TInvByte) do
    begin
      S := IntToStr(I);
      // Inventory.
    end;
  finally
    F.Free;
  end;
end;

procedure TPlayer.Move(AX, AY: ShortInt);
begin
  if (Pos.X = 0) and (AX = -1) then
    if Saga.World.GoLoc(drLeft) then
    begin
      SetPosition(MapWidth - 1, Pos.Y);
      Exit;
    end;
  if (Pos.X = MapWidth - 1) and (AX = 1) then
    if Saga.World.GoLoc(drRight) then
    begin
      SetPosition(0, Pos.Y);
      Exit;
    end;
  if (Pos.Y = 0) and (AY = -1) then
    if Saga.World.GoLoc(drUp) then
    begin
      SetPosition(Pos.X, MapHeight - 1);
      Exit;
    end;
  if (Pos.Y = MapHeight - 1) and (AY = 1) then
    if Saga.World.GoLoc(drDown) then
    begin
      SetPosition(Pos.X, 0);
      Exit;
    end;
  inherited Move(AX, AY);
end;

procedure TPlayer.SaveToFile(FileName: string);
var
  S: string;
  F: TIniFile;
  I: TInvByte;
begin
  F := TIniFile.Create(FileName);
  try
    // Character
    S := 'Player';
    F.WriteString(S, 'Name', Name);
    F.WriteInteger(S, 'X', Pos.X);
    F.WriteInteger(S, 'Y', Pos.Y);
    F.WriteInteger(S, 'Race', Race);
    F.WriteInteger(S, 'Map', Map);
    F.WriteInteger(S, 'Level', Level);
    F.WriteString(S, 'Exp', Format(BarFmt, [Atr[atExp].Cur, Atr[atExp].Max]));
    F.WriteString(S, 'Adr', Format(BarFmt, [Atr[atAdr].Cur, Atr[atAdr].Max]));
    F.WriteString(S, 'Life', Format(BarFmt, [Atr[atLife].Cur,
      Atr[atLife].Max]));
    F.WriteString(S, 'Mana', Format(BarFmt, [Atr[atMana].Cur,
      Atr[atMana].Max]));
    F.WriteInteger(S, 'Score', Score);
    // Inventory
    for I := Low(TInvByte) to High(TInvByte) do
    begin
      S := IntToStr(I);
      { F.WriteString(S, 'ID', Inventory.GetID(I));
        F.WriteInteger(S, 'Count', Inventory.GetCount(I));
        F.WriteInteger(S, 'Weight', Inventory.GetWeight(I));
        F.WriteInteger(S, 'Tough', Inventory.GetTough(I));
        F.WriteBool(S, 'Stack', Inventory.GetStack(I));
        F.WriteBool(S, 'Doll', Inventory.GetDoll(I)); }
    end;
  finally
    F.Free;
  end;
  // Inventory.SaveToFile(ChangeFileExt(FileName, '.inv'));
end;

procedure TPlayer.Pickup;
var
  I, C: Integer;
  // E: TItem;
begin
  I := Saga.World.CurrentItems.Has(Pos.X, Pos.Y);
  if (I > -1) then
  begin
    C := Saga.World.CurrentItems.Count(Pos.X, Pos.Y);
    if (C = 1) then
      Saga.World.CurrentItems.Pickup(I)
    else if (C > 1) then
      Saga.Stages.SetStage(stItems, stGame);
  end;
end;

{ TLook }

procedure TLook.Move(AX, AY: Integer);
begin
  if (Saga.World.CurrentMap.CellInMap(Pos.X + AX, Pos.Y + AY)) then
    SetPosition(Pos.X + AX, Pos.Y + AY);
end;

procedure TLook.Render;
var
  T: TTile;

  function GetCreatures: string;
  var
    I: Integer;
  begin
    Result := '';
    I := Saga.World.CurrentCreatures.Has(Pos.X, Pos.Y);
    if (I > -1) then
      Result := Format(KeyFmt, [Saga.World.CurrentCreatures.Get(I).Symbol,
        Saga.World.CurrentCreatures.Get(I).Name]);
    if (Saga.Player.Has(Pos.X, Pos.Y)) then
      Result := Format(KeyFmt, ['@', Saga.Player.GetFullName]);
  end;

  function GetItems(Pos: TPoint): string;
  var
    I, C: Integer;
    Item: TItem;
  begin
    Result := '';
    I := Saga.World.CurrentItems.Has(Pos.X, Pos.Y);
    if (I > -1) then
    begin
      Item := Saga.World.CurrentItems.Get(I);
      Result := Trim(Format(KeyFmt, [Item.Symbol,
        Saga.World.CurrentItems.GetItemPropStr(Item)]));
      C := Saga.World.CurrentItems.Count(Pos.X, Pos.Y);
      if (C > 1) then
        Result := Format(__('<%s> Несколько (%dx) предметов (%s)'),
          [Saga.World.CurrentItems.Get(I).Symbol, C,
          Saga.World.CurrentItems.Get(I).Name]);
    end;
  end;

begin
  if not Enabled then
  begin
    Saga.Engine.FontColor(cWhiteYel);
    Saga.Engine.FontBackColor(0);
    Saga.Engine.TextOut(0, 39, GetItems(Saga.Player.Pos));
    Exit;
  end;
  Saga.Engine.FontBackColor(cDkBrown);
  Saga.Engine.CharOut(Pos.X, Pos.Y, Saga.World.CurrentMap.GetTopTileChar(Pos.X,
    Pos.Y), cWhiteYel);
  T := Saga.World.CurrentMap.GetTile(Pos.X, Pos.Y, lrTerrain);
  Saga.Engine.FontColor(cWhiteYel);
  Saga.Engine.FontBackColor(0);
  Saga.Engine.TextOut(0, 39, Trim(Format(KeyFmt + ' ' + InfFmt,
    [Saga.Tiles.GetTile(T).Symbol, __(Saga.Tiles.GetTile(T).Name),
    GetCreatures(), GetItems(Pos)])));
end;

{ TItem }

procedure TItem.Assign(Value: TItem);
begin
  Name := Value.Name;
  Active := Value.Active;
  Category := Value.Category;
  Material := Value.Material;
  Level := Value.Level;
  Symbol := Value.Symbol;
  Color := Value.Color;
  Count := Value.Count;
  Calc();
  Durability.Cur := Value.Durability.Cur;
end;

procedure TItem.Calc;
begin
  Durability.Max := (Ord(Category) + Ord(Material)) * (Level + 5);
end;

constructor TItem.Create;
begin
  inherited Create;
  FDurability := TBar.Create;
  FDurability.Max := 10;
  FDurability.SetToMax;
  Color := clItem;
  Symbol := '/';
  Count := 1;
  Category := ctNone;
  Material := mtNone;
end;

destructor TItem.Destroy;
begin
  Durability.Free;
  inherited;
end;

procedure TItem.Render;
begin
  Saga.Engine.CharOut(Pos.X, Pos.Y, Symbol, Color);
end;

{ TItems }

procedure TItems.Add(Symbol: Char; Color, Level: Integer; Name: string;
  Material: TMaterial; Category: TCategory; Durability: Word; Amount: Word = 1);
var
  I: Integer;

  procedure SetItem(Index: Integer);
  begin
    FItem[Index].SetPosition(Saga.Player.Pos);
    FItem[Index].Name := Name;
    FItem[Index].Color := Color;
    FItem[Index].Symbol := Symbol;
    FItem[Index].Level := Level;
    FItem[Index].Count := Amount;
    FItem[Index].Material := Material;
    FItem[Index].Category := Category;
    FItem[Index].Calc;
    FItem[Index].Durability.Cur := Durability;
  end;

begin
  if (Count <> 0) then
    for I := 0 to Count - 1 do
      if not FItem[I].Active then
      begin
        FItem[I].Create();
        SetItem(I);
        Exit;
      end;
  SetLength(FItem, Count + 1);
  FItem[Count - 1] := TItem.Create();
  SetItem(Count - 1);
end;

procedure TItems.Del(const Index: Integer);
begin
  FItem[Index].Active := False;
end;

procedure TItems.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FItem[I].Free;
  SetLength(FItem, 0);
end;

function TItems.Count: Integer;
begin
  Result := Length(FItem);
end;

function TItems.Count(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (FItem[I].Active) and (FItem[I].Pos.X = X) and (FItem[I].Pos.Y = Y) then
      Inc(Result);
end;

constructor TItems.Create;
begin

end;

destructor TItems.Destroy;
begin
  Self.Clear;
  inherited;
end;

function TItems.Get(N: Integer): TItem;
begin
  Result := FItem[N];
end;

function TItems.Has(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
    if (FItem[I].Active) and (FItem[I].Pos.X = X) and (FItem[I].Pos.Y = Y) then
    begin
      Result := I;
      Break;
    end;
end;

function TItems.GetIndex(N, X, Y: Integer): Integer;
var
  I, J: Integer;
begin
  J := 0;
  Result := -1;
  for I := 0 to Count - 1 do
    if (FItem[I].Active) and (FItem[I].Pos.X = X) and (FItem[I].Pos.Y = Y) then
    begin
      if (J = N) then
      begin
        Result := I;
        Break;
      end;
      Inc(J);
    end;
end;

procedure TItems.Render;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (FItem[I].Active) then
      FItem[I].Render;
end;

procedure TItems.LoadFromFile(FileName: string);
var
  I, L: Integer;
  F: TAdvIniFile;
  S: string;
begin
  Self.Clear;
  F := TAdvIniFile.Create(FileName);
  try
    for I := 0 to 99 do
    begin
      S := Format('%d', [I]);
      if (F.SectionExists(S)) then
      begin
        L := Count;
        SetLength(FItem, L + 1);
        FItem[L] := TItem.Create;
        FItem[L].Active := True;
        FItem[L].Name := F.ReadString(S, 'Name', '');
        FItem[L].Level := F.ReadInteger(S, 'Level', 1);
        FItem[L].Category := F.ReadCategory(S, 'Category', ctNone);
        FItem[L].Material := F.ReadMaterial(S, 'Material', mtNone);
        FItem[L].Symbol := F.ReadString(S, 'Symbol', '/')[1];
        FItem[L].Color := F.ReadInteger(S, 'Color', clItem);
        FItem[L].Count := F.ReadInteger(S, 'Count', 1);
        FItem[L].SetPosition(Point(F.ReadInteger(S, 'X', 0),
          F.ReadInteger(S, 'Y', 0)));
        FItem[L].Calc;
        FItem[L].Durability.Cur := F.ReadInteger(S, 'Durability',
          FItem[L].Durability.Max);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TItems.SaveToFile(FileName: string);
var
  I, C: Integer;
  S: string;
  F: TAdvIniFile;
begin
  F := TAdvIniFile.Create(FileName);
  try
    C := 0;
    for I := 0 to Count - 1 do
      if (FItem[I].Active) then
      begin
        S := Format('%d', [C]);
        F.WriteString(S, 'Name', FItem[I].Name);
        F.WriteInteger(S, 'Level', FItem[I].Level);
        F.WriteCategory(S, 'Category', FItem[I].Category);
        F.WriteMaterial(S, 'Material', FItem[I].Material);
        F.WriteString(S, 'Symbol', FItem[I].Symbol);
        F.WriteInteger(S, 'Color', FItem[I].Color);
        F.WriteInteger(S, 'Count', FItem[I].Count);
        F.WriteInteger(S, 'X', FItem[I].Pos.X);
        F.WriteInteger(S, 'Y', FItem[I].Pos.Y);
        F.WriteInteger(S, 'Durability', FItem[I].Durability.Cur);
        Inc(C);
      end;
  finally
    F.Free;
  end;
end;

function TItems.GetItemPropStr(Item: TItem): string;
begin
  Result := '';
  if (Item.Count = 1) then
    Result := Format('(' + BarFmt + ')', [Item.Durability.Cur,
      Item.Durability.Max])
  else if (Item.Count > 1) then
    Result := Format('(%dx)', [Item.Count]);
  Result := Format('%s %s', [__(Item.Name), Result]);
end;

procedure TItems.Pickup(I: Integer);
var
  Item: TItem;
begin
  Item := Saga.World.CurrentItems.Get(I);
  if Saga.Player.Inventory.AddItem(Item) then
  begin
    Saga.Log[lgGame].Add(Format(__('You pick up a %s.'), [Item.Name]));
    Saga.World.CurrentItems.Del(I);
  end;
end;

{ TAdvIniFile }

function TAdvIniFile.ReadCategory(Section, Ident: string;
  DefaultValue: TCategory): TCategory;
var
  S: string;
  C: TCategory;
begin
  S := LowerCase(Trim(ReadString(Section, Ident, CatStr[DefaultValue])));
  Result := ctNone;
  if (S = '') then
    Exit;
  for C := Low(TCategory) to High(TCategory) do
    if (S = CatStr[C]) then
    begin
      Result := C;
      Exit;
    end;
end;

function TAdvIniFile.ReadMaterial(Section, Ident: string;
  DefaultValue: TMaterial): TMaterial;
var
  S: string;
  M: TMaterial;
begin
  S := LowerCase(Trim(ReadString(Section, Ident, MatStr[DefaultValue])));
  Result := mtNone;
  if (S = '') then
    Exit;
  for M := Low(TMaterial) to High(TMaterial) do
    if (S = MatStr[M]) then
    begin
      Result := M;
      Exit;
    end;
end;

procedure TAdvIniFile.WriteCategory(Section, Ident: string; Value: TCategory);
begin
  if (Value = ctNone) then
    Exit;
  WriteString(Section, Ident, CatStr[Value]);
end;

procedure TAdvIniFile.WriteMaterial(Section, Ident: string; Value: TMaterial);
begin
  if (Value = mtNone) then
    Exit;
  WriteString(Section, Ident, MatStr[Value]);
end;

{ TInventor }

function TInventor.AddItem(Value: TItem): Boolean;
var
  I: TInvByte;
  C: Integer;
begin
  for I := Low(TInvByte) to High(TInvByte) do
  begin
    if (Item[I].Active and (Item[I].Name = Value.Name)) then
    begin
      C := Item[I].Count;
      Item[I].Assign(Value);
      Item[I].Count := C + Value.Count;
      Result := True;
      Exit;
    end;
    Item[I].Assign(Value);
    Result := True;
    Exit;
  end;
  Result := False;
end;

procedure TInventor.Clear;
var
  I: TInvByte;
begin
  for I := Low(TInvByte) to High(TInvByte) do
    Self.Clear(I);
end;

procedure TInventor.Clear(I: TInvByte);
begin
  Item[I].Create;
  Item[I].Active := False;
end;

function TInventor.Count: Integer;
var
  I: TInvByte;
begin
  Result := 0;
  for I := Low(TInvByte) to High(TInvByte) do
    if Item[I].Active then
      Inc(Result);
end;

constructor TInventor.Create;
var
  I: TInvByte;
begin
  for I := Low(TInvByte) to High(TInvByte) do
  begin
    Item[I] := TItem.Create;
    Self.Clear(I);
  end;
end;

destructor TInventor.Destroy;
var
  I: TInvByte;
begin
  for I := Low(TInvByte) to High(TInvByte) do
    Item[I].Free;
  inherited;
end;

function TInventor.GetItem(I: TInvByte): TItem;
begin
  Result := FItem[I];
end;

procedure TInventor.SetItem(I: TInvByte; const Value: TItem);
begin
  FItem[I] := Value;
end;

end.
