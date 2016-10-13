unit ForgottenSaga.Entities;

interface

{$I Include.inc}

uses Engine, {$IFNDEF FPC}Types, {$ENDIF}Classes, IniFiles;

{$REGION ' IStorage '}

type
  IStorage = interface
    procedure Clear;
    function Count: Integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  end;

{$ENDREGION ' IStorage '}
{$REGION ' TEntity '}

type
  TEntity = class(TObject)
  public const
    KeyFmt = '{%s} %s';
    BarFmt = '%d/%d';
{$REGION ' TEntity.TBar '}
  public type
    TBar = class(TObject)
    strict private
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
      function IsMin: Boolean;
      function IsMax: Boolean;
      procedure Dec(Value: Integer = 1);
      procedure Inc(Value: Integer = 1);
      property Cur: Integer read GetCur write SetCur;
      property Max: Integer read GetMax write SetMax;
      property Adv: Integer read GetAdv write SetAdv;
      procedure Add(Values: string);
      function ToText: string;
      procedure SetToMin;
      procedure SetToMax;
    end;
{$ENDREGION ' TEntity.TBar '}
  strict private
    FPos: TPoint;
    FHeight: Integer;
    FWidth: Integer;
    FName: string;
    FColor: Integer;
    FSymbol: Char;
    FLevel: Byte;
    FActive: Boolean;
  public
    constructor Create(Width: Integer = 1; Height: Integer = 1);
    procedure SetPosition(const APos: TPoint); overload;
    procedure SetPosition(const X, Y: Integer); overload;
    property Pos: TPoint read FPos;
    function Has(const APos: TPoint): Boolean; overload;
    function Has(const X, Y: Integer): Boolean; overload;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Name: string read FName write FName;
    property Color: Integer read FColor write FColor;
    property Symbol: Char read FSymbol write FSymbol;
    property Level: Byte read FLevel write FLevel;
    property Active: Boolean read FActive write FActive;
    function BackColor: Integer; virtual; abstract;
    procedure Render; virtual; abstract;
  end;

{$ENDREGION ' TEntity '}
{$REGION ' TEntityHelper '}

type
  TEntityHelper = class helper for TEntity
    function Percent(N, P: Integer): Integer;
    function BarWidth(CX, MX, WX: Integer): Integer;
  end;

{$ENDREGION ' TEntityHelper '}
{$REGION ' TCreature '}

type
  TCreature = class(TEntity)
  public type
    TAtrEnum = (atExp, atLife, atMana, atAdr);
    TForce = (fcAlly, fcEnemy);
  public const
    cAdr = 75;
    cExp = 15;
    AtrStr: array [TAtrEnum] of string = ('Exp', 'Life', 'Mana', 'Adrenalin');
    ForceValues: array [Boolean] of TForce = (fcAlly, fcEnemy);
  strict private
    FForce: TForce;
    FScriptFileName: string;
    FDialog: Integer;
    FAtr: array [TAtrEnum] of TEntity.TBar;
    FSkillPoints: Byte;
    FStatPoints: Byte;
    procedure SetSkillPoints(const Value: Byte);
    procedure SetStatPoints(const Value: Byte);
  strict protected
    function GetAtr(I: TAtrEnum): TEntity.TBar;
    procedure SetAtr(I: TAtrEnum; const Value: TEntity.TBar);
  public
    constructor Create;
    destructor Destroy; override;
    property Force: TForce read FForce write FForce;
    property Dialog: Integer read FDialog write FDialog;
    property ScriptFileName: string read FScriptFileName write FScriptFileName;
    function BackColor: Integer; override;
    procedure Move(const AX, AY: ShortInt);
    procedure Render; override;
    property Atr[I: TAtrEnum]: TEntity.TBar read GetAtr write SetAtr;
    property StatPoints: Byte read FStatPoints write SetStatPoints;
    property SkillPoints: Byte read FSkillPoints write SetSkillPoints;
    procedure AddAtr(Atrib: TAtrEnum; Max: Integer; IsToMax: Boolean);
    procedure Fill;
  end;

{$ENDREGION ' TCreature '}
{$REGION ' TItem '}

type
  TItem = class(TEntity)
  public type
    TCategory = (ctNone, ctStaff, ctSpear, ctAxe, ctSword);
    TMaterial = (mtNone, mtWood, mtBone, mtStone, mtMetal);
  public const
    CatStr: array [TCategory] of string = ('', 'staff', 'spear', 'axe',
      'sword');
    MatStr: array [TMaterial] of string = ('', 'wood', 'bone', 'stone',
      'metal');
  strict private
    FCount: Integer;
    FCategory: TCategory;
    FMaterial: TMaterial;
    FDurability: TEntity.TBar;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Assign(Value: TItem);
    procedure Calc;
    function BackColor: Integer; override;
    property Count: Integer read FCount write FCount;
    property Category: TCategory read FCategory write FCategory;
    property Material: TMaterial read FMaterial write FMaterial;
    property Durability: TEntity.TBar read FDurability write FDurability;
    procedure SaveToFile(const FileName, Section: string);
    procedure LoadFromFile(const FileName, Section: string);
  end;

{$ENDREGION ' TItem '}
{$REGION ' TPlayer '}

type
  TPlayer = class(TCreature)
  public const
    InfFmt = '%s %s';
  private const
    DefRadius = 9;
{$REGION ' TPlayer.TLook '}
  strict private
  type
    TLook = class(TEntity)
    public
      procedure Render; override;
      function BackColor: Integer; override;
      procedure Move(AX, AY: Integer);
    end;
{$ENDREGION ' TPlayer.TLook '}
{$REGION ' TPlayer.TInventory '}
  public type
    TInventory = class(TObject)
    public type
      TInvByte = 1 .. 26;
    strict private
      FItem: array [TInvByte] of TItem;
    strict protected
      function GetItem(I: TInvByte): TItem;
      procedure SetItem(I: TInvByte; const Value: TItem);
    public
      constructor Create;
      destructor Destroy; override;
      function Count: Integer;
      property Item[I: TInvByte]: TItem read GetItem write SetItem;
      function AddItem(Value: TItem): Boolean;
      procedure LoadFromFile(const FileName: string);
      procedure SaveToFile(const FileName: string);
      procedure Clear(I: TInvByte); overload;
      procedure Clear; overload;
    end;
{$ENDREGION ' TPlayer.TInventory '}
  strict private
    FMap: Integer;
    FRace: Byte;
    FScore: Word;
    FPrevName: string;
    FLook: TLook;
    FInventory: TInventory;
    function GetRadius: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Score: Word read FScore write FScore;
    property Race: Byte read FRace write FRace;
    property Map: Integer read FMap write FMap;
    function GetRaceName: string;
    function GetFullName: string;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure Move(AX, AY: ShortInt);
    procedure GenName;
    procedure Clear;
    procedure Defeat;
    procedure Pickup;
    procedure Victory;
    procedure AddExp(A: Word);
    property Look: TLook read FLook write FLook;
    property Radius: Integer read GetRadius;
    property Inventory: TInventory read FInventory write FInventory;
  end;

{$ENDREGION ' TPlayer '}
{$REGION ' TGenericEntities '}

type
{$IFDEF FPC}generic{$ENDIF}
  TGenericEntities<T: TEntity> = class(TInterfacedObject)strict private FEntity
  : array of T;
protected
  function GetEntity(Index: Integer): T;
  procedure SetEntity(Index: Integer; const Value: T);
  procedure SetEntitiesLength(NewLength: Integer);
public
  property Entity[Index: Integer]: T read GetEntity write SetEntity;
  function Count: Integer;
  procedure Clear;
  end;

{$ENDREGION ' TGenericEntities '}
{$REGION ' TEntities '}

type
{$IFDEF FPC}generic{$ENDIF} TEntities<T: TEntity> = class({$IFDEF FPC}specialize{$ENDIF}
  TGenericEntities<T>, IStorage)private Sections: TStringList;
public
  constructor Create;
  destructor Destroy;
  override;
  function Has(X, Y: Integer): Integer;
  procedure Render;
  function Count: Integer;
  overload;
  function Count(X, Y: Integer): Integer;
  overload;
  function GetIndex(N, X, Y: Integer): Integer;
  overload;
  function GetIndex(SectionID: string): Integer;
  overload;
  procedure LoadFromFile(const FileName: string);
  virtual;
  abstract;
  procedure SaveToFile(const FileName: string);
  virtual;
  abstract;
  procedure Delete(const Index: Integer);
  procedure Clear;
  end;

{$ENDREGION ' TEntities '}
{$REGION ' TCreatures '}

type
  TCreatures = class({$IFDEF FPC} specialize
{$ENDIF} TEntities<TCreature>, IStorage)
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;

{$ENDREGION ' TCreatures '}
{$REGION ' TItems '}

type
  TItems = class({$IFDEF FPC} specialize {$ENDIF} TEntities<TItem>, IStorage)
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure Add(Symbol: Char; Color, Level: Integer; Name: string;
      Material: TItem.TMaterial; Category: TItem.TCategory; Durability: Word;
      Amount: Word = 1);
    function ToText(Item: TItem): string;
    procedure Pickup(I: Integer);
  end;

{$ENDREGION ' TItems '}
{$REGION ' TTiles '}

type
  TTiles = class(TObject)
  public type
    TLayerTypeEnum = (ltTerrain, ltObjects, ltBoth, ltRes);
    TTileEnum = (tNone, tRes1, tRes2, tRes3, tRes4, tGravel, tDirt, tGrass,
      tStone, tSand, tRock, tWater, tDeepWater, tLava, tRes6, tRes7, tRes8,
      tRes9, tRes10, tWillowTree, tOakTree, tAshTree, tYewTree, tBirchTree,
      tAspenTree, tMapleTree, tWalnutTree, tPineTree, tCedarTree, tSpruceTree,
      tRes11, tRes12, tRes13, tRes14, tRes15, tStDn, tStUp, tRes16, tRes17,
      tRes18, tRes19, tRes20, tStoneFloor, tStoneWall, tClosedDoor, tOpenedDoor,
      tClosedGate, tOpenedGate, tClosedLiuk, tRes21, tRes22, tRes23, tRes24,
      tRes25, tRoad, tRes26, tRes27, tRes28, tRes29, tRes30);
  public const
    TileDarkPercent = 80;
    TileStr: array [TTileEnum] of string = ('NONE', 'RES1', 'RES2', 'RES3',
      'RES4', 'GRAVEL', 'DIRT', 'GRASS', 'STONE', 'SAND', 'ROCK', 'WATER',
      'DEEP_WATER', 'LAVA', 'RES6', 'RES7', 'RES8', 'RES9', 'RES10',
      'WILLOW_TREE', 'OAK_TREE', 'ASH_TREE', 'YEW_TREE', 'BIRCH_TREE',
      'ASPEN_TREE', 'MAPLE_TREE', 'WALNUT_TREE', 'PINE_TREE', 'CEDAR_TREE',
      'SPRUCE_TREE', 'RES11', 'RES12', 'RES13', 'RES14', 'RES15', 'STAIRS_DOWN',
      'STAIRS_UP', 'RES16', 'RES17', 'RES18', 'RES19', 'RES20', 'STONE_FLOOR',
      'STONE_WALL', 'CLOSED_DOOR', 'OPENED_DOOR', 'CLOSED_GATE', 'OPENED_GATE',
      'CLOSED_LIUK', 'RES21', 'RES22', 'RES23', 'RES24', 'RES25', 'ROAD',
      'RES26', 'RES27', 'RES28', 'RES29', 'RES30');
  public type
    TTileProp = record
      Name: string;
      Symbol: System.Char;
      Passable: Boolean;
      Color: Integer;
      Layer: TLayerTypeEnum;
    end;
  strict private
    FTiles: array [TTileEnum] of TTileProp;
  public
    function GetTile(Tile: TTileEnum): TTileProp;
    procedure LoadFromFile(FileName: string);
  end;

{$ENDREGION ' TTiles '}
{$REGION ' TMap '}

type
  TMap = class(TInterfacedObject, IStorage)
  public const
    Size: TEngine.TSize = (Width: 80; Height: 40);
    Offset = 40;
  public type
    TDir = (drLeft, drUp, drRight, drDown, drTop, drBottom);
    TLayerEnum = (lrTerrain, lrObjects);
    TLayer = array [Byte, Byte, TLayerEnum] of TTiles.TTileEnum;
  strict private
    FMap: TLayer;
    FName: string;
    FFileName: string;
    FWidth: Integer;
    FHeight: Integer;
  public
    MapFOV: array [Byte, Byte] of Integer;
    MapNeighbors: array [TDir] of string;
    procedure Clear;
    procedure ClearLayer(Z: TLayerEnum);
    procedure FillLayer(Z: TLayerEnum; Tile: TTiles.TTileEnum);
    procedure ClearFOV;
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function CellInMap(X, Y: Integer): Boolean;
    function HasCellVisible(X, Y: Integer): Boolean;
    function GetTopTileChar(X, Y: Integer): Char;
    procedure SetTile(X, Y: Integer; Z: TLayerEnum; Tile: TTiles.TTileEnum);
    function GetTile(X, Y: Integer; Z: TLayerEnum): TTiles.TTileEnum;
    function HasTile(Tile: TTiles.TTileEnum; X, Y: Integer;
      Z: TLayerEnum = lrTerrain): Boolean;
    procedure Render;
    procedure Gen;
    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

{$ENDREGION ' TMap '}
{$REGION ' TMapGenerator '}

type
  TMapGenerator = class(TObject)
  public type
    TMGTiles = record
      Wall: TTiles.TTileEnum;
      Floor: TTiles.TTileEnum;
    end;
  strict private
    FMap: TMap;
    FStart: TPoint;
    // ?    FMGTiles: TMGTiles;
    FNum: Byte;
  public
    MGTiles: TMGTiles;
    constructor Create();
    destructor Destroy; override;
    property Map: TMap read FMap write FMap;
    property Start: TPoint read FStart write FStart;
    // ?    property MGTiles: TMGTiles read FMGTiles write FMGTiles;
    property Num: Byte read FNum write FNum;
    procedure AddSpot(Tile: TTiles.TTileEnum; Layer: TMap.TLayerEnum);
    procedure GenCave();
  end;

{$ENDREGION ' TMapGenerator '}

implementation

uses SysUtils, Math, ForgottenSaga.Classes, ForgottenSaga.Scenes;

{$REGION ' Path find '}

type
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

function DoPath(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;
  external 'BearLibPF.dll' name 'DoAStar';

{$ENDREGION ' Path find '}
{$REGION ' Field of view '}

const
  FOV_CELL_OPAQUE = 1;
  FOV_CELL_VISIBLE = 2;

type
  IntPtr = ^Integer;

type
  FOVCallback = function(X, Y: Integer; Opaque: IntPtr): Integer; cdecl;

function DoFOV(Width, Height, X, Y, Radius: Integer;
  get_cb, set_cb: FOVCallback; Opaque: IntPtr): Integer; cdecl;
  external 'BearLibFOV.dll' name 'fov_calc';

function GetFOVCallback(X, Y: Integer; Opaque: IntPtr): Integer; cdecl;
begin
  Result := Saga.World.CurrentMap.MapFOV[Y][X];
end;

function SetFOVCallback(X, Y: Integer; Opaque: IntPtr): Integer; cdecl;
begin
  Saga.World.CurrentMap.MapFOV[Y][X] := Saga.World.CurrentMap.MapFOV[Y][X] or
    FOV_CELL_VISIBLE;
end;

{$ENDREGION ' Field of view '}
{$REGION ' TEntity '}

constructor TEntity.Create(Width: Integer = 1; Height: Integer = 1);
begin
  FHeight := Height;
  FWidth := Width;
  Name := '';
  Active := True;
  Color := $00FFFFFF;
  Symbol := '?';
  Level := 1;
end;

function TEntity.Has(const APos: TPoint): Boolean;
begin
  Result := (FPos.X = APos.X) and (FPos.Y = APos.Y)
end;

function TEntity.Has(const X, Y: Integer): Boolean;
begin
  Result := (FPos.X = X) and (FPos.Y = Y)
end;

procedure TEntity.SetPosition(const X, Y: Integer);
begin
  FPos := Point(X, Y)
end;

procedure TEntity.SetPosition(const APos: TPoint);
begin
  FPos := APos
end;

{$REGION ' TEntity.TBar '}

procedure TEntity.TBar.Add(Values: string);
var
  SL: TStringList;
begin
  SL := TUtils.ExplodeString('/', Values);
  FCur := StrToIntDef(Trim(SL[0]), 0);
  FMax := StrToIntDef(Trim(SL[1]), 0);
end;

constructor TEntity.TBar.Create;
begin
  Adv := 0;
end;

procedure TEntity.TBar.Dec(Value: Integer);
begin
  if ((Cur > 0) and (Value > 0)) then
    SetCur(GetCur - Value);
  if (Cur < 0) then
    Cur := 0;
end;

function TEntity.TBar.GetAdv: Integer;
begin
  Result := FAdv;
end;

function TEntity.TBar.GetCur: Integer;
begin
  Result := FCur;
end;

function TEntity.TBar.GetMax: Integer;
begin
  Result := FMax;
end;

procedure TEntity.TBar.Inc(Value: Integer);
begin
  if ((Cur < Max) and (Value > 0)) then
    SetCur(GetCur + Value);
  if (Cur > Max) then
    Cur := Max;
end;

function TEntity.TBar.IsMax: Boolean;
begin
  Result := (Cur >= Max);
end;

function TEntity.TBar.IsMin: Boolean;
begin
  Result := (Cur = 0);
end;

procedure TEntity.TBar.SetAdv(const Value: Integer);
begin
  FAdv := Value;
end;

procedure TEntity.TBar.SetCur(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  if (Value > Max) then
    Value := Max;
  FCur := Value;
end;

procedure TEntity.TBar.SetMax(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  FMax := Value + Adv;
  if (Cur >= Max) then
    SetToMax;
end;

procedure TEntity.TBar.SetToMax;
begin
  Cur := Max;
end;

procedure TEntity.TBar.SetToMin;
begin
  Cur := 0;
end;

function TEntity.TBar.ToText: string;
begin
  Result := Format(BarFmt, [Cur, Max]);
end;

{$ENDREGION ' TEntity.TBar '}
{$ENDREGION ' TEntity '}
{$REGION ' TEntityHelper '}

function TEntityHelper.BarWidth(CX, MX, WX: Integer): Integer;
begin
  Result := Round(CX / MX * WX);
end;

function TEntityHelper.Percent(N, P: Integer): Integer;
begin
  Result := N * P div 100;
end;

{$ENDREGION ' TEntityHelper '}
{$REGION ' TCreature '}

constructor TCreature.Create;
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
        AddAtr(I, 100, True);
      atMana:
        AddAtr(I, 100, True);
    end;
  end;
  Color := $00FFFF00;
  Symbol := '?';
  Force := fcEnemy;
  Dialog := 0;
  ScriptFileName := '';
end;

destructor TCreature.Destroy;
var
  I: TAtrEnum;
begin
  for I := Low(TAtrEnum) to High(TAtrEnum) do
    FAtr[I].Free;
  inherited;
end;

procedure TCreature.Move(const AX, AY: ShortInt);
var
  X, Y: Integer;
var
  Prop: record Ter, Obj: TTiles.TTileProp;
end;

var
  Tile: record Ter, Obj: TTiles.TTileEnum;
end;

var
  Flag: Boolean;

function UseObject(): Boolean;
begin
  Result := False;
  if Tile.Obj = tClosedDoor then
  begin
    Saga.World.CurrentMap.SetTile(X, Y, lrObjects, tOpenedDoor);
    Saga.Log[lgGame].Add(__('You open the door.'));
    Result := True;
  end;
  if Tile.Obj = tClosedLiuk then
  begin
    Saga.World.CurrentMap.SetTile(X, Y, lrObjects, tNone);
    Saga.Log[lgGame].Add(__('You open the liuk.'));
    Result := True;
  end;
  if Tile.Obj = tClosedGate then
  begin
    Saga.World.CurrentMap.SetTile(X, Y, lrObjects, tOpenedGate);
    Saga.Log[lgGame].Add(__('You open the gate.'));
    Result := True;
  end;
end;

begin
  X := Pos.X + AX;
  Y := Pos.Y + AY;
  if (Saga.World.CurrentMap.CellInMap(X, Y)) then
  begin
    Flag := True;
    Tile.Ter := Saga.World.CurrentMap.GetTile(X, Y, lrTerrain);
    Prop.Ter := Saga.Tiles.GetTile(Tile.Ter);
    Tile.Obj := Saga.World.CurrentMap.GetTile(X, Y, lrObjects);
    Prop.Obj := Saga.Tiles.GetTile(Tile.Obj);
    if (Tile.Obj > tNone) then
      Flag := Prop.Obj.Passable;
    if (Tile.Obj > tNone) then
      if UseObject() then
      begin
        Saga.Stages.Render();
        Exit;
      end;
    if Flag and Prop.Ter.Passable then
      SetPosition(X, Y);
  end;
end;

function TCreature.BackColor: Integer;
begin
  case Force of
    fcEnemy:
      Result := $000022;
  else
    Result := $002200;
  end;
end;

procedure TCreature.Render;
begin
  if Saga.World.CurrentMap.HasCellVisible(Pos.X, Pos.Y) then
    Saga.UI.DrawChar(Pos.X, Pos.Y, Symbol, Color, BackColor);
end;

procedure TCreature.AddAtr(Atrib: TAtrEnum; Max: Integer; IsToMax: Boolean);
begin
  Atr[Atrib].Max := Max;
  if IsToMax then
    Atr[Atrib].SetToMax
  else
    Atr[Atrib].SetToMin;
end;

procedure TCreature.Fill;
begin
  Atr[atLife].SetToMax;
  Atr[atMana].SetToMax;
end;

procedure TCreature.SetSkillPoints(const Value: Byte);
begin
  FSkillPoints := Value;
end;

procedure TCreature.SetStatPoints(const Value: Byte);
begin
  FStatPoints := Value;
end;

function TCreature.GetAtr(I: TAtrEnum): TEntity.TBar;
begin
  Result := FAtr[I];
end;

procedure TCreature.SetAtr(I: TAtrEnum; const Value: TEntity.TBar);
begin
  FAtr[I] := Value;
end;

{$ENDREGION ' TCreature '}
{$REGION ' TItem '}

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

function TItem.BackColor: Integer;
begin
  Result := $00002200;
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
  Color := $00FFFF00;
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

procedure TItem.LoadFromFile(const FileName, Section: string);
var
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try
    Name := F.ReadString(Section, 'Name', '');
    Level := F.ReadInteger(Section, 'Level', 1);
    Category := F.ReadCategory(Section, 'Category', ctNone);
    Material := F.ReadMaterial(Section, 'Material', mtNone);
    Symbol := F.ReadString(Section, 'Symbol', '/')[1];
    Color := F.ReadColor(Section, 'Color', '200,200,200');
    Count := F.ReadInteger(Section, 'Count', 1);
    SetPosition(Point(F.ReadInteger(Section, 'X', 0), F.ReadInteger(Section,
      'Y', 0)));
    Calc;
    Durability.Cur := F.ReadInteger(Section, 'Durability', Durability.Max);
  finally
    F.Free;
  end;
end;

procedure TItem.Render;
begin
  if Saga.World.CurrentMap.HasCellVisible(Pos.X, Pos.Y) then
    Saga.UI.DrawChar(Pos.X, Pos.Y, Symbol, Color, BackColor);
end;

procedure TItem.SaveToFile(const FileName, Section: string);
var
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try
    F.WriteString(Section, 'Name', Name);
    F.WriteInteger(Section, 'Level', Level);
    F.WriteCategory(Section, 'Category', Category);
    F.WriteMaterial(Section, 'Material', Material);
    F.WriteString(Section, 'Symbol', Symbol);
    F.WriteColor(Section, 'Color', Color);
    F.WriteInteger(Section, 'Count', Count);
    F.WriteInteger(Section, 'X', Pos.X);
    F.WriteInteger(Section, 'Y', Pos.Y);
    F.WriteInteger(Section, 'Durability', Durability.Cur);
  finally
    F.Free;
  end;
end;

{$ENDREGION ' TItem '}
{$REGION ' TPlayer '}

procedure TPlayer.AddExp(A: Word);
begin
  if (Atr[atExp].Cur + A >= Atr[atExp].Max) then
  begin
    Atr[atExp].Cur := (Atr[atExp].Cur + A - Atr[atExp].Max);
    Score := Score + (Level * 10);
    Level := Level + 1;
    Atr[atExp].Max := (Atr[atExp].Max + (Atr[atExp].Max * 20 div 100));
  end
  else
    Atr[atExp].Inc(A);
end;

procedure TPlayer.Clear;
var
  R: TSaga.TRaceEnum;
begin
  R := TSaga.TRaceEnum(Race);
  Color := Saga.Race[R].Color;
  SetPosition(Saga.Race[R].Pos);
  Map := Saga.Race[R].Map;
  Score := 0;
  Level := 1;
  AddAtr(atExp, cExp, False);
  AddAtr(atAdr, cAdr, False);
  AddAtr(atLife, Saga.Race[R].Life, True);
  AddAtr(atMana, Saga.Race[R].Mana, True);
  Look.Active := False;
  Inventory.Clear();
  FPrevName := '';
end;

constructor TPlayer.Create;
begin
  inherited Create;
  FPrevName := '';
  Look := TLook.Create;
  Inventory := TInventory.Create();
  Look.Active := False;
  SetPosition(40, 20);
  Color := $00FFFFFF;
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
  R := TSaga.RaceName[TSaga.TRaceEnum(Race)];
  SL := TUtils.ExplodeString('|', R);
  for I := 0 to M do
  begin
    Name := Name + TUtils.RandStr(',', __(SL[I]));
    if (I < M) then
      Name := Name + Saga.RaceNameDiv[TSaga.TRaceEnum(Race)];
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
  Result := Saga.Race[TSaga.TRaceEnum(Race)].Name;
end;

function TPlayer.GetRadius: Integer;
begin
  Result := DefRadius + 1;
end;

procedure TPlayer.LoadFromFile(const FileName: string);
var
  S: string;
  F: TIniFile;
  X, Y: Integer;
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
  finally
    F.Free;
  end;
  Inventory.LoadFromFile(FileName);
end;

procedure TPlayer.Move(AX, AY: ShortInt);
begin
  if (Pos.X = 0) and (AX = -1) then
    if TWorld.GoLoc(drLeft) then
    begin
      SetPosition(TMap.Size.Width - 1, Pos.Y);
      Exit;
    end;
  if (Pos.X = TMap.Size.Width - 1) and (AX = 1) then
    if TWorld.GoLoc(drRight) then
    begin
      SetPosition(0, Pos.Y);
      Exit;
    end;
  if (Pos.Y = 0) and (AY = -1) then
    if TWorld.GoLoc(drUp) then
    begin
      SetPosition(Pos.X, TMap.Size.Height - 1);
      Exit;
    end;
  if (Pos.Y = TMap.Size.Height - 1) and (AY = 1) then
    if TWorld.GoLoc(drDown) then
    begin
      SetPosition(Pos.X, 0);
      Exit;
    end;
  inherited Move(AX, AY);
end;

procedure TPlayer.SaveToFile(const FileName: string);
var
  S: string;
  F: TIniFile;
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
  finally
    F.Free;
  end;
  Inventory.SaveToFile(FileName);
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

{$REGION ' TPlayer.TLook '}

function TPlayer.TLook.BackColor: Integer;
begin
  Result := Saga.Colors.GetColor(ceWhite);
end;

procedure TPlayer.TLook.Move(AX, AY: Integer);
begin
  if (Saga.World.CurrentMap.CellInMap(Pos.X + AX, Pos.Y + AY) and
    (Saga.World.CurrentMap.HasCellVisible(Pos.X + AX, Pos.Y + AY))) then
    SetPosition(Pos.X + AX, Pos.Y + AY);
end;

procedure TPlayer.TLook.Render;

  function GetTile(Z: TMap.TLayerEnum): string;
  var
    T: TTiles.TTileEnum;
  begin
    Result := '';
    T := Saga.World.CurrentMap.GetTile(Pos.X, Pos.Y, Z);
    if (T > tNone) then
      Result := Format(KeyFmt, [Saga.Tiles.GetTile(T).Symbol,
        __(Saga.Tiles.GetTile(T).Name)]) + ' ';
  end;

  function GetCreature: string;
  var
    I: Integer;
  begin
    Result := '';
    I := Saga.World.CurrentCreatures.Has(Pos.X, Pos.Y);
    if (I > -1) then
      Result := Format(KeyFmt, [Saga.World.CurrentCreatures.GetEntity(I).Symbol,
        __(Saga.World.CurrentCreatures.GetEntity(I).Name)]) + ' ';
    if (Saga.Player.Has(Pos.X, Pos.Y)) then
      Result := Format(KeyFmt, ['@', Saga.Player.GetFullName]) + ' ';
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
      Item := (Saga.World.CurrentItems.GetEntity(I) as TItem);
      Result := Trim(Saga.World.CurrentItems.ToText(Item));
      C := Saga.World.CurrentItems.Count(Pos.X, Pos.Y);
      if (C > 1) then
        Result := Format(KeyFmt, [Saga.World.CurrentItems.GetEntity(I).Symbol,
          Format('Несколько (%dx) предметов (%s)',
          [C, Saga.World.CurrentItems.GetEntity(I).Name])]);
    end;
  end;

begin
  if not Active then
  begin
    Saga.Engine.ForegroundColor(Saga.Colors.GetColor(ceWhite));
    Saga.Engine.BackgroundColor(0);
    Saga.Engine.Print(0, 39, GetItems(Saga.Player.Pos));
    Exit;
  end;
  Saga.UI.DrawChar(Pos.X, Pos.Y, Saga.World.CurrentMap.GetTopTileChar(Pos.X,
    Pos.Y), 0, BackColor);
  Saga.Engine.ForegroundColor(Saga.Colors.GetColor(ceWhite));
  Saga.Engine.BackgroundColor(0);
  Saga.Engine.Print(0, 39, Trim(GetTile(lrTerrain) + GetTile(lrObjects) +
    GetCreature() + GetItems(Pos)));
end;

{$ENDREGION ' TPlayer.TLook '}
{$REGION ' TPlayer.TInventor '}

function TPlayer.TInventory.AddItem(Value: TItem): Boolean;
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

procedure TPlayer.TInventory.Clear;
var
  I: TInvByte;
begin
  for I := Low(TInvByte) to High(TInvByte) do
    Self.Clear(I);
end;

procedure TPlayer.TInventory.Clear(I: TInvByte);
begin
  Item[I].Create;
  Item[I].Active := False;
end;

function TPlayer.TInventory.Count: Integer;
var
  I: TInvByte;
begin
  Result := 0;
  for I := Low(TInvByte) to High(TInvByte) do
    if Item[I].Active then
      Inc(Result);
end;

constructor TPlayer.TInventory.Create;
var
  I: TInvByte;
begin
  for I := Low(TInvByte) to High(TInvByte) do
  begin
    Item[I] := TItem.Create;
    Self.Clear(I);
  end;
end;

destructor TPlayer.TInventory.Destroy;
var
  I: TInvByte;
begin
  for I := Low(TInvByte) to High(TInvByte) do
    Item[I].Free;
  inherited;
end;

function TPlayer.TInventory.GetItem(I: TInvByte): TItem;
begin
  Result := FItem[I];
end;

procedure TPlayer.TInventory.LoadFromFile(const FileName: string);
var
  I: TInvByte;
  F: TIniFile;
  S: string;
begin
  Self.Clear();
  F := TIniFile.Create(FileName);
  try
    for I := Low(TInvByte) to High(TInvByte) do
    begin
      S := IntToStr(I);
      if F.SectionExists(S) then
      begin
        FItem[I].Active := True;
        FItem[I].LoadFromFile(FileName, S);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TPlayer.TInventory.SaveToFile(const FileName: string);
var
  S: string;
  F: TIniFile;
  I: TInvByte;
begin
  F := TIniFile.Create(FileName);
  try
    for I := Low(TInventory.TInvByte) to High(TInventory.TInvByte) do
    begin
      S := IntToStr(I);
      Item[I].SaveToFile(FileName, S);
      if not Item[I].Active and F.SectionExists(S) then
        F.EraseSection(S);
    end;
  finally
    F.Free;
  end;
end;

procedure TPlayer.TInventory.SetItem(I: TInvByte; const Value: TItem);
begin
  FItem[I] := Value;
end;

{$ENDREGION ' TPlayer.TInventor '}
{$ENDREGION ' TPlayer '}
{$REGION ' TEntities '}

procedure TEntities{$IFNDEF FPC} <T> {$ENDIF}.Clear;
begin
  Sections.Clear;
  inherited Clear;
end;

function TEntities{$IFNDEF FPC} <T> {$ENDIF}.Count: Integer;
begin
  Result := inherited Count;
end;

function TEntities{$IFNDEF FPC} <T> {$ENDIF}.Count(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (Entity[I].Active) and (Entity[I].Pos.X = X) and (Entity[I].Pos.Y = Y)
    then
      Inc(Result);
end;

constructor TEntities{$IFNDEF FPC} <T> {$ENDIF}.Create;
begin
  Sections := TStringList.Create;
end;

procedure TEntities{$IFNDEF FPC} <T> {$ENDIF}.Delete(const Index: Integer);
begin
  Entity[Index].Active := False;
end;

destructor TEntities{$IFNDEF FPC} <T> {$ENDIF}.Destroy;
begin
  Sections.Free;
  Self.Clear;
  inherited;
end;

function TEntities{$IFNDEF FPC} <T>
{$ENDIF}.GetIndex(SectionID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Sections.Count - 1 do
    if (Entity[I].Active) and (SectionID = Sections[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function TEntities{$IFNDEF FPC} <T>
{$ENDIF}.GetIndex(N, X, Y: Integer): Integer;
var
  I, J: Integer;
begin
  J := 0;
  Result := -1;
  for I := Count - 1 downto 0 do
    if (Entity[I].Active) and (Entity[I].Pos.X = X) and (Entity[I].Pos.Y = Y)
    then
    begin
      if (J = N) then
      begin
        Result := I;
        Break;
      end;
      Inc(J);
    end;
end;

function TEntities{$IFNDEF FPC} <T> {$ENDIF}.Has(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
    if (Entity[I].Active) and (Entity[I].Pos.X = X) and (Entity[I].Pos.Y = Y)
    then
    begin
      Result := I;
      Break;
    end;
end;

procedure TEntities{$IFNDEF FPC} <T> {$ENDIF}.Render;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Entity[I].Active then
      Entity[I].Render;
end;

{$ENDREGION ' TEntities '}
{$REGION ' TCreatures '}

constructor TCreatures.Create;
begin
  inherited;
end;

destructor TCreatures.Destroy;
begin
  inherited;
end;

procedure TCreatures.LoadFromFile(const FileName: string);
var
  I, L: Integer;
  F: TIniFile;
begin
  Self.Clear;
  F := TIniFile.Create(FileName);
  try
    F.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
    begin
      if (F.SectionExists(Sections[I])) then
      begin
        L := Count;
        SetEntitiesLength(L + 1);
        Entity[L] := TCreature.Create;
        Entity[L].Name := F.ReadString(Sections[I], 'Name', '');
        Entity[L].SetPosition(Point(F.ReadInteger(Sections[I], 'X', 0),
          F.ReadInteger(Sections[I], 'Y', 0)));
        Entity[L].Atr[atLife].Add(F.ReadString(Sections[I], 'Life',
          Format(TEntity.BarFmt, [100, 100])));
        Entity[L].Symbol := F.ReadString(Sections[I], 'Symbol', '?')[1];
        Entity[L].Color := F.ReadColor(Sections[I], 'Color', '255,255,255');
        Entity[L].Dialog := F.ReadInteger(Sections[I], 'Dialog', 0);
        Entity[L].Level := F.ReadInteger(Sections[I], 'Level', 1);
        Entity[L].ScriptFileName := F.ReadString(Sections[I], 'File', '');
        Entity[L].Force := TCreature.ForceValues
          [not F.ReadBool(Sections[I], 'NPC', False)];
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCreatures.SaveToFile(const FileName: string);
var
  I: Integer;
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try
    for I := 0 to Sections.Count - 1 do
      if (Entity[I].Active) then
      begin
        F.WriteString(Sections[I], 'Name', Entity[I].Name);
        F.WriteInteger(Sections[I], 'X', Entity[I].Pos.X);
        F.WriteInteger(Sections[I], 'Y', Entity[I].Pos.Y);
        F.WriteString(Sections[I], 'Life', Format(TEntity.BarFmt,
          [Entity[I].Atr[atLife].Cur, Entity[I].Atr[atLife].Max]));
        F.WriteInteger(Sections[I], 'Level', Entity[I].Level);
        F.WriteString(Sections[I], 'Symbol', Entity[I].Symbol);
        F.WriteString(Sections[I], 'File', Entity[I].ScriptFileName);
        F.WriteColor(Sections[I], 'Color', Entity[I].Color);
        F.WriteInteger(Sections[I], 'Dialog', Entity[I].Dialog);
        F.WriteBool(Sections[I], 'NPC', Entity[I].Force = fcAlly);
      end;
  finally
    F.Free;
  end;
end;

{$ENDREGION ' TCreatures '}
{$REGION ' TItems '}

procedure TItems.Add(Symbol: Char; Color, Level: Integer; Name: string;
  Material: TItem.TMaterial; Category: TItem.TCategory; Durability: Word;
  Amount: Word = 1);
var
  I: Integer;

  procedure SetItem(Index: Integer);
  begin
    Entity[Index].SetPosition(Saga.Player.Pos);
    Entity[Index].Name := Name;
    Entity[Index].Color := Color;
    Entity[Index].Symbol := Symbol;
    Entity[Index].Level := Level;
    Entity[Index].Count := Amount;
    Entity[Index].Material := Material;
    Entity[Index].Category := Category;
    Entity[Index].Calc;
    Entity[Index].Durability.Cur := Durability;
  end;

begin
  if (Amount = 0) then
    Exit;
  if (Count <> 0) then
    for I := 0 to Count - 1 do
      if not Entity[I].Active then
      begin
        Entity[I].Create();
        SetItem(I);
        Exit;
      end;
  SetEntitiesLength(Count + 1);
  Entity[Count - 1] := TItem.Create();
  SetItem(Count - 1);
end;

constructor TItems.Create;
begin
  inherited;
end;

destructor TItems.Destroy;
begin
  inherited;
end;

procedure TItems.LoadFromFile(const FileName: string);
var
  I, L: Integer;
  F: TIniFile;
begin
  Self.Clear;
  F := TIniFile.Create(FileName);
  try
    F.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
    begin
      if (F.SectionExists(Sections[I])) then
      begin
        L := Count;
        SetEntitiesLength(L + 1);
        Entity[L] := TItem.Create;
        Entity[L].Active := True;
        Entity[L].LoadFromFile(FileName, Sections[I]);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TItems.SaveToFile(const FileName: string);
var
  I, C: Integer;
  F: TIniFile;
begin
  C := 0;
  F := TIniFile.Create(FileName);
  try
    for I := 0 to Count - 1 do
      if (Entity[I].Active) then
      begin
        Entity[I].SaveToFile(FileName, IntToStr(C));
        Inc(C);
      end;
  finally
    F.Free;
  end;
end;

function TItems.ToText(Item: TItem): string;
begin
  Result := '';
  if (Item.Count = 1) then
    Result := Format('(' + TEntity.BarFmt + ')',
      [Item.Durability.Cur, Item.Durability.Max])
  else if (Item.Count > 1) then
    Result := Format('(%dx)', [Item.Count]);
  Result := Format(TEntity.KeyFmt + ' %s', [Item.Symbol, __(Item.Name),
    Result]);
end;

procedure TItems.Pickup(I: Integer);
var
  Item: TItem;
begin
  Item := Saga.World.CurrentItems.Entity[I];
  if Saga.Player.Inventory.AddItem(Item) then
  begin
    Saga.Log[lgGame].Add(Format(__('You pick up a %s.'), [Item.Name]));
    Saga.World.CurrentItems.Delete(I);
  end;
end;

{$ENDREGION ' TItems '}
{$REGION ' TTiles '}

function TTiles.GetTile(Tile: TTileEnum): TTileProp;
begin
  Result := FTiles[Tile];
end;

procedure TTiles.LoadFromFile(FileName: string);
var
  I: TTileEnum;
  S: string;
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try
    for I := Low(TTileEnum) to High(TTileEnum) do
    begin
      S := TileStr[I];
      if F.SectionExists(S) then
      begin
        FTiles[I].Name := F.ReadString(S, 'Name', '');
        FTiles[I].Symbol := F.ReadString(S, 'Symbol', '?')[1];
        FTiles[I].Passable := F.ReadBool(S, 'Passable', False);
        FTiles[I].Color := F.ReadColor(S, 'Color', '100,100,100');
        FTiles[I].Layer := TLayerTypeEnum(F.ReadInteger(S, 'Layer', 0));
      end;
    end;
  finally
    F.Free;
  end;
end;

{$ENDREGION ' TTiles '}
{$REGION ' TMap '}

function TMap.CellInMap(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height);
end;

constructor TMap.Create;
begin
  Self.Width := Self.Size.Width;
  Self.Height := Self.Size.Height;
  Clear;
end;

destructor TMap.Destroy;
begin
  inherited;
end;

procedure TMap.LoadFromFile(const FileName: string);
var
  Z: TLayerEnum;
  I: Integer;
  X, Y: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  L.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  for Z := Low(TLayerEnum) to High(TLayerEnum) do
  begin
    I := L.IndexOf(Format('[%d]', [Ord(Z)])) + 1;
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
      begin
        FMap[Y][X][Z] := TTiles.TTileEnum(Ord(L[Y + I][X + 1]) - Offset);
        if Assigned(Saga) then
          if (FMap[Y][X][Z] > tNone) and not Saga.Tiles.GetTile(FMap[Y][X][Z]).Passable
          then
            MapFOV[Y][X] := FOV_CELL_OPAQUE;
      end;
  end;
  L.Free;
end;

procedure TMap.SaveToFile(const FileName: string);
var
  Z: TLayerEnum;
  X, Y: Integer;
  L: TStringList;
  S: string;
begin
  L := TStringList.Create;
{$IFNDEF FPC}L.WriteBOM := False; {$ENDIF}
  L.Append(Format('; %s', [ExtractFileName(FileName)]));
  for Z := Low(TLayerEnum) to High(TLayerEnum) do
  begin
    L.Append(Format('[%d]', [Ord(Z)]));
    for Y := 0 to Height - 1 do
    begin
      S := '';
      for X := 0 to Width - 1 do
        S := S + Chr(Ord(FMap[Y][X][Z]) + Offset);
      L.Append(S);
    end;
  end;
  L.SaveToFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  L.Free;
end;

procedure TMap.Render;
var
  Z: TLayerEnum;
  X, Y: Integer;
  Color: Integer;
  HasObj: Boolean;
  Tile: record Ter: TTiles.TTileProp;
  Obj: TTiles.TTileProp;
end;

begin
  ClearFOV();
  DoFOV(Self.Width, Self.Height, Saga.Player.Pos.X, Saga.Player.Pos.Y,
    Saga.Player.Radius, @GetFOVCallback, @SetFOVCallback, nil);
  for Z := Low(TLayerEnum) to High(TLayerEnum) do
    for Y := 0 to Self.Height - 1 do
      for X := 0 to Self.Width - 1 do
      begin
        HasObj := HasTile(tNone, X, Y, lrObjects);
        Tile.Ter := Saga.Tiles.GetTile(FMap[Y][X][lrTerrain]);
        Tile.Ter.Color := IfThen(MapFOV[Y][X] and FOV_CELL_VISIBLE > 0,
          Tile.Ter.Color, $00111111);
        case Z of
          lrTerrain:
            if HasObj then
              Saga.UI.DrawChar(X, Y, Tile.Ter.Symbol, Tile.Ter.Color,
                Saga.Engine.DarkColor(Tile.Ter.Color, TTiles.TileDarkPercent));
          lrObjects:
            if not HasObj then
            begin
              Tile.Obj := Saga.Tiles.GetTile(FMap[Y][X][Z]);
              Tile.Obj.Color := IfThen(MapFOV[Y][X] and FOV_CELL_VISIBLE > 0,
                Tile.Obj.Color, $00222222);
              Saga.UI.DrawChar(X, Y, Tile.Obj.Symbol, Tile.Obj.Color,
                Saga.Engine.DarkColor(Tile.Ter.Color, TTiles.TileDarkPercent));
            end;
        end;
      end;
end;

procedure TMap.FillLayer(Z: TLayerEnum; Tile: TTiles.TTileEnum);
var
  X, Y: Integer;
begin
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      FMap[Y][X][Z] := Tile;
end;

procedure TMap.ClearLayer(Z: TLayerEnum);
begin
  FillLayer(Z, tNone);
end;

function TMap.Count: Integer;
begin
  Result := 0;
end;

procedure TMap.Clear;
var
  Z: TLayerEnum;
begin
  for Z := Low(TLayerEnum) to High(TLayerEnum) do
    ClearLayer(Z);
  ClearFOV();
end;

procedure TMap.ClearFOV;
var
  X, Y: Integer;
begin
  for Y := 0 to Self.Height - 1 do
    for X := 0 to Self.Width - 1 do
      MapFOV[Y][X] := (MapFOV[Y][X] and (not FOV_CELL_VISIBLE));
end;

function TMap.HasCellVisible(X, Y: Integer): Boolean;
begin
  Result := MapFOV[Y][X] and FOV_CELL_VISIBLE > 0;
end;

function TMap.HasTile(Tile: TTiles.TTileEnum; X, Y: Integer;
  Z: TLayerEnum = lrTerrain): Boolean;
begin
  Result := FMap[Y][X][Z] = Tile
end;

procedure TMap.SetTile(X, Y: Integer; Z: TLayerEnum; Tile: TTiles.TTileEnum);
begin
  if CellInMap(X, Y) then
    FMap[Y][X][Z] := Tile;
end;

function TMap.GetTile(X, Y: Integer; Z: TLayerEnum): TTiles.TTileEnum;
begin
  Result := FMap[Y][X][Z];
end;

function TMap.GetTopTileChar(X, Y: Integer): Char;
var
  N: Integer;
  Z: TLayerEnum;
begin
  Result := ' ';
  for Z := High(TLayerEnum) downto Low(TLayerEnum) do
    if (GetTile(X, Y, Z) <> tNone) then
    begin
      Result := Saga.Tiles.GetTile(GetTile(X, Y, Z)).Symbol;
      N := Saga.World.CurrentItems.Has(X, Y);
      if (N > -1) then
        Result := Saga.World.CurrentItems.GetEntity(N).Symbol;
      N := Saga.World.CurrentCreatures.Has(X, Y);
      if (N > -1) then
        Result := Saga.World.CurrentCreatures.GetEntity(N).Symbol;
      if (Saga.Player.Has(X, Y)) then
        Result := '@';
      Exit;
    end;
end;

procedure TMap.Gen;
begin
  // GenTestMap();
  // GenOrcVillage();
  // GenDarkCave();
end;

{$ENDREGION ' TMap '}
{$REGION ' TMapGenerator '}

procedure TMapGenerator.AddSpot(Tile: TTiles.TTileEnum; Layer: TMap.TLayerEnum);
var
  I, X, Y, K, L: Integer;
begin
  K := Math.RandomRange(0, Map.Width);
  L := Math.RandomRange(0, Map.Height);
  X := K;
  Y := L;
  for I := 1 to Self.Num do
  begin
    X := X + Math.RandomRange(0, 3) - 1;
    Y := Y + Math.RandomRange(0, 3) - 1;
    Map.SetTile(X, Y, Layer, Tile);
  end;
end;

procedure TMapGenerator.GenCave();
var
  I: Integer;
  kx, ky, K, dx, dy: real;
  X, Y, py, px: Integer;
  Counter: Integer;
begin
  X := TMap.Size.Width;
  Y := TMap.Size.Height;
  Map.SetTile(Start.X, Start.Y, lrTerrain, Self.MGTiles.Floor);
  for I := 0 to (X * Y div Num) do
  begin
    try
      K := (Random(360) + 1) * 3.14159 / 180;
      kx := (X / 3) + (Y / 3) * cos(K);
      ky := (Y / 3) + (Y / 3) * sin(K);
      dx := 1;
      dy := 1;
      while ((abs(dx) < 10) and (abs(dy) < 10)) do
      begin
        dx := Random(100) + 1;
        dy := Random(100) + 1;
      end;
      dx := dx - 50;
      dy := dy - 50;
      dx := dx / 50;
      dy := dy / 50;
      Counter := 0;
      while (True) do
      begin
        if Counter + 1 > 10000 then
          Break;
        Counter := Counter + 1;
        kx := kx + dx;
        ky := ky + dy;
        px := Round(kx);
        py := Round(ky);
        if (px < 0) then
        begin
          px := X;
          kx := px;
        end;
        if (px > X) then
        begin
          px := 1;
          kx := px;
        end;
        if (py < 0) then
        begin
          py := Y;
          ky := py;
        end;
        if (py > Y) then
        begin
          py := 1;
          ky := py;
        end;
        if (px = 0) then
          px := Random(X) + 1;
        if (py = 0) then
          py := Random(Y) + 1;
        if ((px > 1) and (Map.HasTile(Self.MGTiles.Floor, px - 1, py))) or
          ((py > 1) and (Map.HasTile(Self.MGTiles.Floor, px, py - 1))) or
          ((px < X) and (Map.HasTile(Self.MGTiles.Floor, px + 1, py))) or
          ((py < Y) and (Map.HasTile(Self.MGTiles.Floor, px, py + 1))) then
          if (px <> 0) and (px <> TMap.Size.Width - 1) and (py <> 0) and
            (py <> TMap.Size.Height - 1) then
          begin
            Map.SetTile(px, py, lrTerrain, Self.MGTiles.Floor);
            Break;
          end;
      end;
    except
    end;
  end;
  // Map.SetTile(Start.X, Start.Y, lrTerrain, tStUp);
end;

constructor TMapGenerator.Create;
begin
  // Self.FMGTiles.Wall := tStoneWall;
  // Self.FMGTiles.Floor := tStoneFloor;
end;

destructor TMapGenerator.Destroy;
begin

  inherited;
end;

{$ENDREGION ' TMapGenerator '}
{$REGION ' TGenericEntities '}

procedure TGenericEntities{$IFNDEF FPC}<T>{$ENDIF}.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FEntity[I].Free;
  SetLength(FEntity, 0);
end;

function TGenericEntities{$IFNDEF FPC}<T>{$ENDIF}.Count: Integer;
begin
  Result := Length(FEntity);
end;

function TGenericEntities{$IFNDEF FPC}<T>{$ENDIF}.GetEntity(Index: Integer): T;
begin
  Result := FEntity[Index];
end;

procedure TGenericEntities{$IFNDEF FPC}<T>{$ENDIF}.SetEntitiesLength
  (NewLength: Integer);
begin
  SetLength(FEntity, NewLength);
end;

procedure TGenericEntities{$IFNDEF FPC}<T>{$ENDIF}.SetEntity(Index: Integer;
  const Value: T);
begin
  FEntity[Index] := Value;
end;

{$ENDREGION ' TGenericEntities '}

end.
