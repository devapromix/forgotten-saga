unit ForgottenSaga.Classes;

interface

{$I Include.inc}

uses Classes, Types, IniFiles, Engine, ForgottenSaga.Scenes,
  ForgottenSaga.Entities;

{$REGION ' TUtils '}
function __(S: string): string;

type
  TUtils = class(TObject)
    class procedure Box(); overload;
    class procedure Box(const S: string); overload;
    class procedure Box(const I: Integer); overload;
    class function ExplodeString(const Separator, Source: string): TStringList;
    class function GetStr(const Separator: Char; S: string; I: Integer): string;
    class function RandStr(const Separator: Char; S: string): string;
    class function GetPath(SubDir: string = ''): string;
  end;

{$ENDREGION ' TUtils '}
{$REGION ' TUI '}

type
  TUI = class(TObject)
  public const
    PanelWidth = 40;
  strict private
    FEngine: TEngine;
  public
    constructor Create(AEngine: TEngine);
    procedure DrawTitle(Y: Word; Text: string);
    procedure DrawChar(X, Y: Integer; Symbol: System.Char;
      ForegroundColor, BackgroundColor: Integer);
    procedure DrawKey(X, Y: Integer; Caption: string; Key: string;
      Active: Boolean = True); overload;
    procedure DrawKey(X, Y: Integer; Caption: string; Key: string;
      Align: TEngine.TAlign; Active: Boolean = True); overload;
    property Engine: TEngine read FEngine write FEngine;
  end;

{$ENDREGION ' TUI '}
{$REGION ' TIniFile '}

type
  TIniFile = class(IniFiles.TIniFile)
  public
    function ReadCategory(Section, Ident: string; DefaultValue: TItem.TCategory)
      : TItem.TCategory;
    procedure WriteCategory(Section, Ident: string; Value: TItem.TCategory);
    function ReadColor(Section, Ident: string; DefaultValue: string): Integer;
    procedure WriteColor(Section, Ident: string; Value: Integer);
    function ReadMaterial(Section, Ident: string; DefaultValue: TItem.TMaterial)
      : TItem.TMaterial;
    procedure WriteMaterial(Section, Ident: string; Value: TItem.TMaterial);
  end;

{$ENDREGION ' TIniFile '}
{$REGION ' TScript '}

type
  TScript = class(TObject)
{$REGION ' TScript.TVars '}
  strict private
  type
    TVars = class(TInterfacedObject, IStorage)
    strict private
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
{$ENDREGION ' TScript.TVars '}
  strict private
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

{$ENDREGION ' TScript '}
{$REGION ' TLanguage '}

type
  TLanguage = class(TObject)
  strict private
    FID: TStringList;
    FValue: TStringList;
    FCurrent: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(FileName: string);
    function Get(S: string): string;
    procedure SetLanguage(CurrentLanguage: string);
    property Current: string read FCurrent write FCurrent;
  end;

{$ENDREGION ' TLanguage '}
{$REGION ' TColors '}

type
  TColors = class(TObject)
  public type
    TColorsEnum = (ceBlack, ceBlue, ceGreen, ceCyan, ceRed, ceMagenta, ceBrown,
      ceLGray, ceDGray, ceLBlue, ceLGreen, ceLCyan, ceLRed, ceLMagenta,
      ceYellow, ceWhite);
  strict private
  const
    ColorsStr: array [TColorsEnum] of string = ('BLACK', 'BLUE', 'GREEN',
      'CYAN', 'RED', 'MAGENTA', 'BROWN', 'LGRAY', 'DGRAY', 'LBLUE', 'LGREEN',
      'LCYAN', 'LRED', 'LMAGENTA', 'YELLOW', 'WHITE');
  public
  var
    clNotification: Integer;
    clTitle: Integer;
    clHotKey: Integer;
    clButton: Integer;
    clSplText: Integer;
    clGoldText: Integer;
    clAlertText: Integer;
    clMenuAct: Integer;
    clMenuDef: Integer;
    clCursor: Integer;
  strict private
    FColors: array [TColorsEnum] of Integer;
    procedure SetColors;
  public
    procedure LoadFromFile(FileName: string);
    function GetColor(Color: TColorsEnum): Integer; overload;
    function GetColor(Color: string): Integer; overload;
  end;

{$ENDREGION ' TColors '}
{$REGION ' TConfig '}

type
  TConfig = class(TObject)
  strict private

  public
    procedure LoadFromFile(FileName: string);
  end;

{$ENDREGION ' TConfig '}
{$REGION ' TNotification '}

type
  TNotification = class(TObject)
  strict private
    FMessage: string;
    FDuration: Byte;
    FCounter: Byte;
  public
    constructor Create(Duration: Byte = 5);
    procedure Add(S: string);
    procedure Render(Left, Top: Word);
    property Duration: Byte read FDuration write FDuration;
    property Counter: Byte read FCounter write FCounter;
    procedure Dec;
  end;

{$ENDREGION ' TNotification '}
{$REGION ' TWorld '}

type
  TWorld = class(TObject)
  private const
    FMFmt = '%d,';
  strict private
    FMaps: array of TMap;
    FCreatures: array of TCreatures;
    FItems: array of TItems;
    FEngine: TEngine;
    Sections: TStringList;
    function FileName(Dir: string; ID: Byte; Ext: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMap(I: Byte): TMap;
    function GetMapCreatures(I: Byte): TCreatures;
    function GetMapItems(I: Byte): TItems;
    function CurrentMap: TMap;
    function CurrentCreatures: TCreatures;
    function CurrentItems: TItems;
    function Count: Byte;
    function GetMapIndex(MapSectionID: string): Integer;
    procedure SaveToDir(Dir: string);
    procedure LoadFromDir(Dir: string);
    procedure Gen(I: Byte);
    property Engine: TEngine read FEngine write FEngine;
    class function GoLoc(Dir: TMap.TDir): Boolean;
  end;

{$ENDREGION ' TWorld '}
{$REGION ' TRecs '}

type
  TRecs = class(TObject)
  strict private
    FFileName: string;
    procedure Add(Slot: Byte);
  public
    constructor Create(FileName: string);
    procedure Load();
    procedure Save();
  end;

{$ENDREGION ' TRecs '}
{$REGION ' TBattle '}

type
  TBattle = class(TObject)
  strict private
    FID: Integer;
    function EnemyName(): string;
  public
    procedure EnemyMove();
    procedure Finish();
    procedure PlayerMove();
    procedure Start(ID: Byte);
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
  end;

{$ENDREGION ' TBattle '}
{$REGION ' TQuest '}

type
  TQuest = class(TInterfacedObject, IStorage)
  strict private
    FList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Get(Slot, N: Byte): string;
    procedure Replace(Slot, N: Byte; S: string);
    procedure Add(Slot: Byte; Data: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Count: Integer;
  end;

{$ENDREGION ' TQuest '}
{$REGION ' TSaga '}

type
  TSaga = class(TObject)
{$REGION ' TSaga.TRace '}
  public type
    TRace = class(TObject)
    strict private
      FColor: Integer;
      FName: string;
      FPos: TPoint;
      FMap: Byte;
      FLife: Word;
      FMana: Word;
    public
      property Color: Integer read FColor write FColor;
      property Name: string read FName write FName;
      property Pos: TPoint read FPos write FPos;
      property Map: Byte read FMap write FMap;
      property Life: Word read FLife write FLife;
      property Mana: Word read FMana write FMana;
    end;
{$ENDREGION ' TSaga.TRace '}
{$REGION ' TSaga.TLog '}
  public type
    TLog = class(TObject)
    strict private
      FLogStr: string;
      FLen: Word;
    public
      constructor Create(Len: Word);
      procedure Clear;
      procedure ClearTags;
      procedure Add(Text: string; Flag: Boolean = True);
      function Get: string;
      procedure LoadFromFile(FileName: string);
      procedure SaveToFile(FileName: string);
      procedure Render(Left, Top, Width: Word);
    end;
{$ENDREGION ' TSaga.TLog '}
  public type
    TRaceEnum = (rcGoblin, rcOrc, rcTroll);
    TLogEnum = (lgGame, lgIntro, lgDialog, lgBattle, lgQuest);
  public const
    RaceName: array [TRaceEnum] of string =
      ('Avgu,Leo,Tan,Sho,Penr,Lok,Gron,Lar,Midr|sin,neg,zar,kar,tun,rel,bal,rin,kon|or,fin,shog,tal,rod,pin,ol,kan,on',
      'Had,Rod,Shag,Dor,Lid,Tar,Kreg,Bron,Shung|Garum,Turum,Ur,Utak,Udoom,Ud,Urak,Doon,Vuug|Kat,Shak,Gir,Bood,Dreg,Din,Grok,Rig,Sadr',
      'Blind,Glad,Proud,Sharp-sighted,Powerful,Dancer,Guarding,Thunderous,Night|Wolfhound,Wood-goblin,Destroyer,Crusher,Pathfinder,Astrologer,Bootes,Caretaker,Befouler');
    RaceNameDiv: array [TRaceEnum] of string = ('', '-', ' ');
  strict private
    FList: TStringList;
    FPlayer: TPlayer;
    FStages: TStages;
    FWorld: TWorld;
    FBattle: TBattle;
    FEngine: TEngine;
    FTiles: TTiles;
    FQuest: TQuest;
    FRecs: TRecs;
    FLg: TLanguage;
    FTUI: TUI;
    FColors: TColors;
    FNotification: TNotification;
    FLog: array [TLogEnum] of TLog;
    FRace: array [TRaceEnum] of TRace;
    FDialog: TScript;
  strict protected
    function GetLog(I: TLogEnum): TLog;
    procedure SetLog(I: TLogEnum; const Value: TLog);
    function GetRace(I: TRaceEnum): TRace;
    procedure SetRace(I: TRaceEnum; const Value: TRace);
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure New();
    procedure Init;
    procedure InitSlots;
    procedure ClearLogs;
    procedure ClearSlots;
    procedure AddRace(ID: TRaceEnum; Name: string; Life, Mana: Word;
      Pos: TPoint; Map: Byte; Color: Integer);
    function GetSlotData(Slot: Byte): string;
    function GetSlotPath(Slot: Byte): string;
    function LoadFromSlot(Slot: Byte): Boolean;
    procedure SaveToSlot(Slot: Byte);
    property Player: TPlayer read FPlayer write FPlayer;
    property Stages: TStages read FStages write FStages;
    property World: TWorld read FWorld write FWorld;
    property Battle: TBattle read FBattle write FBattle;
    property Engine: TEngine read FEngine write FEngine;
    property Tiles: TTiles read FTiles write FTiles;
    property Quest: TQuest read FQuest write FQuest;
    property Log[I: TLogEnum]: TLog read GetLog write SetLog;
    property Race[I: TRaceEnum]: TRace read GetRace write SetRace;
    property Notification: TNotification read FNotification write FNotification;
    property List: TStringList read FList write FList;
    property Recs: TRecs read FRecs write FRecs;
    property Lg: TLanguage read FLg write FLg;
    property UI: TUI read FTUI write FTUI;
    property Colors: TColors read FColors write FColors;
    property Dialog: TScript read FDialog write FDialog;
  end;

{$ENDREGION ' TSaga '}

var
  Saga: TSaga;

implementation

uses Math, SysUtils, Dialogs;

{$REGION ' TUtils '}

function __(S: string): string;
begin
  Result := '?';
  if (S = '') then
    Exit;
  Result := Saga.Lg.Get(S);
end;

class function TUtils.RandStr(const Separator: Char; S: string): string;
var
  SL: TStringList;
begin
  SL := ExplodeString(Separator, S);
  Result := Trim(SL[Math.RandomRange(0, SL.Count)]);
end;

class procedure TUtils.Box();
begin
  ShowMessage('');
end;

class procedure TUtils.Box(const S: string);
begin
  ShowMessage(S);
end;

class procedure TUtils.Box(const I: Integer);
begin
  ShowMessage(Format('%d', [I]));
end;

class function TUtils.ExplodeString(const Separator, Source: string)
  : TStringList;
begin
  Result := TStringList.Create();
  Result.Text := StringReplace(Source, Separator, #13, [rfReplaceAll]);
end;

class function TUtils.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

class function TUtils.GetStr(const Separator: Char; S: string;
  I: Integer): string;
var
  SL: TStringList;
begin
  SL := ExplodeString(Separator, S);
  Result := Trim(SL[I]);
end;

{$ENDREGION ' TUtils '}
{$REGION ' TWorld '}

function TWorld.Count: Byte;
begin
  Result := Length(FMaps);
end;

constructor TWorld.Create;
var
  I, C: Integer;
  F: TIniFile;
begin
  F := TIniFile.Create(TUtils.GetPath('resources') + 'world.ini');
  try
    Sections := TStringList.Create;
    F.ReadSections(Sections);
    C := Sections.Count;
    SetLength(FMaps, C);
    SetLength(FCreatures, C);
    SetLength(FItems, C);
    for I := 0 to Count - 1 do
    begin
      FMaps[I] := TMap.Create;
      FCreatures[I] := TCreatures.Create;
      FItems[I] := TItems.Create;
      FMaps[I].FileName := Sections[I];
      FMaps[I].Name := F.ReadString(Sections[I], 'Name', '');
      FMaps[I].Level := F.ReadInteger(Sections[I], 'Level', 0);
      FMaps[I].MapNeighbors[drLeft] := F.ReadString(Sections[I], 'Left', '');
      FMaps[I].MapNeighbors[drUp] := F.ReadString(Sections[I], 'Up', '');
      FMaps[I].MapNeighbors[drRight] := F.ReadString(Sections[I], 'Right', '');
      FMaps[I].MapNeighbors[drDown] := F.ReadString(Sections[I], 'Down', '');
      FMaps[I].MapNeighbors[drTop] := F.ReadString(Sections[I], 'Top', '');
      FMaps[I].MapNeighbors[drBottom] := F.ReadString(Sections[I],
        'Bottom', '');
    end;
  finally
    F.Free;
  end;
end;

function TWorld.CurrentCreatures: TCreatures;
begin
  Result := FCreatures[Saga.Player.Map];
end;

function TWorld.CurrentMap: TMap;
begin
  Result := FMaps[Saga.Player.Map];
end;

function TWorld.CurrentItems: TItems;
begin
  Result := FItems[Saga.Player.Map];
end;

destructor TWorld.Destroy;
var
  I: Byte;
begin
  for I := 0 to Count - 1 do
  begin
    FMaps[I].Free;
    FCreatures[I].Free;
    FItems[I].Free;
  end;
  SetLength(FMaps, 0);
  SetLength(FCreatures, 0);
  SetLength(FItems, 0);
  Sections.Free;
  inherited;
end;

function TWorld.GetMapIndex(MapSectionID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Sections.Count - 1 do
    if (MapSectionID = Sections[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function TWorld.GetMap(I: Byte): TMap;
begin
  Result := FMaps[I];
end;

function TWorld.GetMapCreatures(I: Byte): TCreatures;
begin
  Result := FCreatures[I];
end;

function TWorld.GetMapItems(I: Byte): TItems;
begin
  Result := FItems[I];
end;

function TWorld.FileName(Dir: string; ID: Byte; Ext: string): string;
begin
  Result := Dir + FMaps[ID].FileName + Ext;
end;

procedure TWorld.LoadFromDir(Dir: string);
var
  I: Byte;
begin
  for I := 0 to Count - 1 do
  begin
    FMaps[I].LoadFromFile(FileName(Dir, I, '.map'));
    FCreatures[I].LoadFromFile(FileName(Dir, I, '.crt'));
    FItems[I].LoadFromFile(FileName(Dir, I, '.itm'));
  end;
end;

procedure TWorld.SaveToDir(Dir: string);
var
  I: Byte;
begin
  for I := 0 to Count - 1 do
  begin
    FMaps[I].SaveToFile(FileName(Dir, I, '.map'));
    FCreatures[I].SaveToFile(FileName(Dir, I, '.crt'));
    FItems[I].SaveToFile(FileName(Dir, I, '.itm'));
  end;
end;

class function TWorld.GoLoc(Dir: TMap.TDir): Boolean;
var
  MapID: string;
  I, E: Integer;
  S, N: string;
begin
  Result := False;
  MapID := Saga.World.CurrentMap.MapNeighbors[Dir];
  if (MapID <> '') then
  begin
    I := Saga.World.GetMapIndex(MapID);
    if (I < 0) then
      Exit;
    begin
      S := Format(FMFmt, [I]);
      N := __(Saga.World.GetMap(I).Name);
      E := Saga.World.GetMap(I).Level + Round(Saga.Player.Level * 1.25);
      Saga.Log[lgGame].Add(Format(__('You walked in <RED>%s.</>'), [N]));
      if (Pos(S, Saga.Player.Maps) <= 0) then
      begin
        if (I > 0) then
        begin
          Saga.Log[lgGame].Add(Format('Открыта новая территория: %s. Опыт: +%d.', [N, E]));
          Saga.Player.Maps := Saga.Player.Maps + S;
          Saga.Player.AddExp(E);
        end;
      end;
      Saga.Player.Map := I;
      Result := True;
    end;
  end;
end;

procedure TWorld.Gen(I: Byte);
begin
  FMaps[I].Gen;
  FMaps[I].SaveToFile(TUtils.GetPath('resources') + '0.map');
end;

{$ENDREGION ' TWorld '}
{$REGION ' TSaga '}

constructor TSaga.Create(AWidth, AHeight: Integer);
const
  LogLen: array [TLogEnum] of Integer = (500, 1000, 1000, 1400, 1600);
var
  I: Byte;
  L: TLogEnum;
  R: TRaceEnum;
begin
  FEngine := TEngine.Create(AWidth, AHeight);
  FTUI := TUI.Create(FEngine);
  FList := TStringList.Create;
{$IFNDEF FPC}FList.WriteBOM := False; {$ENDIF}
  ClearSlots;
  ForceDirectories(TUtils.GetPath('save'));
  for I := 0 to 9 do
    ForceDirectories(GetSlotPath(I));

  for L := Low(TLogEnum) to High(TLogEnum) do
    Self.Log[L] := TLog.Create(LogLen[L]);

  Lg := TLanguage.Create;
  Lg.SetLanguage('russian');

  FPlayer := TPlayer.Create;

  FStages := TStages.Create;

  FWorld := TWorld.Create;

  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Race[R] := TRace.Create;

  FColors := TColors.Create;

  FBattle := TBattle.Create;

  FQuest := TQuest.Create;

  Tiles := TTiles.Create;
  Tiles.LoadFromFile(TUtils.GetPath('resources') + 'terrain.ini');

  FDialog := TScript.Create;

  FNotification := TNotification.Create();

  FRecs := TRecs.Create(TUtils.GetPath('save') + 'records.txt');

  Stages.SetStage(stMainMenu);
end;

procedure TSaga.Init;
var
  S: TStringList;
  F: string;
  I: Integer;
begin
  // Load intro
  S := TStringList.Create;
  try
    F := TUtils.GetPath('resources') + Lg.Current + '.intro.txt';
    if (FileExists(F)) then
    begin
      S.LoadFromFile(F{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
      for I := 0 to S.Count - 1 do
        Log[lgIntro].Add(__(S[I]), False);
    end;
  finally
    S.Free;
  end;

  // Colors
  Colors.LoadFromFile(TUtils.GetPath('resources') + 'colors.ini');

  // Races
  AddRace(rcGoblin, __('Goblin'), 90, 100, Point(40, 20), 0, $00882295);
  AddRace(rcOrc, __('Orc'), 120, 100, Point(40, 20), 0, $0063C3C6);
  AddRace(rcTroll, __('Troll'), 150, 100, Point(40, 20), 0, $00664567);
end;

destructor TSaga.Destroy;
var
  R: TRaceEnum;
  L: TLogEnum;
begin
  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Race[R].Free;
  for L := Low(TLogEnum) to High(TLogEnum) do
    Self.Log[L].Free;
  FNotification.Free;
  FLg.Free;
  FTUI.Free;
  FColors.Free;
  FRecs.Free;
  FList.Free;
  FPlayer.Free;
  FStages.Free;
  FWorld.Free;
  FBattle.Free;
  FEngine.Free;
  FDialog.Free;
  FTiles.Free;
  FQuest.Free;
  inherited;
end;

procedure TSaga.ClearSlots;
var
  I: Byte;
begin
  FList.Clear;
  for I := 0 to 9 do
    FList.Append('');
end;

function TSaga.GetSlotPath(Slot: Byte): string;
begin
  Result := IncludeTrailingPathDelimiter
    (Format('%s%d', [TUtils.GetPath('save'), Slot]));
end;

function TSaga.GetLog(I: TLogEnum): TLog;
begin
  Result := FLog[I];
end;

function TSaga.GetRace(I: TRaceEnum): TRace;
begin
  Result := FRace[I];
end;

function TSaga.GetSlotData(Slot: Byte): string;
begin
  Result := FList[Slot];
end;

procedure TSaga.New;
begin
  Player.Clear;
  ClearLogs;
  World.LoadFromDir(TUtils.GetPath('resources'));
  // World.Gen(0); // Пока вместо редактора
  Stages.SetStage(stGame);
  Notification.Add('Создан новый мир');
end;

function TSaga.LoadFromSlot(Slot: Byte): Boolean;
begin
  Result := False;
  if not FileExists(GetSlotPath(Slot) + 'game.log') then
    Exit;
  Player.LoadFromFile(GetSlotPath(Slot) + 'player.crt');
  Log[lgGame].LoadFromFile(GetSlotPath(Slot) + 'game.log');
  Quest.LoadFromFile(GetSlotPath(Slot) + 'quest.log');
  World.LoadFromDir(GetSlotPath(Slot));
  Dialog.Vars.LoadFromFile(GetSlotPath(Slot) + 'vars.txt');
  Result := True;
  if Result then
    Notification.Add('Игра успешно загружена');
end;

procedure TSaga.SaveToSlot(Slot: Byte);
begin
  Player.SaveToFile(GetSlotPath(Slot) + 'player.crt');
  Log[lgGame].SaveToFile(GetSlotPath(Slot) + 'game.log');
  Self.Quest.SaveToFile(GetSlotPath(Slot) + 'quest.log');
  World.SaveToDir(GetSlotPath(Slot));
  Dialog.Vars.SaveToFile(GetSlotPath(Slot) + 'vars.txt');
  FList[Slot] := Format('%s %s - %s', [DateTimeToStr(Now), Player.GetFullName,
    __(World.GetMap(Player.Map).Name)]);
  FList.SaveToFile(TUtils.GetPath('save') + 'list.txt'{$IFNDEF FPC},
    TEncoding.UTF8{$ENDIF});
  Notification.Add('Игра успешно сохранена');
end;

procedure TSaga.SetLog(I: TLogEnum; const Value: TLog);
begin
  FLog[I] := Value;
end;

procedure TSaga.SetRace(I: TRaceEnum; const Value: TRace);
begin
  FRace[I] := Value;
end;

procedure TSaga.AddRace(ID: TRaceEnum; Name: string; Life, Mana: Word;
  Pos: TPoint; Map: Byte; Color: Integer);
begin
  Race[ID].Color := Color;
  Race[ID].Name := Name;
  Race[ID].Life := Life;
  Race[ID].Mana := Mana;
  Race[ID].Pos := Pos;
  Race[ID].Map := Map;
end;

procedure TSaga.InitSlots;
var
  F: string;
begin
  ClearSlots;
  F := TUtils.GetPath('save') + 'list.txt';
  if FileExists(F) then
    FList.LoadFromFile(F{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
end;

procedure TSaga.ClearLogs;
begin
  Saga.Quest.Clear;
  Saga.Log[lgGame].Clear;
end;

{$REGION ' TSaga.TLog '}

procedure TSaga.TLog.Add(Text: string; Flag: Boolean = True);
begin
  Text := Trim(Text);
  if (Text = '') then
    Exit;
  ClearTags;
  if Flag then
    FLogStr := Text + ' ' + FLogStr
  else
    FLogStr := FLogStr + ' ' + Text;
  if (TEngine.GetTextLength(FLogStr) > FLen) then
  begin
    Delete(FLogStr, FLen, TEngine.GetTextLength(FLogStr));
    FLogStr := FLogStr + '...';
  end;
end;

procedure TSaga.TLog.Clear;
begin
  FLogStr := '';
end;

procedure TSaga.TLog.ClearTags;
var
  I: Integer;
  F: Boolean;
begin
  F := False;
  for I := 1 to TEngine.GetTextLength(FLogStr) do
  begin
    if (FLogStr[I] = '[') then
      F := True;
    if (FLogStr[I] = ']') then
    begin
      FLogStr[I] := '/';
      F := False;
    end;
    if F then
      FLogStr[I] := '/';
  end;
  FLogStr := StringReplace(FLogStr, '/', '', [rfReplaceAll]);
end;

constructor TSaga.TLog.Create(Len: Word);
begin
  FLen := Len;
  Clear;
end;

function TSaga.TLog.Get: string;
begin
  Result := FLogStr;
end;

procedure TSaga.TLog.LoadFromFile(FileName: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
    FLogStr := S.Text;
  finally
    S.Free;
  end;
end;

procedure TSaga.TLog.SaveToFile(FileName: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
{$IFNDEF FPC}S.WriteBOM := False; {$ENDIF}
    S.Text := FLogStr;
    S.SaveToFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  finally
    S.Free;
  end;
end;

procedure TSaga.TLog.Render(Left, Top, Width: Word);
begin
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  Saga.Engine.Print(Self.Get, Rect(Left, Top, Width, 0));
end;

{$ENDREGION ' TLog '}
{$ENDREGION ' TSaga '}
{$REGION ' TQuest '}

procedure TQuest.Add(Slot: Byte; Data: string);
var
  S: string;
begin
  if (FList[Slot] = '') then
    S := ''
  else
    S := '|';
  FList[Slot] := FList[Slot] + S + Trim(Data);
end;

procedure TQuest.Clear;
var
  I: Integer;
begin
  FList.Clear;
  for I := 0 to 9 do
    FList.Append('');
end;

function TQuest.Count: Integer;
begin
  Result := 10;
end;

constructor TQuest.Create;
begin
  FList := TStringList.Create;
  Self.Clear();
end;

destructor TQuest.Destroy;
begin
  FList.Free;
  inherited;
end;

function TQuest.Get(Slot, N: Byte): string;
var
  SL: TStringList;
begin
  SL := TUtils.ExplodeString('|', FList[Slot]);
  if (N < SL.Count) then
    Result := Trim(SL[N])
  else
    Result := '';
end;

procedure TQuest.LoadFromFile(const FileName: string);
begin
  FList.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
end;

procedure TQuest.Replace(Slot, N: Byte; S: string);
begin
  FList[Slot] := StringReplace(FList[Slot], Get(Slot, N), S, [rfReplaceAll]);
end;

procedure TQuest.SaveToFile(const FileName: string);
begin
{$IFNDEF FPC}FList.WriteBOM := False; {$ENDIF}
  FList.SaveToFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
end;

{$ENDREGION ' TQuest '}
{$REGION ' TNotification '}

procedure TNotification.Add(S: string);
begin
  FMessage := Trim(S);
  FCounter := FDuration;
end;

constructor TNotification.Create(Duration: Byte);
begin
  FMessage := '';
  FCounter := 0;
  FDuration := Duration;
end;

procedure TNotification.Dec;
begin
  if (FCounter > 0) then
    FCounter := FCounter - 1;
end;

procedure TNotification.Render(Left, Top: Word);
begin
  Saga.Engine.ForegroundColor(Saga.Colors.clNotification);
  if (FCounter > 1) then
    Saga.Engine.Print(Left, Top, FMessage);
end;

{$ENDREGION ' TNotification '}
{$REGION ' TRecs '}

procedure TRecs.Add(Slot: Byte);
var
  L, S: Byte;
  N: string;
begin
  S := Saga.Engine.Window.Width - 55;
  L := TEngine.GetTextLength(Saga.Player.GetFullName);
  if (L < S) then
    N := StringOfChar(#32, S - L)
  else
    N := '';
  Saga.List[Slot] := Format('%s%d', [Saga.Player.GetFullName + N,
    Saga.Player.Score]);
  Saga.List.SaveToFile(FFileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
end;

constructor TRecs.Create(FileName: string);
begin
  FFileName := FileName;
end;

procedure TRecs.Load;
begin
  Saga.ClearSlots;
  if FileExists(FFileName) then
    Saga.List.LoadFromFile(FFileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
end;

procedure TRecs.Save;
var
  Slot, I: Byte;
  Score: Word;
begin
  if (Saga.Player.Score = 0) then
    Exit;
  Self.Load;
  for Slot := 0 to 9 do
  begin
    if (Saga.List[Slot] <> '') then
    begin
      Score := StrToIntDef(Trim(Copy(Saga.List[Slot],
        Pos(#32#32, Saga.List[Slot]),
        TEngine.GetTextLength(Saga.List[Slot]))), 0);
      if (Score < Saga.Player.Score) then
      begin
        if (Slot < 9) then
          for I := 9 downto Slot + 1 do
            Saga.List[I] := Saga.List[I - 1];
        Add(Slot);
        Break;
      end
      else
        Continue;
    end
    else
    begin
      Add(Slot);
      Break;
    end;
  end;
  TStageRecMenu(Saga.Stages.GetStage(stRecMenu)).RecPos := Slot;
end;

{$ENDREGION ' TRecs '}
{$REGION ' TLanguage '}

function TLanguage.Get(S: string): string;
var
  I: Integer;
begin
  S := Trim(S);
  I := FID.IndexOf(S);
  if (I < 0) or (FValue[I] = '') then
    Result := S
  else
    Result := FValue[I];

  Result := StringReplace(Result, '<RED>', '[color=red]', [rfReplaceAll]);
  Result := StringReplace(Result, '<GREEN>', '[color=green]', [rfReplaceAll]);
  Result := StringReplace(Result, '<BLUE>', '[color=blue]', [rfReplaceAll]);
  Result := StringReplace(Result, '</>', '[/color]', [rfReplaceAll]);
end;

constructor TLanguage.Create;
begin
  FID := TStringList.Create;
  FValue := TStringList.Create;
  FCurrent := 'russian';
end;

destructor TLanguage.Destroy;
begin
  FreeAndNil(FID);
  FreeAndNil(FValue);
  inherited;
end;

procedure TLanguage.LoadFromFile(FileName: string);
var
  SL: TStringList;
  I, J: Integer;
  S: string;
begin
  if not FileExists(FileName) then
    Exit;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
    for I := 0 to SL.Count - 1 do
    begin
      S := Trim(SL[I]);
      J := Pos('=', S);
      Self.FID.Append(Trim(Copy(S, 1, J - 1)));
      Self.FValue.Append(Trim(Copy(S, J + 1, TEngine.GetTextLength(S))));
    end;
  finally
    SL.Free;
  end;
end;

procedure TLanguage.SetLanguage(CurrentLanguage: string);
begin
  Current := CurrentLanguage;
  Clear;
  LoadFromFile(TUtils.GetPath('resources') + Current + '.txt');
  LoadFromFile(TUtils.GetPath('resources') + Current + '.names.txt');
  LoadFromFile(TUtils.GetPath('resources') + Current + '.world.txt');
  LoadFromFile(TUtils.GetPath('resources') + Current + '.terrain.txt');
  LoadFromFile(TUtils.GetPath('resources') + Current + '.objects.txt');
end;

procedure TLanguage.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

{$ENDREGION ' TLanguage '}
{$REGION ' TColors '}

function TColors.GetColor(Color: TColorsEnum): Integer;
begin
  Result := FColors[Color];
end;

function TColors.GetColor(Color: string): Integer;
var
  I: TColorsEnum;
begin
  Result := $00FFFFFF;
  for I := Low(TColorsEnum) to High(TColorsEnum) do
    if (ColorsStr[I] = Color) then
    begin
      Result := GetColor(I);
      Exit;
    end;
end;

procedure TColors.LoadFromFile(FileName: string);
var
  I: TColorsEnum;
  F: TIniFile;
  R, G, B: Byte;
begin
  F := TIniFile.Create(FileName);
  try
    for I := Low(TColorsEnum) to High(TColorsEnum) do
    begin
      R := F.ReadInteger(ColorsStr[I], 'R', 0);
      G := F.ReadInteger(ColorsStr[I], 'G', 0);
      B := F.ReadInteger(ColorsStr[I], 'B', 0);
      FColors[I] := (R or (G shl 8) or (B shl 16));
    end;
  finally
    F.Free;
  end;
  SetColors;
end;

procedure TColors.SetColors;
begin
  clNotification := GetColor(ceYellow);
  clTitle := GetColor(ceYellow);
  clHotKey := GetColor(ceGreen);
  clButton := GetColor(ceLGray);
  clSplText := GetColor(ceLGray);
  clGoldText := GetColor(ceYellow);
  clAlertText := GetColor(ceRed);
  clMenuAct := GetColor(ceYellow);
  clMenuDef := GetColor(ceLGray);
  clCursor := GetColor(ceDGray);
end;

{$ENDREGION ' TColors '}
{$REGION ' TConfig '}

procedure TConfig.LoadFromFile(FileName: string);
var
  F: TIniFile;
begin
  F := TIniFile.Create(FileName);
  try

  finally
    F.Free;
  end;
end;

{$ENDREGION ' TConfig '}
{$REGION ' TUI '}

procedure TUI.DrawChar(X, Y: Integer; Symbol: System.Char;
  ForegroundColor, BackgroundColor: Integer);
begin
  FEngine.BackgroundColor(BackgroundColor);
  FEngine.ForegroundColor(ForegroundColor);
  FEngine.Print(X, Y, Symbol);
end;

procedure TUI.DrawKey(X, Y: Integer; Caption, Key: string; Active: Boolean);
var
  S: string;
begin
  S := TEngine.kcBegin + Key + TEngine.kcEnd;
  if Active then
    FEngine.ForegroundColor(Saga.Colors.clHotKey)
  else
    FEngine.ForegroundColor(FEngine.DarkColor(Saga.Colors.clHotKey, 60));
  FEngine.Print(X, Y, S);
  FEngine.ForegroundColor(Saga.Colors.clButton);
  FEngine.Print(X + TEngine.GetTextLength(S) + 1, Y, Caption);
end;

procedure TUI.DrawKey(X, Y: Integer; Caption, Key: string;
  Align: TEngine.TAlign; Active: Boolean);
var
  S: string;
  L: Integer;
begin
  case Align of
    aLeft:
      DrawKey(X, Y, Caption, Key, Active);
    aCenter:
      begin
        S := TEngine.kcBegin + Key + TEngine.kcEnd + ' ' + Caption;
        L := ((((FEngine.Char.Width * FEngine.Window.Width) +
          (X * FEngine.Char.Width)) div 2)) -
          ((TEngine.GetTextLength(S) * FEngine.Char.Width) div 2);
        DrawKey(L div FEngine.Char.Width, Y, Caption, Key, Active);
      end;
    aRight:
      begin

      end;
  end;
end;

constructor TUI.Create(AEngine: TEngine);
begin
  Self.FEngine := AEngine;
end;

procedure TUI.DrawTitle(Y: Word; Text: string);
begin
  Engine.ForegroundColor(Saga.Colors.clTitle);
  Engine.Print(0, Y - 1, Text, aCenter);
  Engine.Print(0, Y, StringOfChar('=', TEngine.GetTextLength(Text)), aCenter);
end;

{$ENDREGION ' TUI '}
{$REGION ' TIniFile '}

function TIniFile.ReadCategory(Section, Ident: string;
  DefaultValue: TItem.TCategory): TItem.TCategory;
var
  S: string;
  C: TItem.TCategory;
begin
  S := LowerCase(Trim(ReadString(Section, Ident, TItem.CatStr[DefaultValue])));
  Result := ctNone;
  if (S = '') then
    Exit;
  for C := Low(TItem.TCategory) to High(TItem.TCategory) do
    if (S = TItem.CatStr[C]) then
    begin
      Result := C;
      Exit;
    end;
end;

var
  Color: record R, G, B: Byte;
end;

function TIniFile.ReadColor(Section, Ident, DefaultValue: string): Integer;
var
  S: string;
  SL: TStringList;
  C: TColors;
begin
  Result := $00FFFFFF;
  S := UpperCase(Trim(ReadString(Section, Ident, DefaultValue)));
  if (S = '') then
    Exit;
  if (Pos(',', S) = 0) then
  begin
    C := TColors.Create;
    try
      C.LoadFromFile(TUtils.GetPath('resources') + 'colors.ini');
      Result := C.GetColor(S);
    finally
      C.Free;
    end;
    Exit;
  end;

  SL := TUtils.ExplodeString(',', S);
  if (SL.Count = 0) then
    Exit;
  if (SL.Count > 0) then
    Color.R := StrToIntDef(SL[0], 255)
  else
    Color.R := 255;
  if (SL.Count > 1) then
    Color.G := StrToIntDef(SL[1], 255)
  else
    Color.G := 255;
  if (SL.Count > 2) then
    Color.B := StrToIntDef(SL[2], 255)
  else
    Color.B := 255;
  Result := (Color.R or (Color.G shl 8) or (Color.B shl 16))
end;

function TIniFile.ReadMaterial(Section, Ident: string;
  DefaultValue: TItem.TMaterial): TItem.TMaterial;
var
  S: string;
  M: TItem.TMaterial;
begin
  S := LowerCase(Trim(ReadString(Section, Ident, TItem.MatStr[DefaultValue])));
  Result := mtNone;
  if (S = '') then
    Exit;
  for M := Low(TItem.TMaterial) to High(TItem.TMaterial) do
    if (S = TItem.MatStr[M]) then
    begin
      Result := M;
      Exit;
    end;
end;

procedure TIniFile.WriteCategory(Section, Ident: string;
  Value: TItem.TCategory);
begin
  if (Value = ctNone) then
    Exit;
  WriteString(Section, Ident, TItem.CatStr[Value]);
end;

procedure TIniFile.WriteColor(Section, Ident: string; Value: Integer);
begin
  WriteString(Section, Ident, Format('%d,%d,%d',
    [Byte(Value), Byte(Value shr 8), Byte(Value shr 16)]));
end;

procedure TIniFile.WriteMaterial(Section, Ident: string;
  Value: TItem.TMaterial);
begin
  if (Value = mtNone) then
    Exit;
  WriteString(Section, Ident, TItem.MatStr[Value]);
end;

{$ENDREGION ' TIniFile '}
{$REGION ' TScript '}

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
  FList.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  for I := FList.Count - 1 downto 0 do
  begin
    S := Trim(FList[I]);
    if (S = '') or (S[1] = ';') then
    begin
      FList.Delete(I);
      Continue;
    end;
    if (Pos(';', S) > 0) then
      Delete(S, Pos(';', S), TEngine.GetTextLength(S));
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
    Code := Trim(Copy(Link, P + 1, TEngine.GetTextLength(Link) - P - 1));
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
  SL := TUtils.ExplodeString('&', Code);
  for I := 0 to SL.Count - 1 do
    if FIsNext then
      Continue
    else
      Self.Run(Trim(SL[I]));
end;

procedure TScript.Run(const Code: string);
var
  S, L: string;
  I, E: Integer;

  function GetLastCode(Tag: string; Code: string): string;
  begin
    Result := Trim(Copy(Code, TEngine.GetTextLength(Tag) + 2,
      TEngine.GetTextLength(Code)));
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
      R := Copy(Code, 1, TEngine.GetTextLength(Tag))
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
    Val(Trim(Copy(S, Pos(K, S) + 1, TEngine.GetTextLength(S))), V, A);
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

  procedure SetDialog(Code: string);
  var
    MapIdx: Integer;
    CrtIdx: Integer;
    DlgIdx: Integer;
    SL: TStringList;
  begin
    SL := TUtils.ExplodeString(':', Code);
    MapIdx := Saga.World.GetMapIndex(Trim(SL[0]));
    if (MapIdx < 0) then
      Exit;
    CrtIdx := Saga.World.GetMapCreatures(MapIdx).GetIndex(SL[1]);
    if (CrtIdx < 0) then
      Exit;
    DlgIdx := StrToIntDef(SL[2], 0);
    if (DlgIdx < 0) then
      Exit;
    Saga.World.GetMapCreatures(MapIdx).Entity[CrtIdx].Dialog := DlgIdx;
    // ShowMessage(MapIdx.ToString + ':' + CrtIdx.ToString + ':' +
    // DlgIdx.ToString);
  end;

begin
  if IsTag('endif') then
    FIsIf := False;

  if FIsIf then
    Exit;

  if IsTag('pln') then
  begin
    S := GetLastCode('pln', Code);
{if (Vars.Has(S)) then
      I := Vars.GetInt(S);
    Saga.Log[lgDialog].Add(I.ToString());}
    Saga.Log[lgDialog].Add(S);
  end;

  if IsTag('log') then
    Saga.Log[lgGame].Add(GetLastCode('log', Code));

  if IsTag('box') then
  begin
    S := GetLastCode('box', Code);
    if (Vars.Has(S)) then
      TUtils.Box(Vars.GetInt(S))
    else
      TUtils.Box(S);
  end;

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
    if IsTag('update', S) then
    begin
      Saga.Log[lgGame].Add(__('Новая запись в журнале.'));
      Exit;
    end;
    if IsTag(CloseTag, S) then
    begin
      Saga.Log[lgGame].Add(__('You have completed the quest.'));
      Saga.Quest.Add(I - 1, __('I have completed this quest.'));
      Saga.Quest.Replace(I - 1, 0, TEngine.kcBegin + __('Задание выполнено') +
        TEngine.kcEnd + ' ' + Saga.Quest.Get(I - 1, 0));
      Exit;
    end;
    Saga.Quest.Add(I - 1, S);
  end;

  if (Pos('=', Code) > 0) and not IsTag('if') and not IsTag('btn') then
  begin
    S := AnsiLowerCase(Trim(Copy(Code, 1, Pos('=', Code) - 1)));
    L := Trim(Copy(Code, Pos('=', Code) + 1, TEngine.GetTextLength(Code)));
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

  // Map(ID):Creature(ID):Dialog(Num)
  if IsTag('dialog') then
    SetDialog(GetLastCode('dialog', Code));

  if IsTag('exp') then
  begin
    S := GetLastCode('exp', Code);
    Saga.Player.AddExp(StrToInt(S));
  end;

  if IsTag('heal') then
  begin
    Saga.Player.Atr[atLife].SetToMax;
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

{$REGION ' TScript.TVars '}

procedure TScript.TVars.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

function TScript.TVars.Count: Integer;
begin
  Result := FID.Count;
end;

constructor TScript.TVars.Create;
begin
  FID := TStringList.Create;
  FValue := TStringList.Create;
end;

destructor TScript.TVars.Destroy;
begin
  FID.Free;
  FValue.Free;
  inherited;
end;

procedure TScript.TVars.Empty(const AVar: string);
var
  I: Integer;
begin
  I := FID.IndexOf(AVar);
  if (I < 0) then
    Exit;
  FID.Delete(I);
  FValue.Delete(I);
end;

function TScript.TVars.GetStr(const AVar: string): string;
var
  I: Integer;
begin
  I := FID.IndexOf(AVar);
  if I < 0 then
    Result := ''
  else
    Result := FValue[I];
end;

procedure TScript.TVars.SetStr(const AVar, AValue: string);
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

function TScript.TVars.GetInt(const AVar: string): Integer;
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

procedure TScript.TVars.SetInt(const AVar: string; const AValue: Integer);
begin
  SetStr(AVar, Format('%d', [AValue]));
end;

function TScript.TVars.Has(const AVar: string): Boolean;
begin
  Result := FID.IndexOf(AVar) > -1;
end;

procedure TScript.TVars.LoadFromFile(const FileName: string);
var
  A: TStringList;
  I, J: Integer;
  S: string;
begin
  A := TStringList.Create;
  try
    Self.Clear;
{$IFNDEF FPC}A.WriteBOM := False; {$ENDIF}
    A.LoadFromFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
    for I := 0 to A.Count - 1 do
    begin
      S := Trim(A[I]);
      J := Pos(',', S);
      Self.FID.Append(Trim(Copy(S, 1, J - 1)));
      Self.FValue.Append(Trim(Copy(S, J + 1, TEngine.GetTextLength(S))));
    end;
  finally
    A.Free;
  end;
end;

procedure TScript.TVars.SaveToFile(const FileName: string);
var
  I: Integer;
  A: TStringList;
begin
  A := TStringList.Create;
{$IFNDEF FPC}A.WriteBOM := False; {$ENDIF}
  for I := 0 to FID.Count - 1 do
    A.Append(FID[I] + ',' + FValue[I]);
  A.SaveToFile(FileName{$IFNDEF FPC}, TEncoding.UTF8{$ENDIF});
  A.Free;
end;

{$ENDREGION ' TScript.TVars '}
{$ENDREGION ' TScript '}
{$REGION ' TBattle '}

constructor TBattle.Create;
begin

end;

destructor TBattle.Destroy;
begin

  inherited;
end;

procedure TBattle.EnemyMove;
begin
  Saga.Log[lgBattle].Add(Format('%s атакует тебя (%d).', [EnemyName, 5]));
end;

function TBattle.EnemyName: string;
begin
  Result := Saga.World.CurrentCreatures.Entity[ID].Name;
end;

procedure TBattle.Finish;
begin
  Saga.Stages.SetStage(stGame);
  Saga.Log[lgGame].Add('Ты вышел из боя.');
end;

procedure TBattle.PlayerMove;
begin
  Saga.Log[lgBattle].Add(Format('Ты атакуешь %s (%d)', [EnemyName, 5]));
  EnemyMove();
end;

procedure TBattle.Start(ID: Byte);
begin
  Self.ID := ID;
  Saga.Log[lgBattle].Clear;
  if (Math.RandomRange(1, 3) = 1) then
  begin
    Saga.Log[lgBattle].Add('Твои навыки позволяют тебе атаковать первым.');
  end
  else
  begin
    Saga.Log[lgBattle].Add(Format('%s неожиданно нападает на тебя первым.',
      [EnemyName]));
    EnemyMove();
  end;
end;

{$ENDREGION ' TBattle '}

end.
