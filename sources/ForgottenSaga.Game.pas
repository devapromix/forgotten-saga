unit ForgottenSaga.Game;

interface

uses Classes, Types, Engine, ForgottenSaga.Scenes, Common.Map, ForgottenSaga.Creature, Common.Map.Tiles, ForgottenSaga.Battle;

const
  FSVersion = '0.0.3';     // Версия
  PanelWidth = 40;         // Ширина панели

function __(S: string): string;

type
  TLanguage = class(TObject)
  private
    FID: TStringList;
    FValue: TStringList;
    FCurrent: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(FileName: string);
    function Get(S: string): string;
    property Current: string read FCurrent write FCurrent;
  end;

type
  TNotification = class(TObject)
  private
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

type
  TWorld = class(TObject)
  private
    FMaps: array of TMap;
    FCreatures: array of TCreatures;
    FItems: array of TItems;
    FEngine: TEngine;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMap(I: Byte): TMap;
    function GetMapCreatures(I: Byte): TCreatures;
    function GetMapItems(I: Byte): TItems;
    function CurrentMap: TMap;
    function GoLoc(Dir: TDir): Boolean;
    function CurrentCreatures: TCreatures;
    function CurrentItems: TItems;
    function Count: Byte;
    function FileName(Dir: string; ID: Byte; Ext: string): string;
    procedure SaveToDir(Dir: string);
    procedure LoadFromDir(Dir: string);
    procedure Gen(I: Byte);
    property Engine: TEngine read FEngine write FEngine;
  end;

type
  TRaceEnum = (rcGoblin, rcOrc, rcTroll);

type
  TRace = class(TObject)
  private
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

type
  TLogEnum = (lgGame, lgIntro, lgDialog, lgBattle, lgQuest);

type
  TLog = class(TObject)
  private
    FLogStr: string;
    FLen: Word;
  public
    constructor Create(Len: Word);
    procedure Clear;
    procedure Add(Text: string; Flag: Boolean = True);
    function Get: string;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure Render(Left, Top, Width: Word);
  end;

type
  TRecs = class(TObject)
  private
    FFileName: string;
    procedure Add(Slot: Byte);
  public
    constructor Create(FileName: string);
    procedure Load();
    procedure Save();
  end;

type
  TQuest = class(TObject)
  private
    FList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Get(Slot, N: Byte): string;
    procedure Add(Slot: Byte; Data: string);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
  end;

type
  TSaga = class(TObject)
  private
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
    FNotification: TNotification;
    FLog: array [TLogEnum] of TLog;
    FRace: array [TRaceEnum] of TRace;
  protected
    function GetLog(I: TLogEnum): TLog;
    procedure SetLog(I: TLogEnum; const Value: TLog);
    function GetRace(I: TRaceEnum): TRace;
    procedure SetRace(I: TRaceEnum; const Value: TRace);
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure New();
    procedure Init;
    procedure LoadSlots;
    procedure ClearLogs;
    procedure ClearSlots;
    procedure AddRace(ID: TRaceEnum; Name: string; Life, Mana: Word; Pos: TPoint;
      Map: Byte; Color: Integer);
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
  end;

var
  Saga: TSaga;

implementation

uses SysUtils, IniFiles, Common.Color, Common.Utils;

function __(S: string): string;
begin
  Result := '';
  if (S = '') then Exit;
  Result := Saga.Lg.Get(S);
end;
{ TWorld }

function TWorld.Count: Byte;
begin
  Result := Length(FMaps);
end;

constructor TWorld.Create;
var
  I, C: Integer;
  S: string;
  F: TIniFile;
begin
  F := TIniFile.Create(GetPath('resources') + 'world.ini');
  try
    S := 'Main';
    C := F.ReadInteger(S, 'Count', 0);
    SetLength(FMaps, C);
    SetLength(FCreatures, C);
    SetLength(FItems, C);
    for I := 0 to Count - 1 do
    begin
      S := Format('%d', [I]);
      FMaps[I] := TMap.Create;
      FCreatures[I] := TCreatures.Create;
      FItems[I] := TItems.Create;
      FMaps[I].Name := F.ReadString(S, 'Name', '');
      FMaps[I].FileName := F.ReadString(S, 'FileName', '');
      FMaps[I].Map[drLeft] := F.ReadInteger(S, 'Left', -1);
      FMaps[I].Map[drUp] := F.ReadInteger(S, 'Up', -1);
      FMaps[I].Map[drRight] := F.ReadInteger(S, 'Right', -1);
      FMaps[I].Map[drDown] := F.ReadInteger(S, 'Down', -1);
      FMaps[I].Map[drTop] := F.ReadInteger(S, 'Top', -1);
      FMaps[I].Map[drBottom] := F.ReadInteger(S, 'Bottom', -1);
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
  inherited;
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

function TWorld.GoLoc(Dir: TDir): Boolean;
var
  ID: Integer;
begin
  Result := False;
  ID := Saga.World.CurrentMap.Map[Dir];
  if (ID > -1) then
  begin
    Saga.Log[lgGame].Add(Format(__('Ты вошел в %s.'),
      [Saga.World.GetMap(ID).Name]));
    Saga.Player.Map := ID;
    Result := True;
  end;
end;

procedure TWorld.Gen(I: Byte);
begin
  FMaps[I].Gen;
  FMaps[I].SaveToFile(GetPath('resources') + '0.map');
end;

{ TSaga }

constructor TSaga.Create(AWidth, AHeight: Integer);
const
  LogLen: array [TLogEnum] of Integer = (500, 1000, 1000, 1400, 1600);
var
  I: Byte;
  L: TLogEnum;
  R: TRaceEnum;
begin
  FEngine := TEngine.Create(AWidth, AHeight);
  FList := TStringList.Create;
  ClearSlots;
  ForceDirectories(GetPath('save'));
  for I := 0 to 9 do
    ForceDirectories(GetSlotPath(I));

  for L := Low(TLogEnum) to High(TLogEnum) do
    Self.Log[L] := TLog.Create(LogLen[L]);

  Lg := TLanguage.Create;

  FPlayer := TPlayer.Create;
  FStages := TStages.Create;
  FWorld := TWorld.Create;

  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Race[R] := TRace.Create;

  FBattle := TBattle.Create;
  FQuest := TQuest.Create;
  FTiles := TTiles.Create;

  FNotification := TNotification.Create();
  FRecs := TRecs.Create(GetPath('save') + 'records.txt');

  Stages.SetStage(stMainMenu);
end;

procedure TSaga.Init;
var
  S: TStringList;
  F: string;
  I: Integer;
begin
  S := TStringList.Create;
  try
    F := GetPath('resources') + Lg.Current + '.intro.txt';
    if (FileExists(F)) then
    begin
      S.LoadFromFile(F);
      for I := 0 to S.Count - 1 do
        Log[lgIntro].Add(__(S[I]), False);
    end;
  finally
    S.Free;
  end;

  Lg.Clear;
  Lg.LoadFromFile(GetPath('resources') + Lg.Current + '.txt');
  Lg.LoadFromFile(GetPath('resources') + Lg.Current + '.world.txt');
  Lg.LoadFromFile(GetPath('resources') + Lg.Current + '.tiles.txt');

  AddRace(rcGoblin, __('Goblin'), 90, 100, Point(40, 20), 0, $00882295);
  AddRace(rcOrc,    __('Orc'), 120, 100, Point(40, 20), 0, $0063C3C6);
  AddRace(rcTroll,  __('Troll'), 150, 100, Point(40, 20), 0, $00664567);
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
  FRecs.Free;
  FList.Free;
  FPlayer.Free;
  FStages.Free;
  FWorld.Free;
  FBattle.Free;
  FEngine.Free;
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
  Result := IncludeTrailingPathDelimiter(Format('%s%d', [GetPath('save'), Slot]));
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
  Saga.ClearLogs;
  World.LoadFromDir(GetPath('resources'));
  //World.Gen(0); // Пока вместо редактора
  Stages.SetStage(stGame);
  Saga.Notification.Add('Создан новый мир');
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
  TStageDialog(Saga.Stages.GetStage(stDialog)).Dialog.Vars.LoadFromFile
    (GetSlotPath(Slot) + 'vars.txt');
  Result := True;
  if Result then Saga.Notification.Add('Игра успешно загружена');
end;

procedure TSaga.SaveToSlot(Slot: Byte);
begin
  Player.SaveToFile(GetSlotPath(Slot) + 'player.crt');
  Log[lgGame].SaveToFile(GetSlotPath(Slot) + 'game.log');
  Self.Quest.SaveToFile(GetSlotPath(Slot) + 'quest.log');
  World.SaveToDir(GetSlotPath(Slot));
  TStageDialog(Saga.Stages.GetStage(stDialog)).Dialog.Vars.SaveToFile
    (GetSlotPath(Slot) + 'vars.txt');
  FList[Slot] := Format('%s %s - %s', [DateTimeToStr(Now), Player.GetFullName,
    World.GetMap(Player.Map).Name]);
  FList.SaveToFile(GetPath('save') + 'list.txt');
  Saga.Notification.Add('Игра успешно сохранена');
end;

procedure TSaga.SetLog(I: TLogEnum; const Value: TLog);
begin
  FLog[I] := Value;
end;

procedure TSaga.SetRace(I: TRaceEnum; const Value: TRace);
begin
  FRace[I] := Value;
end;

procedure TSaga.AddRace(ID: TRaceEnum; Name: string; Life, Mana: Word; Pos: TPoint;
  Map: Byte; Color: Integer);
begin
  Race[ID].Color := Color;
  Race[ID].Name := Name;
  Race[ID].Life := Life;
  Race[ID].Mana := Mana;
  Race[ID].Pos := Pos;
  Race[ID].Map := Map;
end;

procedure TSaga.LoadSlots;
var
  F: string;
begin
  ClearSlots;
  F := GetPath('save') + 'list.txt';
  if FileExists(F) then
    FList.LoadFromFile(F);
end;

procedure TSaga.ClearLogs;
begin
  Saga.Quest.Clear;
  Saga.Log[lgGame].Clear;
end;

{ TLog }

procedure TLog.Add(Text: string; Flag: Boolean = True);
begin
  Text := Trim(Text);
  if (Text = '') then
    Exit;
  if Flag then
    FLogStr := Text + ' ' + FLogStr
  else
    FLogStr := FLogStr + ' ' + Text;
  if (Length(FLogStr) > FLen) then
  begin
    Delete(FLogStr, FLen, Length(FLogStr));
    FLogStr := FLogStr + '...';
  end;
end;

procedure TLog.Clear;
begin
  FLogStr := '';
end;

constructor TLog.Create(Len: Word);
begin
  FLen := Len;
  Clear;
end;

function TLog.Get: string;
begin
  Result := FLogStr;
end;

procedure TLog.LoadFromFile(FileName: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(FileName);
    FLogStr := S.Text;
  finally
    S.Free;
  end;
end;

procedure TLog.SaveToFile(FileName: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Text := FLogStr;
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

procedure TLog.Render(Left, Top, Width: Word);
begin
  Saga.Engine.FontColor(clSplText);
  Saga.Engine.TextOut(Get, Rect(Left, Top, Width, 0), aLeft);
end;

{ TQuest }

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
  SL := ExplodeString('|', FList[Slot]);
  if (N < SL.Count) then
    Result := Trim(SL[N])
  else
    Result := '';
end;

procedure TQuest.LoadFromFile(FileName: string);
begin
  FList.LoadFromFile(FileName);
end;

procedure TQuest.SaveToFile(FileName: string);
begin
  FList.SaveToFile(FileName);
end;

{ TNotification }

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
  Saga.Engine.FontColor(clNotification);
  if (FCounter > 1) then
    Saga.Engine.TextOut(Left, Top, FMessage);
end;

{ TRecs }

procedure TRecs.Add(Slot: Byte);
var
  L, S: Byte;
  N: string;
begin
  S := Saga.Engine.Window.Width - 55;
  L := Length(Saga.Player.GetFullName);
  if (L < S) then N := StringOfChar(#32, S - L) else N := '';
  Saga.List[Slot] := Format('%s%d', [Saga.Player.GetFullName + N, Saga.Player.Score]);
  Saga.List.SaveToFile(FFileName);
end;

constructor TRecs.Create(FileName: string);
begin
  FFileName := FileName;
end;

procedure TRecs.Load;
begin
  Saga.ClearSlots;
  if FileExists(FFileName) then Saga.List.LoadFromFile(FFileName);
end;

procedure TRecs.Save;
var
  Slot, I: Byte;
  Score: Word;
begin
  if (Saga.Player.Score = 0) then Exit;
  Self.Load;
  for Slot := 0 to 9 do
  begin
    if (Saga.List[Slot] <> '') then
    begin
      Score := StrToIntDef(Trim(Copy(Saga.List[Slot], Pos(#32#32, Saga.List[Slot]), Length(Saga.List[Slot]))), 0);
      if (Score < Saga.Player.Score) then
      begin
        if (Slot < 9) then
        for I := 9 downto Slot + 1 do
            Saga.List[I] := Saga.List[I - 1];
        Add(Slot);
        Break;
      end else Continue;
    end else begin
      Add(Slot);
      Break;
    end;
  end;
  TStageRecMenu(Saga.Stages.GetStage(stRecMenu)).RecPos := Slot;
end;

{ TLanguage }

function TLanguage.Get(S: string): string;
var
  I: Integer;
begin
  S := Trim(S);
  I := FID.IndexOf(S);
  if (I < 0) or (FValue[I] = '') then Result := S else Result := FValue[I];
end;

constructor TLanguage.Create;
begin
  FID := TStringList.Create;
  FValue := TStringList.Create;
  //FCurrent := 'russian';
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
  if not FileExists(FileName) then Exit;
  SL := TStringList.Create;
  SL.LoadFromFile(FileName);
  try
    for I := 0 to SL.Count - 1 do
    begin
      S := Trim(SL[I]);
      J := Pos('=', S);
      Self.FID.Append(Trim(Copy(S, 1, J - 1)));
      Self.FValue.Append(Trim(Copy(S, J + 1, Length(S))));
    end;
  finally
    SL.Free;
  end;
end;

procedure TLanguage.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

end.
