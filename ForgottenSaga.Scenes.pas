unit ForgottenSaga.Scenes;

interface

uses Classes, ForgottenSaga.Script;

type
  TStageEnum = (stGame, stMainMenu, stGameMenu, stRaceMenu, stNameMenu,
    stTextMenu, stSaveMenu, stLoadMenu, stBattle, stDefeat, stVictory, stDialog,
    stQuestLog, stQuestInfo, stAboutMenu, stRecMenu, stItems, stInv);

type
  TStage = class(TObject)
    procedure Render; virtual; abstract;
    procedure Update(var Key: Word); virtual; abstract;
    procedure Timer; virtual; abstract;
  end;

type
  TStages = class(TStage)
  private
    FStage: array [TStageEnum] of TStage;
    FStageEnum: TStageEnum;
    FPrevStageEnum: TStageEnum;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Back;
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
    property Stage: TStageEnum read FStageEnum write FStageEnum;
    function GetStage(I: TStageEnum): TStage;
    procedure SetStage(StageEnum: TStageEnum); overload;
    procedure SetStage(StageEnum, CurrStageEnum: TStageEnum); overload;
    property PrevStage: TStageEnum read FPrevStageEnum write FPrevStageEnum;
  end;

type
  TStageGame = class(TStage)
  private
    procedure SetNPC(ID: Byte);
    procedure SetDialog(ID: Byte);
    procedure SetBattle(ID: Byte);
    procedure PlayerMove(X, Y: Integer);
  public
    procedure Render; override;
    procedure RenderPlayerInfo;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

type
  TStageCustomMenu = class(TStage)
  private
    FTop: Byte;
    FMenuPos: ShortInt;
  public
    constructor Create;
    procedure RenderCursor(Y: Integer; Color: Integer);
    property Top: Byte read FTop write FTop;
    property MenuPos: ShortInt read FMenuPos write FMenuPos;
    procedure Timer; override;
  end;

type
  TStageMenu = class(TStageCustomMenu)
  private
    FItems: string;
    FCount: Byte;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: Word); override;
    property Items: string read FItems write FItems;
    property Count: Byte read FCount write FCount;
  end;

type
  TStageMainMenu = class(TStageMenu)
  private

  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageGameMenu = class(TStageMenu)
  private

  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageRaceMenu = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageAboutMenu = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageNameMenu = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageTextMenu = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageStorageMenu = class(TStageCustomMenu)
  private
    FKeyFlag: Boolean;
  public
    constructor Create;
    procedure RenderNum;
    procedure Render; override;
    procedure Update(var Key: Word); override;
    property KeyFlag: Boolean read FKeyFlag write FKeyFlag;
  end;

type
  TStageRecMenu = class(TStageStorageMenu)
  private
    FRecPos: ShortInt;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: Word); override;
    property RecPos: ShortInt read FRecPos write FRecPos;
  end;

type
  TStageSaveMenu = class(TStageStorageMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageLoadMenu = class(TStageStorageMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageBattle = class(TStage)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

type
  TStageInv = class(TStage)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

type
  TStageItems = class(TStage)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

type
  TStageQuestLog = class(TStageStorageMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageQuestInfo = class(TStage)
  private
    FID: Integer;
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
    property ID: Integer read FID write FID;
  end;

type
  TLinks = class
  private
    FLabelList: TStringList;
    FNameList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    procedure Append(const ALabel, AName: string);
    function GetLabel(const I: Integer): string;
    function GetName(const I: Integer): string;
  end;

type
  TStageDialog = class(TStage)
  private
    FID: Integer;
    FDialog: TScript;
    FLinkList: TLinks;
    procedure Answer(var Key: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Timer; override;
    procedure Update(var Key: Word); override;
    property ID: Integer read FID write FID;
    property Dialog: TScript read FDialog write FDialog;
    property LinkList: TLinks read FLinkList write FLinkList;
  end;

type
  TStageVictory = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TStageDefeat = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

implementation

uses Forms, SysUtils, Windows, Math, Engine, ForgottenSaga.Game, Common.Color, Common.Utils, ForgottenSaga.Creature, Common.Map.Tiles,
  ForgottenSaga.Inv;

{ TStages }

procedure TStages.Update(var Key: Word);
begin
  if (FStage[Stage] <> nil) then
    FStage[Stage].Update(Key);
end;

procedure TStages.Render;
begin
  Saga.Engine.Clear;
  if (FStage[Stage] <> nil) then
    FStage[Stage].Render;
end;

constructor TStages.Create;
var
  I: TStageEnum;
begin
  for I := Low(TStageEnum) to High(TStageEnum) do
    case I of
      stMainMenu:
        FStage[I] := TStageMainMenu.Create;
      stGameMenu:
        FStage[I] := TStageGameMenu.Create;
      stRaceMenu:
        FStage[I] := TStageRaceMenu.Create;
      stNameMenu:
        FStage[I] := TStageNameMenu.Create;
      stTextMenu:
        FStage[I] := TStageTextMenu.Create;
      stSaveMenu:
        FStage[I] := TStageSaveMenu.Create;
      stLoadMenu:
        FStage[I] := TStageLoadMenu.Create;
      stBattle:
        FStage[I] := TStageBattle.Create;
      stDialog:
        FStage[I] := TStageDialog.Create;
      stVictory:
        FStage[I] := TStageVictory.Create;
      stDefeat:
        FStage[I] := TStageDefeat.Create;
      stGame:
        FStage[I] := TStageGame.Create;
      stQuestLog:
        FStage[I] := TStageQuestLog.Create;
      stQuestInfo:
        FStage[I] := TStageQuestInfo.Create;
      stAboutMenu:
        FStage[I] := TStageAboutMenu.Create;
      stRecMenu:
        FStage[I] := TStageRecMenu.Create;
      stItems:
        FStage[I] := TStageItems.Create;
      stInv:
        FStage[I] := TStageInv.Create;
    end;
end;

destructor TStages.Destroy;
var
  I: TStageEnum;
begin
  for I := Low(TStageEnum) to High(TStageEnum) do
    FStage[I].Free;
  inherited;
end;

procedure TStages.SetStage(StageEnum: TStageEnum);
begin
  Self.Stage := StageEnum;
  if Assigned(Saga) then
    Render;
end;

procedure TStages.SetStage(StageEnum, CurrStageEnum: TStageEnum);
begin
  FPrevStageEnum := CurrStageEnum;
  SetStage(StageEnum);
end;

procedure TStages.Back;
begin
  Stage := FPrevStageEnum;
end;

function TStages.GetStage(I: TStageEnum): TStage;
begin
  Result := FStage[I];
end;

procedure TStages.Timer;
begin
  if (FStage[Stage] <> nil) then
    FStage[Stage].Timer;
end;

{ TStageGame }

procedure TStageGame.SetNPC(ID: Byte);
var
  D: TStageDialog;
begin
  D := TStageDialog(Saga.Stages.GetStage(stDialog));
  D.ID := ID;
  D.Dialog.LoadFromFile(GetPath('data') + Saga.World.CurrentCreatures.Get(ID)
    .FileName);
  D.Dialog.Next(Format('%d', [Saga.World.CurrentCreatures.Get(ID).Dialog]));
end;

procedure TStageGame.SetDialog(ID: Byte);
begin
  SetNPC(ID);
  Saga.Stages.SetStage(stDialog);
end;

procedure TStageGame.SetBattle(ID: Byte);
begin
  Saga.Battle.Start(ID);
  Saga.Stages.SetStage(stBattle);
end;

procedure TStageGame.PlayerMove(X, Y: Integer);
var
  ID: Integer;
begin
  if Saga.Player.Look.Enabled then
  begin
    Saga.Player.Look.Move(X, Y);
    Exit;
  end;
  ID := Saga.World.CurrentCreatures.Has(Saga.Player.Pos.X + X,
    Saga.Player.Pos.Y + Y);
  if (ID > -1) then
    case Saga.World.CurrentCreatures.Get(ID).Force of
      fcAlly:
        SetDialog(ID);
      fcEnemy:
        SetBattle(ID);
    end
  else
    Saga.Player.Move(X, Y);
end;

procedure TStageGame.Render;
begin
  Saga.World.CurrentMap.Render;
  RenderPlayerInfo;
  Saga.Log[lgGame].Render(81, 15, 39);
  Saga.World.CurrentItems.Render;
  Saga.World.CurrentCreatures.Render;
  Saga.Notification.Render(0, 0);
end;

procedure TStageGame.RenderPlayerInfo;
var
  I: TAtrEnum;
begin
  Saga.Engine.FontColor(clSplText);
  Saga.Engine.TextOut(81, 0, __(Saga.World.CurrentMap.Name));
  Saga.Engine.TextOut(81, 1, Saga.Player.GetFullName);
  Saga.Engine.TextOut(81, 2, __('Honor') + ' ' + IntToStr(Saga.Player.Score));
  for I := Low(TAtrEnum) to High(TAtrEnum) do
    Saga.Engine.TextOut(81, ord(I) + 3, __(AtrStr[I]) + ' ' + Saga.Player.Atr[I].ToString);
end;

procedure TStageGame.Timer;
begin
  Saga.Notification.Dec();
  Saga.Stages.Render;
end;

procedure TStageGame.Update(var Key: Word);
begin
//  Box(Ord(Key));
  case Key of
    27:
      begin
        if Saga.Player.Look.Enabled then
        begin
          Saga.Player.Look.Enabled := False;
          Exit;
        end;
        Saga.Stages.SetStage(stGameMenu);
      end;
    113:
      begin
        Saga.LoadSlots;
        Saga.Stages.SetStage(stSaveMenu, stGame);
      end;
    114:
      begin
        Saga.LoadSlots;
        Saga.Stages.SetStage(stLoadMenu, stGame);
      end;
    37, 100, Ord('A'):
      PlayerMove(-1, 0);
    39, 102, Ord('D'):
      PlayerMove(1, 0);
    38, 104, Ord('W'):
      PlayerMove(0, -1);
    40, 98, Ord('X'):
      PlayerMove(0, 1);
    103, 36, Ord('Q'):
      PlayerMove(-1, -1);
    105, 33, Ord('E'):
      PlayerMove(1, -1);
    97, 35, Ord('Z'):
      PlayerMove(-1, 1);
    99, 34, Ord('C'):
      PlayerMove(1, 1);
    101, 12, Ord('S'):
      PlayerMove(0, 0);
    188:
      if Saga.World.CurrentMap.HasTile(tStUp, Saga.Player.Pos.X, Saga.Player.Pos.Y) then
        Saga.World.GoLoc(drTop);
    190:
      if Saga.World.CurrentMap.HasTile(tStDn, Saga.Player.Pos.X, Saga.Player.Pos.Y) then
        Saga.World.GoLoc(drBottom);
    ord('J'):
      Saga.Stages.SetStage(stQuestLog);
    ord('I'):
      Saga.Stages.SetStage(stInv);
    ord('V'): Saga.Player.Victory;       
    ord('B'): Saga.Player.Defeat;
    ord('G'): Saga.Player.Pickup;
    ord('H'): Saga.World.CurrentItems.Add('|', cDkBrown, 1,
      'Посох Шамана', mtBone, ctStaff, 15);
    ord('L'):
      begin
        Saga.Player.Look.SetPosition(Saga.Player.Pos);
        Saga.Player.Look.Enabled := not Saga.Player.Look.Enabled;
      end;
  end;
end;

{ TStageCustomMenu }

constructor TStageCustomMenu.Create;
begin
  Top := 16;
  MenuPos := 0;
end;

procedure TStageCustomMenu.RenderCursor(Y: Integer; Color: Integer);
begin
  Saga.Engine.FontBackColor(Color);
  Saga.Engine.TextOut(0, Y, StringOfChar(#32, Saga.Engine.Window.Width -
    10), aCenter);
  Saga.Engine.FontBackColor(clClear);
end;

procedure TStageCustomMenu.Timer;
begin

end;

{ TStageMenu }

constructor TStageMenu.Create;
begin
  inherited;
  Items := '';
  Count := 0;
end;

procedure TStageMenu.Render;
var
  I: ShortInt;
begin
  Saga.Engine.TitleOut(Top, __('Forgotten Saga'));
  for I := 0 to Count - 1 do
  begin
    Saga.Engine.FontColor(clTitle);
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, cDkGray);
      Saga.Engine.FontColor(clMenuAct);
    end
    else
      Saga.Engine.FontColor(clMenuDef);
    Saga.Engine.TextOut(0, I + Top + 2, __(GetStr('|', Items, I)), aCenter);
  end;
end;

procedure TStageMenu.Update(var Key: Word);
begin
  case Key of
    38, 40: MenuPos := Clamp(MenuPos + Key - 39, 0, Count - 1, False);
  end;
end;

{ TStageMainMenu }

constructor TStageMainMenu.Create;
begin
  inherited;
  Items := 'New game|Load game|High scores table|About|Quit';
  Count := 5;
end;

procedure TStageMainMenu.Render;
begin
  inherited;
end;

procedure TStageMainMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    13:
      case MenuPos of
        0:
          Saga.Stages.SetStage(stRaceMenu);
        1:
          begin
            Saga.LoadSlots;
            Saga.Stages.SetStage(stLoadMenu, stMainMenu);
          end;
        2:
          begin
            TStageRecMenu(Saga.Stages.GetStage(stRecMenu)).RecPos := -1;
            Saga.Recs.Load;
            Saga.Stages.SetStage(stRecMenu, stMainMenu);
          end;
        3:
          begin
            Saga.Stages.SetStage(stAboutMenu, stMainMenu);
          end;
        4:
          Application.Terminate;
      end;
  end;
end;

{ TStageGameMenu }

constructor TStageGameMenu.Create;
begin
  inherited;
  Items := 'Continue|Save game|Load game|Exit game';
  Count := 4;
end;

procedure TStageGameMenu.Render;
begin
  inherited;
end;

procedure TStageGameMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    27:
      Saga.Stages.SetStage(stGame);
    13:
      case MenuPos of
        0:
          Saga.Stages.SetStage(stGame);
        1:
          begin
            Saga.LoadSlots;
            Saga.Stages.SetStage(stSaveMenu, stGameMenu);
          end;
        2:
          begin
            Saga.LoadSlots;
            Saga.Stages.SetStage(stLoadMenu, stGameMenu);
          end;
        3:
          Saga.Stages.SetStage(stMainMenu);
      end;
  end;
end;

{ TStageRaceMenu }

procedure TStageRaceMenu.Render;
var
  R: TRaceEnum;
begin
  Saga.Engine.TitleOut(Top, __('Select race'));

  for R := Low(TRaceEnum) to High(TRaceEnum) do
  begin
    Saga.Engine.FontColor(clTitle);
    if (ord(R) = MenuPos) then
      RenderCursor(ord(R) + Top + 2, cDkGray);
    Saga.Engine.FontColor(Saga.Race[R].Color);
    Saga.Engine.TextOut(0, ord(R) + Top + 2, Saga.Race[R].Name, aCenter);
  end;
  Saga.Engine.KeyOut(0, Top + Ord(High(TRaceEnum)) + 4, __('Back to main menu'), 'ESC', aCenter);
end;

procedure TStageRaceMenu.Update(var Key: Word);
begin
  case Key of
    27:
      Saga.Stages.SetStage(stMainMenu);
    38, 40:
        MenuPos := Clamp(MenuPos + Key - 39, ord(Low(TRaceEnum)),
          ord(High(TRaceEnum)), False);
    13:
      case MenuPos of
        0 .. 2:
          begin
            Saga.Player.Race := MenuPos;
            Saga.Player.GenName;
            Saga.Stages.SetStage(stNameMenu);
          end;
      end;
  end;
end;

{ TStageNameMenu }

procedure TStageNameMenu.Render;
begin
  Saga.Engine.TitleOut(Top, __('What is your name?'));
  Saga.Engine.FontColor(Saga.Race[TRaceEnum(Saga.Player.Race)].Color);
  Saga.Engine.TextOut(0, Top + 2, Saga.Player.GetRaceName + ' [' +
    Saga.Player.Name + ']', aCenter);
  Saga.Engine.KeyOut(0, Top + 4, __('Random name'), 'SPACE', aCenter);
end;

procedure TStageNameMenu.Update(var Key: Word);
begin
  case Key of
    27:
      Saga.Stages.SetStage(stRaceMenu);
    13:
      Saga.Stages.SetStage(stTextMenu);
    32:
      Saga.Player.GenName;
  end;
end;

{ TStageTextMenu }

procedure TStageTextMenu.Render;
begin
  Saga.Engine.TitleOut(13, 'Как все начиналось...');
  Saga.Engine.KeyOut(0, 25, 'Начать игру...', 'ENTER', aCenter);
  Saga.Engine.FontColor(clSplText);
  Saga.Log[lgIntro].Render(15, 15, 85);
end;

procedure TStageTextMenu.Update(var Key: Word);
begin
  case Key of
    27:
      Saga.Stages.SetStage(stNameMenu);
    13:
      Saga.New;
  end;
end;

{ TStageStorageMenu }

constructor TStageStorageMenu.Create;
begin
  Top := 12;
  FKeyFlag := True;
end;

procedure TStageStorageMenu.RenderNum;
var
  I, H: ShortInt;
begin
  Saga.Engine.FontColor(cLtGray);
  for I := 0 to 9 do
  begin
    if (I < 9) then H := 2 else H := 1;
    Saga.Engine.TextOut(H + 20, I + Top + 2, IntToStr(I + 1) + '.', aLeft);
  end;
end;

procedure TStageStorageMenu.Render;
var
  I: ShortInt;
begin
  for I := 0 to 9 do
  begin
    Saga.Engine.FontColor(clTitle);
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, cDkGray);
      Saga.Engine.FontColor(clMenuAct);
    end
    else
      Saga.Engine.FontColor(clMenuDef);
    if (Saga.GetSlotData(I) <> '') then
    begin
      Saga.Engine.TextOut(25, Top + I + 2, Saga.GetSlotData(I), aLeft)
    end
    else
    begin
      if (I <> MenuPos) then
        Saga.Engine.FontColor(clEPMText);
      Saga.Engine.TextOut(25, Top + I + 2, __('Empty slot'), aLeft);
    end;
  end;
  Self.RenderNum;
  if KeyFlag then Saga.Engine.KeyOut(45, Top + 13, __('Back'), 'ESC');
end;

procedure TStageStorageMenu.Update(var Key: Word);
begin
  case Key of
    27:
      Saga.Stages.Back;
    38, 40:
      MenuPos := Clamp(MenuPos + Key - 39, 0, 9, False);
  end;
end;

{ TStageSaveMenu }

procedure TStageSaveMenu.Render;
begin
  Saga.Engine.TitleOut(Top, __('Save game'));
  inherited Render;
  Saga.Engine.KeyOut(57, Top + 13, __('Save'), 'ENTER');
end;

procedure TStageSaveMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    13:
      begin
        Saga.SaveToSlot(MenuPos);
        Saga.Stages.SetStage(stGame);
      end;
  end;
end;

{ TStageLoadMenu }

procedure TStageLoadMenu.Render;
begin
  Saga.Engine.TitleOut(Top, __('Load game'));
  inherited;
  Saga.Engine.KeyOut(57, Top + 13, __('Load'), 'ENTER', FileExists(Saga.GetSlotPath(Self.MenuPos) + 'game.log'));
end;

procedure TStageLoadMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    13:
      if Saga.LoadFromSlot(MenuPos) then
        Saga.Stages.SetStage(stGame);
  end;
end;

{ TStageBattle }

procedure TStageBattle.Render;
begin
  Saga.Engine.TitleOut(5, 'Поединок');
  Saga.Engine.KeyOut(15, 6, 'Атаковать', '1');
  Saga.Engine.KeyOut(15, 7, 'Отступить', '2');

  Saga.Engine.FontColor(Saga.Player.Color);
  Saga.Engine.TextOut(90, 6, Saga.Player.Name + ' (' +
    Saga.Player.Atr[atLife].ToString + ')');
  Saga.Engine.FontColor(Saga.World.CurrentCreatures.Get(Saga.Battle.ID).Color);
  Saga.Engine.TextOut(90, 7, Saga.World.CurrentCreatures.Get(Saga.Battle.ID)
    .Name + ' (' + Saga.World.CurrentCreatures.Get(Saga.Battle.ID)
    .Atr[atLife].ToString + ')');

  Saga.Engine.FontColor(clSplText);
  Saga.Log[lgBattle].Render(35, 6, 55);
end;

procedure TStageBattle.Timer;
begin
  inherited;

end;

procedure TStageBattle.Update(var Key: Word);
begin
  case Key of
    49: // Атаковать
      begin
        Saga.Battle.PlayerMove();
      end;
    50: // Отступить
      begin
        Saga.Log[lgBattle].Add(__('Ты пытаешься уклониться от поединка.'));
        if (Math.RandomRange(1, 5) = 1) then
          Saga.Battle.Finish()
        else
          Saga.Battle.EnemyMove();
      end;
  end;
end;

{ TLinks }

procedure TLinks.Append(const ALabel, AName: string);
begin
  FLabelList.Append(ALabel);
  FNameList.Append(AName);
end;

procedure TLinks.Clear;
begin
  FLabelList.Clear;
  FNameList.Clear;
end;

function TLinks.Count: Integer;
begin
  Result := FLabelList.Count;
end;

constructor TLinks.Create;
begin
  FLabelList := TStringList.Create;
  FNameList := TStringList.Create;
  Self.Clear;
end;

destructor TLinks.Destroy;
begin
  FLabelList.Free;
  FNameList.Free;
  inherited;
end;

function TLinks.GetLabel(const I: Integer): string;
begin
  Result := FLabelList[I];
end;

function TLinks.GetName(const I: Integer): string;
begin
  Result := FNameList[I];
end;

{ TStageDialog }

constructor TStageDialog.Create;
begin
  FLinkList := TLinks.Create;
  FDialog := TScript.Create;
end;

destructor TStageDialog.Destroy;
begin
  FDialog.Free;
  FLinkList.Free;
  inherited;
end;

procedure TStageDialog.Render;
var
  I: Integer;
  S, N: string;
begin
  Saga.Engine.FontColor(Saga.World.CurrentCreatures.Get(ID).Color);
  Saga.Engine.TextOut(0, 9, Saga.World.CurrentCreatures.Get(ID).Name, aCenter);
  Saga.Engine.FontColor(Saga.Player.Color);
  Saga.Engine.TextOut(0, 24, Saga.Player.GetRaceName + ' ' +
    Saga.Player.Name, aCenter);
  Saga.Engine.FontColor(clSplText);
  Saga.Log[lgDialog].Render(35, 10, 55);
  for I := 0 to LinkList.Count - 1 do
  begin
    S := '';
    N := LinkList.GetLabel(I);
    N := SysUtils.StringReplace(N, '(' + Dialog.CloseTag + ')', __(Dialog.CloseStr),
      [SysUtils.rfIgnoreCase]);
    if (Copy(Trim(LinkList.GetName(I)), 1, Length(Dialog.CloseTag))
      = Dialog.CloseTag) then
      S := __(Dialog.CloseStr);
    Saga.Engine.KeyOut(35, I + 25, Trim(N + ' ' + S), Format('%d', [I + 1]));
  end;
end;

procedure TStageDialog.Update(var Key: Word);
begin
  case Key of
    49 .. 54: // 1..5
      begin
        if (Key - 49 > LinkList.Count - 1) or (Key < 49) then
          Exit;
        Answer(Key);
        Render;
      end;
  end;
end;

procedure TStageDialog.Answer(var Key: Word);
var
  ID: string;
begin
  Saga.Log[lgDialog].Clear;
  ID := Trim(LinkList.GetName(Key - 49));
  LinkList.Clear;
  Dialog.Next(ID);
end;

procedure TStageDialog.Timer;
begin
  inherited;

end;

{ TStageVictory }

procedure TStageVictory.Render;
begin
  Saga.Engine.TitleOut(Top, __('Victory!'));
  Saga.Engine.FontColor(clGoldText);
  Saga.Engine.TextOut(0, Top + 2, Format('%s поверг всех врагов', [Saga.Player.GetFullName]), aCenter);
  Saga.Engine.TextOut(0, Top + 3, Format('%s %d', [__('Honor'), Saga.Player.Score]), aCenter);
  Saga.Engine.KeyOut(0, Top + 5, __('Close'), 'ESC', aCenter);
end;

procedure TStageVictory.Update(var Key: Word);
begin
  case Key of
    27:
      begin 
        Saga.Recs.Load;
        Saga.Stages.SetStage(stRecMenu, stMainMenu);
      end;
  end;
end;

{ TStageDefeat }

procedure TStageDefeat.Render;
begin
  Saga.Engine.TitleOut(Top, __('Defeat!'));
  Saga.Engine.FontColor(clAlertText);
  Saga.Engine.TextOut(0, Top + 2, Format('%s повержен!', [Saga.Player.GetFullName]), aCenter);
  Saga.Engine.TextOut(0, Top + 3, Format('%s %d', [__('Honor'), Saga.Player.Score]), aCenter);
  Saga.Engine.KeyOut(0, Top + 5, __('Close'), 'ESC', aCenter);
end;

procedure TStageDefeat.Update(var Key: Word);
begin
  case Key of
    27:
      begin
        Saga.Recs.Load;
        Saga.Stages.SetStage(stRecMenu, stMainMenu);
      end;
  end;
end;

{ TStageQuestLog }

procedure TStageQuestLog.Render;
var
  I: ShortInt;
begin
  Saga.Engine.TitleOut(Top, __('Quest log'));
  for I := 0 to 9 do
  begin
    Saga.Engine.FontColor(clTitle);
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, cDkGray);
      Saga.Engine.FontColor(clMenuAct);
    end
    else
      Saga.Engine.FontColor(clMenuDef);
    if (Saga.Quest.Get(I, 0) <> '') then
    begin
      Saga.Engine.TextOut(25, Top + I + 2, Saga.Quest.Get(I, 0), aLeft)
    end
    else
    begin
      if (I <> MenuPos) then
        Saga.Engine.FontColor(clEPMText);
      Saga.Engine.TextOut(25, Top + I + 2, __('Empty slot'), aLeft);
    end;
  end;
  Self.RenderNum;
  Saga.Engine.KeyOut(45, Top + 13, __('Back'), 'ESC');
  Saga.Engine.KeyOut(57, Top + 13, __('Read'), 'ENTER', (Saga.Quest.Get(MenuPos, 0) <> ''));
end;

procedure TStageQuestLog.Update(var Key: Word);
var
  I: Byte;
begin
  inherited;
  case Key of
    27:
      Saga.Stages.SetStage(stGame);
    13:
      if (Saga.Quest.Get(MenuPos, 0) <> '') then
      begin
        Saga.Log[lgQuest].Clear;
        for I := 1 to 9 do
          Saga.Log[lgQuest].Add(Saga.Quest.Get(MenuPos, I), False);
        TStageQuestInfo(Saga.Stages.GetStage(stQuestInfo)).ID := MenuPos;
        Saga.Stages.SetStage(stQuestInfo);
      end;
  end;
end;

{ TStageQuestInfo }

procedure TStageQuestInfo.Render;
begin
  Saga.Engine.TitleOut(8, Saga.Quest.Get(ID, 0));
  Saga.Engine.FontColor(clSplText);
  Saga.Log[lgQuest].Render(35, 10, 55);
  Saga.Engine.KeyOut(0, 28, __('Back'), 'ESC', aCenter);
end;

procedure TStageQuestInfo.Timer;
begin
  inherited;

end;

procedure TStageQuestInfo.Update(var Key: Word);
begin
  case Key of
    27:
      Saga.Stages.SetStage(stQuestLog);
  end;
end;

{ TStageAboutMenu }

procedure TStageAboutMenu.Render;
begin
  Saga.Engine.TitleOut(Top, __('About'));
  Saga.Engine.KeyOut(0, Top + 5, __('Back to main menu'), 'ESC', aCenter);
end;

procedure TStageAboutMenu.Update(var Key: Word);
begin
  case Key of
    27:
      Saga.Stages.Back;
  end;
end;

{ TStageRecMenu }

constructor TStageRecMenu.Create;
begin
  inherited;
  KeyFlag := False;
end;

procedure TStageRecMenu.Render;
begin
  Saga.Engine.TitleOut(Top, __('High scores table'));
  inherited Render;
  case RecPos of
    0..9:
    begin
      if (Saga.Player.Score > 0) then
      begin
        RenderCursor(RecPos + Top + 2, cDkRed);
        Saga.Engine.FontColor(cWhiteYel);
        Saga.Engine.TextOut(25, RecPos + Top + 2, Saga.GetSlotData(RecPos), aLeft);
        Self.RenderNum;
      end;
    end;
  end;
  Saga.Engine.KeyOut(0, Top + 13, __('Back to main menu'), 'ESC', aCenter);
end;

procedure TStageRecMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    27:
        Saga.Stages.Back;
  end;
end;

{ TStageInv }

procedure TStageInv.Render;
var
  I: TInvByte;
  F: string;
begin
  Saga.Engine.TitleOut(5, __('Inventory'));
  for I := Low(TInvByte) to High(TInvByte) do
    if Saga.Player.Inventory.Item[I].Active then
    begin
      F := Saga.World.CurrentItems.GetItemPropStr(Saga.Player.Inventory.Item[I]);
      Saga.Engine.KeyOut(15, I + 6, F, chr(I + 64));
    end;
  Saga.Engine.KeyOut(0, Saga.Engine.Window.Height - 6, __('Close'), 'ESC', aCenter);
end;

procedure TStageInv.Timer;
begin
  inherited;

end;

procedure TStageInv.Update(var Key: Word);
begin
  inherited;
  case Key of
    27:
      Saga.Stages.SetStage(stGame);
  end;
end;

{ TStageItems }

procedure TStageItems.Render;
var
  I, C: Integer;
  F: string;
begin
  Saga.Engine.TitleOut(5, __('Items'));
  C := 0;
  for I := 0 to Saga.World.CurrentItems.Count - 1 do
  begin
    if (C > High(TInvByte) - 1) then Break;
    if (Saga.World.CurrentItems.Get(I).Active)
      and (Saga.World.CurrentItems.Get(I).Pos.X = Saga.Player.Pos.X)
      and (Saga.World.CurrentItems.Get(I).Pos.Y = Saga.Player.Pos.Y) then
    begin
      F := Saga.World.CurrentItems.GetItemPropStr(Saga.World.CurrentItems.Get(I));
      Saga.Engine.KeyOut(15, C + 7, F, chr(C + 65));
      Inc(C);
    end;
  end;
  Saga.Engine.KeyOut(42, Saga.Engine.Window.Height - 6, __('Close'), 'ESC');
  Saga.Engine.KeyOut(57, Saga.Engine.Window.Height - 6, __('Pickup all items'),
    'SPACE', (Saga.World.CurrentItems.Count(Saga.Player.Pos.X, Saga.Player.Pos.Y) > 0));
end;

procedure TStageItems.Timer;
begin
  inherited;

end;

procedure TStageItems.Update(var Key: Word);
var
  C, I: Integer;
  K: Word;
begin
  inherited;
  case Key of
    27:
      Saga.Stages.SetStage(stGame);
    ord('A')..ord('Z'):
    begin
      C := Saga.World.CurrentItems.Count(Saga.Player.Pos.X, Saga.Player.Pos.Y);
      if (C > 0) then
      begin
        I := Key - (ord('A'));
        if (I < C) then
        begin
          Saga.World.CurrentItems.Pickup(Saga.World.CurrentItems.GetIndex(I,
            Saga.Player.Pos.X, Saga.Player.Pos.Y));
          Self.Render;
        end;
      end;
    end;
    32:
    begin
      K := ord('A');
      for I := 0 to High(TInvByte) - 1 do
        Self.Update(K);
    end;
  end;
end;

end.
