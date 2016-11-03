unit ForgottenSaga.Scenes;

interface

{$I Include.inc}

uses Classes;

{$REGION ' TStages '}

type
  TStageEnum = (stGame, stMainMenu, stGameMenu, stRaceMenu, stNameMenu,
    stTextMenu, stSaveMenu, stLoadMenu, stBattle, stDefeat, stVictory, stDialog,
    stQuestLog, stQuestInfo, stAboutMenu, stRecMenu, stItems, stInv,
    stQuestItems);

type
  TStage = class(TInterfacedObject, IInterface)
    procedure Render; virtual; abstract;
    procedure Update(var Key: Word); virtual; abstract;
    procedure Timer; virtual; abstract;
  end;

type
  TStages = class(TStage)
  strict private
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

{$ENDREGION ' TStages '}
{$REGION ' TStageGame '}

type
  TStageGame = class(TStage)
  strict private
    procedure SetNPC(ID: Byte);
    procedure SetDialog(ID: Byte);
    procedure SetBattle(ID: Byte);
    procedure PlayerMove(X, Y: Integer);
    procedure RenderPlayerInfo;
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

{$ENDREGION ' TStageGame '}
{$REGION ' TStageCustomMenu '}

type
  TStageCustomMenu = class(TStage)
  strict private
    FTop: Byte;
    FMenuPos: ShortInt;
  public
    constructor Create;
    procedure RenderCursor(Y: Integer; Color: Integer);
    property Top: Byte read FTop write FTop;
    property MenuPos: ShortInt read FMenuPos write FMenuPos;
    procedure Timer; override;
  end;

{$ENDREGION ' TStageCustomMenu '}
{$REGION ' TStageCustomMenuHelper '}

type
  TStageCustomMenuHelper = class helper for TStageCustomMenu
    function Clamp(Value, AMin, AMax: Integer; Flag: Boolean): Integer;
  end;

{$ENDREGION ' TStageCustomMenuHelper '}
{$REGION ' Stages Menu '}

type
  TStageMenu = class(TStageCustomMenu)
  strict private
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
  strict private
  const
    FSVersion = '0.0.3';
  public const
    Copyright = 'Copyright (C) 2016 by Sergiy Tkach (DevApromix)';
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
  strict private
    FKeyFlag: Boolean;
  public
    constructor Create;
    procedure RenderNum(N: Integer = 9);
    procedure Render; override;
    procedure Update(var Key: Word); override;
    property KeyFlag: Boolean read FKeyFlag write FKeyFlag;
  end;

type
  TStageRecMenu = class(TStageStorageMenu)
  strict private
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

{$ENDREGION ' Stages Menu '}
{$REGION ' TStageBattle '}

type
  TStageBattle = class(TStage)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

{$ENDREGION ' TStageBattle '}
{$REGION ' Stages Inventory and Items '}

type
  TStageInv = class(TStage)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
  end;

type
  TStageQuestItems = class(TStage)
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

{$ENDREGION ' Stages Inventory and Items '}
{$REGION ' TStageQuestLog '}

type
  TStageQuestLog = class(TStageStorageMenu)
  private
    function GetRealMenuPos(CurrentMenuPos: Integer): Integer;
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

{$ENDREGION ' TStageQuestLog '}
{$REGION ' TStageQuestInfo '}

type
  TStageQuestInfo = class(TStage)
  strict private
    FID: Integer;
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
    procedure Timer; override;
    property ID: Integer read FID write FID;
  end;

{$ENDREGION ' TStageQuestInfo '}
{$REGION ' Stage Dialog '}

type
  TStageDialog = class(TStageMenu)
  strict private
  type
    TLinks = class
    strict private
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
  strict private
    FID: Integer;
    FLinkList: TLinks;
    procedure Answer(var Key: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Timer; override;
    procedure Update(var Key: Word); override;
    property ID: Integer read FID write FID;
    property LinkList: TLinks read FLinkList write FLinkList;
  end;

{$ENDREGION ' Stage Dialog '}
{$REGION ' TStageVictory '}

type
  TStageVictory = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

{$ENDREGION ' TStageVictory '}
{$REGION ' TStageDefeat '}

type
  TStageDefeat = class(TStageCustomMenu)
  private

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

{$ENDREGION ' TStageDefeat '}

implementation

uses SysUtils, Dialogs, Math, Engine, ForgottenSaga.Classes,
  ForgottenSaga.Entities;

{$REGION ' TStages '}

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
      stQuestItems:
        FStage[I] := TStageQuestItems.Create;
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

{$ENDREGION ' TStages '}
{$REGION ' TStageGame '}

procedure TStageGame.SetNPC(ID: Byte);
var
  D: TStageDialog;
begin
  D := TStageDialog(Saga.Stages.GetStage(stDialog));
  D.ID := ID;
  Saga.Dialog.LoadFromFile(TUtils.GetPath('resources') +
    Saga.World.CurrentCreatures.Entity[ID].ScriptFileName);
  Saga.Dialog.Next(Format('%d', [Saga.World.CurrentCreatures.Entity[ID]
    .Dialog]));
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
  if Saga.Player.Look.Active then
  begin
    Saga.Player.Look.Move(X, Y);
    Exit;
  end;
  ID := Saga.World.CurrentCreatures.Has(Saga.Player.Pos.X + X,
    Saga.Player.Pos.Y + Y);
  if (ID > -1) then
    case Saga.World.CurrentCreatures.Entity[ID].Force of
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
  Saga.World.CurrentItems.Render;
  Saga.World.CurrentCreatures.Render;
  Saga.Player.Render;
  Saga.Player.Look.Render;
  Saga.Engine.BackgroundColor(0);
  RenderPlayerInfo;
  Saga.Log[lgGame].Render(81, 15, 39);
  Saga.Notification.Render(0, 0);
end;

procedure TStageGame.RenderPlayerInfo;
var
  I: TCreature.TAtrEnum;
begin
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  Saga.Engine.Print(81, 0, __(Saga.World.CurrentMap.Name));
  Saga.Engine.Print(81, 1, Saga.Player.GetFullName);
  Saga.Engine.Print(81, 2, __('Honor') + ' ' + IntToStr(Saga.Player.Score));
  Saga.Engine.Print(81, 3, __('Gold') + ' ' + IntToStr(Saga.Player.Gold));
  for I := Low(TCreature.TAtrEnum) to High(TCreature.TAtrEnum) do
    Saga.Engine.Print(81, ord(I) + 4, __(TCreature.AtrStr[I]) + ' ' +
      Saga.Player.Atr[I].ToText);
end;

procedure TStageGame.Timer;
begin
  Saga.Notification.Dec();
  Saga.Stages.Render;
end;

procedure TStageGame.Update(var Key: Word);
begin
  // Box(Ord(Key));
  case Key of
    TK_ESCAPE:
      begin
        if Saga.Player.Look.Active then
        begin
          Saga.Player.Look.Active := False;
          Exit;
        end;
        Saga.Stages.SetStage(stGameMenu);
      end;
    TK_F2:
      begin
        Saga.InitSlots;
        Saga.Stages.SetStage(stSaveMenu, stGame);
      end;
    TK_F3:
      begin
        Saga.InitSlots;
        Saga.Stages.SetStage(stLoadMenu, stGame);
      end;
    TK_LEFT, TK_KP_4, TK_A:
      PlayerMove(-1, 0);
    TK_RIGHT, TK_KP_6, TK_D:
      PlayerMove(1, 0);
    TK_UP, TK_KP_8, TK_W:
      PlayerMove(0, -1);
    TK_DOWN, TK_KP_2, TK_X:
      PlayerMove(0, 1);
    TK_KP_7, {$IFDEF VCLENGINE}36, {$ENDIF}TK_Q:
      PlayerMove(-1, -1);
    TK_KP_9, {$IFDEF VCLENGINE}33, {$ENDIF}TK_E:
      PlayerMove(1, -1);
    TK_KP_1, {$IFDEF VCLENGINE}35, {$ENDIF}TK_Z:
      PlayerMove(-1, 1);
    TK_KP_3, {$IFDEF VCLENGINE}34, {$ENDIF}TK_C:
      PlayerMove(1, 1);
    TK_KP_5, {$IFDEF VCLENGINE}12, {$ENDIF}TK_S:
      PlayerMove(0, 0);
    TK_COMMA:
      if Saga.World.CurrentMap.HasTile(tStUp, Saga.Player.Pos.X,
        Saga.Player.Pos.Y) then
        TWorld.GoLoc(drTop);
    TK_PERIOD:
      begin
        if Saga.World.CurrentMap.HasTile(tStDn, Saga.Player.Pos.X,
          Saga.Player.Pos.Y) then
          TWorld.GoLoc(drBottom);
      end;
    // BeginTest
    TK_1:
      TWorld.GoLoc(drLeft);
    TK_2:
      TWorld.GoLoc(drRight);
    // End Test
    TK_J:
      Saga.Stages.SetStage(stQuestLog);
    TK_I:
      Saga.Stages.SetStage(stInv);
    TK_V:
      Saga.Player.Victory;
    TK_B:
      Saga.Player.Defeat;
    TK_G:
      Saga.Player.Pickup;
    TK_H:
      Saga.World.CurrentItems.Add('|', $00AAFF88, 1, 'Посох Шамана', mtBone,
        ctStaff, 14);
    TK_L:
      begin
        Saga.Player.Look.SetPosition(Saga.Player.Pos);
        Saga.Player.Look.Active := not Saga.Player.Look.Active;
      end;
  end;
end;

{$ENDREGION ' TStageGame '}
{$REGION ' TStageCustomMenu '}

constructor TStageCustomMenu.Create;
begin
  Top := 16;
  MenuPos := 0;
end;

procedure TStageCustomMenu.RenderCursor(Y: Integer; Color: Integer);
begin
  Saga.Engine.BackgroundColor(Color);
  Saga.Engine.Print(0, Y, StringOfChar(#32, Saga.Engine.Window.Width -
    10), aCenter);
  Saga.Engine.BackgroundColor(TEngine.clClear);
end;

procedure TStageCustomMenu.Timer;
begin

end;

{$ENDREGION ' TStageCustomMenu '}
{$REGION ' TStageCustomMenuHelper '}

function TStageCustomMenuHelper.Clamp(Value, AMin, AMax: Integer;
  Flag: Boolean): Integer;
begin
  Result := Value;
  if (Result < AMin) then
    if Flag then
      Result := AMin
    else
      Result := AMax;
  if (Result > AMax) then
    if Flag then
      Result := AMax
    else
      Result := AMin;
end;

{$ENDREGION ' TStageCustomMenuHelper '}
{$REGION ' TStageMenu '}

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
  Saga.UI.DrawTitle(Top, __('Forgotten Saga'));
  for I := 0 to Count - 1 do
  begin
    Saga.Engine.ForegroundColor(Saga.Colors.clTitle);
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, Saga.Colors.clCursor);
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuAct);
    end
    else
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuDef);
    if (Items <> '') then
      Saga.Engine.Print(0, I + Top + 2, __(TUtils.GetStr('|', Items, I)
        ), aCenter);
  end;
end;

procedure TStageMenu.Update(var Key: Word);
begin
  case Key of
    TK_DOWN:
      MenuPos := Clamp(MenuPos + 1, 0, Count - 1, False);
    TK_UP:
      MenuPos := Clamp(MenuPos - 1, 0, Count - 1, False);
  end;
end;
{$ENDREGION ' TStageMenu '}
{$REGION ' TStageMainMenu '}

constructor TStageMainMenu.Create;
begin
  inherited;
  Items := 'New game|Load game|High scores table|About FS|Quit';
  Count := 5;
end;

procedure TStageMainMenu.Render;
begin
  inherited Render;
  Saga.Engine.ForegroundColor(Saga.Colors.GetColor(ceLGray));
  Saga.Engine.Print(0, Saga.Engine.Window.Height - 1, Copyright, aCenter);
  Saga.Engine.Print(0, Saga.Engine.Window.Height - 1, 'v.' + FSVersion, aRight);
end;

procedure TStageMainMenu.Update(var Key: Word);
begin
  inherited;
  // Box(MenuPos);
  case Key of
    TK_ENTER:
      case MenuPos of
        0:
          begin
            // Box;
            Saga.Stages.SetStage(stRaceMenu);
          end;
        1:
          begin
            Saga.InitSlots;
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
          Saga.Engine.Close();
      end;
  end;
end;
{$ENDREGION ' TStageMainMenu '}
{$REGION ' TStageGameMenu '}

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
    TK_ESCAPE:
      Saga.Stages.SetStage(stGame);
    TK_ENTER:
      case MenuPos of
        0:
          Saga.Stages.SetStage(stGame);
        1:
          begin
            Saga.InitSlots;
            Saga.Stages.SetStage(stSaveMenu, stGameMenu);
          end;
        2:
          begin
            Saga.InitSlots;
            Saga.Stages.SetStage(stLoadMenu, stGameMenu);
          end;
        3:
          Saga.Stages.SetStage(stMainMenu);
      end;
  end;
end;
{$ENDREGION ' TStageGameMenu '}
{$REGION ' TStageRaceMenu '}

procedure TStageRaceMenu.Render;
var
  R: TSaga.TRaceEnum;
begin
  Saga.UI.DrawTitle(Top, __('Select race'));

  for R := Low(TSaga.TRaceEnum) to High(TSaga.TRaceEnum) do
  begin
    Saga.Engine.ForegroundColor(Saga.Colors.clTitle);
    if (ord(R) = MenuPos) then
      RenderCursor(ord(R) + Top + 2, Saga.Colors.clCursor);
    Saga.Engine.ForegroundColor(Saga.Race[R].Color);
    Saga.Engine.Print(0, ord(R) + Top + 2, Saga.Race[R].Name, aCenter);
  end;
  Saga.UI.DrawKey(0, Top + ord(High(TSaga.TRaceEnum)) + 4,
    __('Back to main menu'), 'ESC', aCenter);
end;

procedure TStageRaceMenu.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Saga.Stages.SetStage(stMainMenu);
    TK_UP:
      MenuPos := Clamp(MenuPos - 1, ord(Low(TSaga.TRaceEnum)),
        ord(High(TSaga.TRaceEnum)), False);
    TK_DOWN:
      MenuPos := Clamp(MenuPos + 1, ord(Low(TSaga.TRaceEnum)),
        ord(High(TSaga.TRaceEnum)), False);
    TK_ENTER:
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
{$ENDREGION ' TStageRaceMenu '}
{$REGION ' TStageNameMenu '}

procedure TStageNameMenu.Render;
begin
  Saga.UI.DrawTitle(Top, __('What is your name?'));
  Saga.Engine.ForegroundColor
    (Saga.Race[TSaga.TRaceEnum(Saga.Player.Race)].Color);
  Saga.Engine.Print(0, Top + 2, Saga.Player.GetRaceName + ' <' +
    Saga.Player.Name + '>', aCenter);
  Saga.UI.DrawKey(0, Top + 4, __('Random name'), 'SPACE', aCenter);
end;

procedure TStageNameMenu.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Saga.Stages.SetStage(stRaceMenu);
    TK_ENTER:
      Saga.Stages.SetStage(stTextMenu);
    TK_SPACE:
      Saga.Player.GenName;
  end;
end;
{$ENDREGION ' TStageNameMenu '}
{$REGION ' TStageTextMenu '}

procedure TStageTextMenu.Render;
begin
  Saga.UI.DrawTitle(13, 'Как все начиналось...');
  Saga.UI.DrawKey(0, 25, 'Начать игру...', 'ENTER', aCenter);
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  Saga.Log[lgIntro].Render(15, 15, 85);
end;

procedure TStageTextMenu.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Saga.Stages.SetStage(stNameMenu);
    TK_ENTER:
      Saga.New;
  end;
end;
{$ENDREGION ' TStageTextMenu '}
{$REGION ' TStageStorageMenu '}

constructor TStageStorageMenu.Create;
begin
  Top := 12;
  FKeyFlag := True;
end;

procedure TStageStorageMenu.RenderNum(N: Integer = 9);
var
  I, H: ShortInt;
begin
  Saga.Engine.ForegroundColor(Saga.Colors.GetColor(ceLGray));
  for I := 0 to N do
  begin
    if (I < N) then
      H := 2
    else
      H := 1;
    Saga.Engine.Print(H + 20, I + Top + 2, IntToStr(I + 1) + '.', aLeft);
  end;
end;

procedure TStageStorageMenu.Render;
var
  I: ShortInt;
begin
  for I := 0 to 9 do
  begin
    Saga.Engine.ForegroundColor(Saga.Colors.clTitle);
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, Saga.Colors.clCursor);
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuAct);
    end
    else
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuDef);
    if (Saga.GetSlotData(I) <> '') then
    begin
      Saga.Engine.Print(25, Top + I + 2, Saga.GetSlotData(I), aLeft)
    end
    else
    begin
      if (I <> MenuPos) then
        Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
      Saga.Engine.Print(25, Top + I + 2, __('Empty slot'), aLeft);
    end;
  end;
  Self.RenderNum;
  if KeyFlag then
    Saga.UI.DrawKey(45, Top + 13, __('Back'), 'ESC');
end;

procedure TStageStorageMenu.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      begin
        Saga.Stages.Back;
      end;
    TK_UP:
      MenuPos := Clamp(MenuPos - 1, 0, 9, False);
    TK_DOWN:
      MenuPos := Clamp(MenuPos + 1, 0, 9, False);
  end;
end;

{$ENDREGION ' TStageStorageMenu '}
{$REGION ' TStageSaveMenu '}

procedure TStageSaveMenu.Render;
begin
  Saga.UI.DrawTitle(Top, __('Save game'));
  inherited Render;
  Saga.UI.DrawKey(57, Top + 13, __('Save'), 'ENTER');
end;

procedure TStageSaveMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    TK_ENTER:
      begin
        Saga.SaveToSlot(MenuPos);
        Saga.Stages.SetStage(stGame);
      end;
  end;
end;

{$ENDREGION ' TStageSaveMenu '}
{$REGION ' TStageLoadMenu '}

procedure TStageLoadMenu.Render;
begin
  Saga.UI.DrawTitle(Top, __('Load game'));
  inherited;
  Saga.UI.DrawKey(57, Top + 13, __('Load'), 'ENTER',
    FileExists(Saga.GetSlotPath(Self.MenuPos) + 'game.log'));
end;

procedure TStageLoadMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    TK_ENTER:
      if Saga.LoadFromSlot(MenuPos) then
        Saga.Stages.SetStage(stGame);
  end;
end;

{$ENDREGION ' TStageLoadMenu '}
{$REGION ' TStageBattle '}

procedure TStageBattle.Render;
var
  Creature: TCreature;
begin
  Saga.UI.DrawTitle(5, 'Поединок');
  Saga.UI.DrawKey(15, 6, 'Атаковать', '1');
  Saga.UI.DrawKey(15, 7, 'Отступить', '2');

  Saga.Engine.ForegroundColor(Saga.Player.Color);
  Saga.Engine.Print(90, 6, Saga.Player.Name + ' (' + Saga.Player.Atr[atLife]
    .ToText + ')');
  Creature := Saga.World.CurrentCreatures.Entity[Saga.Battle.ID];
  Saga.Engine.ForegroundColor(Creature.Color);
  Saga.Engine.Print(90, 7, Creature.Name + ' (' + Creature.Atr[atLife]
    .ToText + ')');
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  Saga.Log[lgBattle].Render(35, 6, 55);
end;

procedure TStageBattle.Timer;
begin

end;

procedure TStageBattle.Update(var Key: Word);
begin
  case Key of
    TK_1: // Атаковать
      begin
        Saga.Battle.PlayerMove();
      end;
    TK_2: // Отступить
      begin
        Saga.Log[lgBattle].Add(__('Ты пытаешься уклониться от поединка.'));
        if (Math.RandomRange(1, 5) = 1) then
          Saga.Battle.Finish()
        else
          Saga.Battle.EnemyMove();
      end;
  end;
end;

{$ENDREGION ' TStageBattle '}
{$REGION ' TStageDialog.TLinks '}

procedure TStageDialog.TLinks.Append(const ALabel, AName: string);
begin
  FLabelList.Append(ALabel);
  FNameList.Append(AName);
end;

procedure TStageDialog.TLinks.Clear;
begin
  FLabelList.Clear;
  FNameList.Clear;
end;

function TStageDialog.TLinks.Count: Integer;
begin
  Result := FLabelList.Count;
end;

constructor TStageDialog.TLinks.Create;
begin
  FLabelList := TStringList.Create;
  FNameList := TStringList.Create;
  Self.Clear;
end;

destructor TStageDialog.TLinks.Destroy;
begin
  FLabelList.Free;
  FNameList.Free;
  inherited;
end;

function TStageDialog.TLinks.GetLabel(const I: Integer): string;
begin
  Result := FLabelList[I];
end;

function TStageDialog.TLinks.GetName(const I: Integer): string;
begin
  Result := FNameList[I];
end;

{$ENDREGION ' TStageDialog.TLinks '}
{$REGION ' TStageDialog '}

constructor TStageDialog.Create;
begin
  inherited;
  Top := 23;
  FLinkList := TLinks.Create;
end;

destructor TStageDialog.Destroy;
begin
  FLinkList.Free;
  inherited;
end;

procedure TStageDialog.Render;
var
  I: Integer;
  S, N, Close: string;
begin
  Self.Count := LinkList.Count;
  for I := 0 to Count - 1 do
  begin
    Saga.Engine.ForegroundColor(Saga.Colors.clTitle);
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, Saga.Colors.clCursor);
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuAct);
    end
    else
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuDef);
  end;
  Saga.Engine.ForegroundColor(Saga.World.CurrentCreatures.Entity[ID].Color);
  Saga.Engine.Print(0, 9, __(Saga.World.CurrentCreatures.Entity[ID]
    .Name), aCenter);
  Saga.Engine.ForegroundColor(Saga.Player.Color);
  Saga.Engine.Print(0, 24, Saga.Player.GetRaceName + ' ' +
    Saga.Player.Name, aCenter);
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  Saga.Log[lgDialog].Render(35, 10, 55);
  for I := 0 to LinkList.Count - 1 do
  begin
    S := '';
    Close := Format('(%s)', [__('Close')]);
    N := LinkList.GetLabel(I);
    N := SysUtils.StringReplace(N, '(' + Saga.Dialog.CloseTag + ')', Close,
      [SysUtils.rfIgnoreCase]);
    if (Copy(Trim(LinkList.GetName(I)), 1,
      TEngine.GetTextLength(Saga.Dialog.CloseTag)) = Saga.Dialog.CloseTag) then
      S := Close;
    Saga.Engine.ForegroundColor(IfThen(I = MenuPos, Saga.Colors.clTitle,
      Saga.Colors.clSplText));
    Saga.Engine.Print(35, I + 25, Format(TEntity.KeyFmt, [IntToStr(I + 1),
      Trim(N + ' ' + S)]));
  end;
end;

procedure TStageDialog.Update(var Key: Word);
begin
  inherited;
  case Key of
    TK_ENTER:
      begin
        Key := MenuPos + TK_1;
        Answer(Key);
        Render;
      end;
    TK_1 .. TK_5: // 1..5
      begin
        if (Key - TK_1 > LinkList.Count - 1) or (Key < TK_1) then
          Exit;
        Answer(Key);
        Render;
      end;
  end;
end;

procedure TStageDialog.Answer(var Key: Word);
var
  S: string;
begin
  Saga.Log[lgDialog].Clear;
  S := Trim(LinkList.GetName(Key - TK_1));
  LinkList.Clear;
  Saga.Dialog.Next(S);
  MenuPos := 0;
end;

procedure TStageDialog.Timer;
begin

end;

{$ENDREGION ' TStageDialog '}
{$REGION ' TStageVictory '}

procedure TStageVictory.Render;
begin
  Saga.UI.DrawTitle(Top, __('Victory!'));
  Saga.Engine.ForegroundColor(Saga.Colors.clGoldText);
  Saga.Engine.Print(0, Top + 2, Format('%s поверг всех врагов',
    [Saga.Player.GetFullName]), aCenter);
  Saga.Engine.Print(0, Top + 3, Format('%s %d', [__('Honor'), Saga.Player.Score]
    ), aCenter);
  Saga.UI.DrawKey(0, Top + 5, __('Close'), 'ESC', aCenter);
end;

procedure TStageVictory.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      begin
        Saga.Recs.Load;
        Saga.Stages.SetStage(stRecMenu, stMainMenu);
      end;
  end;
end;

{$ENDREGION ' TStageVictory '}
{$REGION ' TStageDefeat '}

procedure TStageDefeat.Render;
begin
  Saga.UI.DrawTitle(Top, __('Defeat!'));
  Saga.Engine.ForegroundColor(Saga.Colors.clAlertText);
  Saga.Engine.Print(0, Top + 2, Format('%s повержен!', [Saga.Player.GetFullName]
    ), aCenter);
  Saga.Engine.Print(0, Top + 3, Format('%s %d', [__('Honor'), Saga.Player.Score]
    ), aCenter);
  Saga.UI.DrawKey(0, Top + 5, __('Close'), 'ESC', aCenter);
end;

procedure TStageDefeat.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      begin
        Saga.Recs.Load;
        Saga.Stages.SetStage(stRecMenu, stMainMenu);
      end;
  end;
end;

{$ENDREGION ' TStageDefeat '}
{$REGION ' TStageQuestLog '}

function TStageQuestLog.GetRealMenuPos(CurrentMenuPos: Integer): Integer;
var
  I, J: Integer;
begin
  J := -1;
  for I := 0 to Saga.Quest.Count - 1 do
    if (Saga.Quest.Get(I, 0) <> '') then
    begin
      Inc(J);
      if (J = CurrentMenuPos) then
      begin
        Result := J;
        Break;
      end;
    end;
end;

procedure TStageQuestLog.Render;
var
  I, J: Integer;
  SL: TStringList;

  procedure RenderQuestLogCursor(I: Integer);
  begin
    if (I = MenuPos) then
    begin
      RenderCursor(I + Top + 2, Saga.Colors.clCursor);
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuAct);
    end
    else
      Saga.Engine.ForegroundColor(Saga.Colors.clMenuDef);
  end;

  procedure RenderQuestLogTitles(I: Integer);
  begin
    Saga.Engine.ForegroundColor(Saga.Colors.clTitle);
    if (Saga.Quest.Get(I - 1, 0) <> '') then
    begin
      Saga.Engine.Print(25, Top + J + 2, Saga.Quest.Get(I - 1, 0), aLeft)
    end
    else
    begin
      if (I <> MenuPos) then
        Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
      Saga.Engine.Print(25, Top + J + 2, __('Empty slot'), aLeft);
    end;
  end;

begin
  Saga.UI.DrawTitle(Top, __('Quest log'));
  SL := TUtils.ExplodeString(',', Saga.Player.Quests);
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  for I := 0 to Saga.Quest.Count - 1 do
  begin
    RenderQuestLogCursor(I);
    Saga.Engine.Print(25, Top + I + 2, __('Empty slot'), aLeft);
  end;
  for J := 0 to SL.Count - 1 do
  begin
    RenderQuestLogTitles(StrToInt(SL[J]));
  end;
  // Saga.Engine.Print(0, 0, SL.Text, aLeft); // Debug
  Self.RenderNum();
  Saga.UI.DrawKey(45, Top + 13, __('Back'), 'ESC');
  Saga.UI.DrawKey(57, Top + 13, __('Read'), 'ENTER', (MenuPos <= SL.Count - 1));
end;

procedure TStageQuestLog.Update(var Key: Word);
var
  I, J: Integer;
  SL: TStringList;
begin
  inherited;
  case Key of
    TK_ESCAPE:
      Saga.Stages.SetStage(stGame);
    TK_ENTER:
      begin
        SL := TUtils.ExplodeString(',', Saga.Player.Quests);
        if (MenuPos < SL.Count) then
        begin
          J := StrToInt(SL[MenuPos]) - 1;
          if (Saga.Quest.Get(J, 0) <> '') then
          begin
            Saga.Log[lgQuest].Clear;
            for I := 1 to 24 do
              Saga.Log[lgQuest].Add(Saga.Quest.Get(J, I), False);
            TStageQuestInfo(Saga.Stages.GetStage(stQuestInfo)).ID := J;
            Saga.Stages.SetStage(stQuestInfo);
          end;
        end;
      end;
  end;
end;
{$ENDREGION ' TStageQuestLog '}
{$REGION ' TStageQuestInfo '}

procedure TStageQuestInfo.Render;
var
  S: string;
begin
  S := Saga.Quest.Get(ID, 0);
  Saga.UI.DrawTitle(8, Trim(Copy(S, Pos(TEngine.kcEnd, S) + 1, Length(S))));
  Saga.Engine.ForegroundColor(Saga.Colors.clSplText);
  Saga.Log[lgQuest].Render(35, 10, 55);
  Saga.UI.DrawKey(0, 28, __('Back'), 'ESC', aCenter);
end;

procedure TStageQuestInfo.Timer;
begin

end;

procedure TStageQuestInfo.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Saga.Stages.SetStage(stQuestLog);
  end;
end;

{$ENDREGION ' TStageQuestInfo '}
{$REGION ' TStageAboutMenu '}

procedure TStageAboutMenu.Render;
begin
  Saga.UI.DrawTitle(Top, __('Forgotten Saga'));
  Saga.Engine.ForegroundColor(Saga.Colors.GetColor(ceLGray));
  Saga.Engine.Print(0, Top + 2, 'github.com/devapromix/forgotten-saga',
    aCenter);
  Saga.Engine.Print(0, Top + 3, TStageMainMenu.Copyright, aCenter);
  Saga.UI.DrawKey(0, Top + 5, __('Back to main menu'), 'ESC', aCenter);
end;

procedure TStageAboutMenu.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Saga.Stages.Back;
  end;
end;

{$ENDREGION ' TStageAboutMenu '}
{$REGION ' TStageRecMenu '}

constructor TStageRecMenu.Create;
begin
  inherited;
  KeyFlag := False;
end;

procedure TStageRecMenu.Render;
begin
  Saga.UI.DrawTitle(Top, __('High scores table'));
  inherited Render;
  case RecPos of
    0 .. 9:
      begin
        if (Saga.Player.Score > 0) then
        begin
          RenderCursor(RecPos + Top + 2, Saga.Colors.GetColor(ceRed));
          Saga.Engine.ForegroundColor(Saga.Colors.GetColor(ceWhite));
          Saga.Engine.Print(25, RecPos + Top + 2,
            Saga.GetSlotData(RecPos), aLeft);
          Self.RenderNum;
        end;
      end;
  end;
  Saga.UI.DrawKey(0, Top + 13, __('Back to main menu'), 'ESC', aCenter);
end;

procedure TStageRecMenu.Update(var Key: Word);
begin
  inherited;
  case Key of
    TK_ESCAPE:
      Saga.Stages.Back;
  end;
end;

{$ENDREGION ' TStageRecMenu '}
{$REGION ' TStageInv '}

procedure TStageInv.Render;
var
  I: TPlayer.TInventory.TInvByte;
  F: string;
begin
  Saga.UI.DrawTitle(5, __('Inventory'));
  for I := Low(TPlayer.TInventory.TInvByte)
    to High(TPlayer.TInventory.TInvByte) do
    if Saga.Player.Inventory.Item[I].Active then
    begin
      F := Saga.World.CurrentItems.ToText(Saga.Player.Inventory.Item[I]);
      Saga.UI.DrawKey(15, I + 6, F, chr(I + 64));
    end;
  Saga.UI.DrawKey(44, Saga.Engine.Window.Height - 6, __('Close'), 'ESC');
  Saga.UI.DrawKey(58, Saga.Engine.Window.Height - 6,
    Format('%s (%d)', [__('Quest Items'), Saga.Player.QuestItems.Count]),
    'SPACE', (Saga.Player.QuestItems.Count > 0));
end;

procedure TStageInv.Timer;
begin

end;

procedure TStageInv.Update(var Key: Word);
begin
  case Key of
    TK_SPACE:
      if (Saga.Player.QuestItems.Count > 0) then
        Saga.Stages.SetStage(stQuestItems);
    TK_ESCAPE:
      Saga.Stages.SetStage(stGame);
  end;
end;

{$ENDREGION ' TStageInv '}
{$REGION ' TStageItems '}

procedure TStageItems.Render;
var
  Entity: TEntity;
  I, C: Integer;
  S: string;
begin
  Saga.UI.DrawTitle(5, __('Items'));
  C := 0;
  for I := Saga.World.CurrentItems.Count - 1 downto 0 do
  begin
    if (C > High(TPlayer.TInventory.TInvByte) - 1) then
      Break;
    Entity := Saga.World.CurrentItems.Entity[I];
    if (Entity.Active) and (Entity.Pos.X = Saga.Player.Pos.X) and
      (Entity.Pos.Y = Saga.Player.Pos.Y) then
    begin
      S := Saga.World.CurrentItems.ToText(Entity as TItem);
      Saga.UI.DrawKey(15, C + 7, S, chr(C + 65));
      Inc(C);
    end;
  end;
  Saga.UI.DrawKey(42, Saga.Engine.Window.Height - 6, __('Close'), 'ESC');
  Saga.UI.DrawKey(57, Saga.Engine.Window.Height - 6, __('Pickup all items'),
    'SPACE', (Saga.World.CurrentItems.Count(Saga.Player.Pos.X,
    Saga.Player.Pos.Y) > 0));
end;

procedure TStageItems.Timer;
begin

end;

procedure TStageItems.Update(var Key: Word);
var
  C, I: Integer;
  K: Word;
begin
  case Key of
    TK_ESCAPE:
      Saga.Stages.SetStage(stGame);
    TK_A .. TK_Z:
      begin
        C := Saga.World.CurrentItems.Count(Saga.Player.Pos.X,
          Saga.Player.Pos.Y);
        if (C > 0) then
        begin
          I := Key - TK_A;
          if (I < C) then
          begin
            Saga.World.CurrentItems.Pickup(Saga.World.CurrentItems.GetIndex(I,
              Saga.Player.Pos.X, Saga.Player.Pos.Y));
            Self.Render;
          end;
        end;
      end;
    TK_SPACE:
      begin
        K := TK_A;
        for I := 0 to High(TPlayer.TInventory.TInvByte) - 1 do
          Self.Update(K);
      end;
  end;
end;

{$ENDREGION ' TStageItems '}
{$REGION ' TStageQuestItems '}

procedure TStageQuestItems.Render;
var
  I, C: Integer;
  S: string;
begin
  Saga.UI.DrawTitle(5, __('Quest Items'));
  C := Saga.Player.QuestItems.Count;
  if (C > 26) then
    C := 26;
  for I := 0 to C - 1 do
  begin
    S := Format('%s (%d)', [Saga.Player.QuestItems.Name(I),
      Saga.Player.QuestItems.Value(I)]);
    Saga.UI.DrawKey(15, I + 6, S, chr(I + 65));
  end;
  Saga.UI.DrawKey(0, Saga.Engine.Window.Height - 6, __('Close'),
    'SPACE', aCenter);
end;

procedure TStageQuestItems.Timer;
begin

end;

procedure TStageQuestItems.Update(var Key: Word);
begin
  case Key of
    TK_SPACE:
      Saga.Stages.SetStage(stInv);
  end;
end;

{$ENDREGION ' TStageQuestItems '}

end.
