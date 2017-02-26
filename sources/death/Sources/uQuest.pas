unit uQuest;

interface

type
  // Состояние квеста
  TQuestState = (qsNone, qsQuest, qsDone);

  // Тип квеста
  TQuestType = (qtKill, qtItem, qtLoc);

  // Тип вознаграждения за квест
  TQuestBonusType = (qbGold, qbExp, qbItem);

  // Квест
  TQuest = class(TObject)
  private
    FName: string;
    FDescription: string;
    FState: TQuestState;
    FQType: TQuestType;
    FObjectCount: Integer;
    FBonusCount: Integer;
    FBonusType: TQuestBonusType;
    FBonusID: Integer;
    FObjectID: Integer;
    FProgress: Integer;
    FNPC: string;
    FDifficulty: Byte;
    procedure SetName(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetQType(const Value: TQuestType);
    procedure SetState(const Value: TQuestState);
    procedure SetObjectCount(const Value: Integer);
    procedure SetBonusType(const Value: TQuestBonusType);
    procedure SetBonusCount(const Value: Integer);
    procedure SetBonusID(const Value: Integer);
    procedure SetObjectID(const Value: Integer);
    procedure SetProgress(const Value: Integer);
    procedure SetNPC(const Value: string);
    procedure SetDifficulty(const Value: Byte);
  public
    property Name: string read FName write SetName;
    property NPC: string read FNPC write SetNPC;
    property Description: string read FDescription write SetDescription;
    property State: TQuestState read FState write SetState;
    property Progress: Integer read FProgress write SetProgress;
    property QType: TQuestType read FQType write SetQType;
    property ObjectID: Integer read FObjectID write SetObjectID;
    property ObjectCount: Integer read FObjectCount write SetObjectCount;
    property BonusType: TQuestBonusType read FBonusType write SetBonusType;
    property BonusID: Integer read FBonusID write SetBonusID;
    property BonusCount: Integer read FBonusCount write SetBonusCount;
    property Difficulty: Byte read FDifficulty write SetDifficulty;
    constructor Create(
      AName, ADescription: string;
      AType: TQuestType;
      AObjectID: Integer;
      AObjectCount: Integer;
      ABonusType: TQuestBonusType;
      ABonusID: Integer;
      ABonusCount: Integer);
    destructor Destroy; override;
  end;

  // Все квесты
  TQuests = class(TObject)
  public
    Quest: array of TQuest;
    function Count: Byte;
    function Check(I: Byte): Boolean;
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

var
  Quests: TQuests;

implementation

uses uBox, uStringUtils, SysUtils, IniFiles, uKillList, uMsg, uPC, uLang;

{ TQuest }

constructor TQuest.Create(
      AName, ADescription: string;
      AType: TQuestType;
      AObjectID: Integer;
      AObjectCount: Integer;
      ABonusType: TQuestBonusType;
      ABonusID: Integer;
      ABonusCount: Integer);
begin
  Difficulty := 0;
  State := qsNone;  
  Progress := 0;
  Name := AName;
  NPC := '';
  Description := ADescription;
  QType := AType;
  ObjectID := AObjectID;
  ObjectCount := AObjectCount;
  BonusType := ABonusType;
  BonusID := ABonusID;
  BonusCount := ABonusCount;
end;

destructor TQuest.Destroy;
begin

  inherited;
end;

procedure TQuest.SetBonusType(const Value: TQuestBonusType);
begin
  FBonusType := Value;
end;

procedure TQuest.SetBonusCount(const Value: Integer);
begin
  FBonusCount := Value;
end;

procedure TQuest.SetObjectCount(const Value: Integer);
begin
  FObjectCount := Value;
end;

procedure TQuest.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TQuest.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TQuest.SetQType(const Value: TQuestType);
begin
  FQType := Value;
end;

procedure TQuest.SetState(const Value: TQuestState);
begin
  FState := Value;
end;

procedure TQuest.SetBonusID(const Value: Integer);
begin
  FBonusID := Value;
end;

procedure TQuest.SetObjectID(const Value: Integer);
begin
  FObjectID := Value;
end;

procedure TQuest.SetProgress(const Value: Integer);
begin
  FProgress := Value;
end;

procedure TQuest.SetNPC(const Value: string);
begin
  FNPC := Value;
end;

procedure TQuest.SetDifficulty(const Value: Byte);
begin
  FDifficulty := Value;
end;

{ TQuests }

destructor TQuests.Destroy;
var
  I: Byte;
begin
  if (High(Quest) > 0) then
    for I := 0 to High(Quest) do Quest[I].Free;
  inherited;
end;

function TQuests.Count: Byte;
begin
  Result := Length(Quest);
end;

constructor TQuests.Create(const FileName: string);
var
  I, P: Integer;
  F: TIniFile;
  S: string;
  QT: TQuestType;
  QB: TQuestBonusType;
begin
  try
    F := TIniFile.Create(FileName);
    for I := 0 to 9 do
    with F do
    begin
      S := IntToStr(I);
      if SectionExists(S) then
      begin
        P := Length(Quest);
        SetLength(Quest, P + 1);
        //
        if (ReadString(S, 'Type', '') = 'qtKill') then QT := qtKill;
        if (ReadString(S, 'Type', '') = 'qtItem') then QT := qtItem;
        if (ReadString(S, 'Type', '') = 'qtLoc') then QT := qtLoc;
        //
        if (ReadString(S, 'BonusType', '') = 'qbGold') then QB := qbGold;
        if (ReadString(S, 'BonusType', '') = 'qbExp') then QB := qbExp;
        if (ReadString(S, 'BonusType', '') = 'qbItem') then QB := qbItem;
        //
        Quest[P] := TQuest.Create(
          ReadString(S, 'Name', ''),
          ReadString(S, 'Description', ''),
          QT, ReadInteger(S, 'ObjectID', 0),
          ReadInteger(S, 'ObjectCount', 0),
          QB, ReadInteger(S, 'BonusID', 0),
          ReadInteger(S, 'BonusCount', 0)
          );
      end;
    end;
  finally
    F.Free;
  end;
end; 

function TQuests.Check(I: Byte): Boolean;

  procedure QuestDone(ID: Byte);
  begin
    // Квест выполнен
    Quests.Quest[ID].State := qsDone;
    AddMsg(Lang.Lang(2199));
    Result := True;
  end;

begin
  Result := False;
//  for I := 0 to Length(Quest) - 1 do
  if (Quests.Quest[I].State = qsQuest) then
  begin
    // Проверка условий
    case (Quests.Quest[I].QType) of
      qtKill:
        if (Quests.Quest[I].Progress > 0)
          and (Quests.Quest[I].Progress >=
            Quests.Quest[I].ObjectCount) then
              QuestDone(I);
      qtItem:
        if (Quests.Quest[I].ObjectCount > 0)
          and (Quests.Quest[I].ObjectCount <=
            PC.Items[Quests.Quest[I].ObjectID]) then
        begin
          Dec(PC.Items[Quests.Quest[I].ObjectID],
            Quests.Quest[I].ObjectCount);
          QuestDone(I);
        end;
      qtLoc:
        if (Quests.Quest[I].Progress > 0) then QuestDone(I);
    end; // case
  end;
end;

initialization
  Quests := TQuests.Create(SU.Path + 'quests');

finalization
  Quests.Free;

end.
