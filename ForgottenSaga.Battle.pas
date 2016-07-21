unit ForgottenSaga.Battle;

interface

type
  TBattle = class(TObject)
  private
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

implementation  

uses SysUtils, Math, Engine, ForgottenSaga.Game, ForgottenSaga.Scenes;

{ TBattle }

constructor TBattle.Create;
begin

end;

destructor TBattle.Destroy;
begin

  inherited;
end;

procedure TBattle.EnemyMove;
begin
  Saga.Log[lgBattle].Add(Format('%s атакует теб€ (%d).', [EnemyName, 5]));
end;

function TBattle.EnemyName: string;
begin
  Result := Saga.World.CurrentCreatures.Get(ID).Name;
end;

procedure TBattle.Finish;
begin
  Saga.Stages.SetStage(stGame);
  Saga.Log[lgGame].Add('“ы вышел из бо€.');
end;

procedure TBattle.PlayerMove;
begin
  Saga.Log[lgBattle].Add(Format('“ы атакуешь %s (%d)', [EnemyName, 5]));
  EnemyMove();
end;

procedure TBattle.Start(ID: Byte);
begin
  Self.ID := ID;
  Saga.Log[lgBattle].Clear;
  if (Math.RandomRange(1, 3) = 1) then
  begin
    Saga.Log[lgBattle].Add('“вои навыки позвол€ют тебе атаковать первым.');
  end
  else
  begin
    Saga.Log[lgBattle].Add(Format('%s неожиданно нападает на теб€ первым.', [EnemyName]));
    EnemyMove();
  end;
end;

end.
