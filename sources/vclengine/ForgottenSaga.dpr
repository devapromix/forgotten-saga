program ForgottenSaga;

uses
  Forms,
  Engine in 'Engine.pas',
  ForgottenSaga.MainForm in 'ForgottenSaga.MainForm.pas' {fMain},
  ForgottenSaga.Game in '..\ForgottenSaga.Game.pas',
  ForgottenSaga.Inv in '..\ForgottenSaga.Inv.pas',
  ForgottenSaga.Script in '..\ForgottenSaga.Script.pas',
  ForgottenSaga.Creature in '..\ForgottenSaga.Creature.pas',
  ForgottenSaga.Scenes in '..\ForgottenSaga.Scenes.pas',
  ForgottenSaga.Battle in '..\ForgottenSaga.Battle.pas',
  Common.Utils in '..\Common.Utils.pas',
  Common.Map in '..\Common.Map.pas',
  Common.Map.Generator in '..\Common.Map.Generator.pas',
  Common.Map.Tiles in '..\Common.Map.Tiles.pas',
  Common.Color in '..\Common.Color.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Forgotten Saga';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
