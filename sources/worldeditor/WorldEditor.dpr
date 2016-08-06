program WorldEditor;

uses
  Forms,
  Engine in '..\VCLEngine\Engine.pas',
  WorldEditor.MainForm in 'WorldEditor.MainForm.pas' {fMain},
  WorldEditor.Classes in 'WorldEditor.Classes.pas',
  ForgottenSaga.Game in '..\ForgottenSaga.Game.pas',
  ForgottenSaga.Scenes in '..\ForgottenSaga.Scenes.pas',
  ForgottenSaga.Script in '..\ForgottenSaga.Script.pas',
  Common.Color in '..\Common.Color.pas',
  Common.Map.Tiles in '..\Common.Map.Tiles.pas',
  Common.Map in '..\Common.Map.pas',
  Common.Utils in '..\Common.Utils.pas',
  Common.Map.Generator in '..\Common.Map.Generator.pas',
  ForgottenSaga.Creature in '..\ForgottenSaga.Creature.pas',
  ForgottenSaga.Battle in '..\ForgottenSaga.Battle.pas',
  ForgottenSaga.Inv in '..\ForgottenSaga.Inv.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Forgotten Saga WorldEditor';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
