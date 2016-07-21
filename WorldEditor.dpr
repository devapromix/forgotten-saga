program WorldEditor;

uses
  Forms,
  Engine in 'Engine.pas',
  WorldEditor.MainForm in 'WorldEditor.MainForm.pas' {fMain},
  WorldEditor.Classes in 'WorldEditor.Classes.pas',
  Common.Color in 'Common.Color.pas',
  Common.Map.Tiles in 'Common.Map.Tiles.pas',
  Common.Map in 'Common.Map.pas',
  Common.Utils in 'Common.Utils.pas',
  ForgottenSaga.Game in 'ForgottenSaga.Game.pas',
  Common.Map.Generator in 'Common.Map.Generator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Forgotten Saga WorldEditor';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
