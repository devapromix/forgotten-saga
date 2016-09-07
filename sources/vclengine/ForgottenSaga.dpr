program ForgottenSaga;

uses
  Forms,
  Engine in 'Engine.pas',
  ForgottenSaga.MainForm in 'ForgottenSaga.MainForm.pas' {fMain},
  ForgottenSaga.Classes in '..\ForgottenSaga.Classes.pas',
  ForgottenSaga.Inv in '..\ForgottenSaga.Inv.pas',
  ForgottenSaga.Entities in '..\ForgottenSaga.Entities.pas',
  ForgottenSaga.Scenes in '..\ForgottenSaga.Scenes.pas',
  Common.Map.Generator in '..\Common.Map.Generator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Forgotten Saga';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
