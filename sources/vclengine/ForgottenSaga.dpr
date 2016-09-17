program ForgottenSaga;

uses
  Forms,
  Engine in 'Engine.pas',
  ForgottenSaga.MainForm in 'ForgottenSaga.MainForm.pas' {fMain},
  ForgottenSaga.Classes in '..\ForgottenSaga.Classes.pas',
  ForgottenSaga.Entities in '..\ForgottenSaga.Entities.pas',
  ForgottenSaga.Scenes in '..\ForgottenSaga.Scenes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Forgotten Saga';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
