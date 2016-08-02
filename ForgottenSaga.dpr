program ForgottenSaga;

{$I Include.inc}

uses
  SysUtils, 
{$IFDEF USE_TERMINAL}
  BearLibTerminal in 'BearLibTerminal.pas',
{$ELSE}
  Forms,
  Engine in 'Engine.pas',
  ForgottenSaga.MainForm in 'ForgottenSaga.MainForm.pas' {fMain},
{$ENDIF}
  ForgottenSaga.Game in 'ForgottenSaga.Game.pas',
  ForgottenSaga.Inv in 'ForgottenSaga.Inv.pas',
  ForgottenSaga.Script in 'ForgottenSaga.Script.pas',
  ForgottenSaga.Creature in 'ForgottenSaga.Creature.pas',
  ForgottenSaga.Scenes in 'ForgottenSaga.Scenes.pas',
  ForgottenSaga.Battle in 'ForgottenSaga.Battle.pas',
  Common.Utils in 'Common.Utils.pas',
  Common.Map in 'Common.Map.pas',
  Common.Map.Generator in 'Common.Map.Generator.pas',
  Common.Map.Tiles in 'Common.Map.Tiles.pas',
  Common.Color in 'Common.Color.pas';

//{$IFNDEF USE_TERMINAL}
{$R *.res}
//{$ENDIF}

begin
{$IFDEF USE_TERMINAL}
  terminal_open();
  terminal_set(Format('window: size=%dx%d', [MapWidth + PanelWidth, MapHeight]));
  terminal_print(1, 1, 'Hello, world!');
  terminal_refresh();
  while (terminal_read() <> TK_CLOSE) do
  begin

  end;
  terminal_close();
{$ELSE}
  Application.Initialize;
  Application.Title := 'Forgotten Saga';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
{$ENDIF}
end.
