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

{$IFNDEF USE_TERMINAL}
{$R *.res}
{$ENDIF}

{$IFDEF USE_TERMINAL}
var
  Key: Word;
{$ENDIF}

begin
{$IFDEF USE_TERMINAL}
  terminal_open();
  Saga := TSaga.Create(MapWidth + PanelWidth, MapHeight);
  Saga.Init;
  terminal_set(Format('window.title=%s', [__('Forgotten Saga')]));
  terminal_refresh;
  repeat
    Key := 0;
    Key := terminal_read();
    if (Key = 0) then Continue;
    if Key = $28 then Key := 13;
    if Key = $29 then Key := 27;
    terminal_clear();
    Saga.Stages.Render;
    Saga.Stages.Update(Key);
    terminal_refresh;
  until (Key = TK_CLOSE);
  terminal_close();
  Saga.Free;
{$ELSE}
  Application.Initialize;
  Application.Title := 'Forgotten Saga';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
{$ENDIF}
end.
