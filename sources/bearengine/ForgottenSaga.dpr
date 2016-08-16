program ForgottenSaga;

uses
  SysUtils,
  Engine in 'Engine.pas',
  BearLibTerminal in 'BearLibTerminal.pas',
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
  Common.Color in '..\Common.Color.pas',
  Common.Variables in '..\Common.Variables.pas';

var
  Key: Word = 0;
  Tick: Integer = 0;

begin
  terminal_open();
  Saga := TSaga.Create(MapWidth + PanelWidth, MapHeight);
  try
    Saga.Init;
    terminal_set(Format('window.title=%s', [__('Forgotten Saga')]));
    Saga.Stages.Render;
    terminal_refresh();
    repeat
      Saga.Stages.Render;
      Key := 0;
      if terminal_has_input() then
        Key := terminal_read();
      Saga.Stages.Update(Key);
      if (Tick > 59) then
      begin
        Saga.Stages.Timer;
        Tick := 0;
      end;
      terminal_refresh();
      Inc(Tick);
      terminal_delay(1);
    until (Key = TK_CLOSE);
    terminal_close();
  finally
    Saga.Free;
  end;

end.
