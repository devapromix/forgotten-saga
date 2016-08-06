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
  Common.Color in '..\Common.Color.pas';

var
  Key: Word;

begin
  terminal_open();
  Saga := TSaga.Create(MapWidth + PanelWidth, MapHeight);
  try
    Saga.Init;
    terminal_set(Format('window.title=%s', [__('Forgotten Saga')]));
    terminal_refresh();
    repeat
      Saga.Stages.Render;
      Key := Saga.Engine.GetKey;
      Saga.Stages.Update(Key);
      terminal_refresh();
    until (Key = TK_CLOSE);
    terminal_close();
  finally
    Saga.Free;
  end;
end.
