program ForgottenSaga;

uses
  SysUtils,
  Interfaces,
  Engine in 'Engine.pas',
  BearLibTerminal in 'BearLibTerminal.pas',
  ForgottenSaga.Classes in '..\ForgottenSaga.Classes.pas',
  ForgottenSaga.Entities in '..\ForgottenSaga.Entities.pas',
  ForgottenSaga.Scenes in '..\ForgottenSaga.Scenes.pas';

var
  Key: Word = 0;
  IsRender: Boolean = True;

begin
  terminal_open();
  Saga := TSaga.Create(TMap.Size.Width + TUI.PanelWidth, TMap.Size.Height);
  try
    Saga.Refresh;
    terminal_set(Format('window.title=%s', ['Forgotten Saga']));
    Saga.Stages.Render;
    terminal_refresh();
    repeat
      if IsRender then Saga.Stages.Render;
      Key := 0;
      if terminal_has_input() then
      begin
        Key := terminal_read();
        Saga.Stages.Update(Key);
        IsRender := True;
        Continue;
      end;
      if (Saga.Notification.Tick > 99) then
      begin
        Saga.Stages.Timer;
        Saga.Notification.Tick := 0;
        IsRender := True;
        Continue;
      end;
      if IsRender then terminal_refresh();
      Saga.Notification.Tick := Saga.Notification.Tick + 1;
      terminal_delay(10);
      IsRender := False;
    until (Key = TK_CLOSE);
    terminal_close();
  finally
    Saga.Free;
  end;

end.
