unit ForgottenSaga.MainForm;

interface

{$I Include.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfMain = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

uses Engine, ForgottenSaga.Classes, ForgottenSaga.Entities, ForgottenSaga.Scenes;

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  Saga := TSaga.Create(TMap.Size.Width + TUI.PanelWidth, TMap.Size.Height);
  Saga.Init;
  ClientWidth := Saga.Engine.Surface.Width;
  ClientHeight := Saga.Engine.Surface.Height;
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
  Application.Title := __('Forgotten Saga');
  Caption := Application.Title;
end;

procedure TfMain.FormPaint(Sender: TObject);
begin
  Saga.Stages.Render;
  Canvas.Draw(0, 0, Saga.Engine.Surface);
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Saga.Stages.Update(Key);
  FormPaint(Sender);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  Saga.Free;
end;

procedure TfMain.TimerTimer(Sender: TObject);
begin
  Saga.Stages.Timer;
  if (Saga.Notification.Counter = 0) then
    Exit;
  FormPaint(Sender);
end;

end.
