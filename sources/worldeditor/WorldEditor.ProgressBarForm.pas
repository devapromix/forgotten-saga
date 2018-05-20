unit WorldEditor.ProgressBarForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TNotifyEvent = procedure of object;

type
  TfProgressBar = class(TForm)
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FIsFinish: Boolean;
    FSpeed: Integer;
    procedure Clear;
    procedure Finish;
    procedure SetIsFinish(const Value: Boolean);
    procedure SetSpeed(const Value: Integer);
  public
    OnAct: TNotifyEvent;
    property IsFinish: Boolean read FIsFinish write SetIsFinish;
    property Speed: Integer read FSpeed write SetSpeed;
    procedure Start;
  end;

var
  fProgressBar: TfProgressBar;

implementation

uses Math;

{$R *.dfm}
{ TfProgressBar }

procedure TfProgressBar.Clear;
begin
  IsFinish := False;
  ProgressBar1.Position := 0;
end;

procedure TfProgressBar.Finish;
begin
  IsFinish := True;
  Timer1.Enabled := False;
  ModalResult := mrOk;
  if Assigned(OnAct) then
    OnAct;
end;

procedure TfProgressBar.FormCreate(Sender: TObject);
begin
  Clear;
  Speed := 1;
  Timer1.Enabled := False;
end;

procedure TfProgressBar.SetIsFinish(const Value: Boolean);
begin
  FIsFinish := Value;
end;

procedure TfProgressBar.SetSpeed(const Value: Integer);
begin
  FSpeed := Math.EnsureRange(Value, 1, 100);
end;

procedure TfProgressBar.Start;
begin
  Clear;
  Timer1.Interval := 100 div Speed;
  Timer1.Enabled := True;
end;

procedure TfProgressBar.Timer1Timer(Sender: TObject);
begin
  if ProgressBar1.Position >= ProgressBar1.Max then
    Finish;
  ProgressBar1.Position := ProgressBar1.Position + 1;
end;

end.
