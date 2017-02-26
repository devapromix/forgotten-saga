unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfMain = class(TForm)
    DoTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
    procedure DoTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

uses uInit, uKeys, uDraw, uConst, uDungeon, uTypes, uUtils, uScreenshot;

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  Init();
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) then Keys(Key, Shift) else Keys(Key);
end;

procedure TfMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Keys(Key);
end;

procedure TfMain.FormPaint(Sender: TObject);
begin
  Draw();
end;

procedure TfMain.DoTimerTimer(Sender: TObject);
begin
  if GameFrame = gfName then
  begin
    ShowCursor := not ShowCursor;
    Draw();
  end;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Cave);
end;

procedure TfMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 44) then TakeScreenShot();
end;

end.
