unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Math;

type
  TDamage = record
    Min, Max: Integer;
  end;

type
  TMob = record
    Name: string;
    Life: Integer;
    Damage: TDamage;
    Agility: Integer;
  end;

var
  Player, Enemy: TMob;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  with Player do
  begin
    Name := 'Player';
    Life := 50;
    Damage.Min := 5;
    Damage.Max := 10;
    Agility := 10;
  end;
  with Enemy do
  begin
    Name := 'Enemy';
    Life := 50;
    Damage.Min := 5;
    Damage.Max := 10;
    Agility := 10;
  end;
end;

type
  TMobs = array of TMob;

function AttackMob(var A, B: TMob): string;
var
  I, D: Integer;
begin
  if (A.Life = 0) or (B.Life = 0) then Exit;
  if (RandomRange(0, A.Agility) < RandomRange(0, B.Agility)) then
  begin
    Result := Format('%s miss. ', [A.Name]);
    Exit;
  end;
  D := Math.RandomRange(A.Damage.Min, A.Damage.Max + 1);
  B.Life := B.Life - D;
  if (B.Life < 0) then B.Life := 0;
  Result := Format('%s=>%s(%d). ', [A.Name, B.Name, D]);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S: string;
begin
  S := S + AttackMob(Player, Enemy);
  S := S + AttackMob(Enemy, Player);
  Form1.RichEdit1.Lines.Add(Trim(S));
  Label1.Caption := Format('Player Life: %d', [Player.Life]);
  Label2.Caption := Format('Enemy Life: %d', [Enemy.Life]);
end;

end.
